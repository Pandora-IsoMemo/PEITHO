#' Workflow Files UI
#'
#' @param id Module ID
#' @param title Title to display above the file tree
#' @return Shiny UI elements for the workflow files tab
#' @export
workflow_files_ui <- function(id, title = "") {
  ns <- NS(id)

  tagList(
    if (nzchar(title)) tags$h4(title) else NULL,
    br(),
    fluidRow(
      column(
        width = 4,
        htmlOutput(ns("wf_dir")),
        shinyTree::shinyTree(ns("file_tree"), search = FALSE, theme = "proton")
      ),
      column(
        width = 8,
        tags$strong(textOutput(ns("selected_file_label"), inline = TRUE)),
        br(),
        textAreaInput(
          ns("file_editor"),
          label = NULL,
          value = "",
          width = "100%",
          height = "420px",
          resize = "vertical"
        ),
        br(),
        actionButton(ns("save_file"), "Save file", icon = icon("save")),
        actionButton(ns("show_defaults"), "Show package defaults", icon = icon("book")),
        br(),
        htmlOutput(ns("editor_status"))
      )
    )
  )
}

#' Workflow Files Server
#'
#' @param id Module ID
#' @param active_dir Reactive expression containing the active workflow directory path
#' @return Shiny server logic for rendering the workflow file tree
#' @export
workflow_files_server <- function(id, active_dir) {
  moduleServer(id, function(input, output, session) {
    selected_file <- reactiveVal(NULL)
    selected_label <- reactiveVal(NULL)

    build_file_tree <- function(path) {
      entries <- list.files(path, full.names = TRUE, no.. = TRUE)
      tree <- lapply(entries, function(entry) {
        if (dir.exists(entry)) {
          structure(build_file_tree(entry), sticon = "folder")
        } else {
          structure("", sticon = "file")
        }
      })
      names(tree) <- basename(entries)
      tree
    }

    resolve_selected_file <- function(root_path, selected_slice) {
      selected_slice_to_parts <- function(x) {
        if (is.null(x) || length(x) == 0L) return(character(0))

        if (is.character(x) && is.null(names(x))) {
          return(x)
        }

        if (is.list(x)) {
          node_name <- x[["_names"]]
          if (is.character(node_name) && length(node_name) == 1L && nzchar(node_name)) {
            child <- x[[node_name]]
            return(c(node_name, selected_slice_to_parts(child)))
          }

          child_names <- setdiff(names(x), "_names")
          if (length(child_names) == 1L) {
            node_name <- child_names[[1]]
            child <- x[[node_name]]
            return(c(node_name, selected_slice_to_parts(child)))
          }
        }

        character(0)
      }

      selected_parts <- selected_slice_to_parts(selected_slice)
      if (length(selected_parts) == 0L) return(NULL)

      candidate <- do.call(file.path, as.list(c(root_path, selected_parts)))
      root_norm <- normalizePath(root_path, winslash = "/", mustWork = TRUE)
      candidate_norm <- normalizePath(candidate, winslash = "/", mustWork = FALSE)

      in_root <- identical(candidate_norm, root_norm) ||
        startsWith(candidate_norm, paste0(root_norm, "/"))
      if (!in_root || !file.exists(candidate_norm) || dir.exists(candidate_norm)) {
        return(NULL)
      }

      candidate_norm
    }

    can_display_file <- function(path) {
      ext <- tolower(tools::file_ext(path))
      ext %in% c("r", "txt", "md", "csv", "yaml", "yml", "json")
    }

    can_edit_file <- function(path) {
      name <- basename(path)
      can_display_file(path) &&
        grepl("inputs|functions|commands", name, ignore.case = TRUE)
    }

    output$wf_dir <- renderText({
      req(active_dir(), dir.exists(active_dir()))
      paste(basename(active_dir()))
    })

    output$file_tree <- shinyTree::renderTree({
      req(active_dir(), dir.exists(active_dir()))
      build_file_tree(active_dir())
    })

    output$selected_file_label <- renderText({
      label_override <- selected_label()
      if (!is.null(label_override) && nzchar(label_override)) {
        return(paste("Selected file:", label_override))
      }

      path <- selected_file()
      if (is.null(path)) {
        "No file selected"
      } else {
        paste("Selected file:", basename(path))
      }
    })

    output$editor_status <- renderText("")

    observeEvent(input$file_tree, {
      req(active_dir(), dir.exists(active_dir()))

      selected_slices <- shinyTree::get_selected(input$file_tree, format = "slices")
      req(length(selected_slices) == 1L)

      path <- resolve_selected_file(active_dir(), selected_slices[[1]])
      req(!is.null(path))

      if (!can_display_file(path)) {
        selected_file(NULL)
        selected_label(NULL)
        updateTextAreaInput(session, "file_editor", value = "")
        output$editor_status <- renderText("Selected file type is not supported in this view.")
        return()
      }

      content <- tryCatch(
        paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
        error = function(e) {
          output$editor_status <- renderText(
            paste("Could not read file:", conditionMessage(e))
          )
          NULL
        }
      )
      req(!is.null(content))

      updateTextAreaInput(session, "file_editor", value = content)
      if (can_edit_file(path)) {
        selected_file(path)
        selected_label(NULL)
        output$editor_status <- renderText("")
      } else {
        selected_file(NULL)
        selected_label(basename(path))
        output$editor_status <- renderText("Read-only file: display only.")
      }
    })

    observeEvent(input$show_defaults, {
      defaults_text <- tryCatch(
        PEITHO:::default_workflow_functions_text(),
        error = function(e) {
          output$editor_status <- renderText(
            paste("Could not load package defaults:", conditionMessage(e))
          )
          NULL
        }
      )
      req(!is.null(defaults_text))

      selected_file(NULL)
      selected_label("PEITHO package defaults")
      updateTextAreaInput(session, "file_editor", value = defaults_text)
      gh_url <- "https://github.com/Pandora-IsoMemo/PEITHO/blob/main/R/01-default-functions.R"
      output$editor_status <- renderText(
        HTML(paste(
          "Read-only view of the currently loaded default function definitions.",
          sprintf("<a href='%s' target='_blank'>View original source on GitHub</a>", gh_url),
          sep = " | "
        ))
      )
    })

    observeEvent(input$save_file, {
      path <- selected_file()
      if (is.null(path)) {
        output$editor_status <- renderText(
          "This file is read-only. Only inputs/functions/commands files can be edited."
        )
        return()
      }
      req(file.exists(path), !dir.exists(path))

      value <- input$file_editor
      if (is.null(value)) value <- ""
      lines <- strsplit(value, "\n", fixed = TRUE)[[1]]

      tryCatch({
        writeLines(lines, con = path, useBytes = TRUE)
        output$editor_status <- renderText(
          sprintf("Saved %s", basename(path))
        )
      }, error = function(e) {
        output$editor_status <- renderText(
          paste("Could not save file:", conditionMessage(e))
        )
      })
    })
  })
}