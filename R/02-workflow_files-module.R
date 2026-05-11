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
    fluidRow(
      column(
        width = 4,
        div(
          class = "well well-sm",
          tags$h4(icon("book"), "PEITHO defaults"),
          tags$p(
            "Inspect package defaults and copy them into workflow for customization.",
            class = "text-muted"
          ),
          actionButton(
            ns("show_defaults"),
            "Show",
            icon = icon("book"),
            width = "100%"
          ),
          tags$hr(),
          actionButton(
            ns("copy_defaults"),
            "Copy",
            icon = icon("copy"),
            class = "btn-danger btn-block"
          ),
          tags$p(
            "Overwrites custom functions in the workflow.",
            class = "text-danger small"
          ),
          actionButton(
            ns("reset_defaults"),
            "Reset workflow functions",
            icon = icon("undo"),
            class = "btn-danger btn-block"
          ),
          tags$p(
            "Removes custom function definitions from the workflow and reverts to package defaults.",
            class = "text-danger small"
          )
        ),

        div(
          class = "well well-sm",
          tags$h4(icon("list"), "All available functions"),
          tags$p(
            "Browse all functions available in the current session, including custom workflow functions.",
            class = "text-muted"
          ),
          actionButton(ns("browse_functions"), "Browse functions",
                       icon = icon("list"),
                       class = "btn-default btn-block")
        ),

        div(
          class = "well well-sm",
          tags$h4(icon("folder-open"), "Workflow files"),
          tags$p("Browse files in the current workflow.", class = "text-muted"),
          htmlOutput(ns("wf_dir")),
          shinyTree::shinyTree(ns("file_tree"), search = FALSE, theme = "proton")
        )
      ),
      column(
        width = 8,
        tags$p(
          "View selected content and edit supported workflow files.",
          class = "text-muted"
        ),
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
    status_msg <- reactiveVal("")

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
        file.exists(path) &&
        !dir.exists(path) &&
        grepl("inputs|functions|commands", name, ignore.case = TRUE) &&
        file.access(path, 2) == 0
    }

    get_functions_path <- function(dir_path = active_dir()) {
      if (is.null(dir_path) || !dir.exists(dir_path)) return(NULL)
      PEITHO:::workflow_file_paths(path = dir_path)$functions_path
    }

    write_text_file_utf8 <- function(path, value) {
      if (is.null(value)) value <- ""
      lines <- if (identical(value, "")) character(0) else strsplit(value, "\n", fixed = TRUE)[[1]]

      con <- file(path, open = "w", encoding = "UTF-8")
      on.exit(close(con), add = TRUE)
      writeLines(lines, con = con)
    }

    is_functions_file <- function(path) {
      functions_path <- get_functions_path(dirname(path))
      if (is.null(functions_path)) return(FALSE)

      identical(
        normalizePath(path, winslash = "/", mustWork = TRUE),
        normalizePath(functions_path, winslash = "/", mustWork = FALSE)
      )
    }

    set_functions_saved_status <- function(path) {
      if (is_running_online()) {
        status_msg(sprintf(
          "Saved %s. Warning: Running online; skipping loading %s into the environment!",
          basename(path),
          basename(path)
        ))
      } else {
        status_msg(sprintf(
          "Saved %s. Updated custom functions will be reloaded on the next workflow run.",
          basename(path)
        ))
      }
    }

    output$wf_dir <- renderUI({
      dir_path <- active_dir()
      if (is.null(dir_path) || !dir.exists(dir_path)) {
        return(tags$span(
          class = "text-muted",
          "Import a workflow or load the example."
        ))
      }

      tags$strong(sprintf("Workflow folder: %s", basename(dir_path)))
    })

    output$file_tree <- shinyTree::renderTree({
      dir_path <- active_dir()
      if (is.null(dir_path) || !dir.exists(dir_path)) {
        return(list())
      }

      build_file_tree(dir_path)
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

    output$editor_status <- renderUI(status_msg())

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
        status_msg("Selected file type is not supported in this view.")
        return()
      }

      content <- tryCatch(
        paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
        error = function(e) {
          status_msg(paste("Could not read file:", conditionMessage(e)))
          NULL
        }
      )
      req(!is.null(content))

      updateTextAreaInput(session, "file_editor", value = content)
      if (can_edit_file(path)) {
        selected_file(path)
        selected_label(NULL)
        status_msg("")
      } else {
        selected_file(NULL)
        selected_label(basename(path))
        status_msg("Read-only file: display only.")
      }
    })

    observeEvent(input$browse_functions, {
      extra_env <- NULL
      dir_path <- active_dir()

      if (!is.null(dir_path) && dir.exists(dir_path)) {
        functions_path <- get_functions_path(dir_path)

        if (file_nonempty(functions_path)) {
          extra_env <- tryCatch(
            PEITHO:::load_workflow_script_env(
              script_path = functions_path,
              parent_env = asNamespace("PEITHO"),
              show_functions_path = FALSE
            ),
            error = function(e) {
              status_msg(paste("Could not load custom functions:", conditionMessage(e)))
              NULL
            }
          )
        }
      }

      fn_df <- tryCatch(
        PEITHO:::available_functions_df(extra_env = extra_env),
        error = function(e) NULL
      )

      showModal(modalDialog(
        title = "Available functions from loaded namespace",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        if (is.null(fn_df) || nrow(fn_df) == 0L) {
          tags$p("No functions found.")
        } else {
          DT::DTOutput(session$ns("fn_browser_tbl"))
        }
      ))

      if (!is.null(fn_df) && nrow(fn_df) > 0L) {
        output$fn_browser_tbl <- DT::renderDT({
          DT::datatable(
            fn_df,
            rownames = FALSE,
            filter = "top",
            options = list(
              pageLength = 20,
              scrollY = "420px",
              scrollCollapse = TRUE,
              dom = "ftipr"
            )
          )
        })
      }
    })

    observeEvent(input$show_defaults, {
      defaults_text <- tryCatch(
        PEITHO:::default_workflow_functions_text(),
        error = function(e) {
          status_msg(paste("Could not load package defaults:", conditionMessage(e)))
          NULL
        }
      )
      req(!is.null(defaults_text))

      selected_file(NULL)
      selected_label("PEITHO package defaults")
      updateTextAreaInput(session, "file_editor", value = defaults_text)
      gh_url <- "https://github.com/Pandora-IsoMemo/PEITHO/blob/main/R/01-default-functions.R"
      status_msg(HTML(paste(
        "Read-only view of the currently loaded default function definitions.",
        sprintf("<a href='%s' target='_blank'>View original source on GitHub</a>", gh_url),
        sep = " | "
      )))
    })

    observeEvent(input$save_file, {
      path <- selected_file()
      if (is.null(path)) {
        status_msg("This file is read-only. Only inputs/functions/commands files can be edited.")
        return()
      }
      req(file.exists(path), !dir.exists(path))

      value <- input$file_editor

      tryCatch({
        write_text_file_utf8(path, value)

        if (is_functions_file(path)) {
          set_functions_saved_status(path)
        } else {
          status_msg(sprintf("Saved %s", basename(path)))
        }
      }, error = function(e) {
        status_msg(paste("Could not save file:", conditionMessage(e)))
      })
    })

    observeEvent(input$copy_defaults, {
      req(active_dir(), dir.exists(active_dir()))

      defaults_text <- tryCatch(
        PEITHO:::default_workflow_functions_text(),
        error = function(e) {
          status_msg(paste("Could not load package defaults:", conditionMessage(e)))
          NULL
        }
      )
      req(!is.null(defaults_text))

      functions_path <- get_functions_path()
      req(!is.null(functions_path))

      tryCatch({
        write_text_file_utf8(functions_path, defaults_text)

        selected_file(functions_path)
        selected_label(NULL)
        updateTextAreaInput(session, "file_editor", value = defaults_text)
        set_functions_saved_status(functions_path)
      }, error = function(e) {
        status_msg(paste("Could not copy defaults:", conditionMessage(e)))
      })
    })

    observeEvent(input$reset_defaults, {
      req(active_dir(), dir.exists(active_dir()))

      functions_path <- get_functions_path()
      req(!is.null(functions_path))

      tryCatch({
        write_text_file_utf8(functions_path, "")

        selected_file(functions_path)
        selected_label(NULL)
        updateTextAreaInput(session, "file_editor", value = "")
        status_msg("Cleared functions.R. Workflow will use package defaults on next run.")
      }, error = function(e) {
        status_msg(paste("Could not clear functions.R:", conditionMessage(e)))
      })
    })
  })
}