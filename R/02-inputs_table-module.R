#' Inputs Table UI
#'
#' @param id Module ID
#' @param title Title to display above the table
#' @return Shiny UI elements for the inputs table
#' @export
inputs_table_ui <- function(id, title = "") {
  ns <- NS(id)
  tagList(
    tags$h4(title),
    fluidRow(
      column(
        width = 4,
        selectInput(ns("selected_row"), "Select row", choices = NULL, width = "100%")
      ),
      column(
        width = 4,
        br(),
        div(
          class = "d-flex justify-content-end gap-2",
          actionButton(ns("add_after"), "Add new row after", icon = icon("plus")),
          actionButton(ns("remove_row"), "Remove row",    icon = icon("trash"))
        )
      )
    ),
    br(),
    DT::DTOutput(ns("tbl"))
  )
}

#' Inputs Table Server
#'
#' @param id Module ID
#' @param wf Reactive expression containing the workflow object
#' @return Shiny server logic for rendering the inputs table
#' @export
inputs_table_server <- function(id, wf) {
  moduleServer(id, function(input, output, session) {
    output$tbl <- DT::renderDT({
      PEITHO:::logDebug("%s: Render inputs table", id)
      wf_val <- wf()
      if (is.null(wf_val)) return(NULL)
      inputs <- wf_val$input_list
      if (is.null(inputs) || length(inputs) == 0) return(NULL)
      df <- data.frame(
        Name = names(inputs),
        Value = unlist(inputs, use.names = FALSE),
        stringsAsFactors = FALSE
      )
      DT::datatable(
        df,
        rownames = FALSE, # do NOT change to TRUE: the row indices will be messed up when editing
        options = list(
          dom = "t",              # hide default search/length/pagination row
          paging = FALSE,
          scrollY = "360px",
          scrollCollapse = TRUE
        ),
        editable = "cell"
      ) |>
        DT::formatStyle(
          columns = names(df),
          `white-space` = "pre-wrap"
        )
    })

    observe({
      wf_val <- wf()
      if (is.null(wf_val)) {
        updateSelectInput(session, "selected_row", choices = character(0), selected = character(0))
        return()
      }

      input_list_val <- wf_val$input_list
      if (is.null(input_list_val) || length(input_list_val) == 0L) {
        updateSelectInput(session, "selected_row", choices = character(0), selected = character(0))
        return()
      }

      df <- data.frame(
        Name = names(input_list_val),
        Value = unlist(input_list_val, use.names = FALSE),
        stringsAsFactors = FALSE
      )

      selected <- resolve_selected_row(isolate(input$selected_row), n_rows = nrow(df))
      choices <- build_row_selector_choices(df)

      updateSelectInput(
        session,
        "selected_row",
        choices = choices,
        selected = selected
      )
    })

    observeEvent(input$tbl_cell_edit, {
      wf_val <- wf()
      if (is.null(wf_val)) return()

      info <- input$tbl_cell_edit
      row_idx <- info$row
      col_idx <- as.integer(info$col + 1L)

      input_list_val <- wf_val$input_list
      if (is.null(input_list_val) || length(input_list_val) == 0) return()
      if (row_idx < 1 || row_idx > length(input_list_val)) return()
      if (!(col_idx %in% c(1L, 2L))) return()

      if (col_idx == 1) {
        old_name <- names(input_list_val)[row_idx]
        new_name <- trimws(as.character(info$value))

        if (identical(new_name, "")) {
          showNotification("Input name cannot be empty.", type = "error")
          return()
        }

        if (!identical(old_name, new_name) && new_name %in% names(input_list_val)) {
          showNotification(
            sprintf("Name '%s' already exists. Please choose a different name.", new_name),
            type = "error"
          )
          return()
        }

        names(input_list_val)[row_idx] <- new_name
      } else {
        input_name <- names(input_list_val)[row_idx]
        old_value <- input_list_val[[input_name]]
        input_list_val[[input_name]] <- DT::coerceValue(info$value, old_value)
      }

      PEITHO:::logDebug("%s: Update workflow input list via table cell edit", id)
      wf_val <- PEITHO:::update_input_list(
        x = wf_val,
        new_list = input_list_val
      ) |>
        shinyTools::shinyTryCatch(
          errorTitle = "Updating workflow input list failed",
          warningTitle = "Updating workflow input list warning"
        )

      wf(wf_val)
    })

    observeEvent(input$remove_row, {
      wf_val <- wf()
      if (is.null(wf_val)) return()

      input_list_val <- wf_val$input_list
      n <- length(input_list_val)
      if (n == 0L) return()

      row_idx <- as.integer(input$selected_row)
      if (is.na(row_idx) || row_idx < 1L || row_idx > n) return()

      input_list_val <- input_list_val[-row_idx]

      PEITHO:::logDebug("%s: Remove workflow input at row %d", id, row_idx)
      wf_val <- PEITHO:::update_input_list(
        x = wf_val,
        new_list = input_list_val
      ) |>
        shinyTools::shinyTryCatch(
          errorTitle = "Removing workflow input failed",
          warningTitle = "Removing workflow input warning"
        )

      wf(wf_val)
    })

    observeEvent(input$add_after, {
      wf_val <- wf()
      if (is.null(wf_val)) return()

      input_list_val <- wf_val$input_list %||% list()
      n <- length(input_list_val)

      row_idx <- as.integer(input$selected_row)
      if (n == 0L) {
        row_idx <- 0L
      } else if (is.na(row_idx) || row_idx < 1L || row_idx > n) {
        return()
      }

      new_name <- next_unique_input_name(names(input_list_val))
      new_item <- list("")
      names(new_item) <- new_name
      input_list_val <- append(input_list_val, new_item, after = row_idx)

      PEITHO:::logDebug("%s: Add workflow input '%s' after row %d", id, new_name, row_idx)
      wf_val <- PEITHO:::update_input_list(
        x = wf_val,
        new_list = input_list_val
      ) |>
        shinyTools::shinyTryCatch(
          errorTitle = "Adding workflow input failed",
          warningTitle = "Adding workflow input warning"
        )

      wf(wf_val)
    })
  })
}