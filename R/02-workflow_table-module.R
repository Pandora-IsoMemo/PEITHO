#' Workflow Table UI
#'
#' @param id Module ID
#' @param title Title to display above the table
#' @return Shiny UI elements for the workflow table
#' @export
workflow_table_ui <- function(id, title = "") {
  ns <- NS(id)

  tagList(
    tags$h4(title),
    fluidRow(
      column(
        width = 4,
        style = "margin-bottom: -3em",
        selectInput(ns("selected_row"), "Select row", choices = NULL, width = "100%")
      ),
      column(
        width = 4,
        style = "margin-bottom: -3em",
        br(),
        div(
          class = "d-flex justify-content-end gap-2",
          actionButton(ns("add_after"), "Add new row after", icon = icon("plus")),
          actionButton(ns("remove_row"), "Remove row",    icon = icon("trash"))
        )
      )
    ),
    DT::DTOutput(ns("tbl"))
  )
}

#' Workflow Table Server
#'
#' @param id Module ID
#' @param wf Reactive expression containing the workflow object
#' @return Shiny server logic for rendering the workflow table
#' @export
workflow_table_server <- function(id, wf) {
  moduleServer(id, function(input, output, session) {
    output$tbl <- DT::renderDT({
      wf_val <- wf()
      if (is.null(wf_val)) return(NULL)
      DT::datatable(
        as.data.frame(wf_val),
        rownames = FALSE, # do NOT change to TRUE: the row indices will be messed up when editing
        options = list(
          paging = FALSE,
          scrollY = "360px",
          scrollCollapse = TRUE
        ),
        editable = list(
          target = "cell",
          disable = list(columns = c(0))  # except first column
        )
      )
    })

    observe({
      wf_val <- wf()
      if (is.null(wf_val)) {
        updateSelectInput(session, "selected_row", choices = character(0), selected = character(0))
        return()
      }

      wf_df <- as.data.frame(wf_val)
      n <- nrow(wf_df)
      if (n == 0L) {
        updateSelectInput(session, "selected_row", choices = character(0), selected = character(0))
        return()
      }

      selected <- isolate(input$selected_row)
      if (is.null(selected) || !nzchar(selected) || !selected %in% as.character(seq_len(n))) {
        selected <- as.character(n)
      }

      row_values <- as.character(seq_len(n))
      if ("Name" %in% colnames(wf_df)) {
        row_names <- as.character(wf_df$Name)
        row_names[is.na(row_names)] <- ""
        row_labels <- ifelse(
          nzchar(row_names),
          paste0(row_values, " - ", row_names),
          row_values
        )
      } else {
        row_labels <- row_values
      }

      updateSelectInput(
        session,
        "selected_row",
        choices = stats::setNames(row_values, row_labels),
        selected = selected
      )
    })

    observeEvent(input$tbl_cell_edit, {
      wf_val <- wf()
      if (is.null(wf_val)) return()

      info <- input$tbl_cell_edit
      row_idx <- info$row
      col_idx <- as.integer(info$col + 1L)

      wf_df <- as.data.frame(wf_val)
      if (row_idx < 1 || row_idx > nrow(wf_df)) return()
      if (col_idx < 1L || col_idx > ncol(wf_df)) return()

      field_name <- colnames(wf_df)[col_idx]
      old_value <- wf_df[[col_idx]][row_idx]
      new_value <- DT::coerceValue(info$value, old_value)

      wf_val <- update(wf_val, row_idx, field_name, new_value) |>
        shinyTryCatch(errorTitle = "Editing Workflow failed")

      wf(wf_val)
    })

    observeEvent(input$remove_row, {
      wf_val <- wf()
      if (is.null(wf_val)) return()

      row_idx <- as.integer(input$selected_row)
      wf_df <- as.data.frame(wf_val)
      if (is.na(row_idx) || row_idx < 1L || row_idx > nrow(wf_df)) return()

      wf_val <- remove_step(wf_val, row_idx) |>
        shinyTryCatch(errorTitle = "Removing step failed")
      wf(wf_val)
    })

    observeEvent(input$add_after, {
      wf_val <- wf()
      if (is.null(wf_val)) return()

      row_idx <- as.integer(input$selected_row)
      wf_df <- as.data.frame(wf_val)
      if (is.na(row_idx) || row_idx < 1L || row_idx > nrow(wf_df)) return()

      new_step <- new_workflowstep(
        entry   = row_idx + 1L,
        command = "identity",   # placeholder — user edits via cell
        name    = paste("Step", row_idx + 1L)
      )

      wf_val <- add_step(wf_val, new_step, position = row_idx + 1L) |>
        shinyTryCatch(errorTitle = "Adding step failed")
      wf(wf_val)
    })
  })
}