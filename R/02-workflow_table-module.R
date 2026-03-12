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
    DT::DTOutput(ns("tbl"))
  )
}

#' Workflow Table Server
#'
#' @param id Module ID
#' @param wf Reactive expression containing the workflow object
#' @param is_active_tab Reactive expression indicating whether the inputs tab is active
#' @return Shiny server logic for rendering the workflow table
#' @export
workflow_table_server <- function(id, wf, is_active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
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
        editable = "cell"
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
      old_value <- wf_df[[row_idx, col_idx]]
      new_value <- DT::coerceValue(info$value, old_value)

      wf_val <- update(wf_val, row_idx, field_name, new_value) |>
        shinyTryCatch(errorTitle = "Editing Workflow failed")

      wf(wf_val)
    })
  })
}