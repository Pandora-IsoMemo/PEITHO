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
  })
}