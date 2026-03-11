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
    DT::DTOutput(ns("tbl")),
    uiOutput(ns("edit"))
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
        rownames = TRUE,
        options = list(pageLength = 10),
        editable = "cell"
      )
    })

    observeEvent(input$tbl_cell_edit, {
      wf_val <- wf()
      if (is.null(wf_val)) return()

      info <- input$tbl_cell_edit
      row_idx <- info$row
      col_idx_raw <- info$col

      wf_df <- as.data.frame(wf_val)
      if (row_idx < 1 || row_idx > nrow(wf_df)) return()

      # DT edit column index can shift with table config (e.g. rownames on/off),
      # so support both 0-based and 1-based indices to keep this robust.
      n_cols <- ncol(wf_df)
      if (col_idx_raw >= 0 && col_idx_raw < n_cols) {
        col_idx <- as.integer(col_idx_raw + 1L)
      } else if (col_idx_raw >= 1 && col_idx_raw <= n_cols) {
        col_idx <- as.integer(col_idx_raw)
      } else {
        return()
      }

      field_name <- colnames(wf_df)[col_idx]
      old_value <- wf_df[[row_idx, col_idx]]
      new_value <- DT::coerceValue(info$value, old_value)

      wf_val <- update(wf_val, row_idx, field_name, new_value) |>
        shinyTryCatch(errorTitle = "Editing Workflow failed")

      wf(wf_val)
    })

    output$edit <- renderUI({
      # hide the edit UI when no workflow is loaded
      if (is.null(wf())) return(NULL)

      workflow_edit_ui(ns("edit"))
    })

    workflow_edit_server("edit", wf, is_active_tab)
  })
}

workflow_edit_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$hr(),
    tags$h4("Edit Workflow Step"),
    fluidRow(
      column(
        3,
        selectInput(ns("step_select"), "Select step", choices = NULL, selected = NULL)
      ),
      column(
        3,
        selectInput(ns("field_select"), "Select field", choices = NULL, selected = NULL)
      ),
      column(
        4,
        style = "margin-top: 1em;",
        textInput(ns("new_value"), label = NULL, value = "", width = "100%")
      ),
      column(
        2,
        align = "left",
        style = "margin-top: 1em;",
        actionButton(ns("edit_btn"), "Edit")
      )
    )
  )
}

workflow_edit_server <- function(id, wf, is_active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      req(isTRUE(is_active_tab()))

      if (is.null(wf())) {
        PEITHO:::logDebug("%s: Reset inputs if empty wf", id)
        updateSelectInput(session, "step_select", choices = NULL, selected = NULL)
        updateSelectInput(session, "field_select", choices = NULL, selected = NULL)
      }
    })

    observe({
      req(isTRUE(is_active_tab()), wf())
      PEITHO:::logDebug("%s: Update inputs", id)

      wf_val <- wf()
      wf_df <- as.data.frame(wf_val)
      step_choices <- wf_df[["Name"]]
      if (isTRUE(input$step_select %in% step_choices)) {
        selected_input <- input$step_select
      } else {
        selected_input <- NULL
      }
      updateSelectInput(session, "step_select", choices = step_choices, selected = selected_input)

      field_choices <- colnames(wf_df)
      if (isTRUE(input$field_select %in% field_choices)) {
        selected_field <- input$field_select
      } else {
        selected_field <- NULL
      }
      updateSelectInput(session, "field_select", choices = field_choices, selected = selected_field)
    })

    observe({
      req(input$step_select)
      req(input$field_select)

      wf_val <- wf()
      if (is.null(wf_val)) return()

      wf_df <- as.data.frame(wf_val)

      step_idx <- which(wf_df[["Name"]] == input$step_select)
      if (length(step_idx) == 0) return()

      current_value <- wf_df[[step_idx, input$field_select]]
      updateTextInput(session, "new_value", value = as.character(current_value))
    }) |>
      bindEvent(input$step_select, input$field_select)

    observe({
      req(input$step_select)
      req(input$field_select)
      req(input$new_value)

      wf_val <- wf()
      if (is.null(wf_val)) return()

      step_idx <- which(as.data.frame(wf_val)[["Name"]] == input$step_select)
      if (length(step_idx) == 0) return()

      field_name <- input$field_select
      new_value <- input$new_value

      # Update the workflow object with the new value
      wf_val <- update(wf_val, step_idx, field_name, new_value) |>
        shinyTryCatch(errorTitle = "Editing Workflow failed")

      # Trigger reactive update by assigning the modified workflow back to the reactive value
      wf(wf_val)
    }) |>
      bindEvent(input$edit_btn)
  })
}