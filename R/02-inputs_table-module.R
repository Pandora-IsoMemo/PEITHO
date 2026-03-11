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
    DT::DTOutput(ns("tbl")),
    uiOutput(ns("edit"))
  )
}

#' Inputs Table Server
#'
#' @param id Module ID
#' @param wf Reactive expression containing the workflow object
#' @param is_active_tab Reactive expression indicating whether the inputs tab is active
#' @return Shiny server logic for rendering the inputs table
#' @export
inputs_table_server <- function(id, wf, is_active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$tbl <- DT::renderDT({
      PEITHO:::logDebug("%s: Render inputs table", id)
      wf_val <- wf()
      if (is.null(wf_val)) return(NULL)
      inputs <- wf_val$input_list
      if (is.null(inputs) || length(inputs) == 0) return(NULL)
      DT::datatable(
        data.frame(
          Name = names(inputs),
          Value = unlist(inputs, use.names = FALSE),
          stringsAsFactors = FALSE
        ),
        rownames = FALSE,
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

      input_list_val <- wf_val$input_list
      if (is.null(input_list_val) || length(input_list_val) == 0) return()
      if (row_idx < 1 || row_idx > length(input_list_val)) return()

      # DT edit column index can shift with table config (e.g. rownames on/off),
      # so support both 0-based and 1-based indices to keep this robust.
      n_cols <- 2L
      if (col_idx_raw >= 0 && col_idx_raw < n_cols) {
        col_idx <- as.integer(col_idx_raw + 1L)
      } else if (col_idx_raw >= 1 && col_idx_raw <= n_cols) {
        col_idx <- as.integer(col_idx_raw)
      } else {
        return()
      }

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

    output$edit <- renderUI({
      # hide the edit UI when no workflow is loaded or the inputs tab is not active
      if (is.null(wf()) || isFALSE(is_active_tab())) return(NULL)

      PEITHO:::logDebug("%s: Render inputs edit", id)
      input_edit_ui(ns("edit"))
    })

    input_edit_server("edit", wf, is_active_tab)
  })
}

input_edit_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$hr(),
    tags$h4("Edit Workflow Inputs"),
    fluidRow(
      column(
        3,
        selectInput(ns("input_select"), "Select input", choices = NULL, selected = NULL)
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

input_edit_server <- function(id, wf, is_active_tab) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(isTRUE(is_active_tab()))

      if (is.null(wf())) {
        PEITHO:::logDebug("%s: Reset inputs if empty wf", id)
        # hide the step select input when no workflow is loaded
        updateSelectInput(session, "input_select", choices = NULL, selected = NULL)
        updateSelectInput(session, "field_select", choices = NULL, selected = NULL)
      }
    })

    observe({
      req(isTRUE(is_active_tab()), wf())
      PEITHO:::logDebug("%s: Update inputs", id)

      wf_val <- wf()
      input_choices <- names(wf_val$input_list)

      if (isTRUE(input$input_select %in% input_choices)) {
        selected_input <- input$input_select
      } else {
        selected_input <- NULL
      }
      updateSelectInput(
        session, "input_select", choices = input_choices, selected = selected_input
      )

      field_choices <- c("Name", "Value")

      if (isTRUE(input$field_select %in% field_choices)) {
        selected_field <- input$field_select
      } else {
        selected_field <- NULL
      }
      updateSelectInput(
        session, "field_select", choices = field_choices, selected = selected_field
      )
    })

    observe({
      req(input$input_select, input$field_select)

      wf_val <- wf()
      if (is.null(wf_val)) return()

      input_list <- wf_val$input_list

      current_value <- switch(
        input$field_select,
        "Name" = input$input_select,
        "Value" = input_list[[input$input_select]],
        NULL
      )

      PEITHO:::logDebug("%s: Update new_value text input", id)
      updateTextInput(session, "new_value", value = as.character(current_value))
    })

    observe({
      req(input$input_select)
      req(input$field_select)
      req(input$new_value)

      wf_val <- wf()
      if (is.null(wf_val)) return()

      input_list_val <- wf_val$input_list

      if (input$field_select == "Name") {
        old_name <- input$input_select
        new_name <- trimws(as.character(input$new_value))

        idx <- match(old_name, names(input_list_val))
        if (is.na(idx)) return()

        # prevent duplicates (unless it's the same name)
        if (!identical(old_name, new_name) && new_name %in% names(input_list_val)) {
          showNotification(
            sprintf("Name '%s' already exists. Please choose a different name.", new_name),
            type = "error"
          )
          return()
        }

        # Rename the input in the input list
        names(input_list_val)[idx] <- new_name
      } else if (input$field_select == "Value") {
        # Update the value of the selected input
        input_list_val[[input$input_select]] <- input$new_value
      }

      PEITHO:::logDebug("%s: Update workflow input list", id)
      wf_val <- PEITHO:::update_input_list(
        x = wf_val,
        new_list = input_list_val
      ) |>
        shinyTools::shinyTryCatch(
          errorTitle = "Updating workflow input list failed",
          warningTitle = "Updating workflow input list warning"
        )

      # Trigger reactive update by assigning the modified workflow back to the reactive value
      wf(wf_val)
    }) |>
      bindEvent(input$edit_btn)
  })
}