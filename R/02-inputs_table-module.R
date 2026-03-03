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
    tableOutput(ns("tbl")),
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
    output$tbl <- renderTable({
      PEITHO:::logDebug("%s: Render inputs table", id)
      wf_val <- wf()
      if (is.null(wf_val)) return(NULL)
      inputs <- extract_inputs(wf_val)
      if (is.null(inputs) || length(inputs) == 0) return(NULL)
      data.frame(
        name = names(inputs),
        value = unlist(inputs, use.names = FALSE),
        stringsAsFactors = FALSE
      )
    }, rownames = FALSE)

    output$edit <- renderUI({
      # hide the edit UI when no workflow is loaded or the inputs tab is not active
      if (is.null(wf()) || !is_active_tab()) return(NULL)

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

    # TO DO: integrate update_input_list.workflow()

    observe({
      req(isTRUE(is_active_tab()))
      PEITHO:::logDebug("%s: Update input_select", id)

      wf_val <- wf()
      if (is.null(wf_val)) {
        # hide the step select input when no workflow is loaded
        updateSelectInput(session, "input_select", choices = NULL, selected = NULL)
      } else {
        input_choices <- names(wf_val$input_list)
        updateSelectInput(session, "input_select", choices = input_choices, selected = NULL)
      }
    })

    observe({
      req(input$input_select)

      wf_val <- wf()
      if (is.null(wf_val)) return()

      current_value <- wf_val$input_list[[input$input_select]]
      PEITHO:::logDebug("%s: Update new_value text input", id)
      updateTextInput(session, "new_value", value = as.character(current_value))
    }) |>
      bindEvent(input$input_select)

    observe({
      req(input$input_select)
      req(input$new_value)

      wf_val <- wf()
      if (is.null(wf_val)) return()

      input_list_val <- wf_val$input_list

      input_list_val[[input$input_select]] <- input$new_value

      PEITHO:::logDebug("%s: Update workflow input list", id)
      wf_val <- PEITHO:::update_input_list(
        x = wf_val,
        input_list = input_list_val
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