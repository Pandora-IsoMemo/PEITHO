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
    tabPanel("Workflow", tableOutput(ns("tbl"))),
    uiOutput(ns("edit"))
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
    ns <- session$ns
    output$tbl <- renderTable({
      wf_val <- wf()
      if (is.null(wf_val)) return(NULL)
      as.data.frame(wf_val)
    }, rownames = TRUE)

    output$edit <- renderUI({
      # hide the edit UI when no workflow is loaded
      if (is.null(wf())) return(NULL)

      workflow_edit_ui(ns("edit"))
    })

    workflow_edit_server("edit", wf)
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

workflow_edit_server <- function(id, wf) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      wf_val <- wf()
      if (is.null(wf_val)) {
        # hide the step select input when no workflow is loaded
        updateSelectInput(session, "step_select", choices = NULL, selected = NULL)
        updateSelectInput(session, "field_select", choices = NULL, selected = NULL)
      } else {
        wf_df <- as.data.frame(wf_val)
        step_choices <- wf_df[["Name"]]
        updateSelectInput(session, "step_select", choices = step_choices, selected = NULL)
        field_choices <- colnames(wf_df)
        updateSelectInput(session, "field_select", choices = field_choices, selected = NULL)
      }
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
      wf_val <- update.workflow(wf_val, step_idx, field_name, new_value) |>
        shinyTryCatch(errorTitle = "Editing Workflow failed")

      # Trigger reactive update by assigning the modified workflow back to the reactive value
      wf(wf_val)
    }) |>
      bindEvent(input$edit_btn)
  })
}