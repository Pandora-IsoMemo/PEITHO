#' Results Table UI Module
#'
#' @param id Shiny module id
#' @param title Title to display above the table
#' @return Shiny UI output for the results table
#' @export
results_table_ui <- function(id, title = "") {
  ns <- NS(id)
  tagList(
    tags$h4(title),
    sliderInput(
      ns("max_char"),
      "Max characters to display",
      min = 10,
      max = 1000,
      value = 50,
      step = 10
    ),
    DT::DTOutput(ns("results_table")),
    tags$hr(),
    tags$h5("Selected Cell Content"),
    verbatimTextOutput(ns("selected_cell_value"), placeholder = TRUE)#,
    #uiOutput(ns("select"))
  )
}
#' Results Table Server Module
#'
#' @param id Shiny module id
#' @param wf_run reactive workflow run object
#' @param is_active_tab Reactive expression indicating whether the inputs tab is active
#' @return None. Registers output$results_table
#' @export
results_table_server <- function(id, wf_run, is_active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$results_table <- DT::renderDT({
      wfr <- wf_run()
      if (is.null(wfr)) return(NULL)
      df <- as.data.frame(wfr, max_char = input$max_char, max_items = 5)
      DT::datatable(
        df,
        rownames = TRUE,
        selection = list(mode = "single", target = "cell"),
        options = list(
          paging = FALSE,
          scrollY = "360px",
          scrollCollapse = TRUE
        )
      ) |>
        DT::formatStyle(
          columns = names(df),
          `white-space` = "pre-wrap"
        )
    })

    output$selected_cell_value <- renderText({
      click <- input$results_table_cell_clicked

      if (is.null(wf_run())) {
        return("Run a workflow to enable cell selection.")
      }

      if (is.null(click$value)) {
        return("Click a cell in the table to view its content.")
      }

      as.character(click$value)
    })

    # output$select <- renderUI({
    #   # hide the edit UI when no workflow is loaded
    #   if (is.null(wf_run())) return(NULL)

    #   field_select_ui(ns("field_select"))
    # })

    # sel <- field_select_server(
    #   "field_select",
    #   reactive(as.data.frame(wf_run(), max_char = input$max_char, max_items = 5)),
    #   is_active_tab
    # )

    # observe({
    #   req(isTRUE(is_active_tab()), wf_run())
    #   PEITHO:::logDebug(
    #     "%s: Selected field: %s",
    #     id,
    #     paste(sel$selected_row(), sel$selected_column(), sep = ", ")
    #   )
    # })
  })
}

# field_select_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     tags$hr(),
#     tags$h4("Select Field"),
#     fluidRow(
#       column(
#         3,
#         selectInput(ns("row_select"), "Select row", choices = NULL, selected = NULL)
#       ),
#       column(
#         3,
#         selectInput(ns("column_select"), "Select column", choices = NULL, selected = NULL)
#       )
#     )
#   )
# }

# field_select_server <- function(id, df, is_active_tab) {
#   moduleServer(id, function(input, output, session) {
#     observe({
#       req(isTRUE(is_active_tab()))

#       if (is.null(df())) {
#         PEITHO:::logDebug("%s: Reset inputs if empty wf", id)
#         # hide the step select input when no df is loaded
#         updateSelectInput(session, "row_select", choices = NULL, selected = NULL)
#         updateSelectInput(session, "column_select", choices = NULL, selected = NULL)
#       }
#     })

#     observe({
#       req(isTRUE(is_active_tab()), df())
#       PEITHO:::logDebug("%s: Update inputs", id)

#       df_val <- df()
#       row_choices <- rownames(df_val)

#       if (isTRUE(input$row_select %in% row_choices)) {
#         selected_row <- input$row_select
#       } else {
#         selected_row <- NULL
#       }
#       updateSelectInput(
#         session, "row_select", choices = row_choices, selected = selected_row
#       )

#       column_choices <- colnames(df_val)

#       if (isTRUE(input$column_select %in% column_choices)) {
#         selected_column <- input$column_select
#       } else {
#         selected_column <- NULL
#       }
#       updateSelectInput(
#         session, "column_select", choices = column_choices, selected = selected_column
#       )
#     })

#     list(
#       selected_row = reactive(input$row_select),
#       selected_column = reactive(input$column_select)
#     )
#   })
# }
