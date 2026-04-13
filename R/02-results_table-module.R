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
    tags$p(
      "Review run results. Click a cell to inspect full content below.",
      class = "text-muted"
    ),
    fluidRow(
      column(
        4,
        sliderInput(
          ns("max_char"),
          "Max characters to display for error and output fields",
          min = 10,
          max = 1000,
          value = 50,
          step = 10,
          width = "100%"
        )
      )
    ),
    DT::DTOutput(ns("results_table")),
    tags$hr(),
    tags$h5("Selected Cell Content"),
    fluidRow(
      column(
        4,
        numericInput(
          ns("max_rows_cell"),
          "Max rows to display for selected cell",
          min = 1,
          value = 5,
          width = "100%"
        )
      ),
      column(
        4,
        br(),
        actionButton(ns("apply_max_rows_cell"), "Apply")
      )
    ),
    verbatimTextOutput(ns("selected_cell_value"), placeholder = TRUE)
  )
}
#' Results Table Server Module
#'
#' @param id Shiny module id
#' @param wf_run reactive workflow run object
#' @return None. Registers output$results_table
#' @export
results_table_server <- function(id, wf_run) {
  moduleServer(id, function(input, output, session) {
    max_rows_cell_applied <- reactiveVal(5)

    observeEvent(input$apply_max_rows_cell, {
      max_rows_cell_applied(input$max_rows_cell)
    })

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
      wfr <- wf_run()
      click <- input$results_table_cell_clicked

      if (is.null(wfr)) {
        return("Run a workflow to enable cell selection.")
      }

      if (is.null(click$row) || is.null(click$col)) {
        return("Click a cell in the table to view its content.")
      }

      # Resolve the clicked location against the untruncated data to show full content.
      df_full <- as.data.frame(wfr, max_char = NULL, max_items = max_rows_cell_applied())

      row_idx <- click$row
      col_idx <- click$col

      if (row_idx < 1 || row_idx > nrow(df_full) || col_idx < 1 || col_idx > ncol(df_full)) {
        return(as.character(click$value))
      }

      full_value <- df_full[[col_idx]][[row_idx]]

      if (is.character(full_value)) {
        return(full_value)
      }

      as.character(full_value)
    })
  })
}