#' Results Table UI Module
#'
#' @param id Shiny module id
#' @return Shiny UI output for the results table
#' @export
results_table_ui <- function(id) {
  ns <- NS(id)
  tableOutput(ns("results_table"))
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
    output$results_table <- renderTable({
      wfr <- wf_run()
      if (is.null(wfr)) return(NULL)
      as.data.frame(wfr)
    }, rownames = TRUE)
  })
}
