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
    tabPanel("Workflow", tableOutput(ns("tbl")))
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
  })
}