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
    tableOutput(ns("tbl"))
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
    output$tbl <- renderTable({
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
  })
}
