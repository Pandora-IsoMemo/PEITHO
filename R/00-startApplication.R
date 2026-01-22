#' Start Application
#'
#' @param port port
#' @param launch.browser If true, the system's default web browser will be launched
#'
#' @export
startApplication <- function(port = base::getOption("shiny.port"),
                             launch.browser = base::getOption("shiny.launch.browser", base::interactive())) {
  shiny::runApp(
    base::system.file("app", package = "PEITHO"),
    port = port,
    host = "0.0.0.0",
    launch.browser = launch.browser
  )
}
