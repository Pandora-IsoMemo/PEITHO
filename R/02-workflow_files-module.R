#' Workflow Files UI
#'
#' @param id Module ID
#' @param title Title to display above the file tree
#' @return Shiny UI elements for the workflow files tab
#' @export
workflow_files_ui <- function(id, title = "") {
  ns <- NS(id)

  tagList(
    if (nzchar(title)) tags$h4(title) else NULL,
    shinyTree::shinyTree(ns("file_tree"), search = TRUE, theme = "proton")
  )
}

#' Workflow Files Server
#'
#' @param id Module ID
#' @param active_dir Reactive expression containing the active workflow directory path
#' @return Shiny server logic for rendering the workflow file tree
#' @export
workflow_files_server <- function(id, active_dir) {
  moduleServer(id, function(input, output, session) {
    build_file_tree <- function(path) {
      entries <- list.files(path, full.names = TRUE, no.. = TRUE)
      tree <- lapply(entries, function(entry) {
        if (dir.exists(entry)) {
          structure(build_file_tree(entry), sticon = "folder")
        } else {
          structure("", sticon = "file")
        }
      })
      names(tree) <- basename(entries)
      tree
    }

    output$file_tree <- shinyTree::renderTree({
      req(active_dir(), dir.exists(active_dir()))
      build_file_tree(active_dir())
    })
  })
}