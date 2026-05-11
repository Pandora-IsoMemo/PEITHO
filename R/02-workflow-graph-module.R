#' Graph UI
#'
#' @param id Module ID
#' @param title Title to display above the graph (default: "Workflow graph").
#' @return Shiny UI elements for the workflow graph
#' @export 
workflow_graph_ui <- function(id, title = "Workflow graph") {
  ns <- NS(id)
  tagList(
    tags$h4(title),
    tags$p(
      paste(
        "Visualize the workflow structure.",
        "Nodes represent inputs (circles) and steps (rectangles).",
        "Edges indicate dependencies between inputs and steps."
      ),
      class = "text-muted"
    ),
    DiagrammeR::grVizOutput(ns("graph"), height = "600px")
  )
}

#' Graph Server
#'
#' @param id Module ID
#' @param wf Reactive expression containing the workflow object
#' @return Shiny server logic for rendering the workflow graph
#' @export
workflow_graph_server <- function(id, wf) {
  moduleServer(id, function(input, output, session) {
    output$graph <- DiagrammeR::renderGrViz({
      wf_val <- wf()
      if (is.null(wf_val)) return(NULL)

      graph_tables <- as.graph_tables(wf_val)

      step_ids <- graph_tables$nodes$id[graph_tables$nodes$type == "step"]
      input_ids <- graph_tables$nodes$id[graph_tables$nodes$type == "input"]


      DiagrammeR::create_graph(directed = TRUE) |>
        DiagrammeR::add_nodes_from_table(
          graph_tables$nodes,
          label_col = "label",
          type_col = "type"
        ) |>
        DiagrammeR::add_edges_from_table(
          graph_tables$edges,
          from_col = "from",
          to_col = "to",
          from_to_map = "id_external"
        ) |>
        DiagrammeR::set_node_attrs("fontcolor", "black") |>
        set_node_style(step_ids, "#AED6F1", "rectangle") |>
        set_node_style(input_ids, "#A9DFBF", "ellipse") |>
        DiagrammeR::render_graph(layout = "tree")
    })
  })
}

set_node_style <- function(
  graph, node_ids, fillcolor, shape, style = "filled"
) {
  graph |>
    DiagrammeR::set_node_attrs(
      node_attr = "fillcolor", values = fillcolor, nodes = node_ids
    ) |>
    DiagrammeR::set_node_attrs(
      node_attr = "style", values = style, nodes = node_ids
    ) |>
    DiagrammeR::set_node_attrs(
      node_attr = "shape", values = shape, nodes = node_ids
    )
}