# Tests for as.graph_tables.workflow method

# Helper function to create a step with optional required_steps
mk_step_with_deps <- function(entry, name, required_steps = character(0)) {
  step <- new_workflowstep(
    entry = entry,
    command = "identity",
    name = name,
    label = name,
    comments = paste("Step", name),
    args = "",
    loop = "no"
  )
  # Manually set required_steps for testing
  step$required_steps <- required_steps
  step
}

test_that("as.graph_tables produces correctly structured output", {
  # Create a simple 3-step workflow with no dependencies
  s1 <- mk_step_with_deps(1L, "Step_1")
  s2 <- mk_step_with_deps(2L, "Step_2")
  s3 <- mk_step_with_deps(3L, "Step_3")

  wf <- suppressWarnings(new_workflow(
    name = "Graph Test Workflow",
    steps = list(s1, s2, s3),
    input_list = list(),
    use_peitho_folder = FALSE
  ))

  result <- as.graph_tables(wf)

  # Verify result is a list with nodes and edges
  expect_type(result, "list")
  expect_named(result, c("nodes", "edges"))
  expect_true(inherits(result$nodes, "data.frame"))
  expect_true(inherits(result$edges, "data.frame"))
})

test_that("as.graph_tables creates correct nodes for steps", {
  s1 <- mk_step_with_deps(1L, "First_Step")
  s2 <- mk_step_with_deps(2L, "Second_Step")

  wf <- suppressWarnings(new_workflow(
    name = "Nodes Test",
    steps = list(s1, s2),
    input_list = list(),
    use_peitho_folder = FALSE
  ))

  result <- as.graph_tables(wf)
  nodes <- result$nodes

  # Check nodes dataframe structure and content
  expect_equal(nrow(nodes), 2)
  expect_named(nodes, c("id", "name", "label", "command", "entry", "order", "type"))

  # Check node data
  expect_equal(nodes$id, c("1", "2"))
  expect_equal(nodes$name, c("First_Step", "Second_Step"))
  expect_equal(nodes$label, c("First_Step", "Second_Step"))
  expect_equal(nodes$command, c("identity", "identity"))
  expect_equal(unname(nodes$entry), c(1L, 2L))
  expect_equal(nodes$order, c(1L, 2L))
  expect_equal(nodes$type, c("step", "step"))
})

test_that("as.graph_tables creates empty edges for independent steps", {
  s1 <- mk_step_with_deps(1L, "Independent_1")
  s2 <- mk_step_with_deps(2L, "Independent_2")
  s3 <- mk_step_with_deps(3L, "Independent_3")

  wf <- suppressWarnings(new_workflow(
    name = "No Dependencies",
    steps = list(s1, s2, s3),
    input_list = list(),
    use_peitho_folder = FALSE
  ))

  result <- as.graph_tables(wf)
  edges <- result$edges

  # No dependencies means empty edges dataframe
  expect_equal(nrow(edges), 0)
  expect_named(edges, c("from", "to", "rel"))
})

test_that("as.graph_tables creates correct edges for dependent steps", {
  # Create steps with dependencies
  s1 <- mk_step_with_deps(1L, "First")
  s2 <- mk_step_with_deps(2L, "Second", required_steps = "First")
  s3 <- mk_step_with_deps(3L, "Third", required_steps = c("First", "Second"))

  wf <- suppressWarnings(new_workflow(
    name = "With Dependencies",
    steps = list(s1, s2, s3),
    input_list = list(),
    use_peitho_folder = FALSE
  ))

  result <- as.graph_tables(wf)
  edges <- result$edges

  # Should have 3 edges total:
  # First -> Second
  # First -> Third
  # Second -> Third
  expect_equal(nrow(edges), 3)

  # Sort edges by 'to' column for consistent testing
  edges_sorted <- edges[order(edges$to, edges$from), ]

  # Check edge structure
  expect_equal(edges_sorted$rel, c("required_steps", "required_steps", "required_steps"))

  # Verify the from-to relationships
  edge_pairs <- paste0(edges_sorted$from, "->", edges_sorted$to)
  expected_pairs <- c("1->2", "1->3", "2->3")
  expect_setequal(edge_pairs, expected_pairs)
})

test_that("as.graph_tables handles single step workflow", {
  s1 <- mk_step_with_deps(1L, "Only_Step")

  wf <- suppressWarnings(new_workflow(
    name = "Single Step",
    steps = list(s1),
    input_list = list(),
    use_peitho_folder = FALSE
  ))

  result <- as.graph_tables(wf)

  expect_equal(nrow(result$nodes), 1)
  expect_equal(nrow(result$edges), 0)
  expect_equal(result$nodes$name, "Only_Step")
  expect_equal(unname(result$nodes$entry), 1L)
})

test_that("as.graph_tables handles complex dependencies", {
  # Create a diamond dependency pattern:
  #    A
  #   / \
  #  B   C
  #   \ /
  #    D
  s1 <- mk_step_with_deps(1L, "A")
  s2 <- mk_step_with_deps(2L, "B", required_steps = "A")
  s3 <- mk_step_with_deps(3L, "C", required_steps = "A")
  s4 <- mk_step_with_deps(4L, "D", required_steps = c("B", "C"))

  wf <- suppressWarnings(new_workflow(
    name = "Diamond Pattern",
    steps = list(s1, s2, s3, s4),
    input_list = list(),
    use_peitho_folder = FALSE
  ))

  result <- as.graph_tables(wf)
  edges <- result$edges

  # Should have 4 edges: A->B, A->C, B->D, C->D
  expect_equal(nrow(edges), 4)

  # Check that D depends on both B and C
  d_deps <- edges[edges$to == "4", ]
  expect_equal(nrow(d_deps), 2)
  expect_setequal(d_deps$from, c("2", "3"))
})

test_that("as.graph_tables preserves step order", {
  steps_list <- list()
  for (i in 1:5) {
    steps_list[[i]] <- mk_step_with_deps(
      as.integer(i),
      paste0("Step_", i)
    )
  }

  wf <- suppressWarnings(new_workflow(
    name = "Order Test",
    steps = steps_list,
    input_list = list(),
    use_peitho_folder = FALSE
  ))

  result <- as.graph_tables(wf)
  nodes <- result$nodes

  # Order should match step sequence
  expect_equal(nodes$order, 1L:5L)
  expect_equal(unname(nodes$entry), 1L:5L)
})

test_that("as.graph_tables handles non-sequential entry numbers", {
  # Create steps with non-sequential entry numbers
  s1 <- mk_step_with_deps(10L, "Step_Ten")
  s2 <- mk_step_with_deps(20L, "Step_Twenty")
  s3 <- mk_step_with_deps(30L, "Step_Thirty")

  wf <- suppressWarnings(new_workflow(
    name = "Non Sequential",
    steps = list(s1, s2, s3),
    input_list = list(),
    use_peitho_folder = FALSE
  ))

  result <- as.graph_tables(wf)
  nodes <- result$nodes

  # Entry numbers should be preserved
  expect_equal(unname(nodes$entry), c(10L, 20L, 30L))

  # But order should be sequential based on position
  expect_equal(nodes$order, c(1L, 2L, 3L))

  # IDs should be based on entry numbers
  expect_equal(nodes$id, c("10", "20", "30"))
})

test_that("as.graph_tables edge IDs match node IDs by entry", {
  s1 <- mk_step_with_deps(1L, "Start")
  s2 <- mk_step_with_deps(2L, "Middle", required_steps = "Start")
  s3 <- mk_step_with_deps(3L, "End", required_steps = "Middle")

  wf <- suppressWarnings(new_workflow(
    name = "ID Consistency",
    steps = list(s1, s2, s3),
    input_list = list(),
    use_peitho_folder = FALSE
  ))

  result <- as.graph_tables(wf)
  nodes <- result$nodes
  edges <- result$edges

  # All node IDs should be string representations of entry numbers
  expect_true(all(nodes$id %in% as.character(nodes$entry)))

  # All edge from/to IDs should exist in nodes
  expect_true(all(edges$from %in% nodes$id))
  expect_true(all(edges$to %in% nodes$id))
})

test_that("as.graph_tables returns tibbles with expected classes", {
  s1 <- mk_step_with_deps(1L, "A")
  s2 <- mk_step_with_deps(2L, "B", required_steps = "A")

  wf <- suppressWarnings(new_workflow(
    name = "Class Test",
    steps = list(s1, s2),
    input_list = list(),
    use_peitho_folder = FALSE
  ))

  result <- as.graph_tables(wf)

  # Check that both are data frames
  expect_s3_class(result$nodes, "data.frame")
  expect_s3_class(result$edges, "data.frame")

  # Check column types
  expect_equal(typeof(result$nodes$id), "character")
  expect_equal(typeof(result$nodes$name), "character")
  expect_equal(typeof(result$nodes$entry), "integer")
  expect_equal(typeof(result$nodes$order), "integer")

  expect_equal(typeof(result$edges$from), "character")
  expect_equal(typeof(result$edges$to), "character")
  expect_equal(typeof(result$edges$rel), "character")
})

test_that("as.graph_tables edge rel values are 'required_steps' for step dependencies", {
  s1 <- mk_step_with_deps(1L, "Alpha")
  s2 <- mk_step_with_deps(2L, "Beta", required_steps = c("Alpha"))
  s3 <- mk_step_with_deps(3L, "Gamma", required_steps = c("Alpha", "Beta"))

  wf <- suppressWarnings(new_workflow(
    name = "Rel Test",
    steps = list(s1, s2, s3),
    input_list = list(),
    use_peitho_folder = FALSE
  ))

  result <- as.graph_tables(wf)

  # All rel values should be "required_steps" (since no inputs)
  expect_true(all(result$edges$rel == "required_steps"))
})

test_that("as.graph_tables includes input nodes and required_inputs edges", {
  # Create a helper to set required_inputs
  mk_step_with_inputs <- function(entry, name, required_inputs = character(0), required_steps = character(0)) {
    step <- new_workflowstep(
      entry = entry,
      command = "identity",
      name = name,
      label = name,
      comments = paste("Step", name),
      args = "",
      loop = "no"
    )
    step$required_inputs <- required_inputs
    step$required_steps <- required_steps
    step
  }

  s1 <- mk_step_with_inputs(1L, "Init", required_inputs = c("input1"))
  s2 <- mk_step_with_inputs(2L, "Process", required_inputs = c("input1", "input2"), required_steps = "Init")

  wf <- suppressWarnings(new_workflow(
    name = "With Inputs",
    steps = list(s1, s2),
    input_list = list(input1 = "value1", input2 = "value2"),
    use_peitho_folder = FALSE
  ))

  result <- as.graph_tables(wf)
  nodes <- result$nodes
  edges <- result$edges

  # Should have 4 nodes: 2 steps + 2 inputs
  expect_equal(nrow(nodes), 4)

  # Check for step nodes
  step_nodes <- nodes[nodes$type == "step", ]
  expect_equal(nrow(step_nodes), 2)
  expect_equal(step_nodes$name, c("Init", "Process"))

  # Check for input nodes
  input_nodes <- nodes[nodes$type == "input", ]
  expect_equal(nrow(input_nodes), 2)
  expect_setequal(input_nodes$name, c("input1", "input2"))
  expect_equal(input_nodes$id, c("input_input1", "input_input2"))
  expect_true(all(is.na(input_nodes$command)))

  # Should have 4 edges: 1 required_inputs (Init) + 2 required_inputs (Process) + 1 required_steps
  expect_equal(nrow(edges), 4)

  # Check edge types
  required_input_edges <- edges[edges$rel == "required_inputs", ]
  required_step_edges <- edges[edges$rel == "required_steps", ]
  expect_equal(nrow(required_input_edges), 3)
  expect_equal(nrow(required_step_edges), 1)

  # Verify input edges are from input nodes to step nodes
  expect_true(all(required_input_edges$from %in% input_nodes$id))
  expect_true(all(required_input_edges$to %in% step_nodes$id))
})
