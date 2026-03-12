
# Helper to create workflow steps using package constructor and operationparam
step1 <- new_workflowstep(
  entry = 6,
  command = "strsplit",
  args = "x = @#*I*#@input_1@#*I*#@, split = \", \"",
  name = "Custom Name",
  label = "Custom Label",
  loop = "auto"
)
step2 <- new_workflowstep(
  entry = 7,
  command = "strsplit",
  args = "x = \"foo, bar\", split = \", \"",
  name = "Custom Name 2",
  label = "Custom Label 2",
  loop = "auto"
)

# Test running a workflow with two steps

test_that("run.workflow executes all steps and returns results", {
  wf <- new_workflow(
    name = "Test Workflow",
    steps = list(step1, step2),
    input_list = list("input_1" = "hallo, test"),
    use_peitho_folder = FALSE
  )
  state <- new_workflowstate()
  result <- run.workflow(wf, state)
  expect_type(result, "list")
  expect_s3_class(result$workflow, "workflow")
  expect_s3_class(result$state, "workflowstate")
  expect_length(result$state$stepruns, 2)
  expect_equal(result$state$stepruns[[1]]$output, list(c("hallo", "test")))
  expect_equal(result$state$stepruns[[2]]$output, list(c("foo", "bar")))
})

# Test workflow with error in step

test_that("run.workflow stops on error", {
  error_fn <- function() stop("fail")
  assign("error_fn", error_fn, envir = .GlobalEnv)
  step_err <- new_workflowstep(entry = 3, command = "error_fn")
  wf <- new_workflow(
    name = "Workflow with error",
    steps = list(step1, step_err, step2),
    input_list = list("input_1" = "hallo, test"),
    use_peitho_folder = FALSE
  )
  state <- new_workflowstate(initial_input = "hallo, test")
  expect_error(run.workflow(wf, state), "No parameters")
  rm(error_fn, envir = .GlobalEnv)
})

# Test workflow with no steps

test_that("run.workflow returns empty results for workflow with no steps", {
  wf <- suppressWarnings(
    new_workflow(name = "Empty Workflow", steps = list(), use_peitho_folder = FALSE)
  )
  state <- new_workflowstate(initial_input = "hallo, test")
  result <- run.workflow(wf, state)
  expect_type(result, "list")
  expect_s3_class(result$workflow, "workflow")
  expect_s3_class(result$state, "workflowstate")
  expect_length(result$state$stepruns, 0)
})
