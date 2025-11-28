# Helper to create workflow steps using package constructor
step1 <- new_workflowstep(
  id = 6,
  operation = "strsplit",
  params = list(x = "hallo, test", split = ", ")
)
step2 <- new_workflowstep(
  id = 7,
  operation = "strsplit",
  params = list(x = "foo, bar", split = ", ")
)

# Test running a workflow with two steps

test_that("run_workflow.workflow executes all steps and returns results", {
  wf <- new_workflow(steps = list(step1, step2), name = "Test Workflow")
  state <- new_workflowstate()
  result <- run_workflow.workflow(wf, state)
  expect_type(result, "list")
  expect_s3_class(result$workflow, "workflow")
  expect_s3_class(result$state, "workflowstate")
  expect_length(result$state$stepruns, 2)
  expect_equal(result$state$stepruns[[1]]$output, list(c("hallo", "test")))
  expect_equal(result$state$stepruns[[2]]$output, list(c("foo", "bar")))
})

# Test workflow with error in step

test_that("run_workflow.workflow handles step error gracefully when stop_on_error = FALSE", {
  error_fn <- function() stop("fail")
  assign("error_fn", error_fn, envir = .GlobalEnv)
  step_err <- new_workflowstep(id = 3, operation = "error_fn")
  wf <- new_workflow(steps = list(step1, step_err, step2), name = "Workflow with error")
  state <- new_workflowstate()
  result <- run_workflow.workflow(wf, state, stop_on_error = FALSE)
  expect_type(result, "list")
  expect_s3_class(result$workflow, "workflow")
  expect_s3_class(result$state, "workflowstate")
  expect_length(result$state$stepruns, 3)
  expect_true(!is.null(result$state$stepruns[[2]]$error))
  rm(error_fn, envir = .GlobalEnv)
})

# Test workflow with no steps

test_that("run_workflow.workflow returns empty results for workflow with no steps", {
  wf <- new_workflow(steps = list(), name = "Empty Workflow")
  state <- new_workflowstate()
  result <- run_workflow.workflow(wf, state)
  expect_type(result, "list")
  expect_s3_class(result$workflow, "workflow")
  expect_s3_class(result$state, "workflowstate")
  expect_length(result$state$stepruns, 0)
})
