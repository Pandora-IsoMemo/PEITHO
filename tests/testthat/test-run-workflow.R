
# Helper to create workflow steps using package constructor and operationparam
param1_1 <- new_operationparam(6, 1, name = "x", value = "hallo, test", type = "input")
param1_2 <- new_operationparam(6, 2, name = "split", value = ", ", type = "literal")
step1 <- new_workflowstep(
  id = 6,
  operation = "strsplit",
  params = list(param1_1, param1_2)
)
param2_1 <- new_operationparam(7, 1, name = "x", value = "foo, bar", type = "literal")
param2_2 <- new_operationparam(7, 2, name = "split", value = ", ", type = "literal")
step2 <- new_workflowstep(
  id = 7,
  operation = "strsplit",
  params = list(param2_1, param2_2)
)

# Test running a workflow with two steps

test_that("run.workflow executes all steps and returns results", {
  wf <- new_workflow(name = "Test Workflow", steps = list(step1, step2), use_peitho_folder = FALSE)
  state <- new_workflowstate(initial_input = "hallo, test")
  result <- run.workflow(wf, state)
  expect_type(result, "list")
  expect_s3_class(result$workflow, "workflow")
  expect_s3_class(result$state, "workflowstate")
  expect_length(result$state$stepruns, 2)
  expect_equal(result$state$stepruns[[1]]$output, list(c("hallo", "test")))
  expect_equal(result$state$stepruns[[2]]$output, list(c("foo", "bar")))
})

# Test workflow with error in step

test_that("run.workflow handles step error gracefully when stop_on_error = FALSE", {
  error_fn <- function() stop("fail")
  assign("error_fn", error_fn, envir = .GlobalEnv)
  step_err <- new_workflowstep(id = 3, operation = "error_fn")
  wf <- new_workflow(
    name = "Workflow with error",
    steps = list(step1, step_err, step2),
    use_peitho_folder = FALSE
  )
  state <- new_workflowstate(initial_input = "hallo, test")
  result <- run.workflow(wf, state, stop_on_error = FALSE)
  expect_type(result, "list")
  expect_s3_class(result$workflow, "workflow")
  expect_s3_class(result$state, "workflowstate")
  expect_length(result$state$stepruns, 3)
  expect_true(!is.null(result$state$stepruns[[2]]$error))
  rm(error_fn, envir = .GlobalEnv)
})

# Test workflow with no steps

test_that("run.workflow returns empty results for workflow with no steps", {
  wf <- new_workflow(name = "Empty Workflow", steps = list(), use_peitho_folder = FALSE)
  state <- new_workflowstate(initial_input = "hallo, test")
  result <- run.workflow(wf, state)
  expect_type(result, "list")
  expect_s3_class(result$workflow, "workflow")
  expect_s3_class(result$state, "workflowstate")
  expect_length(result$state$stepruns, 0)
})
