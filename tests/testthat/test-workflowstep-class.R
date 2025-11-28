# Dummy function for testing
strsplit_dummy <- function(x, split) strsplit(x, split)

# Test valid construction

test_that("new_workflowstep creates valid object with base operation", {
  step <- new_workflowstep(id = 1, operation = "strsplit")
  expect_s3_class(step, "workflowstep")
  expect_equal(step$name, "Step 1")
  expect_equal(step$operation, "strsplit")
})

test_that("new_workflowstep fails for missing function", {
  expect_error(new_workflowstep(id = 3, operation = "not_a_function"),
               "not found in loaded name space")
})

# Test label and name logic

test_that("new_workflowstep sets name and label correctly", {
  step <- new_workflowstep(id = 4, operation = "strsplit")
  expect_equal(step$name, "Step 4")
  expect_equal(step$label, "Step 4")
  step2 <- new_workflowstep(
    id = 5,
    operation = "strsplit",
    name = "Custom Name",
    label = "Custom Label"
)
  expect_equal(step2$name, "Custom Name")
  expect_equal(step2$label, "Custom Label")
})

# Test params

test_that("new_workflowstep stores params", {
  step <- new_workflowstep(
    id = 6,
    operation = "strsplit",
    params = list(x = "hallo, test", split = ", ")
)
  expect_equal(step$params$x, "hallo, test")
  expect_equal(step$params$split, ", ")
})

# Test run_step.workflowstep

test_that("run_step.workflowstep executes operation and updates state", {
  step <- new_workflowstep(
    id = 6,
    operation = "strsplit",
    params = list(x = "hallo, test", split = ", ")
  )
  state <- new_workflowstate()
  steprun <- run_step.workflowstep(step, state)
  expect_s3_class(steprun, c("workflowrun", "list"))
  expect_equal(steprun$output, list(c("hallo", "test")))
  expect_null(steprun$error)
})

test_that("run_step.workflowstep handles operation error", {
  error_fn <- function() stop("fail")
  assign("error_fn", error_fn, envir = .GlobalEnv)
  step <- new_workflowstep(id = 2, operation = "error_fn")
  state <- new_workflowstate()
  steprun <- run_step.workflowstep(step, state)
  expect_s3_class(steprun, c("workflowrun", "list"))
  expect_null(steprun$output)
  expect_true(inherits(steprun$error, "error"))
  rm(error_fn, envir = .GlobalEnv)
})

test_that("run_step.workflowstep errors if state is not workflowstate", {
  step <- new_workflowstep(
    id = 3,
    operation = "strsplit",
    params = list(x = "hallo, test", split = ", ")
  )
  not_state <- list()
  expect_error(run_step.workflowstep(step, not_state), "must be a 'workflowstate' object")
})
