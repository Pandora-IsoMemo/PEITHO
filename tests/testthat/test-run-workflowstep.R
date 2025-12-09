test_that("new_workflowstep creates valid object with base operation", {
  step <- new_workflowstep(id = 1, operation = "strsplit")
  expect_s3_class(step, "workflowstep")
  expect_equal(step$name, "Step 1")
  expect_equal(step$operation, "strsplit")
})

test_that("new_workflowstep fails for missing function", {
  expect_error(new_workflowstep(id = 3, operation = "not_a_function"), "not found")
})

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

test_that("new_workflowstep stores params", {
  step <- new_workflowstep(
    id = 6,
    operation = "strsplit",
    params = list(x = "hallo, test", split = ", ")
)
  expect_equal(step$params$x, "hallo, test")
  expect_equal(step$params$split, ", ")
})

test_that("run.workflowstep errors if state is not workflowstate", {
  step <- new_workflowstep(
    id = 3,
    operation = "strsplit",
    params = list(x = "hallo, test", split = ", ")
  )
  not_state <- list()
  expect_error(run.workflowstep(step, not_state), "must be a 'workflowstate' object")
})

# Test valid construction with operationparam
test_that("new_workflowstep creates valid object with operationparam params", {
  param1 <- new_operationparam(1, 1, "x", "hallo, test")
  param2 <- new_operationparam(1, 2, "split", ", ")
  step <- new_workflowstep(id = 1, operation = "strsplit", params = list(param1, param2))
  expect_s3_class(step, "workflowstep")
  expect_equal(step$name, "Step 1")
  expect_equal(step$operation, "strsplit")
  expect_length(step$params, 2)
})

# Test run.workflowstep with valid operationparam
test_that("run.workflowstep executes operation and returns correct output", {
  param1 <- new_operationparam(6, 1, "x", "hallo, test")
  param2 <- new_operationparam(6, 2, "split", ", ")
  step <- new_workflowstep(id = 6, operation = "strsplit", params = list(param1, param2))
  state <- new_workflowstate()
  steprun <- run.workflowstep(step, state, path_to_folder = tempdir())
  expect_s3_class(steprun, "workflowsteprun")
  expect_equal(steprun$output, list(c("hallo", "test")))
  expect_null(steprun$error)
})

test_that("run.workflowstep handles operation error", {
  error_fn <- function() stop("fail")
  assign("error_fn", error_fn, envir = .GlobalEnv)
  param1 <- new_operationparam(2, 1, "x", "foo")
  step <- new_workflowstep(id = 2, operation = "error_fn", params = list(param1))
  state <- new_workflowstate()
  steprun <- run.workflowstep(step, state, path_to_folder = tempdir())
  expect_s3_class(steprun, "workflowsteprun")
  expect_null(steprun$output)
  expect_true(inherits(steprun$error, "error"))
  rm(error_fn, envir = .GlobalEnv)
})

test_that("run.workflowstep errors if state is not workflowstate", {
  param1 <- new_operationparam(3, 1, "x", "foo")
  step <- new_workflowstep(id = 3, operation = "strsplit", params = list(param1))
  not_state <- list()
  expect_error(run.workflowstep(step, not_state, path_to_folder = tempdir()), "must be a 'workflowstate' object")
})
