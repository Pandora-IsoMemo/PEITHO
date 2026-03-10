test_that("new_workflowstep creates valid object with base function", {
  step <- new_workflowstep(entry = 1, command = "strsplit")
  expect_s3_class(step, "workflowstep")
  expect_equal(step$name, "Step 1")
  expect_equal(step$command, "strsplit")
})

test_that("new_workflowstep fails for missing function", {
  expect_error(new_workflowstep(entry = 3, command = "not_a_function"), "not found")
})

test_that("new_workflowstep sets name and label correctly", {
  step <- new_workflowstep(entry = 4, command = "strsplit")
  expect_equal(step$name, "Step 4")
  expect_equal(step$label, "Step 4")
  step2 <- new_workflowstep(
    entry = 5,
    command = "strsplit",
    name = "Custom Name",
    label = "Custom Label"
  )
  expect_equal(step2$name, "Custom Name")
  expect_equal(step2$label, "Custom Label")
})

test_that("run.workflowstep errors if state is not workflowstate", {
  step <- new_workflowstep(
    entry = 3,
    command = "strsplit",
    args = "x = \"hallo, test\", split = \", \"",
    loop = "auto"
  )
  not_state <- list()
  expect_error(run.workflowstep(step, not_state), "must be a 'workflowstate' object")
})

# Test run.workflowstep with valid operationparam
test_that("run.workflowstep executes command and returns correct output", {
  step <- new_workflowstep(
    entry = 7,
    command = "strsplit",
    args = "x = \"hallo, test\", split = \", \"",
    loop = "auto"
  )
  state <- new_workflowstate()
  steprun <- run.workflowstep(step, state, path_to_folder = tempdir())
  expect_s3_class(steprun, "workflowsteprun")
  expect_equal(steprun$output, list(c("hallo", "test")))
  expect_null(steprun$error)
})

test_that("run.workflowstep handles command error", {
  error_fn <- function() stop("fail")
  assign("error_fn", error_fn, envir = .GlobalEnv)
  step <- new_workflowstep(
    entry = 7,
    command = "error_fn",
    args = "x = \"foo\"",
    loop = "auto"
  )
  state <- new_workflowstate()
  steprun <- run.workflowstep(step, state, path_to_folder = tempdir())
  expect_s3_class(steprun, "workflowsteprun")
  expect_null(steprun$output)
  expect_true(inherits(steprun$error, "error"))
  rm(error_fn, envir = .GlobalEnv)
})
