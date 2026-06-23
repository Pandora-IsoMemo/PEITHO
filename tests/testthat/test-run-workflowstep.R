test_that("new_workflowstep creates valid object with base function", {
  step <- new_workflowstep(entry = 1, command = "strsplit")
  expect_s3_class(step, "workflowstep")
  expect_equal(step$name, "Step 1")
  expect_equal(step$command, "strsplit")
})

test_that("new_workflowstep warns for missing function on construction", {
  expect_warning(new_workflowstep(entry = 3, command = "not_a_function"), "not found")
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
  expect_equal(steprun$output, list(list(c("hallo", "test"))))
  expect_equal(steprun$error, list(NULL))
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
  expect_equal(steprun$output, list(character(0)))
  expect_true(inherits(steprun$error[[1]], "error"))
  rm(error_fn, envir = .GlobalEnv)
})

test_that("make_param_from_arg stores result selector separately", {
  param <- PEITHO:::make_param_from_arg(
    arg = "@#*L*#@step 1@#*L*#@[c(1, 3)]",
    arg_name = "x",
    step_i = 2,
    arg_i = 1,
    cmd_loop = "no",
    input_list = list()
  )

  expect_equal(param$type, "result")
  expect_equal(param$label, "step_1")
  expect_equal(param$selector, "c(1,3)")
})

test_that("run.workflowstep without looping returns lists with single output and NULL error", {
  step <- new_workflowstep(
    entry   = 1,
    command = "toupper",
    args    = "x = \"hello\"",
    loop    = "no"
  )
  state <- new_workflowstate()
  steprun <- run.workflowstep(step, state, env = globalenv())

  expect_s3_class(steprun, "workflowsteprun")
  expect_equal(steprun$output, list("HELLO"))
  expect_equal(length(steprun$error), 1L)
  expect_true(all(sapply(steprun$error, is.null)))
  expect_false(steprun$has_error)
})

test_that("run.workflowstep with looping iterates over list arg and returns list of outputs", {
  step <- new_workflowstep(
    entry   = 2,
    command = "toupper",
    args    = "x = @#*L*#@step 1@#*L*#@",
    loop    = "auto"
  )
  state <- new_workflowstate()
  state$results_by_name[["step_1"]] <- c("hello", "world", "foo")

  steprun <- run.workflowstep(step, state, step_i = 2, env = globalenv())

  expect_s3_class(steprun, "workflowsteprun")
  expect_equal(steprun$output, list("HELLO", "WORLD", "FOO"))
  expect_equal(length(steprun$error), 3L)
  expect_true(all(sapply(steprun$error, is.null)))
  expect_false(steprun$has_error)
})

test_that("extract_arg_list applies selector to previous result", {
  state <- new_workflowstate()
  state$results_by_name[["step_1"]] <- c("a", "b", "c")

  param <- new_operationparam(
    step_id = 2,
    position = 1,
    name = "x",
    value = NULL,
    type = "result",
    label = "step_1",
    selector = "c(1,3)"
  )

  out <- PEITHO:::extract_arg_list(param, state)
  expect_equal(out$x, list("a", "c"))
})

test_that("run.workflowstep with samples repeats each iteration", {
  step <- new_workflowstep(
    entry = 2,
    command = "toupper",
    args = "x = @#*L*#@step 1@#*L*#@",
    iteration = "auto",
    samples = 2
  )

  state <- new_workflowstate(run_id = "sample_run")
  state$results_by_name[["step_1"]] <- c("hello", "world")

  steprun <- run.workflowstep(step, state, step_i = 2, step_idx = 2, env = globalenv())

  expect_length(steprun$output, 4L)
  expect_equal(steprun$output, list("HELLO", "HELLO", "WORLD", "WORLD"))
  expect_equal(steprun$meta$sample_total, 2L)
  expect_equal(steprun$meta$iteration_total, 2L)
})

test_that("run.workflowstep with samples on non-iterative step repeats command", {
  call_env <- new.env(parent = emptyenv())
  call_env$count <- 0L
  sampled_upper <- function(x) {
    call_env$count <- call_env$count + 1L
    toupper(x)
  }
  assign("sampled_upper", sampled_upper, envir = .GlobalEnv)
  on.exit(rm("sampled_upper", envir = .GlobalEnv), add = TRUE)

  step <- new_workflowstep(
    entry = 1,
    command = "sampled_upper",
    args = "x = \"hello\"",
    iteration = "no",
    samples = 3
  )

  state <- new_workflowstate(run_id = "sample_run_non_iter")
  steprun <- run.workflowstep(step, state, env = globalenv())

  expect_equal(call_env$count, 3L)
  expect_length(steprun$output, 3L)
  expect_equal(steprun$output, list("HELLO", "HELLO", "HELLO"))
})
