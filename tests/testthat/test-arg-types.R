test_that("coerce_arg converts supported types", {
  expect_equal(PEITHO:::coerce_arg("abc", "character", "x"), "abc")
  expect_equal(PEITHO:::coerce_arg("1.5", "numeric", "x"), 1.5)
  expect_equal(PEITHO:::coerce_arg("2", "integer", "x"), 2L)
  expect_equal(PEITHO:::coerce_arg("TRUE", "logical", "x"), TRUE)
  expect_equal(PEITHO:::coerce_arg("0", "logical", "x"), FALSE)
})

test_that("coerce_arg errors on invalid values", {
  expect_error(
    PEITHO:::coerce_arg("abc", "numeric", "x"),
    "must be numeric"
  )

  expect_error(
    PEITHO:::coerce_arg("1.2", "integer", "x"),
    "must be integer"
  )

  expect_error(
    PEITHO:::coerce_arg("maybe", "logical", "x"),
    "must be logical"
  )
})

test_that("new_workflowstep stores sparse arg_types", {
  step <- new_workflowstep(
    entry = 1,
    command = "paste",
    args = "x = \"a\", collapse = \" \"",
    arg_types = list(collapse = "character")
  )

  expect_equal(step$arg_types, c(collapse = "character"))
})

test_that("new_workflowstep warns on unused arg_types keys", {
  expect_warning(
    new_workflowstep(
      entry = 1,
      command = "paste",
      args = "x = \"a\"",
      arg_types = list(not_present = "logical")
    ),
    "Unused 'arg_types' keys"
  )
})

test_that("workflow_steps_from_files reads arg_types from commands", {
  commands <- list(
    list(
      name = "Step 1",
      command = "paste",
      args = "x = \"a\", collapse = \" \"",
      arg_types = list(collapse = "character")
    )
  )

  tmpdir <- tempfile("workflowtest_argtypes_")
  dir.create(tmpdir)
  write(
    jsonlite::toJSON(commands, auto_unbox = TRUE, pretty = TRUE),
    file.path(tmpdir, "commands.json")
  )
  write(
    jsonlite::toJSON(list(), auto_unbox = TRUE, pretty = TRUE),
    file.path(tmpdir, "inputs.json")
  )

  wf_paths <- workflow_file_paths(path = tmpdir)
  steps <- workflow_steps_from_files(workflow_file_paths = wf_paths)
  expect_equal(steps[[1]]$arg_types, c(collapse = "character"))

  unlink(tmpdir, recursive = TRUE)
})

test_that("run.workflowstep coerces literal args using sparse arg_types", {
  typed_capture <- function(x, y, flag) {
    paste(class(x), class(y), class(flag), sep = "|")
  }
  assign("typed_capture", typed_capture, envir = .GlobalEnv)
  on.exit(rm("typed_capture", envir = .GlobalEnv), add = TRUE)

  step <- new_workflowstep(
    entry = 1,
    command = "typed_capture",
    args = "x = \"2\", y = \"abc\", flag = \"TRUE\"",
    arg_types = list(x = "integer", flag = "logical")
  )

  state <- new_workflowstate()
  steprun <- run.workflowstep(step, state, env = globalenv())

  expect_equal(steprun$output, list("integer|character|logical"))
})

test_that("as.commands_record.workflowstep includes arg_types only when set", {
  step_no_types <- new_workflowstep(
    entry = 1,
    command = "paste",
    args = "x = \"a\""
  )
  rec_no_types <- as.commands_record(step_no_types)
  expect_false("arg_types" %in% names(rec_no_types))

  step_with_types <- new_workflowstep(
    entry = 2,
    command = "paste",
    args = "x = \"a\", collapse = \" \"",
    arg_types = list(collapse = "character")
  )
  rec_with_types <- as.commands_record(step_with_types)
  expect_true("arg_types" %in% names(rec_with_types))
  expect_equal(rec_with_types$arg_types$collapse, "character")
})
