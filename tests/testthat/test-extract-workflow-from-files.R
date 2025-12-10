
# Helper: create a temporary folder with commands.json, inputs.json, and results_summary.json
create_test_files <- function(
  commands = NULL,
  inputs = NULL,
  results = NULL,
  functions = NULL,
  commands_name = "commands.json",
  inputs_name = "inputs.json",
  results_name = "results_summary.json",
  functions_name = "functions.R"
) {
  tmpdir <- tempfile("workflowtest_")
  dir.create(tmpdir)
  if (!is.null(commands)) {
    write(jsonlite::toJSON(commands, auto_unbox = TRUE, pretty = TRUE), file.path(tmpdir, commands_name))
  }
  if (!is.null(inputs)) {
    write(jsonlite::toJSON(inputs, auto_unbox = TRUE, pretty = TRUE), file.path(tmpdir, inputs_name))
  }
  if (!is.null(results)) {
    write(jsonlite::toJSON(results, auto_unbox = TRUE, pretty = TRUE), file.path(tmpdir, results_name))
  }
  if (!is.null(functions)) {
    write(functions, file.path(tmpdir, functions_name))
  }
  tmpdir
}

test_that("extract_workflow_from_files returns correct steps and param types from commands.json and inputs.json", {
  commands <- list(
    list(name = "Step 1", command = "strsplit", args = "x = @#*I*#@myinput@#*I*#@, pattern= ', '", label = "First Step", loop = "no"),
    list(name = "Step 2", command = "toupper", args = "x = \"hello world\"", label = "Second Step", loop = "no")
  )
  inputs <- list(myinput = "foo, bar")
  tmpdir <- create_test_files(commands = commands, inputs = inputs)
  wf_paths <- workflow_file_paths(
    path = tmpdir,
    inputs = "inputs.json",
    commands = "commands.json",
    results = "results_summary.json",
    functions = "functions.R"
  )
  steps <- extract_workflow_from_files(workflow_file_paths = wf_paths)
  expect_type(steps, "list")
  expect_length(steps, 2)
  expect_s3_class(steps[[1]], "workflowstep")
  expect_equal(steps[[1]]$name, "Step 1")
  expect_equal(steps[[1]]$label, "First Step")
  expect_equal(steps[[1]]$params[[1]]$type, "input")
  expect_equal(steps[[1]]$params[[1]]$label, "myinput")
  expect_equal(steps[[1]]$params[[1]]$value, "foo, bar")
  expect_equal(steps[[1]]$params[[2]]$type, "literal")
  expect_equal(steps[[1]]$params[[2]]$name, "pattern")
  expect_equal(steps[[1]]$params[[2]]$value, ", ")
  expect_equal(steps[[2]]$operation, "toupper")
  expect_equal(steps[[2]]$params[[1]]$type, "literal")
  expect_equal(steps[[2]]$params[[1]]$name, "x")
  expect_equal(steps[[2]]$params[[1]]$value, "hello world")
  unlink(tmpdir, recursive = TRUE)
})

test_that("extract_workflow_from_files returns empty list if commands.json missing", {
  tmpdir <- create_test_files()
  wf_paths <- workflow_file_paths(
    path = tmpdir,
    inputs = "inputs.json",
    commands = "commands.json",
    results = "results_summary.json",
    functions = "functions.R"
  )
  steps <- extract_workflow_from_files(workflow_file_paths = wf_paths)
  expect_type(steps, "list")
  expect_length(steps, 0)
  unlink(tmpdir, recursive = TRUE)
})

test_that("extract_workflow_from_files returns empty list if inputs.json is missing", {
  commands <- list(
    list(name = "Step 1", command = "strsplit", args = "x = @#*I*#@myinput@#*I*#@, pattern = ', '", label = "First Step", loop = "no")
  )
  # Provide an empty inputs.json file
  tmpdir <- create_test_files(commands = commands)
  wf_paths <- workflow_file_paths(
    path = tmpdir,
    inputs = "inputs.json",
    commands = "commands.json",
    results = "results_summary.json",
    functions = "functions.R"
  )
  steps <- extract_workflow_from_files(workflow_file_paths = wf_paths)
  expect_type(steps, "list")
  expect_length(steps, 0)
  unlink(tmpdir, recursive = TRUE)
})

test_that("extract_workflow_from_files supports custom file names", {
  commands <- list(
    list(name = "Step 1", command = "strsplit", args = "x = @#*I*#@myinput@#*I*#@, pattern = ', '", label = "First Step", loop = "no")
  )
  inputs <- list(myinput = "foo, bar")
  tmpdir <- create_test_files(commands = commands, inputs = inputs, commands_name = "custom_commands.json")
  wf_paths <- workflow_file_paths(
    path = tmpdir,
    inputs = "inputs.json",
    commands = "custom_commands.json",
    results = "results_summary.json",
    functions = "functions.R"
  )
  steps <- extract_workflow_from_files(workflow_file_paths = wf_paths)
  expect_type(steps, "list")
  expect_length(steps, 1)
  expect_equal(steps[[1]]$name, "Step 1")
  unlink(tmpdir, recursive = TRUE)
})

test_that("extract_workflow_from_files parses args as named list and supports multiple param types", {
  commands <- list(
    list(name = "Step 1", command = "strsplit", args = "x = 'foo, bar', pattern = ', ', n = 2", label = "First Step", loop = "no")
  )
  inputs <- list(myinput = "foo, bar")
  tmpdir <- create_test_files(commands = commands, inputs = inputs)
  wf_paths <- workflow_file_paths(
    path = tmpdir,
    inputs = "inputs.json",
    commands = "commands.json",
    results = "results_summary.json",
    functions = "functions.R"
  )
  steps <- extract_workflow_from_files(workflow_file_paths = wf_paths)
  expect_type(steps, "list")
  expect_length(steps, 1)
  expect_equal(steps[[1]]$params[[1]]$name, "x")
  expect_equal(steps[[1]]$params[[2]]$name, "pattern")
  expect_equal(steps[[1]]$params[[3]]$name, "n")
  expect_equal(steps[[1]]$params[[3]]$type, "literal")
  expect_equal(steps[[1]]$params[[3]]$value, "2")
  unlink(tmpdir, recursive = TRUE)
})

