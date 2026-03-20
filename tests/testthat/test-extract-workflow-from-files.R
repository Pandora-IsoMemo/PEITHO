
# Helper: create a temporary folder with commands.json, inputs.json, and results_summary.json
create_test_files <- function(
  commands = NULL,
  inputs = NULL,
  results = NULL,
  functions = NULL,
  commands_name = "commands.json", # names should be the same as used in workflow_file_paths()
  inputs_name = "inputs.json",
  results_name = "results_summary.json",
  functions_name = "functions.R"
) {
  tmpdir <- tempfile("workflowtest_")
  dir.create(tmpdir)
  if (!is.null(commands)) {
    write(
      jsonlite::toJSON(commands, auto_unbox = TRUE, pretty = TRUE),
      file.path(tmpdir, commands_name)
    )
  }
  if (!is.null(inputs)) {
    write(
      jsonlite::toJSON(inputs, auto_unbox = TRUE, pretty = TRUE),
      file.path(tmpdir, inputs_name)
    )
  }
  if (!is.null(results)) {
    write(
      jsonlite::toJSON(results, auto_unbox = TRUE, pretty = TRUE),
      file.path(tmpdir, results_name)
    )
  }
  if (!is.null(functions)) {
    write(functions, file.path(tmpdir, functions_name))
  }
  tmpdir
}

test_that("workflow_steps_from_files correctly returns from commands.json and inputs.json", {
  commands <- list(
    list(
      name = "Step 1",
      command = "strsplit",
      args = "x = @#*I*#@myinput@#*I*#@, pattern= ', '",
      label = "First Step",
      loop = "no"
    ),
    list(
      name = "Step 2",
      command = "toupper",
      args = "x = \"hello world\"",
      label = "Second Step",
      loop = "no"
    )
  )
  inputs <- list(myinput = "foo, bar")
  tmpdir <- create_test_files(commands = commands, inputs = inputs)
  wf_paths <- workflow_file_paths(path = tmpdir)
  steps <- workflow_steps_from_files(workflow_file_paths = wf_paths)
  expect_type(steps, "list")
  expect_length(steps, 2)
  expect_s3_class(steps[[1]], "workflowstep")
  expect_equal(steps[[1]]$name, "Step 1")
  expect_equal(steps[[1]]$label, "First Step")
  expect_equal(steps[[2]]$command, "toupper")
  expect_equal(steps[[2]]$args, "x = \"hello world\"")
  unlink(tmpdir, recursive = TRUE)
})

test_that("workflow_steps_from_files returns empty list if commands.json missing", {
  tmpdir <- create_test_files()
  wf_paths <- workflow_file_paths(path = tmpdir)
  steps <- suppressWarnings(workflow_steps_from_files(workflow_file_paths = wf_paths))
  expect_type(steps, "list")
  expect_length(steps, 0)
  unlink(tmpdir, recursive = TRUE)
})

test_that("workflow_steps_from_files returns workflow even if inputs.json is missing", {
  commands <- list(list(
    name = "Step 1",
    command = "strsplit",
    args = "x = @#*I*#@myinput@#*I*#@, pattern = ', '",
    label = "First Step",
    loop = "no"
  ))
  # Provide an empty inputs.json file
  tmpdir <- create_test_files(commands = commands, inputs = list())
  wf_paths <- workflow_file_paths(path = tmpdir)
  steps <- suppressWarnings(workflow_steps_from_files(workflow_file_paths = wf_paths))
  expect_type(steps, "list")
  expect_length(steps, 1)
  unlink(tmpdir, recursive = TRUE)
})

test_that("workflow_steps_from_files supports custom file names", {
  commands <- list(
    list(
      name = "Step 1",
      command = "strsplit",
      args = "x = @#*I*#@myinput@#*I*#@, pattern = ', '",
      label = "First Step",
      loop = "no"
    )
  )
  inputs <- list(myinput = "foo, bar")
  tmpdir <- create_test_files(
    commands = commands,
    inputs = inputs,
    commands_name = "custom_commands.json"
  )
  wf_paths <- workflow_file_paths(
    path = tmpdir,
    commands = "custom_commands.json"
  )
  steps <- workflow_steps_from_files(workflow_file_paths = wf_paths)
  expect_type(steps, "list")
  expect_length(steps, 1)
  expect_equal(steps[[1]]$name, "Step 1")
  unlink(tmpdir, recursive = TRUE)
})

test_that("is_input_tag identifies valid and invalid input tags", {
  expect_true(PEITHO:::is_input_tag("@#*I*#@myinput@#*I*#@"))
  expect_false(PEITHO:::is_input_tag("@#*I*#@myinput@#*I*#@@#*I*#@"))
  expect_true(PEITHO:::is_input_tag("@#*I*#@@#*I*#@"))
  expect_false(PEITHO:::is_input_tag("@#*L*#@myinput@#*L*#@"))
  expect_false(PEITHO:::is_input_tag("myinput"))

  tags <- c(
    "@#*I*#@a@#*I*#@",
    "@#*L*#@a@#*L*#@",
    "@#*I*#@a",
    "a@#*I*#@"
  )
  expect_equal(PEITHO:::is_input_tag(tags), c(TRUE, FALSE, FALSE, FALSE))
})

test_that("is_result_tag identifies valid and invalid result tags", {
  expect_true(PEITHO:::is_result_tag("@#*L*#@step_1@#*L*#@"))
  expect_true(PEITHO:::is_result_tag("@#*L*#@step_1@#*L*#@[2]")) # new format
  expect_false(PEITHO:::is_result_tag("@#*L*#@step_1@#*L*#@[2]@#*L*#@"))
  expect_true(PEITHO:::is_result_tag("@#*L*#@@#*L*#@"))
  expect_false(PEITHO:::is_result_tag("@#*I*#@step_1@#*I*#@"))
  expect_false(PEITHO:::is_result_tag("step_1"))

  tags <- c(
    "@#*L*#@x@#*L*#@",
    "@#*I*#@x@#*I*#@",
    "@#*L*#@x",
    "x@#*L*#@"
  )
  expect_equal(PEITHO:::is_result_tag(tags), c(TRUE, FALSE, FALSE, FALSE))
})

test_that("extract_tag_varname extracts and normalizes tag variable names", {
  expect_equal(
    PEITHO:::extract_tag_varname("@#*I*#@my input@#*I*#@", input_pattern()),
    "my_input"
  )
  expect_equal(
    PEITHO:::extract_tag_varname("@#*L*#@  step label  @#*L*#@", result_pattern()),
    "step_label"
  )
  expect_equal(
    PEITHO:::extract_tag_varname("@#*L*#@my step label@#*L*#@[2]", result_pattern()), # new format
    "my_step_label[2]"
  )

  tags <- c("@#*I*#@a b@#*I*#@", "@#*I*#@ c d @#*I*#@")
  expect_equal(
    PEITHO:::extract_tag_varname(tags, input_pattern()),
    c("a_b", "c_d")
  )

  # Current behavior: if the pattern does not match, the original value is normalized.
  expect_equal(
    PEITHO:::extract_tag_varname("plain value", input_pattern()),
    "plain_value"
  )
})
