# Helper: create a temporary folder with commands.json and inputs.txt
create_test_files <- function(commands, input_txt = NULL) {
  tmpdir <- tempfile("workflowtest_")
  dir.create(tmpdir)
  write(jsonlite::toJSON(commands, auto_unbox = TRUE, pretty = TRUE), file.path(tmpdir, "commands.json"))
  if (!is.null(input_txt)) {
    writeLines(input_txt, file.path(tmpdir, "inputs.txt"))
  }
  tmpdir
}

test_that("extract_workflow_from_files returns workflow steps from commands.json with correct param types and labels", {
  commands <- list(
    list(name = "Step 1", command = "strsplit", args = "x = 'foo, bar', split = ', '", label = "First Step", loop = "no"),
    list(name = "Step 2", command = "toupper", args = "x = 'hello world'", label = "Second Step", loop = "no"),
    list(name = "Step 3", command = "strsplit", args = 'x = "foo, bar", split = ", "', label = "Third Step", loop = "no")
  )
  # create an empty inputs.txt file to avoid warning
  tmpdir <- create_test_files(commands, input_txt = "")
  steps <- extract_workflow_from_files(path_to_folder = tmpdir)
  expect_type(steps, "list")
  expect_length(steps, 3)
  expect_s3_class(steps[[1]], "workflowstep")
  expect_equal(steps[[1]]$name, "Step 1")
  expect_equal(steps[[1]]$label, "First Step")
  expect_equal(steps[[1]]$params[[1]]$type, "literal")
  expect_equal(steps[[1]]$params[[1]]$name, "x")
  expect_equal(steps[[1]]$params[[1]]$value, "foo, bar")
  expect_equal(steps[[1]]$params[[2]]$type, "literal")
  expect_equal(steps[[1]]$params[[2]]$name, "split")
  expect_equal(steps[[1]]$params[[2]]$value, ", ")
  expect_equal(steps[[2]]$operation, "toupper")
  expect_equal(steps[[2]]$params[[1]]$type, "literal")
  expect_equal(steps[[2]]$params[[1]]$name, "x")
  expect_equal(steps[[2]]$params[[1]]$value, "hello world")
  expect_equal(steps[[3]]$name, "Step 3")
  expect_equal(steps[[3]]$label, "Third Step")
  expect_equal(steps[[3]]$params[[1]]$value, "foo, bar")
  expect_equal(steps[[3]]$params[[2]]$value, ", ")
  unlink(tmpdir, recursive = TRUE)
})

test_that("extract_workflow_from_files handles missing commands.json gracefully", {
  tmpdir <- tempfile("workflowtest_")
  dir.create(tmpdir)
  steps <- extract_workflow_from_files(path_to_folder = tmpdir)
  expect_type(steps, "list")
  expect_length(steps, 0)
  unlink(tmpdir, recursive = TRUE)
})

test_that("extract_workflow_from_files loads inputs.txt for @#*I*#@ tags and sets param type to input", {
  commands <- list(
    list(name = "Step 1", command = "strsplit", args = "x = @#*I*#@myinput@#*I*#@, split = ', '", label = "First Step", loop = "no")
  )
  input_txt <- c(
    "@#*I*#@myinput@#*I*#@",
    "foo, bar",
    "@#*I*#@myinput@#*I*#@"
  )
  tmpdir <- create_test_files(commands, input_txt)
  steps <- extract_workflow_from_files(path_to_folder = tmpdir)
  expect_type(steps, "list")
  expect_length(steps, 1)
  expect_s3_class(steps[[1]], "workflowstep")
  expect_equal(steps[[1]]$params[[1]]$type, "input")
  expect_equal(steps[[1]]$params[[1]]$label, "myinput")
  expect_equal(steps[[1]]$params[[1]]$value, "foo, bar")
  expect_equal(steps[[1]]$params[[2]]$type, "literal")
  expect_equal(steps[[1]]$params[[2]]$name, "split")
  expect_equal(steps[[1]]$params[[2]]$value, ", ")
  unlink(tmpdir, recursive = TRUE)
})
