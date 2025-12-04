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

test_that("extract_workflow_from_files returns workflow steps from commands.json", {
  commands <- list(
    list(name = "Step 1", command = "strsplit", args = "x = 'foo, bar', split = ', '", label = "First Step"),
    list(name = "Step 2", command = "toupper", args = "x = 'hello world'", label = "Second Step")
  )
  tmpdir <- create_test_files(commands)
  steps <- extract_workflow_from_files(path_to_folder = tmpdir)
  expect_type(steps, "list")
  expect_length(steps, 2)
  expect_s3_class(steps[[1]], "workflowstep")
  expect_equal(steps[[1]]$name, "Step 1")
  expect_equal(steps[[2]]$operation, "toupper")
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

test_that("extract_workflow_from_files loads inputs.txt for @#*I*#@ tags", {
  commands <- list(
    list(name = "Step 1", command = "strsplit", args = "x = @#*I*#@myinput@#*I*#@, split = ', '", label = "First Step")
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
  expect_true(any(grepl("input", steps[[1]]$params[[1]]$type)))
  expect_equal(steps[[1]]$params[[1]]$value, "foo, bar")
  unlink(tmpdir, recursive = TRUE)
})
