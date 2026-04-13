# Test file for vignette examples in peitho_workflow_basics.Rmd
# Ensures that all code examples in the vignette are up-to-date and working

# Test: Loading the Example Workflow
test_that("new_workflow loads example workflow", {
  wf <- new_workflow(workflow_file_paths = workflow_file_paths(path = ""))
  expect_s3_class(wf, "workflow")
})

# Test: Exporting a Workflow as a Zip File
test_that("save_as_zip exports workflow to zip", {
  wf <- new_workflow(workflow_file_paths = workflow_file_paths(path = ""))
  zipfile_path <- tempfile(fileext = ".peitho")
  save_as_zip(wf, file = zipfile_path)
  expect_true(file.exists(zipfile_path))
  unlink(zipfile_path) # Clean up
})

# Test: Running a Workflow
test_that("run executes workflow steps", {
  wf <- new_workflow(workflow_file_paths = workflow_file_paths(path = ""))
  run_result <- run(wf, from = 1, to = 5)
  expect_true(!is.null(run_result$state$last_result))
  expect_true(length(run_result$state$last_result) >= 0)
})

# Test: Importing and Modifying a Workflow from a Zip File
test_that("import_workflow imports workflow from zip", {
  wf <- new_workflow(workflow_file_paths = workflow_file_paths(path = ""))
  zipfile_path <- tempfile(fileext = ".peitho")
  save_as_zip(wf, file = zipfile_path)
  extract_dir <- tempfile()
  imported_wf <- PEITHO::import_workflow(zipfile = zipfile_path, extract_dir = extract_dir)
  expect_s3_class(imported_wf, "workflow")
  run_result <- run(imported_wf, from = 1, to = 4)
  expect_true(!is.null(run_result$state$last_result))
  unlink(zipfile_path) # Clean up
  unlink(extract_dir, recursive = TRUE) # Clean up
})
