# Test for PEITHO workflow zip export/import and run

test_that("workflow can be exported, imported, and run as in example_wf.R", {
  skip_on_cran()
  skip_if_not_installed("PEITHO")

  # Use a tempdir for outputs
  tmp_dir <- tempdir()
  zipfile_path <- file.path(tmp_dir, "my_workflow.peitho")
  extract_dir <- file.path(tmp_dir, "imported")

  # Create workflow from example files
  my_wf <- PEITHO::new_workflow(
    workflow_file_paths = PEITHO::workflow_file_paths(path = "")
  )

  # Export as zip
  PEITHO::save_as_zip(my_wf, file = zipfile_path)
  expect_true(file.exists(zipfile_path))

  # Run workflow (steps 1 to 5)
  my_run_1 <- PEITHO::run(my_wf, from = 1, to = 5)
  expect_type(my_run_1$state$last_result, "character")
  expect_length(my_run_1$state$last_result, 1)

  # Truncate output
  trunc_out <- PEITHO:::trunc(my_run_1$state$last_result, n_char = 100)
  trunc_out <- strsplit(trunc_out, split = " ..., ")[[1]]
  trunc_out <- gsub(" ...", "", trunc_out)
  expect_true(all(nchar(trunc_out) <= 100))

  # Import from zip
  my_wf_imported <- PEITHO::import_workflow(
    zipfile = zipfile_path,
    extract_dir = extract_dir
  )
  expect_true(dir.exists(extract_dir))

  # Run imported workflow (steps 1 to 4)
  my_run_2 <- PEITHO::run(my_wf_imported, from = 1, to = 4)
  expect_type(my_run_2$state$last_result, "list")
  expect_length(my_run_2$state$last_result, 3)

  # Truncate output
  trunc_out2 <- PEITHO:::trunc(my_run_2$state$last_result, n_char = 100)
  trunc_out2 <- strsplit(trunc_out2, split = " ..., ")[[1]]
  trunc_out2 <- gsub(" ...", "", trunc_out2)
  expect_true(all(nchar(trunc_out2) <= 100))
})
