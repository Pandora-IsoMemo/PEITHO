test_that("new_workflow duplicate step handling is configurable", {
  step1 <- new_workflowstep(entry = 1, command = "strsplit", name = "Step A")
  step2 <- new_workflowstep(entry = 1, command = "strsplit", name = "Step B")

  expect_error(
    new_workflow(
      name = "Duplicate Steps Error",
      steps = list(step1, step2),
      input_list = list(input_1 = "x"),
      use_peitho_folder = FALSE,
      error_on_warn = TRUE
    ),
    "duplicate step entry"
  )

  wf <- expect_warning(
    new_workflow(
      name = "Duplicate Steps Warn",
      steps = list(step1, step2),
      input_list = list(input_1 = "x"),
      use_peitho_folder = FALSE
    ),
    "duplicate step entry"
  )
  expect_s3_class(wf, "workflow")
})
