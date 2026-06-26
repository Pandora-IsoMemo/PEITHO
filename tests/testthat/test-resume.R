# Tests for resume cursor logic and partial-run continuation
# -------------------------------------------------------------------------
# Helpers
# -------------------------------------------------------------------------

make_results_file <- function(dir, records) {
  path <- file.path(dir, "results_summary.json")
  jsonlite::write_json(records, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  path
}

make_iter_record <- function(run_id, step, iteration_id, iteration_total,
                             status = "ok", result = "r", error = NULL) {
  list(
    run_id         = run_id,
    step           = as.integer(step),
    entry          = as.integer(step),
    name           = paste("Step", step),
    label          = paste("Step", step),
    record_kind    = "iteration_result",
    iteration_id   = as.integer(iteration_id),
    iteration_total = as.integer(iteration_total),
    status         = status,
    result         = result,
    error          = error,
    timestamp      = "2026-01-01T00:00:00Z"
  )
}

make_final_record <- function(run_id, step, record_kind = "step_summary",
                              status = "finished") {
  list(
    run_id      = run_id,
    step        = as.integer(step),
    entry       = as.integer(step),
    name        = paste("Step", step),
    label       = paste("Step", step),
    record_kind = record_kind,
    iteration_id = NULL,
    status      = status,
    result      = NULL,
    error       = NULL,
    timestamp   = "2026-01-01T00:00:00Z"
  )
}

# -------------------------------------------------------------------------
# get_resume_cursor
# -------------------------------------------------------------------------

test_that("get_resume_cursor returns step 1 and NULL iteration for empty file", {
  dir <- tempfile()
  dir.create(dir)
  make_results_file(dir, list())

  cursor <- PEITHO:::get_resume_cursor("run_1", dir)
  expect_equal(cursor$resume_step, 1L)
  expect_null(cursor$resume_iteration)
})

test_that("get_resume_cursor returns step 1 and NULL iteration when file is missing", {
  dir <- tempfile()
  dir.create(dir)

  cursor <- PEITHO:::get_resume_cursor("run_1", dir)
  expect_equal(cursor$resume_step, 1L)
  expect_null(cursor$resume_iteration)
})

test_that("get_resume_cursor advances past fully finished step", {
  dir <- tempfile()
  dir.create(dir)
  make_results_file(dir, list(
    make_final_record("run_1", 1L, status = "finished")
  ))

  cursor <- PEITHO:::get_resume_cursor("run_1", dir)
  expect_equal(cursor$resume_step, 2L)
  expect_null(cursor$resume_iteration)
})

test_that("get_resume_cursor returns correct iteration for partially completed looped step", {
  dir <- tempfile()
  dir.create(dir)
  make_results_file(dir, list(
    make_final_record("run_1", 1L, status = "finished"),
    make_iter_record("run_1", 2L, 1L, 5L),
    make_iter_record("run_1", 2L, 2L, 5L),
    make_iter_record("run_1", 2L, 3L, 5L)
  ))

  cursor <- PEITHO:::get_resume_cursor("run_1", dir)
  expect_equal(cursor$resume_step, 2L)
  expect_equal(cursor$resume_iteration, 4L)  # max(3) + 1
})

test_that("get_resume_cursor ignores records from other run_ids", {
  dir <- tempfile()
  dir.create(dir)
  make_results_file(dir, list(
    make_final_record("run_OTHER", 1L, status = "finished"),
    make_iter_record("run_OTHER", 2L, 1L, 3L)
  ))

  cursor <- PEITHO:::get_resume_cursor("run_1", dir)
  expect_equal(cursor$resume_step, 1L)
  expect_null(cursor$resume_iteration)
})

test_that("get_resume_cursor returns step after max when all steps are finished", {
  dir <- tempfile()
  dir.create(dir)
  make_results_file(dir, list(
    make_final_record("run_1", 1L, status = "finished"),
    make_final_record("run_1", 2L, status = "finished")
  ))

  cursor <- PEITHO:::get_resume_cursor("run_1", dir)
  expect_equal(cursor$resume_step, 3L)
  expect_null(cursor$resume_iteration)
})

# -------------------------------------------------------------------------
# read_iteration_records_for_step
# -------------------------------------------------------------------------

test_that("read_iteration_records_for_step returns ordered records for matching step", {
  dir <- tempfile()
  dir.create(dir)
  make_results_file(dir, list(
    make_iter_record("run_1", 2L, 3L, 5L),
    make_iter_record("run_1", 2L, 1L, 5L),
    make_iter_record("run_1", 2L, 2L, 5L),
    make_iter_record("run_1", 1L, 1L, 3L)   # different step — should be excluded
  ))

  records <- PEITHO:::read_iteration_records_for_step("run_1", 2L, dir)
  expect_length(records, 3L)
  ids <- vapply(records, function(r) r$iteration_id, integer(1))
  expect_equal(ids, 1:3)
})

test_that("read_iteration_records_for_step returns empty list when no records match", {
  dir <- tempfile()
  dir.create(dir)
  make_results_file(dir, list(
    make_iter_record("run_1", 1L, 1L, 2L)
  ))

  records <- PEITHO:::read_iteration_records_for_step("run_1", 2L, dir)
  expect_length(records, 0L)
})

# -------------------------------------------------------------------------
# run.workflowstep with resume_from_iteration
# -------------------------------------------------------------------------

test_that("run.workflowstep with resume_from_iteration skips completed iterations", {
  # Arrange: step loops over 3 values; first 2 are already in JSON
  dir <- tempfile()
  dir.create(dir)
  results_path <- file.path(dir, "results_summary.json")
  make_results_file(dir, list(
    make_iter_record("run_1", 2L, 1L, 3L, result = "HELLO"),
    make_iter_record("run_1", 2L, 2L, 3L, result = "WORLD")
  ))

  call_env <- new.env(parent = emptyenv())
  call_env$count <- 0L
  spy_toupper <- function(x) {
    call_env$count <- call_env$count + 1L
    toupper(x)
  }
  assign("spy_toupper", spy_toupper, envir = .GlobalEnv)
  on.exit(rm("spy_toupper", envir = .GlobalEnv), add = TRUE)

  step_spy <- new_workflowstep(
    entry   = 2L,
    command = "spy_toupper",
    args    = "x = @#*L*#@step 1@#*L*#@",
    loop    = "auto"
  )
  state <- new_workflowstate(run_id = "run_1")
  state$results_by_name[["step_1"]] <- c("hello", "world", "foo")

  steprun <- run.workflowstep(
    step_spy,
    state,
    env                  = globalenv(),
    step_i               = 2L,
    step_idx             = 2L,
    results_path         = results_path,
    resume_from_iteration = 3L
  )

  # Only iteration 3 should have been executed
  expect_equal(call_env$count, 1L)

  # All 3 outputs should be present (2 from JSON, 1 freshly run)
  expect_length(steprun$output, 3L)
  expect_equal(steprun$output[[3]], "FOO")
})

test_that("run.workflowstep with resume_from_iteration = NULL runs all iterations", {
  step <- new_workflowstep(
    entry   = 2L,
    command = "toupper",
    args    = "x = @#*L*#@step 1@#*L*#@",
    loop    = "auto"
  )
  state <- new_workflowstate(run_id = "run_full")
  state$results_by_name[["step_1"]] <- c("a", "b", "c")

  steprun <- run.workflowstep(step, state, step_i = 2L, env = globalenv(), resume_from_iteration = NULL)
  expect_length(steprun$output, 3L)
  expect_equal(steprun$output, list("A", "B", "C"))
})

# -------------------------------------------------------------------------
# run.workflow with resume_run_id
# -------------------------------------------------------------------------

test_that("run.workflow with resume_run_id skips completed steps", {
  dir <- tempfile()
  dir.create(dir)
  wfp <- PEITHO:::workflow_file_paths(
    path     = dir,
    inputs   = "inputs.json",
    commands = "commands.json",
    results  = "results_summary.json",
    functions = NULL
  )
  jsonlite::write_json(list(), wfp$inputs_path, auto_unbox = TRUE, pretty = TRUE)
  jsonlite::write_json(list(), wfp$commands_path, auto_unbox = TRUE, pretty = TRUE)

  # Pre-populate results: step 1 already finished
  run_id_to_resume <- "20260101000000_aabbccdd"
  make_results_file(dir, list(
    make_final_record(run_id_to_resume, 1L, status = "finished")
  ))

  call_log <- character(0)
  step1_fn <- function(x) { call_log <<- c(call_log, "step1"); toupper(x) }
  step2_fn <- function(x) { call_log <<- c(call_log, "step2"); toupper(x) }
  assign("step1_fn", step1_fn, envir = .GlobalEnv)
  assign("step2_fn", step2_fn, envir = .GlobalEnv)
  on.exit({
    rm("step1_fn", envir = .GlobalEnv)
    rm("step2_fn", envir = .GlobalEnv)
  }, add = TRUE)

  step1 <- new_workflowstep(entry = 1L, command = "step1_fn",
                             args = "x = \"hello\"", loop = "no")
  step2 <- new_workflowstep(entry = 2L, command = "step2_fn",
                             args = "x = \"world\"", loop = "no")

  wf <- new_workflow(
    name = "Resume test",
    steps = list(step1, step2),
    input_list = list(),
    use_peitho_folder = FALSE
  )
  # Attach file paths so the runner can write results
  wf$workflow_file_paths <- wfp

  result <- run.workflow(wf, resume_run_id = run_id_to_resume)

  # step1 must NOT have been called; step2 must have been called
  expect_false("step1" %in% call_log)
  expect_true("step2" %in% call_log)

  # Returned run_id must equal the resumed one
  expect_equal(result$run_id, run_id_to_resume)
})

test_that("run.workflow without resume_run_id generates a fresh run_id", {
  step <- new_workflowstep(
    entry = 1L, command = "toupper",
    args = "x = \"hello\"", loop = "no"
  )
  wf <- new_workflow(
    name = "Fresh run",
    steps = list(step),
    input_list = list(),
    use_peitho_folder = FALSE
  )

  result <- run.workflow(wf)
  expect_type(result$run_id, "character")
  expect_true(nzchar(result$run_id))
})

test_that("get_resume_cursor returns sample-aware cursor for sample_result records", {
  dir <- tempfile()
  dir.create(dir)

  records <- list(
    list(
      run_id = "run_sample",
      step = 1L,
      entry = 1L,
      name = "Step 1",
      label = "Step 1",
      record_kind = "sample_result",
      sample_id = 1L,
      sample_total = 2L,
      iteration_id = 1L,
      iteration_total = 2L,
      status = "ok",
      result = "A",
      error = NULL
    ),
    list(
      run_id = "run_sample",
      step = 1L,
      entry = 1L,
      name = "Step 1",
      label = "Step 1",
      record_kind = "sample_result",
      sample_id = 1L,
      sample_total = 2L,
      iteration_id = 2L,
      iteration_total = 2L,
      status = "ok",
      result = "B",
      error = NULL
    )
  )

  make_results_file(dir, records)

  cursor <- PEITHO:::get_resume_cursor("run_sample", dir)
  expect_equal(cursor$resume_step, 1L)
  expect_equal(cursor$resume_sample, 2L)
  expect_equal(cursor$resume_iteration, 2L)
})

test_that("run.workflowstep resumes from sample + iteration for sampled runs", {
  dir <- tempfile()
  dir.create(dir)
  results_path <- file.path(dir, "results_summary.json")

  # Completed sample 1 (all iterations) and sample 2 iteration 1
  make_results_file(dir, list(
    list(
      run_id = "run_sample_resume",
      step = 2L,
      entry = 2L,
      name = "Step 2",
      label = "Step 2",
      record_kind = "sample_result",
      sample_id = 1L,
      sample_total = 2L,
      iteration_id = 1L,
      iteration_total = 2L,
      status = "ok",
      result = "HELLO",
      error = NULL
    ),
    list(
      run_id = "run_sample_resume",
      step = 2L,
      entry = 2L,
      name = "Step 2",
      label = "Step 2",
      record_kind = "sample_result",
      sample_id = 1L,
      sample_total = 2L,
      iteration_id = 2L,
      iteration_total = 2L,
      status = "ok",
      result = "WORLD",
      error = NULL
    ),
    list(
      run_id = "run_sample_resume",
      step = 2L,
      entry = 2L,
      name = "Step 2",
      label = "Step 2",
      record_kind = "sample_result",
      sample_id = 2L,
      sample_total = 2L,
      iteration_id = 1L,
      iteration_total = 2L,
      status = "ok",
      result = "HELLO",
      error = NULL
    )
  ))

  call_env <- new.env(parent = emptyenv())
  call_env$count <- 0L
  spy_toupper <- function(x) {
    call_env$count <- call_env$count + 1L
    toupper(x)
  }
  assign("spy_toupper", spy_toupper, envir = .GlobalEnv)
  on.exit(rm("spy_toupper", envir = .GlobalEnv), add = TRUE)

  step <- new_workflowstep(
    entry = 2L,
    command = "spy_toupper",
    args = "x = @#*L*#@step 1@#*L*#@",
    iteration = "auto",
    samples = 2
  )
  state <- new_workflowstate(run_id = "run_sample_resume")
  state$results_by_name[["step_1"]] <- c("hello", "world")

  steprun <- run.workflowstep(
    step,
    state,
    env = globalenv(),
    step_i = 2L,
    step_idx = 2L,
    results_path = results_path,
    resume_from_sample = 2L,
    resume_from_iteration = 2L
  )

  expect_equal(call_env$count, 1L)
  expect_length(steprun$output, 4L)
  expect_equal(steprun$output[[4]], "WORLD")
})
