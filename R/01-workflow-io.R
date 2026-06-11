# -------------------------------------------------------------------------
# Workflow input/output helpers
# -------------------------------------------------------------------------
# This file provides JSON and ZIP I/O utilities used to read, update,
# and persist workflow data and run summaries.

# -------------------------------------------------------------------------
# Read/write workflow results summary JSON
# -------------------------------------------------------------------------

read_json_if_exists <- function(path) {
  if (!file_nonempty(path)) return(list())
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

results_file_path <- function(path_to_folder, results_file = "results_summary.json") {
  file.path(path_to_folder, results_file)
}

results_timestamp_utc <- function(time = Sys.time()) {
  format(as.POSIXct(time, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

normalize_error_message <- function(err) {
  if (is.null(err)) return(NULL)

  if (inherits(err, "condition")) {
    msg <- conditionMessage(err)
    return(if (nzchar(msg)) msg else NULL)
  }

  if (is.character(err)) {
    err <- err[!is.na(err)]
    err <- err[nzchar(err)]
    return(if (length(err)) paste(err, collapse = "\n") else NULL)
  }

  msg <- tryCatch(
    paste(capture.output(str(err)), collapse = "\n"),
    error = function(...) NULL
  )

  if (!is.null(msg) && nzchar(msg)) return(msg)

  as.character(err)
}

is_same_record_key <- function(a, b) {
  same_iteration_id <- is.null(a$iteration_id) && is.null(b$iteration_id) || identical(a$iteration_id, b$iteration_id)
  same_sample_id <- is.null(a$sample_id) && is.null(b$sample_id) || identical(a$sample_id, b$sample_id)

  identical(a$run_id, b$run_id) &&
    identical(a$step, b$step) &&
    identical(a$record_kind, b$record_kind) &&
    same_iteration_id &&
    same_sample_id
}

upsert_results_record <- function(
  record,
  path_to_folder,
  results_file = "results_summary.json"
) {
  path <- results_file_path(path_to_folder, results_file)
  records <- read_json_if_exists(path)

  replace_idx <- which(vapply(records, function(x) is_same_record_key(x, record), logical(1)))
  if (length(replace_idx)) {
    records[[replace_idx[1L]]] <- record
  } else {
    records[[length(records) + 1L]] <- record
  }

  jsonlite::write_json(
    records,
    path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  invisible(TRUE)
}

new_iteration_result_record <- function(
  run_id,
  step,
  workflowstep,
  iteration_id,
  iteration_total,
  sample_id = 1L,
  sample_total = 1L,
  result,
  error,
  timestamp = results_timestamp_utc()
) {
  err <- normalize_error_message(error)
  sample_id <- as.integer(sample_id %||% 1L)
  sample_total <- as.integer(sample_total %||% 1L)
  has_samples <- sample_total > 1L

  list(
    run_id = run_id,
    step = as.integer(step),
    entry = workflowstep$entry,
    name = workflowstep$name,
    label = workflowstep$label,
    record_kind = if (has_samples) "sample_result" else "iteration_result",
    iteration_id = as.integer(iteration_id),
    iteration_total = as.integer(iteration_total),
    sample_id = if (has_samples) sample_id else NULL,
    sample_total = if (has_samples) sample_total else NULL,
    status = if (is.null(err) || !nzchar(err)) "ok" else "error",
    result = result,
    error = err,
    errors = if (is.null(err)) "" else err,
    timestamp = timestamp
  )
}

new_step_final_record <- function(
  steprun,
  step,
  timestamp = results_timestamp_utc()
) {
  iteration_total <- steprun$meta$iteration_total %||% length(steprun$output)
  sample_total <- steprun$meta$sample_total %||% 1L
  is_looped <- isTRUE(steprun$meta$is_looped)
  completed_iterations <- steprun$meta$completed_iterations %||% if (sample_total > 1L) length(steprun$output) %/% sample_total else length(steprun$output)

  errs <- lapply(steprun$error %||% list(), normalize_error_message)
  errs <- Filter(function(x) !is.null(x) && nzchar(x), errs)

  err_text <- if (length(errs)) paste(errs, collapse = "\n") else NULL

  list(
    run_id = steprun$run_id,
    step = as.integer(step),
    entry = steprun$step$entry,
    name = steprun$step$name,
    label = steprun$step$label,
    record_kind = if (is_looped) "step_summary" else "step_result",
    iteration_id = NULL,
    iteration_total = if (is_looped) as.integer(iteration_total) else NULL,
    sample_id = NULL,
    sample_total = if (sample_total > 1L) as.integer(sample_total) else NULL,
    status = if (length(errs)) "error" else "finished",
    result = if (is_looped) NULL else steprun$output,
    error = err_text,
    errors = if (is.null(err_text)) if (is_looped) NULL else "" else err_text,
    completed_iterations = if (is_looped) as.integer(completed_iterations) else NULL,
    completed_samples = if (is_looped && sample_total > 1L) as.integer(sample_total) else NULL,
    error_count = as.integer(length(errs)),
    first_iteration_id = if (is_looped && length(steprun$output) > 0L) 1L else NULL,
    last_iteration_id = if (is_looped) as.integer(completed_iterations) else NULL,
    first_sample_id = if (is_looped && sample_total > 1L) 1L else NULL,
    last_sample_id = if (is_looped && sample_total > 1L) as.integer(sample_total) else NULL,
    timestamp = timestamp
  )
}

#' Reconstruct legacy per-step results from flat iteration records
#'
#' Converts the flat list of result records (written by the intermediate
#' persistence layer) back into the legacy per-step summary format that
#' consumers such as the Shiny results table expect. For looped steps the
#' individual `iteration_result` records are collected in order and their
#' results are assembled into a list; for non-looped steps the single
#' `step_result` record is returned as-is.
#'
#' @param records A list of result records as read from the results JSON file.
#' @param run_id Optional character string. When supplied only records matching
#'   this run identifier are included.
#' @return A list with one element per workflow step, each element being a
#'   named list with fields `run_id`, `entry`, `name`, `label`, `result`, and
#'   `errors` — matching the legacy per-step summary format.
#' @export
reconstruct_step_results <- function(records, run_id = NULL) {
  if (!is.null(run_id)) {
    records <- Filter(function(x) identical(x$run_id, run_id), records)
  }
  if (length(records) == 0L) return(list())

  step_ids <- sort(unique(vapply(records, function(x) as.integer(x$step %||% x$entry), integer(1))))

  lapply(step_ids, function(step_id) {
    step_records <- Filter(function(x) as.integer(x$step %||% x$entry) == step_id, records)

    iter_records <- Filter(function(x) x$record_kind %in% c("iteration_result", "sample_result"), step_records)
    if (length(iter_records) > 0L) {
      iter_order <- order(
        vapply(iter_records, function(x) as.integer(x$iteration_id %||% 0L), integer(1)),
        vapply(iter_records, function(x) as.integer(x$sample_id %||% 1L), integer(1))
      )
      iter_records <- iter_records[iter_order]
    }

    final_record <- Filter(function(x) x$record_kind %in% c("step_result", "step_summary"), step_records)
    final_record <- if (length(final_record)) final_record[[length(final_record)]] else step_records[[1L]]

    errors <- vapply(iter_records, function(x) x$error %||% "", character(1))
    errors <- errors[nzchar(errors)]

    list(
      run_id = final_record$run_id,
      entry = final_record$entry,
      name = final_record$name,
      label = final_record$label,
      result = if (length(iter_records)) {
        lapply(iter_records, function(x) x$result)
      } else {
        final_record$result
      },
      errors = if (length(errors)) errors else ""
    )
  })
}

#' Read and reconstruct per-step results from the results JSON file
#'
#' Reads the flat results JSON file from disk and reconstructs the legacy
#' per-step summary format via `reconstruct_step_results()`. Use this as the
#' compatibility entry-point for consumers that expect one result entry per
#' workflow step (e.g. resume logic, export helpers).
#'
#' @param path_to_folder Path to the workflow folder containing the results file.
#' @param results_file Name of the results JSON file (default:
#'   `"results_summary.json"`).
#' @param run_id Optional character string. When supplied only records matching
#'   this run identifier are included.
#' @return A list with one element per workflow step in the legacy per-step
#'   summary format. See `reconstruct_step_results()` for details.
#' @export
read_reconstructed_step_results <- function(
  path_to_folder,
  results_file = "results_summary.json",
  run_id = NULL
) {
  records <- read_json_if_exists(results_file_path(path_to_folder, results_file))
  reconstruct_step_results(records, run_id = run_id)
}

# -------------------------------------------------------------------------
# Resume cursor helpers
# -------------------------------------------------------------------------

#' Determine where to resume a partially completed workflow run
#'
#' Reads the flat results records and returns the step index and iteration
#' index at which execution should continue.  A step is considered finished
#' when a `step_result` or `step_summary` record with `status = "finished"`
#' exists for that step.  For a looped step that started but never finished,
#' `resume_iteration` is set to `max(completed iteration_id) + 1`.
#'
#' @param run_id Character.  The run identifier to search for.
#' @param path_to_folder Path to the workflow folder.
#' @param results_file Name of the results JSON file (default:
#'   `"results_summary.json"`).
#' @return A list with three elements:
#'   \describe{
#'     \item{`resume_step`}{Integer.  Index of the first step to execute.}
#'     \item{`resume_iteration`}{Integer or `NULL`.  For a looped step that
#'       was partially completed, the first iteration index that still needs
#'       to run.  `NULL` when no iterations have been recorded yet.}
#'     \item{`resume_sample`}{Integer or `NULL`.  For sampled runs, the
#'       sample index for the first unfinished pair. `NULL` for legacy
#'       iteration-only rows.}
#'   }
#' @export
get_resume_cursor <- function(
  run_id,
  path_to_folder,
  results_file = "results_summary.json"
) {
  records <- read_json_if_exists(results_file_path(path_to_folder, results_file))

  if (!is.null(run_id) && nzchar(run_id)) {
    records <- Filter(function(x) identical(x$run_id, run_id), records)
  }

  if (length(records) == 0L) {
    return(list(resume_step = 1L, resume_iteration = NULL, resume_sample = NULL))
  }

  # Steps that have a completed final record
  final_records <- Filter(
    function(x) x$record_kind %in% c("step_result", "step_summary"),
    records
  )
  finished_steps <- vapply(
    Filter(function(x) identical(x$status, "finished"), final_records),
    function(x) as.integer(x$step %||% 0L),
    integer(1)
  )

  # All step indices seen in any record
  all_steps <- sort(unique(vapply(
    records,
    function(x) as.integer(x$step %||% 0L),
    integer(1)
  )))
  all_steps <- all_steps[all_steps > 0L]

  if (length(all_steps) == 0L) {
    return(list(resume_step = 1L, resume_iteration = NULL, resume_sample = NULL))
  }

  # First step not yet finished
  unfinished <- setdiff(all_steps, finished_steps)
  resume_step <- if (length(unfinished) > 0L) {
    min(unfinished)
  } else {
    # All seen steps finished — continue from the step after the last
    max(all_steps) + 1L
  }

  # For the resume step, find the maximum completed iteration_id
  detail_records_for_step <- Filter(
    function(x) {
      as.integer(x$step %||% 0L) == resume_step &&
        x$record_kind %in% c("iteration_result", "sample_result")
    },
    records
  )

  resume_iteration <- NULL
  resume_sample <- NULL
  if (length(detail_records_for_step) > 0L) {
    sample_ids <- vapply(detail_records_for_step, function(x) as.integer(x$sample_id %||% 1L), integer(1))
    iter_ids <- vapply(detail_records_for_step, function(x) as.integer(x$iteration_id %||% 0L), integer(1))
    ord <- order(iter_ids, sample_ids)
    last <- detail_records_for_step[[ord[length(ord)]]]

    last_sample <- as.integer(last$sample_id %||% 1L)
    last_iteration <- as.integer(last$iteration_id %||% 0L)
    iteration_total <- as.integer(last$iteration_total %||% 1L)
    sample_total <- as.integer(last$sample_total %||% 1L)

    if (last_sample < sample_total) {
      resume_sample <- last_sample + 1L
      resume_iteration <- last_iteration
    } else if (last_iteration < iteration_total) {
      resume_sample <- 1L
      resume_iteration <- last_iteration + 1L
    }

    # Legacy iteration-only records should keep sample cursor NULL.
    if (sample_total <= 1L) {
      resume_sample <- NULL
      if (!is.null(resume_iteration) && resume_iteration == 1L) {
        resume_iteration <- NULL
      }
    }
  }

  list(resume_step = resume_step, resume_iteration = resume_iteration, resume_sample = resume_sample)
}

#' Read existing iteration records for a specific step and run
#'
#' Returns detail records (`iteration_result` and `sample_result`) ordered by
#' `(iteration_id, sample_id)`.
#' Used by `run.workflowstep()` to reconstruct skipped-iteration outputs
#' when resuming a partially completed looped step.
#'
#' @param run_id Character.  The run identifier.
#' @param step Integer.  The step index.
#' @param path_to_folder Path to the workflow folder.
#' @param results_file Name of the results JSON file (default:
#'   `"results_summary.json"`).
#' @return A list of matching records, each a named list, ordered by
#'   `iteration_id`.
read_iteration_records_for_step <- function(
  run_id,
  step,
  path_to_folder,
  results_file = "results_summary.json"
) {
  records <- read_json_if_exists(results_file_path(path_to_folder, results_file))
  step <- as.integer(step)

  iter_records <- Filter(
    function(x) {
      identical(x$run_id, run_id) &&
        as.integer(x$step %||% 0L) == step &&
        x$record_kind %in% c("iteration_result", "sample_result")
    },
    records
  )

  if (length(iter_records) > 0L) {
    ord <- order(vapply(
      iter_records,
      function(x) as.integer(x$iteration_id %||% 0L),
      integer(1)
    ), vapply(
      iter_records,
      function(x) as.integer(x$sample_id %||% 1L),
      integer(1)
    ))
    iter_records <- iter_records[ord]
  }

  iter_records
}

update_json_summary <- function(
  result,
  idx,
  path_to_folder,
  results_file    = "results_summary.json"
) {
  # load json
  results <- read_json_if_exists(path = file.path(path_to_folder, results_file))

  if (idx <= length(results)) {
    results[[idx]] <- result
    # remove all later results
    if (length(results) > idx) {
      results <- results[1:idx]
    }
  } else {
    results[[length(results) + 1L]] <- result
  }
  # write into results file
  jsonlite::write_json(
    results,
    file.path(path_to_folder, results_file),
    auto_unbox = TRUE,
    pretty = TRUE
  )
}


# -------------------------------------------------------------------------
# Save/load workflow as ZIP file
# -------------------------------------------------------------------------

#' Save workflow as a ZIP file
#'
#' @param x A `workflow` object.
#' @param file Path to the output ZIP file.
#' @param ... Additional arguments (not used).
#' @export
save_as_zip.workflow <- function(
  x,
  file,
  ...
) {
  DataTools::build_download_zip(
    zipfile = file,
    package_name = "PEITHO",
    include_paths = c(
      x$workflow_file_paths$inputs_path,
      x$workflow_file_paths$commands_path,
      x$workflow_file_paths$results_path,
      x$workflow_file_paths$functions_path
    ),
    include_root = x$workflow_file_paths$path_to_folder
  )
}

#' Import workflow from a ZIP file
#'
#' @param zipfile Path to the input ZIP file.
#' @param extract_dir Directory to extract the workflow files to.
#' @return A `workflow` object created from the extracted files.
#' @export
import_workflow <- function(
  zipfile,
  extract_dir
) {
  DataTools::import_bundle_zip(
    zipfile = zipfile,
    extract_dir = extract_dir,
    keep_dir = TRUE
  )

  PEITHO::new_workflow(
    workflow_file_paths = workflow_file_paths(path = extract_dir)
  )
}


is_file_backed_workflow <- function(x) {
  !is.null(x$workflow_file_paths) && length(x$workflow_file_paths) > 0L
}


write_commands_json <- function(x) {
  if (!is_file_backed_workflow(x)) return(invisible(FALSE))

  path <- x$workflow_file_paths$commands_path
  if (is.null(path) || !nzchar(path)) return(invisible(FALSE))

  jsonlite::write_json(
    as.commands_record(x),
    path = path,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  invisible(TRUE)
}

write_inputs_json <- function(x) {
  if (!is_file_backed_workflow(x)) return(invisible(FALSE))

  path <- x$workflow_file_paths$inputs_path
  if (is.null(path) || !nzchar(path)) return(invisible(FALSE))

  jsonlite::write_json(
    x$input_list,
    path = path,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  invisible(TRUE)
}