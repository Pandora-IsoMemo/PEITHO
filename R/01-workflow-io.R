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

  identical(a$run_id, b$run_id) &&
    identical(a$step, b$step) &&
    identical(a$record_kind, b$record_kind) &&
    same_iteration_id
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
  result,
  error,
  timestamp = results_timestamp_utc()
) {
  err <- normalize_error_message(error)

  list(
    run_id = run_id,
    step = as.integer(step),
    entry = workflowstep$entry,
    name = workflowstep$name,
    label = workflowstep$label,
    record_kind = "iteration_result",
    iteration_id = as.integer(iteration_id),
    iteration_total = as.integer(iteration_total),
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
  is_looped <- isTRUE(steprun$meta$is_looped)

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
    status = if (length(errs)) "error" else "finished",
    result = if (is_looped) NULL else steprun$output,
    error = err_text,
    errors = if (is.null(err_text)) if (is_looped) NULL else "" else err_text,
    completed_iterations = if (is_looped) as.integer(length(steprun$output)) else NULL,
    error_count = as.integer(length(errs)),
    first_iteration_id = if (is_looped && length(steprun$output) > 0L) 1L else NULL,
    last_iteration_id = if (is_looped) as.integer(length(steprun$output)) else NULL,
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

    iter_records <- Filter(function(x) identical(x$record_kind, "iteration_result"), step_records)
    if (length(iter_records) > 0L) {
      iter_order <- order(vapply(iter_records, function(x) x$iteration_id %||% 0L, integer(1)))
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