# -------------------------------------------------------------------------
# Workflow input/output helpers
# -------------------------------------------------------------------------
# This file provides JSON and ZIP I/O utilities used to read, update,
# and persist workflow data and run summaries.

read_json_if_exists <- function(path) {
  if (!file_nonempty(path)) return(list())
  jsonlite::fromJSON(path, simplifyVector = FALSE)
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