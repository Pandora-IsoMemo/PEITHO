# ---- workflow class ----

#' Helper to get file paths for PEITHO workflow files
#' 
#' This function constructs full file paths for the workflow files located in a specified folder.
#' Validation of file existence is performed in `extract_workflow_from_files()`.
#' 
#' @param path Path to folder containing workflow files (default: PEITHO example folder).
#' @param inputs Name of the inputs file (default: "inputs.json").
#' @param commands Name of the commands file (default: "commands.json").
#' @param results Name of the results summary file (default: "results.json").
#' @param functions Name of the R script file containing custom functions (default: "functions.R").
#' @return A list with full paths to the specified files.
#' @export
workflow_file_paths <- function(
  path      = "",
  inputs    = "",
  commands  = "",
  results   = "",
  functions = ""
) {
  cfg <- config()

  # fill empty args from config
  # use example wf folder if empty
  if (path == "")      path      <- system.file(cfg$pathToFolder, package = "PEITHO")
  # use default filenames if empty
  if (inputs == "")    inputs    <- cfg$inputs
  if (commands == "")  commands  <- cfg$commands
  if (results == "")   results   <- cfg$results
  if (functions == "") functions <- cfg$functions

  list(
    path_to_folder = path,
    inputs_path    = file.path(path, inputs),
    commands_path  = file.path(path, commands),
    results_path   = file.path(path, results),
    functions_path = file.path(path, functions)
  )
}

#' Create a new workflow object
#'
#' This object represents a workflow, consisting of a sequence of workflow steps,
#' the current step index, and additional metadata.
#' @param name    A name for the workflow.
#' @param steps   A list of `workflowstep` objects defining the steps of the workflow.
#' @param current The index of the current step in the workflow.
#' @param use_peitho_folder Logical; if `TRUE`, load steps from PEITHO example folder.
#' @param workflow_file_paths A list of file paths for workflow files (see `workflow_file_paths()`).
#' @param ...     Additional metadata to store with the workflow.
#' @return A `workflow` object.
#' @export
new_workflow <- function(
  name    = "Untitled workflow",
  steps = list(),
  current = if (length(steps)) 1L else NA_integer_,
  use_peitho_folder = TRUE,
  workflow_file_paths = list(),
  ...
) {
  # 1) Decide where steps come from
  if (use_peitho_folder) {
    PEITHO:::logDebug("Loading workflow from PEITHO folder...")
    if (length(workflow_file_paths) == 0L) {
      PEITHO:::logDebug("Using default PEITHO workflow file paths...")
      workflow_file_paths <- PEITHO:::workflow_file_paths()
    }

    validate_workflow_file_paths(workflow_file_paths)

    if (length(steps)) {
      PEITHO:::logWarn("Argument 'steps' is ignored when 'use_peitho_folder' is TRUE.")
    }

    steps <- PEITHO:::extract_workflow_from_files(
      workflow_file_paths = workflow_file_paths,
      show_functions_path = FALSE
    )
  } else {
    PEITHO:::logDebug("Creating workflow from provided steps...")
    if (length(steps) == 0L) {
      PEITHO:::logWarn("No steps provided for workflow.")
    }
    # if not use PEITHO folder, clear file paths
    workflow_file_paths <- list()
  }

  # 2) Validations
  validate_workflow_steps(steps)
  validate_unique_step_names(steps)

  # 3) Determine current step
  current <- validate_current_index(current, length(steps))

  # 4) Build workflow object
  structure(
    list(
      name           = name,
      steps          = steps,
      current        = current,
      workflow_file_paths = workflow_file_paths,
      dots           = list(...)
    ),
    class = c("workflow", "list")
  )
}

# user-facing helper (function with more validation) -----------------------

workflow <- function(steps = list(), name = "Untitled workflow", current = 1L, ...) {
  new_workflow(steps = steps, name = name, current = current, ...)
}

#' Extract input values from a workflow
#'
#' This function extracts user input values from the steps of a workflow. It looks for parameters of type "input" in each step and collects their values into a named list.
#'
#' @param x A `workflow` object.
#' @param ... Additional arguments (not used).
#' @return A named list of input values extracted from the workflow steps.
#' @export
extract_inputs.workflow <- function(x, ...) {
  inputs <- list()

  for (step in x$steps) {
    step_inputs <- list()
    for (param in step$params) {
      if (inherits(param, "operationparam") && param$type == "input") {
        step_inputs[[param$label]] <- param$value
      }
    }

    if (length(step_inputs) == 0) next

    inputs <- c(inputs, step_inputs)
  }

  inputs
}

#' Convert a workflow object to a data frame
#'
#' This method converts a `workflow` object into a data frame summarizing its steps.
#'
#' @param x A `workflow` object.
#' @param ... Additional arguments (not used).
#' @return A data frame summarizing the workflow steps.
#' @export
as.data.frame.workflow <- function(x, ...) {
  steps <- x$steps
  data.frame(
    name = vapply(steps, function(s) s$name, character(1)),
    label = vapply(steps, function(s) s$label, character(1)),
    comments = vapply(steps, function(s) s$comments, character(1)),
    operation = vapply(steps, function(s) s$operation, character(1)),
    params = vapply(steps, function(s) flatten_params(s$params), character(1)),
    stringsAsFactors = FALSE
  )
}

flatten_params <- function(params) {
  if (length(params) == 0) return("")
  paste(
    vapply(params, function(p) {
      if (!is.null(p$name) && p$type %in% c("input", "result")) {
        paste0(p$name, "=", p$tag, toString(p$label), p$tag)
      } else if (!is.null(p$name) && p$type == "literal") {
        paste0(p$name, "=", toString(p$value))
      } else {
        ""
      }
    }, character(1)),
    collapse = ", "
  )
}

# print method -------------------------------------------------------------

#' Print method for workflow objects
#'
#' @param x A `workflow` object.
#' @param ... Additional arguments (not used).
#' @export
print.workflow <- function(x, ...) {
  cat("<workflow>\n")
  cat("  name:   ", x$name, "\n", sep = "")
  cat("  steps:  ", length(x$steps), "\n", sep = "")
  cat("  current:", x$current, "\n", sep = "")
  if (length(x$steps)) {
    cat("  step summary:\n")
    for (i in seq_along(x$steps)) {
      s <- x$steps[[i]]
      is_curr <- if (i == x$current) "*" else " "
      cat(
        sprintf(
          "   %s [%d] %s (operation: %s)\n",
          is_curr, s$id, s$name, s$operation
        )
      )
    }
  }
  if (length(x$workflow_file_paths)) {
    cat("  path to folder: ", x$workflow_file_paths$path_to_folder, "\n", sep = "")
    cat("  inputs file:   ", x$workflow_file_paths$inputs_path, "\n", sep = "")
    cat("  commands file: ", x$workflow_file_paths$commands_path, "\n", sep = "")
    cat("  results file:  ", x$workflow_file_paths$results_path, "\n", sep = "")
    cat("  functions file:", x$workflow_file_paths$functions_path, "\n", sep = "")
  }
  if (length(x$dots)) cat("  dots:   ", length(x$dots), "\n", sep = "")
  cat("  available fields: $", paste(names(x), collapse = ", $"), "\n", sep = "")
  invisible(x)
}

# -------------------------------------------------------------------------
# Validation helpers for workflow construction
# -------------------------------------------------------------------------
# These helpers centralize validations that were previously inline in new_workflow().
# Keeping them here makes new_workflow() easier to read, and ensures consistent checks
# for other constructors/helpers that may create/modify workflows later.

validate_workflow_file_paths <- function(workflow_file_paths) {
  # Minimal structural validation; actual file existence is checked in extract_workflow_from_files()
  if (!is.list(workflow_file_paths)) {
    stop("'workflow_file_paths' must be a list.", call. = FALSE)
  }
  if (length(workflow_file_paths) == 0L) {
    return(invisible(TRUE))
  }
  if (is.null(workflow_file_paths$path_to_folder) || !nzchar(workflow_file_paths$path_to_folder)) {
    stop("'workflow_file_paths$path_to_folder' must be a non-empty string.", call. = FALSE)
  }
  if (!dir.exists(workflow_file_paths$path_to_folder)) {
    stop("Argument 'path_to_folder' does not exist.", call. = FALSE)
  }
  # add check if functions script is not empty here?
  invisible(TRUE)
}

validate_workflow_steps <- function(steps) {
  if (!is.list(steps)) {
    stop("'steps' must be a list.", call. = FALSE)
  }
  if (length(steps) == 0L) return(invisible(TRUE))

  ok <- vapply(steps, inherits, logical(1), what = "workflowstep")
  if (!all(ok)) {
    stop("All elements of 'steps' must be of class 'workflowstep'.", call. = FALSE)
  }
  invisible(TRUE)
}

validate_unique_step_names <- function(steps) {
  if (!length(steps)) return(invisible(TRUE))
  nms <- vapply(steps, `[[`, character(1), "name")

  if (anyDuplicated(nms)) {
    dup <- unique(nms[duplicated(nms)])
    stop("Workflow has duplicate step names: ", paste(dup, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

validate_current_index <- function(current, n_steps) {
  if (is.null(current)) {
    return(if (n_steps) 1L else NA_integer_)
  }
  if (n_steps == 0L) return(NA_integer_)

  current <- as.integer(current)
  if (is.na(current)) return(NA_integer_)

  # clamp to [1, n_steps]
  max(1L, min(current, n_steps))
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

# Accessor functions (some will be added later) ------------------------

# current_step.workflow <- function(x, ...) {
#   if (length(x$steps) == 0L || is.na(x$current)) {
#     return(NULL)
#   }
#   x$steps[[x$current]]
# }

# next_step.workflow <- function(x, wrap = FALSE, ...) {
#   if (length(x$steps) == 0L || is.na(x$current)) {
#     return(x)
#   }

#   if (x$current < length(x$steps)) {
#     x$current <- x$current + 1L
#   } else if (wrap) {
#     x$current <- 1L
#   } # else stay at last

#   x
# }

# previous_step.workflow <- function(x, wrap = FALSE, ...) {
#   if (length(x$steps) == 0L || is.na(x$current)) {
#     return(x)
#   }

#   if (x$current > 1L) {
#     x$current <- x$current - 1L
#   } else if (wrap) {
#     x$current <- length(x$steps)
#   } # else stay at first

#   x
# }

# goto_step.workflow <- function(x, index = NULL, id = NULL, ...) {
#   if (length(x$steps) == 0L || is.na(x$current)) {
#     return(x)
#   }

#   if (!is.null(index)) {
#     index <- as.integer(index)
#     if (index >= 1L && index <= length(x$steps)) {
#       x$current <- index
#     } else {
#       PEITHO:::logWarn("Index out of range; 'workflow$current' unchanged.")
#     }
#     return(x)
#   }

#   if (!is.null(id)) {
#     ids <- vapply(x$steps, function(s) s$id, integer(1))
#     idx <- which(ids == as.integer(id))
#     if (length(idx) == 1L) {
#       x$current <- idx
#     } else {
#       PEITHO:::logWarn("No step with matching 'id'; 'workflow$current' unchanged.")
#     }
#     return(x)
#   }

#   PEITHO:::logWarn("Provide either 'index' or 'id' to goto_step().")
#   x
# }

#' Run the entire workflow
#'
#' @param x A `workflow` object.
#' @param state A `workflowstate` object representing the initial state.
#' @param from An integer index of the step to start from.
#' @param to An integer index of the step to end at.
#' @param env An environment to look up operation functions. Defaults to `NULL`, which uses
#'  each step's own env or the caller's env.
#' @param ... Additional arguments passed to `run.workflowstep()`.
#' @return A list containing the final workflow, state, and results of each step.
#' @export
run.workflow <- function(
  x,
  state = list(),
  from  = 1L,
  to    = length(x$steps),
  env = NULL,
  ...
) {
  # unpack additional args
  additional_args <- list(...)

  # for now we always stop on error!!!
  stop_on_error <- TRUE

  if (!inherits(x, "workflow")) {
    stop("Argument 'x' must be of class 'workflow'.")
  }
  if (length(x$steps) == 0L) {
    PEITHO:::logWarn("Workflow has no steps.")
    return(
      list(
        workflow = x,
        state    = state,
        results  = list()
      )
    )
  }

  if (length(state) == 0L) {
    state <- ""
    # get input param from first step
    for (p in x$steps[[1]]$params) {
      if (p$type == "input") {
        state <- p$value
        break
      }
    }
  }

  if (!inherits(state, "workflowstate")) {
    state <- new_workflowstate(initial_input = state)
  }

  from <- max(1L, as.integer(from))
  to   <- min(length(x$steps), as.integer(to))
  idxs <- seq(from, to)
  PEITHO:::logDebug("Running workflow from step %d to %d", from, to)

  for (j in seq_along(idxs)) {
    if (shiny::isRunning()) {
      shiny::incProgress(1 / length(idxs), detail = paste("Running step", j, "of", length(idxs)))
    }
    PEITHO:::logInfo("Running step %d of %d", j, length(idxs))
    i <- idxs[j]
    step <- x$steps[[i]]

    # run the step, with env explicitly passed
    steprun <- run(step, state, env = env, ...)

    # update workflow state and append steprun
    state <- update(state, steprun, idx = i)
    # save summary to results file and handle errors
    steprun_summary <- summary(steprun)

    if (length(x$workflow_file_paths) > 0) {
      if (!file.exists(x$workflow_file_paths$results_path) || i == 1L) {
        # if missing or first step, create empty results file
        jsonlite::write_json(
          list(),
          x$workflow_file_paths$results_path,
          auto_unbox = TRUE,
          pretty = TRUE
        )
      }

      update_json_summary(
        steprun_summary,
        idx = i,
        path_to_folder = x$workflow_file_paths$path_to_folder,
        results_file = basename(x$workflow_file_paths$results_path)
      )
    }

    if (stop_on_error && !(length(steprun_summary$errors) == 1 && steprun_summary$errors == "")) {
      stop(
        "step ", i, " (id=", step$id, "): ", paste(steprun_summary$errors, collapse = "; "),
        call. = FALSE
      )
    }
  }

  new_workflowrun(x, state)
}


# Helpers ----------------------------------------------------------------

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
    file.path(path_to_folder, results_file), auto_unbox = TRUE, pretty = TRUE
  )
}

