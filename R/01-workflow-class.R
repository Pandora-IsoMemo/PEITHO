# ---- workflow class ----

#' Workflow run object
#' 
#' This object represents a completed run of a workflow, containing the workflow definition
#' and the final state after execution.
#' @param workflow A `workflow` object.
#' @param state A `workflowstate` object.
#' @return A `workflowrun` object.
#' @export
new_workflowrun <- function(workflow, state) {
  if (!inherits(workflow, "workflow")) {
    stop("Argument 'workflow' must be of class 'workflow'.")
  }
  if (!inherits(state, "workflowstate")) {
    stop("Argument 'state' must be of class 'workflowstate'.")
  }

  structure(
    list(
      workflow = workflow,
      state    = state,
      errors   = state$errors,
      last_result   = state$last_result
    ),
    class = "workflowrun"
  )
}

#' Print method for workflowrun objects
#'
#' @param x A `workflowrun` object.
#' @param ... Additional arguments (not used).
#' @export
print.workflowrun <- function(x, ...) {
  cat("<workflowrun>\n")
  cat("  workflow: ", x$workflow$name, "\n", sep = "")
  cat("  steps:    ", length(x$workflow$steps), "\n", sep = "")
  cat("Finished with state:\n")
  print(x$state)
  cat("  available fields: $", paste(names(x), collapse = ", $"), "\n", sep = "")
  invisible(x)
}

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
#' @param config_path Path to configuration JSON file (default: package's config.json).
#' @return A list with full paths to the specified files.
#' @export
workflow_file_paths <- function(
  path      = "",
  inputs    = "",
  commands  = "",
  results   = "",
  functions = "",
  config_path = system.file("config", "config.json", package = "PEITHO")
) {
  cfg <- jsonlite::fromJSON(config_path)

  # fill empty args from config
  if (path == "")      path      <- system.file(cfg$path_to_folder, package = "PEITHO")
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
      workflow_file_paths <- workflow_file_paths()
    }
    if (!dir.exists(workflow_file_paths$path_to_folder)) {
      stop("Argument 'path_to_folder' does not exist.", call. = FALSE)
    }
    if (length(steps)) {
      PEITHO:::logWarn("Argument 'steps' is ignored when 'use_peitho_folder' is TRUE.")
    }
    steps <- extract_workflow_from_files(workflow_file_paths = workflow_file_paths)
  } else {
    PEITHO:::logDebug("Creating workflow from provided steps...")
    if (length(steps) == 0L) {
      PEITHO:::logWarn("No steps provided for workflow.")
    }
    # if not use PEITHO folder, clear file paths
    workflow_file_paths <- list()
  }

  # 2) Validate steps are workflowstep objects
  if (length(steps)) {
    ok <- vapply(steps, inherits, logical(1), what = "workflowstep")
    if (!all(ok)) {
      stop("All elements of 'steps' must be of class 'workflowstep'.", call. = FALSE)
    }
  }

  # 3) Determine current step
  if (is.null(current)) {
    # default if not specified
    current <- if (length(steps)) 1L else NA_integer_
  } else if (length(steps) == 0L) {
    current <- NA_integer_
  } else {
    # clamp to [1, length(steps)]
    current <- max(1L, min(as.integer(current), length(steps)))
  }

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

# Accessor functions (some added later) ----------------------------------------

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
#' @param object A `workflow` object.
#' @param state A `workflowstate` object representing the initial state.
#' @param from An integer index of the step to start from.
#' @param to An integer index of the step to end at.
#' @param env An environment to look up operation functions. Defaults to `NULL`, which uses
#'  each step's own env or the caller's env.
#' @param ... Additional arguments passed to `run.workflowstep()`.
#' @return A list containing the final workflow, state, and results of each step.
#' @export
run.workflow <- function(
  object,
  state = list(),
  from  = 1L,
  to    = length(object$steps),
  env = NULL,
  ...
) {
  # for now we always stop on error!!!
  stop_on_error <- TRUE

  if (!inherits(object, "workflow")) {
    stop("Argument 'object' must be of class 'workflow'.")
  }
  if (length(object$steps) == 0L) {
    PEITHO:::logWarn("Workflow has no steps.")
    return(
      list(
        workflow = object,
        state    = state,
        results  = list()
      )
    )
  }

  if (length(state) == 0L) {
    state <- ""
    # get input param from first step
    for (p in object$steps[[1]]$params) {
      if (p$type == "input") {
        state <- p$value
        break
      }
    }
  }

  if (!inherits(state, "workflowstate")) {
    state <- new_workflowstate(initial_input = state)
  }

  # env is stored in workflowstep or passed, else use parent frame
  # no need to load functions here again, as each step has its own env
  # env <- load_workflow_script_env(
  #   object$workflow_file_paths$functions_path,
  #   parent_env = env
  # )

  from <- max(1L, as.integer(from))
  to   <- min(length(object$steps), as.integer(to))
  idxs <- seq(from, to)
  PEITHO:::logDebug("Running workflow from step %d to %d", from, to)

  for (j in seq_along(idxs)) {
    PEITHO:::logInfo("Running step %d of %d", j, length(idxs))
    i <- idxs[j]
    step <- object$steps[[i]]

    # run the step, with env explicitly passed
    steprun <- run(step, state, env = env, ...)

    # update workflow state and append steprun
    state <- update(state, steprun, idx = i)
    # save summary to results file and handle errors
    steprun_summary <- summary(steprun)

    if (length(object$workflow_file_paths) > 0) {
      if (!file.exists(object$workflow_file_paths$results_path) || i == 1L) {
        # if missing or first step, create empty results file
        jsonlite::write_json(
          list(),
          object$workflow_file_paths$results_path,
          auto_unbox = TRUE,
          pretty = TRUE
        )
      }

      update_json_summary(
        steprun_summary,
        idx = i,
        path_to_folder = object$workflow_file_paths$path_to_folder,
        results_file = basename(object$workflow_file_paths$results_path)
      )
    }

    if (stop_on_error && !(length(steprun_summary$errors) == 1 && steprun_summary$errors == "")) {
      stop(
        "step ", i, " (id=", step$id, "): ", paste(steprun_summary$errors, collapse = "; "),
        call. = FALSE
      )
    }
  }

  new_workflowrun(object, state)
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
