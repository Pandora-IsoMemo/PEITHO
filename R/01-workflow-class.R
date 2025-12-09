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
      state    = state
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
  cat("  stepruns: ", length(x$state$stepruns), "\n", sep = "")
  invisible(x)
}

#' Create a new workflow object
#'
#' This object represents a workflow, consisting of a sequence of workflow steps,
#' the current step index, and additional metadata.
#' @param name    A name for the workflow.
#' @param steps   A list of `workflowstep` objects defining the steps of the workflow.
#' @param current The index of the current step in the workflow.
#' @param use_peitho_folder Logical; if `TRUE`, load steps from PEITHO example folder.
#' @param script  An optional R script file defining custom functions for the workflow,
#'                stored in the `dots` field.
#' @param ...     Additional metadata to store with the workflow.
#' @inheritParams extract_workflow_from_files
#' @return A `workflow` object.
#' @export
new_workflow <- function(
  name    = "Untitled workflow",
  steps = list(),
  current = if (length(steps)) 1L else NA_integer_,
  use_peitho_folder = TRUE,
  path_to_folder = system.file("scripts", "peitho_files", package = "PEITHO"),
  script = NULL, # e.g. "functions.R"
  ...
) {
  # 1) Decide where steps come from
  if (use_peitho_folder) {
    if (!dir.exists(path_to_folder)) {
      stop("Argument 'path_to_folder' does not exist.", call. = FALSE)
    }
    if (length(steps)) {
      logWarn("Argument 'steps' is ignored when 'use_peitho_folder' is TRUE.", call. = FALSE)
    }
    steps <- extract_workflow_from_files(path_to_folder = path_to_folder)
  } else {
    if (length(steps) == 0L) {
      logWarn("No steps provided for workflow.", call. = FALSE)
    }
    # no special meaning for path_to_folder when not using PEITHO files
    path_to_folder <- NULL
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

  if (!is.null(script)) {
    script_path <- file.path(path_to_folder, script)
  } else {
    script_path <- NULL
  }

  # 4) Build workflow object
  structure(
    list(
      name           = name,
      steps          = steps,
      current        = current,
      path_to_folder = path_to_folder,
      script         = script_path,
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
  cat("  path_to_folder: ", x$path_to_folder, "\n", sep = "")
  cat("  script:        ", x$script %||% "NULL", "\n", sep = "")
  if (length(x$dots)) cat("  dots:   ", length(x$dots), "\n", sep = "")
  cat("  available fields: ", paste(names(x), collapse = ", "), "\n", sep = "")

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
#       logWarn("Index out of range; 'workflow$current' unchanged.", call. = FALSE)
#     }
#     return(x)
#   }

#   if (!is.null(id)) {
#     ids <- vapply(x$steps, function(s) s$id, integer(1))
#     idx <- which(ids == as.integer(id))
#     if (length(idx) == 1L) {
#       x$current <- idx
#     } else {
#       logWarn("No step with matching 'id'; 'workflow$current' unchanged.", call. = FALSE)
#     }
#     return(x)
#   }

#   logWarn("Provide either 'index' or 'id' to goto_step().", call. = FALSE)
#   x
# }

#' Run the entire workflow
#'
#' @param object A `workflow` object.
#' @param state A `workflowstate` object representing the initial state.
#' @param from An integer index of the step to start from.
#' @param to An integer index of the step to end at.
#' @param stop_on_error Logical; if `TRUE`, stop execution on the first error.
#' @param results_file Name of the results summary file to update (default: "results_summary.json").
#' @param env An environment to look up operation functions. Default is the parent frame.
#' @param ... Additional arguments passed to `run.workflowstep()`.
#' @return A list containing the final workflow, state, and results of each step.
#' @export
run.workflow <- function(
  object,
  state = list(),
  from  = 1L,
  to    = length(object$steps),
  stop_on_error = TRUE,
  results_file = "results_summary.json",
  env = parent.frame(),
  ...
) {
  if (length(object$steps) == 0L) {
    logWarn("Workflow has no steps.")
    return(
      list(
        workflow = object,
        state    = state,
        results  = list()
      )
    )
  }

  script_path <- object$script
  if (!is.null(script_path)) {
    if (!file.exists(script_path)) {
      stop("Custom script file not found: ", script_path, call. = FALSE)
    }

    if (!is_running_online()) {
      logInfo("Loading custom script for workflow: ", script_path)
      script_env <- new.env(parent = env)
      sys.source(script_path, envir = script_env)

      env <- script_env  # use this env for all steps
    } else {
      logWarn("Running online; skipping loading custom script: ", script_path)
    }
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

  from <- max(1L, as.integer(from))
  to   <- min(length(object$steps), as.integer(to))
  idxs <- seq(from, to)

  for (j in seq_along(idxs)) {
    i <- idxs[j]
    step <- object$steps[[i]]

    # run the step, with env explicitly passed
    steprun <- run(step, state, env = env, ...)

    # update workflow state and append steprun
    state <- add_steprun(state, steprun, idx = i)
    # save summary to results file
    steprun_summary <- summary(steprun)

    if (!is.null(object$path_to_folder)) {
      if (!file.exists(file.path(object$path_to_folder, results_file))) {
        # create empty results file
        jsonlite::write_json(
          list(),
          file.path(object$path_to_folder, results_file), auto_unbox = TRUE, pretty = TRUE
        )
      }
      update_json_summary(
        steprun_summary,
        idx = i,
        path_to_folder = object$path_to_folder,
        results_file = results_file
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
  results <- get_results(
    path_to_folder = path_to_folder,
    result_file    = results_file
  )

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
