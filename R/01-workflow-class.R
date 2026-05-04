# -------------------------------------------------------------------------
# Workflow class constructors and core helpers
# -------------------------------------------------------------------------
# This file defines workflow object constructors and shared helpers for
# creating file-backed workflows and managing their core metadata.

# ---- workflow class ----

#' Helper to get file paths for PEITHO workflow files
#'
#' This function constructs full file paths for the workflow files located in a specified folder.
#' Validation of file existence is performed in `workflow_steps_from_files()`.
#'
#' @param path Path to folder containing workflow files (default: PEITHO example folder).
#' @param inputs Name of the inputs file (default: "inputs.json").
#' @param commands Name of the commands file (default: "commands.json").
#' @param results Name of the results summary file (default: "results.json").
#' @param functions Name of the R script file containing custom functions (default: "functions.R").
#'  Use `NULL` to skip loading a custom functions file.
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
  if (!is.null(functions) && functions == "") functions <- cfg$functions

  functions_path <- if (is.null(functions)) NULL else file.path(path, functions)

  list(
    path_to_folder = path,
    inputs_path    = file.path(path, inputs),
    commands_path  = file.path(path, commands),
    results_path   = file.path(path, results),
    functions_path = functions_path
  )
}

#' Create a new workflow object
#'
#' This object represents a workflow, consisting of a sequence of workflow steps,
#' the current step index, and additional metadata.
#' @param name    A name for the workflow.
#' @param workflow_file_paths A list of file paths for workflow files (see `workflow_file_paths()`).
#' @param use_peitho_folder Logical; if `TRUE`, load steps from PEITHO example folder. If `FALSE`,
#'  use provided `steps` and ignore `workflow_file_paths`, useful for testing.
#' @param input_list A list of inputs for the workflow steps.
#' @param steps   A list of `workflowstep` objects defining the steps of the workflow.
#' @param current The index of the current step in the workflow.
#' @param error_on_warn Logical; if `TRUE`, validation issues will raise errors.
#'  If `FALSE`, they will raise warnings instead.
#' @param ...     Additional metadata to store with the workflow.
#' @return A `workflow` object.
#' @export
new_workflow <- function(
  name    = "Untitled workflow",
  workflow_file_paths = list(),
  use_peitho_folder = TRUE,
  input_list = list(),
  steps = list(),
  current = if (length(steps)) 1L else NA_integer_,
  error_on_warn = FALSE,
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

    if (length(input_list)) {
      warn <- "Argument 'input_list' is ignored when 'use_peitho_folder' is TRUE."
      PEITHO:::logWarn("%s", warn)
      warning(warn, immediate. = TRUE, call. = FALSE)
    }

    # we have duplicated validation of paths ...
    input_list <- PEITHO:::extract_input_list_from_files(workflow_file_paths$inputs_path)

    if (length(steps)) {
      warn <- "Argument 'steps' is ignored when 'use_peitho_folder' is TRUE."
      PEITHO:::logWarn("%s", warn)
      warning(warn, immediate. = TRUE, call. = FALSE)
    }

    steps <- PEITHO:::workflow_steps_from_files(
      workflow_file_paths = workflow_file_paths,
      show_functions_path = FALSE
    )
  } else {
    if (length(workflow_file_paths)) {
      warn <- "Argument 'workflow_file_paths' is ignored when 'use_peitho_folder' is FALSE."
      PEITHO:::logWarn("%s", warn)
      warning(warn, immediate. = TRUE, call. = FALSE)
    }

    PEITHO:::logDebug("Creating workflow from provided steps...")
    if (length(input_list) == 0L) {
      warn <- "No 'input_list' provided for workflow."
      PEITHO:::logWarn("%s", warn)
      warning(warn, immediate. = TRUE, call. = FALSE)
    }
    if (length(steps) == 0L) {
      warn <- "No steps provided for workflow."
      PEITHO:::logWarn("%s", warn)
      warning(warn, immediate. = TRUE, call. = FALSE)
    }
    # if not use PEITHO folder, clear file paths
    workflow_file_paths <- list()
  }

  # 2) Validations, if not valid stopping with error
  validate_steps_class(steps, error_on_warn = TRUE)
  validate_workflow(list(steps = steps, input_list = input_list), error_on_warn = error_on_warn)

  # 3) Determine current step
  current <- validate_current_index(current, length(steps))

  # 4) Build workflow object
  structure(
    list(
      name           = name,
      input_list     = input_list,
      steps          = steps,
      current        = current,
      workflow_file_paths = workflow_file_paths,
      dots           = list(...)
    ),
    class = c("workflow", "list")
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
          "   %s [%d] %s (command: %s)\n",
          is_curr, s$entry, s$name, s$command
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
  df_list <- lapply(steps, as.data.frame.workflowstep)
  do.call(rbind, df_list)
}

#' Convert a workflow object to commands.json record format
#'
#' This method converts a `workflow` object into a list format suitable for writing to a
#' `commands.json` file, which is used to define the workflow steps in a structured way.
#' It applies the conversion to each step in the workflow.
#'
#' @param x A `workflow` object.
#' @param ... Additional arguments (not used).
#' @return A list summarizing workflow steps for commands.json.
#' @export
as.commands_record.workflow <- function(x, ...) {
  lapply(x$steps, as.commands_record.workflowstep)
}

step_name_to_position <- function(x, step_name) {
  step_names <- vapply(x$steps, function(s) s$name, character(1))
  match(step_name, step_names)

}

#' Get a specific field from a workflow
#'
#' @param x  A `workflow` object.
#' @param field The name of the field to retrieve (e.g., "Name", "Comments").
#' @param step_name Name of the step to retrieve the field from.
#' @param ... Additional arguments (not used).
#' @return The value of the specified field from the workflow or a specific step.
#' @export
get_field.workflow <- function(x, field, step_name, ...) {
  position <- step_name_to_position(x, step_name)
  if (is.na(position)) {
    stop("Step with name '", step_name, "' not found in workflow.")
  }
  step <- x$steps[[position]]
  get_field(step, field)
}

extract_step_names <- function(x) {
  step_entries <- vapply(x$steps, function(s) s$entry, integer(1))
  step_names <- vapply(x$steps, function(s) s$name, character(1))
  names(step_entries) <- step_names
  # return named vector for selectInput
  step_entries
}

#' Run the entire workflow
#'
#' @param x A `workflow` object.
#' @param state A `workflowstate` object representing the initial state.
#' @param from An integer index of the step to start from.
#' @param to An integer index of the step to end at.
#' @param env An environment to look up command functions. Defaults to `NULL`, which uses
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
  # for now we always stop on error!!!
  stop_on_error <- TRUE

  # validata workflow
  if (!inherits(x, "workflow")) {
    stop("Argument 'x' must be of class 'workflow'.")
  }
  # check steps
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

  # create run_id
  ts <- format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC")
  rdm_suffix <- sprintf("%08x", sample.int(.Machine$integer.max, 1L))
  run_id <- paste0(ts, "_", rdm_suffix)
  PEITHO:::logInfo("Starting workflow run with ID: '%s'", run_id)

  # initialize state if not already a workflowstate
  if (!inherits(state, "workflowstate")) {
    state <- new_workflowstate(initial_input = state, run_id = run_id)
  } else {
    state$run_id <- run_id
  }

  validate_workflow(x, error_on_warn = TRUE)
  if (
    is.null(env) &&
      length(x$workflow_file_paths) > 0L &&
      !is.null(x$workflow_file_paths$functions_path)
  ) {
    env <- load_workflow_script_env(
      script_path = x$workflow_file_paths$functions_path,
      parent_env = asNamespace("PEITHO"),
      show_functions_path = FALSE
    )
  }

  # RUN workflow steps
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
    steprun <- run(step, state, env = env, step_i = j, input_list = x$input_list, ...)

    # update workflow state and append steprun
    state <- update(state, steprun, idx = i)
    # save summary to results file and handle errors
    steprun_summary <- summary(steprun)

    if (length(x$workflow_file_paths) > 0) {
      if (!file_nonempty(x$workflow_file_paths$results_path) || i == 1L) {
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
        "step ", i, " (entry=", step$entry, "): ", paste(steprun_summary$errors, collapse = "; "),
        call. = FALSE
      )
    }
  }

  new_workflowrun(x, state, run_id = run_id)
}

