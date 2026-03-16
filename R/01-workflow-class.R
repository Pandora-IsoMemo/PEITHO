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
  validate_workflow_steps(steps, error_on_warn = TRUE)
  validate_unique_steps(steps, error_on_warn = error_on_warn)
  validate_numeric_entries(steps, error_on_warn = error_on_warn)
  validate_required_inputs(steps, input_list, error_on_warn = error_on_warn)
  validate_required_steps(steps, error_on_warn = error_on_warn)

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

# -------------------------------------------------------------------------
# Validation helpers for workflow construction
# -------------------------------------------------------------------------
# These helpers centralize validations that were previously inline in new_workflow().
# Keeping them here makes new_workflow() easier to read, and ensures consistent checks
# for other constructors/helpers that may create/modify workflows later.

validate_workflow_file_paths <- function(workflow_file_paths) {
  # Minimal structural validation; actual file existence is checked in workflow_steps_from_files()
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

validate_workflow_steps <- function(steps, error_on_warn = TRUE) {
  if (!is.logical(error_on_warn) || length(error_on_warn) != 1L) {
    stop("'error_on_warn' must be a single logical value.", call. = FALSE)
  }
  if (!is.list(steps)) {
    msg <- "'steps' must be a list."
    if (error_on_warn) {
      stop(msg, call. = FALSE)
    }
    warning(msg, immediate. = TRUE, call. = FALSE)
    return(invisible(TRUE))
  }
  if (length(steps) == 0L) return(invisible(TRUE))

  ok <- vapply(steps, inherits, logical(1), what = "workflowstep")
  if (!all(ok)) {
    msg <- "All elements of 'steps' must be of class 'workflowstep'."
    if (error_on_warn) {
      stop(msg, call. = FALSE)
    }
    warning(msg, immediate. = TRUE, call. = FALSE)
  }
  invisible(TRUE)
}

validate_unique_steps <- function(
  steps,
  id_fields = c("entry", "name"),
  error_on_warn = TRUE
) {
  if (!length(steps)) return(invisible(TRUE))
  if (!is.logical(error_on_warn) || length(error_on_warn) != 1L) {
    stop("'error_on_warn' must be a single logical value.", call. = FALSE)
  }

  for (field in id_fields) {
    vals <- vapply(steps, function(s) as.character(s[[field]]), character(1))
    if (anyDuplicated(vals)) {
      dup <- unique(vals[duplicated(vals)])
      msg <- sprintf(
        "Workflow has duplicate step %s(s): %s",
        field,
        paste(dup, collapse = ", ")
      )
      if (error_on_warn) {
        stop(msg, call. = FALSE)
      }
      warning(msg, immediate. = TRUE, call. = FALSE)
    }
  }
  invisible(TRUE)
}

validate_numeric_entries <- function(steps, error_on_warn = TRUE) {
  if (!length(steps)) return(invisible(TRUE))
  if (!is.logical(error_on_warn) || length(error_on_warn) != 1L) {
    stop("'error_on_warn' must be a single logical value.", call. = FALSE)
  }
  entries <- vapply(steps, function(s) s$entry, integer(1))
  if (any(is.na(entries))) {
    msg <- "All steps must have a numeric 'entry' field."
    if (error_on_warn) {
      stop(msg, call. = FALSE)
    }
    warning(msg, immediate. = TRUE, call. = FALSE)
  }
  invisible(TRUE)
}

validate_required_inputs <- function(steps, input_list, error_on_warn = TRUE) {
  if (!length(steps)) return(invisible(TRUE))
  if (!is.logical(error_on_warn) || length(error_on_warn) != 1L) {
    stop("'error_on_warn' must be a single logical value.", call. = FALSE)
  }
  # extract required_fields and check if exist
  required_inputs <- unique(unlist(lapply(steps, function(s) {
    x <- s[["required_inputs"]]
    if (!is.character(x)) {
      msg <- sprintf("'required_inputs' in step '%s' must be a character vector.", s$name)
      if (error_on_warn) {
        stop(msg, call. = FALSE)
      }
      warning(msg, immediate. = TRUE, call. = FALSE)
    }
    x
  }), use.names = FALSE))

  # check if required inputs exist in input_list
  missing_inputs <- setdiff(required_inputs, names(input_list))
  if (length(missing_inputs)) {
    msg <- sprintf("Missing required inputs: %s", paste(missing_inputs, collapse = ", "))
    if (error_on_warn) {
      stop(msg, call. = FALSE)
    }
    warning(msg, immediate. = TRUE, call. = FALSE)
  }
}

validate_required_steps <- function(steps, error_on_warn = TRUE) {
  if (!length(steps)) return(invisible(TRUE))
  if (!is.logical(error_on_warn) || length(error_on_warn) != 1L) {
    stop("'error_on_warn' must be a single logical value.", call. = FALSE)
  }
  required_steps <- unique(unlist(lapply(steps, function(s) {
    x <- s[["required_steps"]]
    if (!is.character(x)) {
      msg <- sprintf("'required_steps' in step '%s' must be a character vector.", s$name)
      if (error_on_warn) {
        stop(msg, call. = FALSE)
      }
      warning(msg, immediate. = TRUE, call. = FALSE)
    }
    x
  }), use.names = FALSE))

  # check if required steps exist in steps
  step_names <- vapply(steps, function(s) s$name, character(1))
  missing_steps <- setdiff(required_steps, step_names)
  if (length(missing_steps)) {
    msg <- sprintf("Missing required steps: %s", paste(missing_steps, collapse = ", "))
    if (error_on_warn) {
      stop(msg, call. = FALSE)
    }
    warning(msg, immediate. = TRUE, call. = FALSE)
  }
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

#' Update a workflow step with a new value
#'
#' This function updates a specific field of a `workflowstep` object with a new value. It is used
#' to modify step details such as name, label, comments, command, parameters, or loop settings.
#'
#' @param x A `workflow` object to update.
#' @param step The index of the step to update.
#' @param value The new value to assign to the specified field.
#' @param field The name of the field to update (one of "name", "label", "comments", "command",
#'  "args", "loop")
#' @param ... Additional arguments (not used).
#' @return The updated `workflow` object.
#' @export
update.workflow <- function(x, step, field, value, ...) {
  # pass field & value NOT a whole step -> we need to update the wf object AND the commands json
  # get commands file path
  wf_file_paths <- x$workflow_file_paths
  # update the steps
  updated_step <- update(x$steps[[step]], wf_file_paths, field = field, value = value,  ...)
  x$steps[[step]] <- updated_step
  # validate updated workflow
  validate_unique_steps(x$steps, error_on_warn = FALSE)
  validate_numeric_entries(x$steps, error_on_warn = FALSE)
  validate_required_inputs(x$steps, x$input_list, error_on_warn = FALSE)
  validate_required_steps(x$steps, error_on_warn = FALSE)
  # return updated workflow
  x
}

#' Update the input list of a workflow
#'
#' This method updates the `input_list` of a `workflow` object and optionally writes the
#' updated list to the corresponding inputs file if the workflow is file-backed.
#'
#' @param x The `workflow` object to update.
#' @param new_list A named list of input values to update in the workflow.
#' @param write_file Logical; if `TRUE`, the updated input list will be written to the
#'  inputs file if the workflow has associated file paths.
#' @param ... Additional arguments (not used).
#' @return The updated `workflow` object with the new input list and optionally updated parameters.
#' @export
update_input_list.workflow <- function(
  x,
  new_list,
  write_file = TRUE,
  ...
) {
  if (!is.list(new_list)) {
    stop("'new_list' must be a list.", call. = FALSE)
  }

  # 1) update in-memory
  x$input_list <- new_list

  # 2) persist to inputs file (if file-backed)
  if (write_file && length(x$workflow_file_paths)) {
    in_path <- x$workflow_file_paths$inputs_path
    if (!is.null(in_path) && nzchar(in_path)) {
      jsonlite::write_json(new_list, in_path, auto_unbox = TRUE, pretty = TRUE)
    }
  }

  x
}

#' Add a new step to the workflow
#'
#' This method adds a new `workflowstep` to the `steps` list of a `workflow` object at a specified
#' position. It also performs validation after adding the step and updates the current index and
#' entries of all steps to maintain consistency.
#'
#' @param x The `workflow` object to update.
#' @param new_step A `workflowstep` object to add to the workflow.
#' @param position An integer index specifying where to insert the new step (default: at the end
#'  of the steps list).
#' @param ... Additional arguments (not used).
#' @return The updated `workflow` object with the new step added.
#' @export
add_step.workflow <- function(x, new_step, position = length(x$steps) + 1L, ...) {
  if (!inherits(new_step, "workflowstep")) {
    stop("'new_step' must be of class 'workflowstep'.", call. = FALSE)
  }
  position <- as.integer(position)
  if (position < 1L || position > length(x$steps) + 1L) {
    stop("Position must be between 1 and ", length(x$steps) + 1L, ".", call. = FALSE)
  }
  x$steps <- append(x$steps, list(new_step), after = position - 1L)
  # update entries of all steps to maintain numeric order
  for (i in seq_along(x$steps)) {
    x$steps[[i]]$entry <- i
  }
  # validate workflow after adding step
  validate_unique_steps(x$steps, error_on_warn = FALSE)
  validate_numeric_entries(x$steps, error_on_warn = FALSE)
  validate_required_inputs(x$steps, x$input_list, error_on_warn = FALSE)
  validate_required_steps(x$steps, error_on_warn = FALSE)

  # update current index if needed
  if (is.na(x$current)) {
    x$current <- position
  } else if (position <= x$current) {
    x$current <- x$current + 1L
  }

  # update the commands.json (only if the workflow is file-backed)
  if (length(x$workflow_file_paths) && !is.null(x$workflow_file_paths$commands_path)) {
    updated_commands <- as.commands_record(x)

    write_json(
      updated_commands,
      path = x$workflow_file_paths$commands_path,
      auto_unbox = TRUE,
      pretty = TRUE
    )
  }

  # return updated workflow
  x
}

#' Remove a step from the workflow
#'
#' This method removes a `workflowstep` from the `steps` list of a `workflow` object at a specified
#' position. It also performs validation after removing the step and updates the current index and
#' entries of all steps to maintain consistency.
#'
#' @param x The `workflow` object to update.
#' @param position An integer index specifying which step to remove.
#' @param ... Additional arguments (not used).
#' @return The updated `workflow` object with the specified step removed.
#' @export
remove_step.workflow <- function(x, position, ...) {
  position <- as.integer(position)
  if (position < 1L || position > length(x$steps)) {
    stop("Step index must be between 1 and ", length(x$steps), ".", call. = FALSE)
  }
  x$steps <- x$steps[-position]
  # validate workflow after removing step
  validate_unique_steps(x$steps, error_on_warn = FALSE)
  validate_numeric_entries(x$steps, error_on_warn = FALSE)
  validate_required_inputs(x$steps, x$input_list, error_on_warn = FALSE)
  validate_required_steps(x$steps, error_on_warn = FALSE)
  # update current index if needed
  if (!is.na(x$current)) {
    if (x$current == position) {
      x$current <- NA_integer_
    } else if (x$current > position) {
      x$current <- x$current - 1L
    }
  }
  # update entries of all steps to maintain numeric order
  for (i in seq_along(x$steps)) {
    x$steps[[i]]$entry <- i
  }

  # update the commands.json
  updated_commands <- as.commands_record(x)

  # Only write to disk for file-backed workflows with a valid commands_path
  if (!is.null(x$workflow_file_paths) &&
      length(x$workflow_file_paths) > 0L &&
      !is.null(x$workflow_file_paths$commands_path)) {
    write_json(
      updated_commands,
      path = x$workflow_file_paths$commands_path,
      auto_unbox = TRUE,
      pretty = TRUE
    )
  }

  # return updated workflow
  x
}

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
#     ids <- vapply(x$steps, function(s) s$entry, integer(1))
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
  # unpack additional args
  additional_args <- list(...)

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

  if (!inherits(state, "workflowstate")) {
    state <- new_workflowstate(initial_input = state)
  }

  validate_unique_steps(x$steps, error_on_warn = TRUE)
  validate_numeric_entries(x$steps, error_on_warn = TRUE)
  validate_required_inputs(x$steps, x$input_list, error_on_warn = TRUE)
  validate_required_steps(x$steps, error_on_warn = TRUE)

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
        "step ", i, " (entry=", step$entry, "): ", paste(steprun_summary$errors, collapse = "; "),
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
    file.path(path_to_folder, results_file),
    auto_unbox = TRUE,
    pretty = TRUE
  )
}
