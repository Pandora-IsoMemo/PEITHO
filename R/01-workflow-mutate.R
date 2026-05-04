# -------------------------------------------------------------------------
# Workflow mutation helpers and methods
# -------------------------------------------------------------------------
# These methods modify workflow objects in memory and optionally persist
# selected updates to the corresponding workflow files on disk.

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
  dots <- list(...)
  warn_on_validation <- get_warn_on_validation(dots, default = FALSE)

  # pass field & value NOT a whole step -> we need to update the wf object AND the commands json
  # get commands file path
  wf_file_paths <- x$workflow_file_paths
  # update the steps
  updated_step <- update(x$steps[[step]], wf_file_paths, field = field, value = value,  ...)
  x$steps[[step]] <- updated_step
  # validate updated workflow
  validate_workflow_with_policy(x, warn_on_validation = warn_on_validation)
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
  dots <- list(...)
  warn_on_validation <- get_warn_on_validation(dots, default = FALSE)

  x$steps <- append(x$steps, list(new_step), after = position - 1L)
  # update entries of all steps to maintain numeric order
  for (i in seq_along(x$steps)) {
    x$steps[[i]]$entry <- i
  }
  # validate workflow after adding step
  validate_workflow_with_policy(x, warn_on_validation = warn_on_validation)

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
  dots <- list(...)
  warn_on_validation <- get_warn_on_validation(dots, default = FALSE)

  x$steps <- x$steps[-position]
  # validate workflow after removing step
  validate_workflow_with_policy(x, warn_on_validation = warn_on_validation)
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

# Navigation methods ------------------------------------------------------

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
