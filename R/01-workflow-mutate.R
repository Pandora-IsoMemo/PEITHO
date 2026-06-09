# -------------------------------------------------------------------------
# Workflow mutation helpers and methods
# -------------------------------------------------------------------------
# These methods modify workflow objects in memory and optionally persist
# selected updates to the corresponding workflow files on disk.

renumber_workflow_steps <- function(x) {
  for (i in seq_along(x$steps)) {
    x$steps[[i]]$entry <- i
  }

  x
}

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
  if (write_file) {
    write_inputs_json(x)
  }

  x
}

write_text_file_utf8 <- function(path, value) {
  if (is.null(value)) value <- ""

  lines <- if (identical(value, "")) {
    character(0)
  } else {
    strsplit(value, "\n", fixed = TRUE)[[1]]
  }

  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con = con)
}

#' Write content to the functions file of a workflow
#'
#' This helper function writes the provided content to the functions file
#' of a workflow, if the workflow is file-backed and has a valid functions path.
#' If the directory path is NULL or does not exist, the function will return
#' NULL without performing any write operation.
#'
#' @param dir_path The directory path of the workflow. If NULL or non-existent,
#'                 no write will occur.
#' @param content The content to write to the functions file.
#' @return NULL if the directory path is invalid; otherwise,
#'         the content is written to the functions file.
#' @export
write_functions_file <- function(dir_path, content) {
  if (is.null(dir_path) || !dir.exists(dir_path)) return(NULL)

  functions_path <- PEITHO:::workflow_file_paths(path = dir_path)$functions_path
  if (is.null(functions_path)) {
    stop("No functions path available in this workflow.")
  }

  write_text_file_utf8(functions_path, content)
}

#' Update the functions file of a workflow
#'
#' This method updates the content of the functions file associated with a
#' `workflow` object. If the workflow is file-backed, the updated content
#' will be written to the functions file.
#'
#' @param x The `workflow` object to update.
#' @param new_functions new functions
#' @param ... Additional arguments (not used).
#' @return The updated `workflow` object with the new functions file content.
#' @export
update_functions.workflow <- function(x, new_functions, ...) {
  if (!is.character(new_functions) || length(new_functions) != 1L) {
    stop("'new_functions' must be a single character string.", call. = FALSE)
  }

  # update functions file (if file-backed)
  write_functions_file(x$workflow_file_paths$path_to_folder, new_functions)

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
  x <- renumber_workflow_steps(x)

  # validate workflow after adding step
  validate_workflow_with_policy(x, warn_on_validation = warn_on_validation)

  # update current index if needed
  if (is.na(x$current)) {
    x$current <- position
  } else if (position <= x$current) {
    x$current <- x$current + 1L
  }

  # update the commands.json (only if the workflow is file-backed)
  write_commands_json(x)

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
  x <- renumber_workflow_steps(x)

  # Only write to disk for file-backed workflows with a valid commands_path
  write_commands_json(x)

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
