#' Get a specific field
#'
#' Generic get method for workflow-related objects.
#' @param x The object to extract the field from.
#' @param field The name of the field to retrieve.
#' @param ... Additional arguments passed to methods.
#' @export
get_field <- function(x, field, ...) {
  UseMethod("get_field")
}

#' Update an object
#'
#' Generic update method for workflow-related objects.
#' @param x The object to update.
#' @param ... Additional arguments passed to methods.
#' @export
update <- function(x, ...) {
  UseMethod("update")
}

#' Update the input list of a workflow step
#'
#' Generic method to update the input list of a workflow step.
#' @param x The object to update.
#' @param ... Additional arguments passed to methods.
#' @export
update_input_list <- function(x, ...) {
  UseMethod("update_input_list")
}

#' Run a workflow step
#'
#' This is a generic function to run a workflow step or an entire workflow.
#' @param x A `workflowstep` object or a `workflow` object.
#' @param state A `workflowstate` object.
#' @param ... Additional arguments (not used).
#' @return A `workflowsteprun` object representing the result of executing the step or
#'  a list containing the updated `workflow` and `workflowstate` objects.
#' @export
run <- function(x, state, ...) {
  UseMethod("run")
}

#' Save an object as a ZIP file
#'
#' This is a generic function to save various objects as ZIP files.
#' @param x The object to save.
#' @param file The path to the output ZIP file.
#' @param ... Additional arguments (not used).
#' @return None. The object is saved to the specified ZIP file.
#' @export
save_as_zip <- function(x, file, ...) {
  UseMethod("save_as_zip")
}