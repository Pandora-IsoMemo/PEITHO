#' Update a workflow state after running a step
#'
#' This is a generic function to update a workflow state after executing a step.
#' @param x A `workflowstate` object.
#' @param steprun A `workflowsteprun` object representing the result of the executed step.
#' @param idx The index of the step run to update.
#' @param ... Additional arguments (not used).
#' @return The updated `workflowstate` object.
#' @export
update <- function(x, steprun, idx, ...) {
  UseMethod("update")
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

#' Extract inputs from a workflow
#'
#' This is a generic function to extract user input values from a workflow or workflow run.
#' @param x A `workflow` object or a `workflowrun` object.
#' @param ... Additional arguments (not used).
#' @return A named list of input values.
#' @export
extract_inputs <- function(x, ...) {
  UseMethod("extract_inputs")
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