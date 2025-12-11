#' Update a workflow state after running a step
#'
#' This is a generic function to update a workflow state after executing a step.
#' @param object A `workflowstate` object.
#' @param steprun A `workflowsteprun` object representing the result of the executed step.
#' @param idx The index of the step run to update.
#' @param ... Additional arguments (not used).
#' @return The updated `workflowstate` object.
#' @export
update <- function(object, steprun, idx, ...) {
  UseMethod("update")
}

#' Run a workflow step
#' This is a generic function to run a workflow step or an entire workflow.
#' @param object A `workflowstep` object or a `workflow` object.
#' @param state A `workflowstate` object.
#' @param ... Additional arguments (not used).
#' @return A `workflowsteprun` object representing the result of executing the step or
#'  a list containing the updated `workflow` and `workflowstate` objects.
#' @export
run <- function(object, state, ...) {
  UseMethod("run")
}