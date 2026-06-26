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

#' Convert an object to commands.json record format
#'
#' Generic method to create records matching the commands.json schema.
#' @param x The object to convert.
#' @param ... Additional arguments passed to methods.
#' @export
as.commands_record <- function(x, ...) {
  UseMethod("as.commands_record")
}

#' Convert a workflow to graph tables
#'
#' Generic method to convert a workflow object into graph tables for visualization or analysis.
#' @param x The workflow object to convert.
#' @param ... Additional arguments passed to methods.
#' @return A list containing graph tables representing the workflow structure.
#' @export
as.graph_tables <- function(x, ...) {
  UseMethod("as.graph_tables")
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

#' Update the input list of a workflow
#'
#' Generic method to update the input list of a workflow object.
#' @param x The object to update.
#' @param ... Additional arguments passed to methods.
#' @export
update_input_list <- function(x, ...) {
  UseMethod("update_input_list")
}

#' Update the custom functions of a workflow
#'
#' Generic method to update the custom functions file of a workflow object.
#' @param x The object to update.
#' @param ... Additional arguments passed to methods.
#' @export
update_functions <- function(x, ...) {
  UseMethod("update_functions")
}

#' Add a step to a workflow
#'
#' Generic method to add a step to a workflow object.
#' @param x The workflow object to which the step will be added.
#' @param new_step The step to add to the workflow.
#' @param position The position in the workflow where the new step should be added.
#' @param ... Additional arguments passed to methods.
#' @return The updated workflow object with the new step added.
#' @export
add_step <- function(x, new_step, position, ...) {
  UseMethod("add_step")
}

#' Remove a step from a workflow
#'
#' Generic method to remove a step from a workflow object.
#' @param x The workflow object from which the step will be removed.
#' @param position The position of the step to remove from the workflow.
#' @param ... Additional arguments passed to methods.
#' @return The updated workflow object with the specified step removed.
#' @export
remove_step <- function(x, position, ...) {
  UseMethod("remove_step")
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
