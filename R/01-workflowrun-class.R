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

#' Convert a workflowrun object to a data frame
#'
#' This method converts a `workflowrun` object into a data frame summarizing its state.
#' 
#' @param x A `workflowrun` object.
#' @param ... Additional arguments (not used).
#' @return A data frame summarizing the workflow run state.
#' @export
as.data.frame.workflowrun <- function(x, ...) {
  as.data.frame.workflowstate(x$state, ...)
}

#' Extract input values from a workflow run
#'
#' This function extracts all input values used in the workflow run.
#'
#' @param wfr A `workflowrun` object.
#' @return A named list of input values.
#' @export
extract_inputs <- function(wfr) {
  inputs <- list()

  for (step in wfr$workflow$steps) {
    step_inputs <- list()
    for (param in step$params) {
      if (inherits(param, "operationparam") && param$type == "input") {
        step_inputs[[param$label]] <- param$value
      }
    }

    if (length(step_inputs) == 0) next

    inputs <- c(inputs, step_inputs)
  }

  inputs
}