# state used while running a workflow --------------------------------------

#' Create a new workflow state object
#'
#' This object keeps track of the state while executing a workflow,
#' including the initial input, the last result, and the list of executed step runs.
#' @param initial_input The initial input provided to the workflow.
#' @return A `workflowstate` object.
#' @export
new_workflowstate <- function(initial_input = NULL) {
  structure(
    list(
      initial_input = initial_input,
      last_result   = initial_input,   # last step’s result
      stepruns      = list()           # list of workflowsteprun objects
    ),
    class = c("workflowstate", "list")
  )
}

#' Print method for workflowstate objects
#'
#' @param x A `workflowstate` object.
#' @param ... Additional arguments (not used).
#' @export
print.workflowstate <- function(x, ...) {
  cat("<workflowstate>\n")
  cat("  initial_input: ", if (is.null(x$initial_input)) "NULL" else "…", "\n", sep = "")
  cat("  last_result:   ", if (is.null(x$last_result)) "NULL" else "…", "\n", sep = "")
  cat("  stepruns:      ", length(x$stepruns), "\n", sep = "")
  invisible(x)
}

#' Add a workflow step run to the workflow state
#' 
#' This function appends a `workflowsteprun` object to the list of executed step runs
#' 
#' @param x A `workflowstate` object.
#' @param steprun A `workflowsteprun` object to add.
#' @param ... Additional arguments (not used).
#' @return The updated `workflowstate` object.
#' @export
add_steprun.workflowstate <- function(x, steprun, ...) {
  if (!inherits(x, "workflowstate")) {
    stop("Argument 'state' must be of class 'workflowstate'.")
  }
  if (!inherits(steprun, "workflowsteprun")) {
    stop("Argument 'steprun' must be of class 'workflowsteprun'.")
  }

  x$stepruns[[length(x$stepruns) + 1L]] <- steprun
  x$last_result <- if (is.null(steprun$error)) steprun$output else x$last_result
  x
}

