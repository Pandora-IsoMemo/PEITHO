# one executed step --------------------------------------------------------

#' Create a new workflow step run object
#'
#' This object records the execution of a single step within a workflow,
#' including the step definition, the arguments used, the output or error,
#' and any additional metadata.
#' @param step   A `workflowstep` object representing the step definition.
#' @param args   A list of arguments that were passed to the step's operation.
#' @param output The output produced by the step, if successful.
#' @param error  An error object if the step failed, otherwise `NULL`.
#' @param ...    Additional metadata to store with the step run.
#' @return A `workflowsteprun` object.
#' @export
new_workflowsteprun <- function(step, args, output = NULL, error = NULL, ...) {
  # validate that step is a workflowstep
  if (!inherits(step, "workflowstep")) {
    stop("Argument 'step' must be of class 'workflowstep'.")
  }

  structure(
    list(
      step   = step,   # the workflowstep definition at run time
      args   = args,   # actual arguments passed to the function
      output = output, # result (if no error)
      error  = error,  # condition object (if any)
      meta   = list(...)
    ),
    class = c("workflowsteprun", "list")
  )
}

#' Print method for workflowsteprun objects
#'
#' @param x A `workflowsteprun` object.
#' @param ... Additional arguments (not used).
#' @export
print.workflowsteprun <- function(x, ...) {
  cat("<workflowsteprun>\n")
  cat("  step id:   ", x$step$id, "  (", x$step$name, ")\n", sep = "")
  cat("  operation: ", x$step$operation, "\n", sep = "")
  cat("  has error: ", !is.null(x$error), "\n", sep = "")
  invisible(x)
}
