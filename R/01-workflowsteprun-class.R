# ---- workflowsteprun class ----

#' Create a new workflow step run object
#'
#' This object records the execution of a single step within a workflow,
#' including the step definition, the arguments used, the output or error,
#' and any additional metadata.
#' @param step   A `workflowstep` object representing the step definition.
#' @param args   A list of arguments that were passed to the step's operation.
#'  Contains actual values used during execution (e.g. results of previous steps).
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

  if (!is.list(args)) args <- as.list(args)

  structure(
    list(
      step   = step,   # the workflowstep definition at run time
      args   = args,   # actual arguments passed to the function
      output = output, # result (if no error)
      error  = error,  # condition object (if any)
      has_error = if (is.list(error)) {
        any(!sapply(error, is.null))
      } else {
        !is.null(error)
      },
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
  cat("  args:      ", paste(names(x$args), collapse = ", "), "\n", sep = "")
  cat("  has error: ", x$has_error, "\n", sep = "")
  cat("  available fields: $", paste(names(x), collapse = ", $"), "\n", sep = "")
  invisible(x)
}

#' Summary method for workflowsteprun objects
#'
#' @param object A `workflowsteprun` object.
#' @param ... Additional arguments (not used).
#' @export
summary.workflowsteprun <- function(object, ...) {
  if (length(object$error) > 0) {
    is_error <- !sapply(object$error, is.null)
  } else {
    is_error <- !is.null(object$error)
  }

  list(
    entry      = object$step$id,
    name       = object$step$name,
    label      = object$step$label,
    result     = object$output,
    errors     = if (!any(is_error)) "" else {
      sapply(object$error[is_error], function(e) {
        if (inherits(e, "condition")) {
          conditionMessage(e)
        } else if (is.character(e)) {
          e
        } else {
          as.character(e)
        }
      })
    }
  )
}