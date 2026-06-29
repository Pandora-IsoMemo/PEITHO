# ---- workflowsteprun class ----

#' Create a new workflow step run object
#'
#' This object records the execution of a single step within a workflow,
#' including the step definition, the arguments used, the output or error,
#' and any additional metadata.
#' @param step   A `workflowstep` object representing the step definition.
#' @param args   A list of arguments that were passed to the step's command.
#'  Contains actual values used during execution (e.g. results of previous steps).
#' @param output List of outputs produced by the step if it executed
#'               successfully, otherwise list(NULL).
#' @param error  List of error conditions if the step encountered errors,
#'               otherwise list(NULL).
#' @param run_id A unique identifier for the workflow run this step belongs to.
#' @param ...    Additional metadata to store with the step run.
#' @return A `workflowsteprun` object.
#' @export
new_workflowsteprun <- function(step, args, output = NULL, error = NULL, run_id = NULL, ...) {
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
      run_id = run_id, # workflow run identifier
      has_error = any(!sapply(error, is.null)), # logical flag for convenience
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
  if (!is.null(x$run_id) && nzchar(x$run_id)) {
    cat("  run id:    ", x$run_id, "\n", sep = "")
  }
  cat("  step id:   ", x$step$entry, "  (", x$step$name, ")\n", sep = "")
  cat("  command: ", x$step$command, "\n", sep = "")
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
  # Collect errors: normalize to a flat list
  errs <- object$error
  if (is.null(errs)) {
    errs <- list()
  } else if (!is.list(errs) || inherits(errs, "condition")) {
    errs <- list(errs)
  }

  # Also check for error attributes on output items
  # (e.g., from functions that return errors as attributes rather than throwing)
  if (is.list(object$output)) {
    for (i in seq_along(object$output)) {
      out_item <- object$output[[i]]
      if (!is.null(out_item) && !is.null(attr(out_item, "error", exact = TRUE))) {
        errs[[length(errs) + 1L]] <- attr(out_item, "error", exact = TRUE)
      }
    }
  }

  # Normalize via shared helper (defined in 01-workflow-io.R)
  err_msgs <- Filter(
    function(x) !is.null(x) && nzchar(x),
    lapply(errs, normalize_error_message)
  )

  # error: NULL (clean) or single combined string
  error_out <- if (length(err_msgs)) paste(unlist(err_msgs), collapse = "\n") else NULL

  list(
    run_id = object$run_id,
    entry  = object$step$entry,
    name   = object$step$name,
    label  = object$step$label,
    result = object$output,
    error  = error_out,           # NULL or string — clean field
    errors = error_out %||% "",  # "" or string — backward compat
    status = if (is.null(error_out)) "ok" else "error"
  )
}