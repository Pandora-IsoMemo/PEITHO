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
      errors        = NULL,            # all errors encountered
      last_result   = initial_input,   # last step’s result
      stepruns      = list()           # list of workflowsteprun objects
    ),
    class = c("workflowstate", "list")
  )
}

# helper for truncating
truncate_one <- function(x, n_char) {
  if (nchar(x) > n_char) paste0(substr(x, 1, n_char), " ...") else x
}

truncate_many <- function(vals, n_char, n_items, collapse = ", ") {
  vals <- as.character(vals)
  if (length(vals) > n_items) {
    vals <- c(vals[1:n_items], paste0("... (", length(vals) - n_items, " more items)"))
  }
  paste(vapply(vals, truncate_one, character(1), n_char = n_char), collapse = collapse)
}

trunc <- function(val, n_char = 40, n_items = 5) {
  if (is.null(val)) return("NULL")

  # lists of characters
  if (is.list(val) && all(vapply(val, is.character, logical(1)))) {
    return(truncate_many(unlist(val), n_char = n_char, n_items = n_items))
  }

  # character vectors
  if (is.character(val) && length(val) > 1) {
    return(truncate_many(val, n_char = n_char, n_items = n_items))
  }

  # everything else → treat as scalar
  val <- as.character(val)
  truncate_one(val, n_char = n_char)
}


#' Print method for workflowstate objects
#'
#' @param x A `workflowstate` object.
#' @param ... Additional arguments (not used).
#' @export
print.workflowstate <- function(x, ...) {
  # in which step errors occurred?
  is_error <- sapply(x$stepruns, function(sr) sr$has_error)
  cat("<workflowstate>\n")
  cat("  stepruns:      ", length(x$stepruns), "\n", sep = "")
  cat("  has error:    ", any(is_error), "\n", sep = "")
  if (any(is_error)) {
    cat("  error in steps:", paste(which(is_error), collapse = ", "), "\n", sep = " ")
    cat("                 (use summary() to see error details)\n")
    cat("  first error:   ", trunc(x$stepruns[[which(is_error)[1]]]$error), "\n", sep = "")
  }
  cat("  initial_input: ", trunc(x$initial_input), "\n", sep = "")
  cat("  last_result:   ", trunc(x$last_result), "\n", sep = "")
  invisible(x)
}

#' Add a workflow step run to the workflow state
#' 
#' This function appends a `workflowsteprun` object to the list of executed step runs
#' 
#' @param x A `workflowstate` object.
#' @param steprun A `workflowsteprun` object to add.
#' @param idx The index at which to add the step run.
#' @param ... Additional arguments (not used).
#' @return The updated `workflowstate` object.
#' @export
add_steprun.workflowstate <- function(x, steprun, idx, ...) {
  if (!inherits(x, "workflowstate")) {
    stop("Argument 'state' must be of class 'workflowstate'.")
  }
  if (!inherits(steprun, "workflowsteprun")) {
    stop("Argument 'steprun' must be of class 'workflowsteprun'.")
  }
  if (length(x$initial_input) == 0 && idx == 1L) {
    # set input param from first step if not set
    for (p in steprun$step$params) {
      if (p$type == "input") {
        initial_input <- p$value
        break
      }
    }

    x$initial_input <- initial_input
  }

  if (idx <= length(x$stepruns)) {
    x$stepruns[[idx]] <- steprun
    # remove all later stepruns
    if (length(x$stepruns) > idx) {
      x$stepruns <- x$stepruns[1:idx]
    }
  } else {
    x$stepruns[[length(x$stepruns) + 1L]] <- steprun
  }
  x$last_result <- if (!steprun$has_error) steprun$output else x$last_result
  x$errors <- if (steprun$has_error) c(x$errors, steprun$error) else x$errors
  x
}

