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

truncate_one <- function(x, n = 20) {
  if (nchar(x) > n) paste0(substr(x, 1, n), " ...") else x
}

truncate_many <- function(vals, n = 20, collapse = ", ") {
  vals <- as.character(vals)
  paste(vapply(vals, truncate_one, character(1), n = n), collapse = collapse)
}


#' Print method for workflowstate objects
#'
#' @param x A `workflowstate` object.
#' @param ... Additional arguments (not used).
#' @export
print.workflowstate <- function(x, ...) {
  # helper for truncating
  trunc <- function(val, n = 20) {
    if (is.null(val)) return("NULL")

    # lists of characters
    if (is.list(val) && all(vapply(val, is.character, logical(1)))) {
      return(truncate_many(unlist(val), n = n))
    }

    # character vectors
    if (is.character(val) && length(val) > 1) {
      return(truncate_many(val, n = n))
    }

    # everything else → treat as scalar
    val <- as.character(val)
    truncate_one(val, n = n)
  }

  cat("<workflowstate>\n")
  cat("  initial_input: ", trunc(x$initial_input), "\n", sep = "")
  cat("  last_result:   ", trunc(x$last_result), "\n", sep = "")
  cat("  stepruns:      ", length(x$stepruns), "\n", sep = "")
  cat("  available fields: ", paste(names(x), collapse = ", "), "\n", sep = "")
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

  x$last_result <- if (is.null(steprun$error)) steprun$output else x$last_result
  x
}

