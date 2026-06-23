# state used while running a workflow --------------------------------------

#' Create a new workflow state object
#'
#' This object keeps track of the state while executing a workflow,
#' including the initial input, the last result, and the list of executed step runs.
#' @param initial_input The initial input provided to the workflow.
#' @param run_id A unique identifier for the workflow run (optional).
#' @return A `workflowstate` object.
#' @export
new_workflowstate <- function(initial_input = NULL, run_id = NULL) {
  structure(
    list(
      initial_input = initial_input,
      errors        = NULL,            # all errors encountered
      last_result   = initial_input,   # last step’s result
      stepruns      = list(),           # list of workflowsteprun objects
      run_id        = run_id,           # unique identifier for the workflow run
      # caches (duplication on purpose, we may later decide if we use 'id' or 'name' at the end)
      results_by_id   = list(),
      results_by_name = list(),
      last_result_id  = NA_integer_,
      last_result_name = NA_character_
    ),
    class = c("workflowstate", "list")
  )
}

# helper for truncating
truncate_one <- function(x, n_char) {
  if (!is.null(n_char) && is.numeric(n_char) && nchar(x) > n_char && n_char > 0)  {
    paste0(substr(x, 1, n_char), " ...")
  } else {
    x
  }
}

truncate_many <- function(vals, n_char, n_items, collapse = ", \n") {
  vals <- as.character(vals)
  if (!is.null(n_items) && is.numeric(n_items) && length(vals) > n_items && n_items > 0) {
    vals <- c(vals[1:n_items], paste0("... (", length(vals) - n_items, " more items)"))
  }
  paste(vapply(vals, truncate_one, character(1), n_char = n_char), collapse = collapse)
}

trunc <- function(val, n_char = 40, n_items = 5) {
  if (is.null(val)) return("NULL")

  # unwrap single-element lists
  # (we often have lists of length 1 due to how we store outputs and errors)
  if (is.list(val) && length(val) == 1L) {
    val <- val[[1L]]
  }

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
  step_summaries <- lapply(x$stepruns, summary)
  is_error <- vapply(step_summaries, function(s) !is.null(s$error), logical(1))
  cat("<workflowstate>\n")
  cat("  stepruns:      ", length(x$stepruns), "\n", sep = "")
  cat("  has error:    ", any(is_error), "\n", sep = "")
  if (any(is_error)) {
    cat("  error in steps:", paste(which(is_error), collapse = ", "), "\n", sep = " ")
    cat("                 (use summary() to see error details)\n")
    first_error <- step_summaries[[which(is_error)[1]]]$error
    cat("  first error:   ", trunc(first_error), "\n", sep = "")
  }
  cat("  initial_input: ", trunc(x$initial_input), "\n", sep = "")
  cat("  last_result:   ", trunc(x$last_result), "\n", sep = "")
  invisible(x)
}

#' Convert a workflowstate to a data frame
#'
#' This function summarizes the workflow state by converting the list of step runs
#' into a data frame. Truncation of error and output fields can be controlled via
#' `max_char` and `max_items` passed through `...`.
#'
#' @param x A `workflowstate` object.
#' @param row.names `NULL` or a character vector giving the row names for the data frame.
#' @param optional Logical, if `TRUE`, column names are not syntactically adjusted.
#' @param ... Additional arguments. Supports `max_char` (default: 50) and
#'   `max_items` (default: 5) to control truncation of error and output fields.
#' @return A data frame summarizing the workflow state.
#' @export
as.data.frame.workflowstate <- function(x, row.names = NULL, optional = FALSE, ...) {
  dots <- list(...)
  max_char <- if ("max_char" %in% names(dots)) dots$max_char else 50
  max_items <- if ("max_items" %in% names(dots)) dots$max_items else 5

  sr <- x$stepruns
  sr_summaries <- lapply(sr, summary)

  data.frame(
    entry     = vapply(sr, function(s) s$step$entry, integer(1)),
    name      = vapply(sr, function(s) s$step$name, character(1)),
    label     = vapply(sr, function(s) s$step$label, character(1)),
    has_error = vapply(sr_summaries, function(s) !is.null(s$error), logical(1)),
    error     = vapply(sr_summaries, function(s) if (!is.null(s$error)) trunc(s$error, n_char = max_char, n_items = max_items) else "", character(1)),
    output    = vapply(sr_summaries, function(s) trunc(s$result, n_char = max_char, n_items = max_items), character(1)),
    row.names = row.names,
    check.names = !isTRUE(optional),
    stringsAsFactors = FALSE
  )
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
update.workflowstate <- function(x, steprun, idx, ...) {
  # validations and initial input
  if (!inherits(x, "workflowstate")) {
    stop("Argument 'x' must be of class 'workflowstate'.")
  }
  if (!inherits(steprun, "workflowsteprun")) {
    stop("Argument 'steprun' must be of class 'workflowsteprun'.")
  }

  # add or update steprun at index
  if (idx <= length(x$stepruns)) {
    x$stepruns[[idx]] <- steprun
    # remove all later stepruns
    if (length(x$stepruns) > idx) {
      x$stepruns <- x$stepruns[1:idx]
    }
  } else {
    x$stepruns[[length(x$stepruns) + 1L]] <- steprun
  }

  sr_summary <- summary(steprun)
  
  #if (is.null(sr_summary$error)) {
    # Always cache results, even if they have errors
    # (errored steps will have character(0) as output from normalize_step_part)
    x$last_result <- steprun$output

    # cache with stable keys
    sid <- steprun$step$entry
    sname <- steprun$step$name

    x$last_result_id <- sid
    x$last_result_name <- sname

    # key by id
    x$results_by_id[[sid]] <- steprun$output

    # key by name (names should be unique once you start prefixing subflows)
    x$results_by_name[[sname]] <- steprun$output
  #} # else: on error, do not update last_result or caches

  x$errors <- if (!is.null(sr_summary$error)) c(x$errors, list(sr_summary$error)) else x$errors
  x
}
