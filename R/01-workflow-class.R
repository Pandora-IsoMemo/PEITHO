# ---- workflow class ----

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
      state    = state
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
  cat("  stepruns: ", length(x$state$stepruns), "\n", sep = "")
  invisible(x)
}

#' Create a new workflow object
#'
#' This object represents a workflow, consisting of a sequence of workflow steps,
#' the current step index, and additional metadata.
#' @param steps   A list of `workflow_step` objects defining the steps of the workflow.
#' @param name    A name for the workflow.
#' @param current The index of the current step in the workflow.
#' @param ...    Additional metadata to store with the workflow.
#' @return A `workflow` object.
#' @export
new_workflow <- function(
  steps   = list(),
  name    = "Untitled workflow",
  current = if (length(steps)) 1L else NA_integer_,
  ...
) {
  # ensure steps are workflow_step objects
  if (length(steps)) {
    ok <- vapply(steps, inherits, logical(1), what = "workflowstep")
    if (!all(ok)) {
      stop("All elements of 'steps' must be of class 'workflowstep'.", call. = FALSE)
    }
  }

  if (length(steps) == 0L) {
    current <- NA_integer_
  } else {
    current <- max(1L, min(as.integer(current), length(steps)))
  }

  structure(
    list(
      name    = name,
      steps   = steps,
      current = current,
      dots    = list(...)
    ),
    class = c("workflow", "list")
  )
}

# user-facing helper (function with more validation) -----------------------

workflow <- function(steps = list(), name = "Untitled workflow", current = 1L, ...) {
  new_workflow(steps = steps, name = name, current = current, ...)
}

# print method -------------------------------------------------------------

#' Print method for workflow objects
#'
#' @param x A `workflow` object.
#' @param ... Additional arguments (not used).
#' @export
print.workflow <- function(x, ...) {
  cat("<workflow>\n")
  cat("  name:   ", x$name, "\n", sep = "")
  cat("  steps:  ", length(x$steps), "\n", sep = "")
  cat("  current:", x$current, "\n", sep = "")

  if (length(x$steps)) {
    cat("  step summary:\n")
    for (i in seq_along(x$steps)) {
      s <- x$steps[[i]]
      is_curr <- if (i == x$current) "*" else " "
      cat(
        sprintf(
          "   %s [%d] %s (%s | src: %s)\n",
          is_curr, s$id, s$name, s$operation, s$source_external
        )
      )
    }
  }

  invisible(x)
}

#' Accessor functions ------------------------------------------------------

#' Get the current step of the workflow
#'
#' @param x A `workflow` object.
#' @param ... Additional arguments (not used).
#' @return The current `workflow_step` object, or `NULL` if no current step.
#' @export
current_step.workflow <- function(x, ...) {
  if (length(x$steps) == 0L || is.na(x$current)) {
    return(NULL)
  }
  x$steps[[x$current]]
}

#' Move to the next step in the workflow
#'
#' @param x A `workflow` object.
#' @param wrap Logical; if `TRUE`, wrap around to the first step when at the end.
#' @param ... Additional arguments (not used).
#' @return The updated `workflow` object with the current step advanced.
#' @export
next_step.workflow <- function(x, wrap = FALSE, ...) {
  if (length(x$steps) == 0L || is.na(x$current)) {
    return(x)
  }

  if (x$current < length(x$steps)) {
    x$current <- x$current + 1L
  } else if (wrap) {
    x$current <- 1L
  } # else stay at last

  x
}

#' Move to the previous step in the workflow
#'
#' @param x A `workflow` object.
#' @param wrap Logical; if `TRUE`, wrap around to the last step when at the beginning.
#' @param ... Additional arguments (not used).
#' @return The updated `workflow` object with the current step moved back.
#' @export
previous_step.workflow <- function(x, wrap = FALSE, ...) {
  if (length(x$steps) == 0L || is.na(x$current)) {
    return(x)
  }

  if (x$current > 1L) {
    x$current <- x$current - 1L
  } else if (wrap) {
    x$current <- length(x$steps)
  } # else stay at first

  x
}


#' Go to a specific step in the workflow
#'
#' @param x A `workflow` object.
#' @param index An integer index of the step to go to.
#' @param id An integer id of the step to go to.
#' @param ... Additional arguments (not used).
#' @return The updated `workflow` object with the current step set.
#' @export
goto_step.workflow <- function(x, index = NULL, id = NULL, ...) {
  if (length(x$steps) == 0L || is.na(x$current)) {
    return(x)
  }

  if (!is.null(index)) {
    index <- as.integer(index)
    if (index >= 1L && index <= length(x$steps)) {
      x$current <- index
    } else {
      warning("Index out of range; 'workflow$current' unchanged.", call. = FALSE)
    }
    return(x)
  }

  if (!is.null(id)) {
    ids <- vapply(x$steps, function(s) s$id, integer(1))
    idx <- which(ids == as.integer(id))
    if (length(idx) == 1L) {
      x$current <- idx
    } else {
      warning("No step with matching 'id'; 'workflow$current' unchanged.", call. = FALSE)
    }
    return(x)
  }

  warning("Provide either 'index' or 'id' to goto_step().", call. = FALSE)
  x
}

#' Run the entire workflow
#'
#' @param wf A `workflow` object.
#' @param state A `workflowstate` object representing the initial state.
#' @param from An integer index of the step to start from.
#' @param to An integer index of the step to end at.
#' @param stop_on_error Logical; if `TRUE`, stop execution on the first error.
#' @param ... Additional arguments passed to `run_step()`.
#' @return A list containing the final workflow, state, and results of each step.
#' @export
run_workflow.workflow <- function(
  wf,
  state = list(),
  from  = 1L,
  to    = length(wf$steps),
  stop_on_error = TRUE,
  ...
) {
    if (!inherits(state, "workflowstate")) {
    state <- new_workflowstate(initial_input = state)
  }

  if (length(wf$steps) == 0L) {
    warning("Workflow has no steps.")
    return(
      list(
        workflow = wf,
        state    = state,
        results  = list()
      )
    )
  }

  from <- max(1L, as.integer(from))
  to   <- min(length(wf$steps), as.integer(to))
  idxs <- seq(from, to)

  #results <- vector("list", length(idxs))

  for (j in seq_along(idxs)) {
    i <- idxs[j]
    step <- wf$steps[[i]]

    steprun <- run_step(step, state, ...)

    state <- add_steprun(state, steprun)

    #results[[j]] <- steprun$output

    if (stop_on_error && !is.null(steprun$error)) {
      stop(
        "Error in step ", i, " (id=", step$id, "): ", steprun$error$message,
        call. = FALSE
      )
    }
  }

  new_workflowrun(wf, state)
}
