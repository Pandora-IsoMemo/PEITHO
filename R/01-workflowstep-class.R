# ---- workflowstep class ----

# constructor --------------------------------------------------------------

#' Create a new workflow step object
#'
#' This object defines a single step within a workflow,
#' including its unique identifier, name, operation, parameters, and other metadata.
#' @param id             An integer identifier for the step.
#' @param name           A human-readable name for the step. Defaults to "Step <id>".
#' @param label          A label for the step, used in UIs. Defaults to the same as `name`.
#' @param operation      A character string specifying the name of the function to execute for
#'                       this step, e.g. "strsplit".
#'                       This function must exist in the loaded name space.
#' @param params         A list of parameters to pass to the operation function.
#' @param include_last   A logical indicating whether to include the last result
#'                       from the workflow state as an input to this step's operation.
#'                       Default is `TRUE`.
#' @param source_external A character string indicating the source of external data for this step.
#'                        Default is `"None"`.
#' @param ...            Additional metadata to store with the step.
#' @return A `workflowstep` object.
#' @export
new_workflowstep <- function(
  id,
  name            = NULL,
  label           = NULL,
  operation       = NULL,            # operation name, must exist in name space
  params          = list(),          # free-form list for step-specific parameters
  include_last    = TRUE,
  source_external = "None",
  ...
) {
  if (is.null(name)) {
    name <- paste("Step", id)
  }

  if (is.null(label)) {
    label <- name
  }

  # check if operation is part of loaded name space
  if (!exists(operation, mode = "function", inherits = TRUE)) {
    stop(
      "Operation '", operation, "' of step ", id, " ('", label,
      "') not found in loaded name space.",
      " Please define the function before using it as a workflow step."
    )
  }

  structure(
    list(
      id             = as.integer(id),
      name           = name,
      label          = label,
      operation      = operation,
      source_external = source_external,
      include_last   = isTRUE(include_last),
      params         = params,
      dots           = list(...)     # extension point
    ),
    class = c("workflowstep", "list")
  )
}

# user-facing helper (function with more validation) -----------------------
workflowstep <- function(
  id,
  name            = NULL,
  operation       = "chat",
  source_external = "None",
  include_last    = TRUE,
  params          = list(),
  ...
) {
  new_workflowstep(
    id             = id,
    name           = name,
    operation      = operation,
    source_external = source_external,
    include_last   = include_last,
    params         = params,
    ...
  )
}

# print method -------------------------------------------------------------

#' Print method for workflowstep objects
#'
#' @param x A `workflowstep` object.
#' @param ... Additional arguments (not used).
#' @export
print.workflowstep <- function(x, ...) {
  cat("<workflowstep>\n")
  cat("  id:             ", x$id, "\n", sep = "")
  cat("  name:           ", x$name, "\n", sep = "")
  if (x$label != x$name) {
    cat("  label:          ", x$label, "\n", sep = "")
  }
  cat("  operation:      ", x$operation, "\n", sep = "")
  cat("  source_external:", x$source_external, "\n", sep = "")
  cat("  include_last:   ", x$include_last, "\n", sep = "")
  if (length(x$params)) {
    cat("  params:\n")
    str(x$params, indent.str = "    ")
  }
  invisible(x)
}

#' Run a workflow step
#'
#' This function executes a single workflow step, updating the workflow state
#' with the result or error from the step execution.
#' @param step  A `workflowstep` object representing the step to execute.
#' @param state A `workflowstate` object representing the current state of the workflow.
#' @param env   An environment to look up the operation function. Default is the parent frame.
#' @param ...   Additional arguments (not used).
#' @return The updated `workflowstate` object after executing the step.
#' @export
run_step.workflowstep <- function(step,
                                  state, # possibly we'll need last results from here later
                                  env = parent.frame(),  # where to look up operation
                                  ...) {
  if (!inherits(state, "workflowstate")) {
    stop("'state' must be a 'workflowstate' object.", call. = FALSE)
  }

  # 1) resolve the function
  # for a package you might use: env = asNamespace("PEITHO")
  fn <- get(step$operation, envir = env, mode = "function", inherits = TRUE)

  # 2) assemble arguments
  args <- step$params
  if (!is.list(args)) args <- list()

  # if desired, include last result as an input argument
  if (isTRUE(step$include_last)) {
    # convention: make it available as 'input'
    # You can change this to 'x', 'prev', etc.
    #args$input <- state$last_result

    # will be added later
  }

  # 3) actually call the function
  out <- NULL
  err <- NULL
  res <- tryCatch(
    {
      out <- do.call(fn, args)
      out
    },
    error = function(e) {
      err <<- e
      NULL
    }
  )

  # 4) create execution record
  steprun <- new_workflowsteprun(
    step   = step,
    args   = args,
    output = res,
    error  = err
  )

  # # 5) update state
  # state <- add_steprun(state, steprun)
  # state

  steprun
}