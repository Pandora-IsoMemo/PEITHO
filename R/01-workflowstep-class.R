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
  cat("  has error: ", !is.null(x$error), "\n", sep = "")
  cat("  available fields: ", paste(names(x), collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' Summary method for workflowsteprun objects
#'
#' @param x A `workflowsteprun` object.
#' @param ... Additional arguments (not used).
#' @export
summary.workflowsteprun <- function(x, ...) {
  if (length(x$error) > 0) {
    is_error <- !sapply(x$error, is.null)
  } else {
    is_error <- !is.null(x$error)
  }

  list(
    entry      = x$step$id,
    name       = x$step$name,
    label      = x$step$label,
    result     = x$output,
    errors     = if (!any(is_error)) "" else sapply(x$error[is_error], conditionMessage)
  )
}

resolve_operation <- function(op_name, env) {
  if (!is.character(op_name) || length(op_name) != 1L || !nzchar(op_name)) {
    stop("'operation' must be a non-empty character string.", call. = FALSE)
  }
  if (!exists(op_name, mode = "function", envir = env, inherits = TRUE)) {
    stop("Operation '", op_name, "' not found in given environment.", call. = FALSE)
  }
  get(op_name, envir = env, mode = "function", inherits = TRUE)
}

# ---- workflowstep class ----

# constructor --------------------------------------------------------------

#' Create a new workflow step object
#'
#' This object defines a single step within a workflow,
#' including its unique identifier, name, operation, parameters, and other metadata.
#' @param id             An integer identifier for the step.
#' @param operation      A character string specifying the name of the function to execute for
#'                       this step, e.g. "strsplit".
#'                       This function must exist in the loaded name space.
#' @param name           A human-readable name for the step. Defaults to "Step <id>".
#' @param label          A label for the step, used in UIs. Defaults to the same as `name`.
#' @param comments       A character string with comments or description for the step.
#' @param params         A list of parameters to pass to the operation function.
#' @param ...            Additional metadata to store with the step.
#' @return A `workflowstep` object.
#' @export
new_workflowstep <- function(
  id,
  operation,                         # operation name, must exist in name space
  name            = NULL,
  label           = NULL,
  comments        = "",
  params          = list(),          # free-form list for step-specific parameters
  loop            = "",              # loop variable name (if any)
  ...
) {
  if (is.null(name)) {
    name <- paste("Step", id)
  }

  if (is.null(label)) {
    label <- name
  }

  resolve_operation(operation, env = parent.frame())

  structure(
    list(
      id              = as.integer(id),
      name            = name,
      label           = label,
      comments        = comments,
      operation       = operation,
      params          = params,
      loop            = loop,
      dots            = list(...)     # extension point
    ),
    class = c("workflowstep", "list")
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
  if (nzchar(x$comments)) {
    cat("  comments:       ", x$comments, "\n", sep = "")
  }
  cat("  operation:      ", x$operation, "\n", sep = "")
  if (length(x$params)) {
    cat("  params:\n")
    str(x$params, indent.str = "    ")
  }
  cat("  available fields: ", paste(names(x), collapse = ", "), "\n", sep = "")
  invisible(x)
}

run_with_error <- function(fn, args) {
  tryCatch(
    list(output = do.call(fn, args), error = NULL),
    error = function(e) list(output = NULL, error = e)
  )
}

#' Run a workflow step
#'
#' This function executes a single workflow step, updating the workflow state
#' with the result or error from the step execution.
#' @param step  A `workflowstep` object representing the step to execute.
#' @param state A `workflowstate` object representing the current state of the workflow.
#' @param result_path Path to the results summary JSON file.
#' @param env   An environment to look up the operation function. Default is the parent frame.
#' @param ...   Additional arguments (not used).
#' @return A `workflowsteprun` object recording the execution of the step.
#' @export
run_step.workflowstep <- function(
  step,
  state, # possibly we'll need last results from here later
  result_path = system.file("scripts", "peitho_files", "results_summary.json", package = "PEITHO"),
  env = parent.frame(),  # where to look up operation
  ...
) {
  if (!inherits(state, "workflowstate")) {
    stop("'state' must be a 'workflowstate' object.", call. = FALSE)
  }
  # 1) resolve the function
  # for a package you might use: env = asNamespace("PEITHO")
  fn <- resolve_operation(step$operation, env)

  # 2) assemble arguments
  params <- step$params
  if (!is.list(params)) params <- list()

  args <- list()

  for (param in params) {
    if (!inherits(param, "operationparam")) {
      stop("All entries in 'params' must be of class 'operationparam'.", call. = FALSE)
    }
    arg_list <- extract_arg(param, result_path = result_path)
    args <- c(args, arg_list)
  }

  # find lists among args that need to be looped over
  # (for now we only support looping over a single argument)
  is_arg_list <- logical(length(args))
  is_param_config_loop <- logical(length(args))
  for (idx in seq_along(args)) {
    is_arg_list[idx] <- is.list(args[[idx]]) && length(args[[idx]]) > 1L
    if (is_arg_list[idx]) {
      is_param_config_loop[idx] <- params[[idx]]$loop %in% c("yes", "auto")
    } else {
      is_param_config_loop[idx] <- params[[idx]]$loop == "yes"
    }
  }

  # if multiple args to loop over, throw error
  if (sum(is_arg_list) > 1 || sum(is_param_config_loop) > 1) {
    stop("Looping over multiple arguments is not supported.", call. = FALSE)
  }

  # if loop_param and loop_arg disagree, throw error
  if (!identical(which(is_param_config_loop), which(unname(is_arg_list)))) {
    stop(
      "Mismatch between 'loop' setting in operationparam and actual argument value.",
      call. = FALSE
    )
  }

  # 3) actually call the function, if needed than in a loop
  if (any(is_param_config_loop)) {
    loop_index <- which(is_param_config_loop)[1]
    loop_values <- args[[loop_index]]
    runs <- lapply(loop_values, function(v) {
      args[[loop_index]] <- v
      run_with_error(fn, args) # <--- RUN FUNCTION HERE, loop run
    })
    results <- lapply(runs, `[[`, "output")
    errors  <- lapply(runs, `[[`, "error")

    # return list of results/errors
    steprun <- new_workflowsteprun(
      step   = step,
      args   = args,
      output = results,
      error  = errors
    )
  } else {
    run <- run_with_error(fn, args) # <--- RUN FUNCTION HERE, single run
    steprun <- new_workflowsteprun(
      step   = step,
      args   = args,
      output = run$output,
      error  = run$error
    )
  }

  steprun
}