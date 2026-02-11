# ---- workflowstep class ----

# constructor --------------------------------------------------------------

#' Create a new workflow step object
#'
#' This object defines a single step within a workflow,
#' including its unique identifier, name, operation, parameters, and other metadata.
#' @param id             An integer identifier for the step.
#' @param operation      A character string specifying the name of the function to execute for
#'                       this step, e.g. "strsplit". This function must exist in the loaded
#'                       name space or in a custom script environment.
#' @param name           A human-readable name for the step. Defaults to "Step <id>".
#' @param label          A label for the step, used in UIs. Defaults to the same as `name`.
#' @param comments       A character string with comments or description for the step.
#' @param params         A list of parameters to pass to the operation function.
#' @param loop           A character string indicating if the step should be looped over.
#'                       Can be "yes", "no", or "auto".
#' @param env            An environment to look up the operation function. Default is the parent
#'                       frame.
#' @param ...            Additional metadata to store with the step.
#' @return A `workflowstep` object.
#' @export
new_workflowstep <- function(
  id,
  operation,                     # function name (incl. custom name in script), exist in name space
  name            = NULL,
  label           = NULL,
  comments        = "",
  params          = list(),      # free-form list for step-specific parameters
  loop            = "",          # loop variable name (if any)
  env             = parent.frame(),  # where to look up operation
  ...
) {
  if (is.null(name)) name <- paste("Step", id)
  if (is.null(label)) label <- name

  resolve_operation(operation, env = env)

  structure(
    list(
      id              = as.integer(id),
      name            = name,
      label           = label,
      comments        = comments,
      operation       = operation,
      params          = params,
      loop            = loop,
      env             = env,
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
    utils::str(x$params, indent.str = "    ")
  }
  cat("  available fields: $", paste(names(x), collapse = ", $"), "\n", sep = "")
  invisible(x)
}

resolve_operation <- function(op_name, env) {
  PEITHO:::logDebug("  Resolving operation function: %s", op_name)
  if (!is.character(op_name) || length(op_name) != 1L || !nzchar(op_name)) {
    stop("'operation' must be a non-empty character string.", call. = FALSE)
  }
  if (!exists(op_name, mode = "function", envir = env, inherits = TRUE)) {
    stop("Operation '", op_name, "' not found in given environment.", call. = FALSE)
  }
  get(op_name, envir = env, mode = "function", inherits = TRUE)
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
#' @param object  A `workflowstep` object representing the step to execute.
#' @param state A `workflowstate` object representing the current state of the workflow.
#' @param env   An environment to look up the operation function. Defaults to the step's
#'  own env or the caller's env.
#' @param ...   Additional arguments (not used).
#' @return A `workflowsteprun` object recording the execution of the step.
#' @export
run.workflowstep <- function(
  object,
  state,
  env = NULL,  # where to look up operation
  ...
) {
  if (!inherits(state, "workflowstate")) {
    stop("'state' must be a 'workflowstate' object.", call. = FALSE)
  }
  # default env: use step-specific env if present, else caller’s env
  if (is.null(env)) {
    env <- if (!is.null(object$env)) object$env else parent.frame()
  }
  # 1) resolve the function
  # for a package you might use: env = asNamespace("PEITHO")
  fn <- resolve_operation(object$operation, env)

  # 2) assemble arguments
  params <- object$params
  if (!is.list(params)) params <- list()

  args <- list()

  PEITHO:::logDebug("  Extract arguments for operation '%s'", object$operation)
  for (param in params) {
    if (!inherits(param, "operationparam")) {
      stop("All entries in 'params' must be of class 'operationparam'.", call. = FALSE)
    }

    arg_list <- extract_arg_list(param, state = state)
    args <- c(args, arg_list)
  }

  if (length(args) == 0L) {
    stop("No parameters found for workflow step.", call. = FALSE)
  }

  # find lists among args that need to be looped over
  # (for now we only support looping over a single argument)
  PEITHO:::logDebug("  Check for looping over arguments")
  is_arg_list <- detect_list_args(args)

  is_param_config_loop <- detect_param_config_loop(params, is_arg_list)
  # if multiple args to loop over, throw error
  if (sum(is_arg_list) > 1 || sum(is_param_config_loop) > 1) {
    stop("Looping over multiple arguments is not supported.", call. = FALSE)
  }

  arg_list_indices    <- which(unname(is_arg_list))
  loop_param_indices  <- which(is_param_config_loop)

  # if loop_param and loop_arg disagree, throw error
  if (!identical(loop_param_indices, arg_list_indices)) {
    PEITHO:::logWarn(
      "WARNING! Detected list argument(s) for operation '%s', but 'loop' is set to '%s'.",
      object$operation,
      params[[arg_list_indices[1]]]$loop
    )
  }

  # 3) actually call the function, if needed then in a loop
  if (any(is_param_config_loop)) {
    PEITHO:::logDebug(
      "  Running operation: WITH LOOPING over argument index %d",
      loop_param_indices
    )
    loop_index <- loop_param_indices[1]
    loop_values <- args[[loop_index]]

    runs <- lapply(loop_values, function(v) {
      args[[loop_index]] <- v
      run_with_error(fn, args) # <--- RUN FUNCTION HERE, loop run
    })
    results <- lapply(runs, `[[`, "output")
    errors  <- lapply(runs, `[[`, "error")

    PEITHO:::logInfo("  %d loop iterations for operation '%s':", length(runs), object$operation)
    max_result_length <- max(lengths(results))
    if (max_result_length > 1L) {
      PEITHO:::logWarn(
        "     WARNING! Multiple results per iteration! Ensure that downstream steps handle list inputs."
      )
    } else {
      PEITHO:::logInfo("     %d single results.", length(results))
    }

    # return list of results/errors
    steprun <- new_workflowsteprun(
      step   = object,
      args   = args,
      output = results,
      error  = errors
    )
  } else {
    PEITHO:::logDebug("  Running operation: NO LOOPING")

    run <- run_with_error(fn, args) # <--- RUN FUNCTION HERE, single run

    # check if result has length > 1 or not
    is_single_result <- length(run$output) == 1L
    PEITHO:::logInfo(
      "  Operation '%s': %s result%s",
      object$operation,
      if (is_single_result) "single" else length(run$output),
      if (is_single_result) "" else "s"
    )

    steprun <- new_workflowsteprun(
      step   = object,
      args   = args,
      output = run$output,
      error  = run$error
    )
  }

  steprun
}

# helpers ----------------------------------------------------------------------

detect_list_args <- function(args) {
  vapply(args, function(x) is.list(x) && length(x) > 1L, logical(1))
}

detect_param_config_loop <- function(params, is_arg_list) {
  mapply(function(param, is_list) {
    if (is_list) {
      param$loop %in% c("yes", "auto")
    } else {
      param$loop == "yes"
    }
  }, params, is_arg_list)
}
