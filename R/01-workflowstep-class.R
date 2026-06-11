# ---- workflowstep class ----

# constructor -----------------------------------------------------------------

#' Create a new workflow step object
#'
#' This object defines a single step within a workflow,
#' including its unique identifier, name, command, parameters, and other metadata.
#' @param entry          An integer representing the step's position in the workflow.
#'                       This should be unique for each step.
#' @param command        A character string specifying the name of the function to execute for
#'                       this step, e.g. "strsplit". This function must exist in the loaded
#'                       name space or in a custom script environment.
#' @param name           A human-readable name for the step. Defaults to "Step <entry>".
#' @param label          A label for the step, used in UIs. Defaults to the same as `name`.
#' @param comments       A character string with comments or description for the step.
#' @param args           The original argument string from the workflow file, for reference.
#' @param iteration      A character string indicating if the step should iterate over
#'                       list arguments. Can be "yes", "no", or "auto".
#' @param samples        Integer number of samples to run per iteration. Must be
#'                       >= 1. Defaults to 1 (current behavior).
#' @param loop           Deprecated alias for `iteration` (kept for backward compatibility).
#' @param env            An environment to look up the command function.
#'                       Defaults to the caller's env. Warns if the command
#'                       cannot be found, but does not throw an error at this
#'                       stage.
#' @param ...            Additional metadata to store with the step.
#' @return A `workflowstep` object.
#' @export
new_workflowstep <- function(
  entry,
  command,                       # function name (incl. custom name in script), exist in name space
  name            = NULL,
  label           = NULL,
  comments        = "",
  args            = "",          # original argument string from workflow file, for reference
  iteration       = "auto",      # iteration behavior for list arguments
  samples         = 1L,           # number of repeated runs per iteration
  loop            = NULL,          # deprecated alias
  env             = parent.frame(),
  ...
) {
  if (is.null(name)) name <- paste("Step", entry)
  if (is.null(label)) label <- name

  resolve_operation(command, env = env, warn_only = TRUE)

  required_fields <- parse_required_fields(args)

  # Backward compatibility: `loop` overrides when explicitly supplied.
  if (!is.null(loop)) {
    iteration <- loop
  }

  samples <- as.integer(samples)
  if (is.na(samples) || length(samples) != 1L || samples < 1L) {
    stop("Argument 'samples' must be a single integer >= 1.", call. = FALSE)
  }

  required_inputs <- required_fields$inputs
  required_steps  <- required_fields$steps

  structure(
    list(
      entry           = as.integer(entry),
      name            = name,
      label           = label,
      comments        = comments,
      command         = command,
      required_inputs = required_inputs,
      required_steps  = required_steps,
      args            = args,
      iteration       = iteration,
      samples         = samples,
      loop            = iteration,
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
  cat("  entry:          ", x$entry, "\n", sep = "")
  cat("  name:           ", x$name, "\n", sep = "")
  if (x$label != x$name) {
    cat("  label:          ", x$label, "\n", sep = "")
  }
  if (nzchar(x$comments)) {
    cat("  comments:       ", x$comments, "\n", sep = "")
  }
  cat("  command:      ", x$command, "\n", sep = "")
  cat("  args:         ", x$args, "\n", sep = "")
  cat("  available fields: $", paste(names(x), collapse = ", $"), "\n", sep = "")
  invisible(x)
}

flatten_params <- function(params) {
  if (length(params) == 0) return("")
  paste(
    vapply(params, function(p) {
      if (!is.null(p$name) && p$type %in% c("input", "result")) {
        paste0(p$name, "=", p$tag, toString(p$label), p$tag)
      } else if (!is.null(p$name) && p$type == "literal") {
        paste0(p$name, "=", toString(p$value))
      } else {
        ""
      }
    }, character(1)),
    collapse = ", "
  )
}

#' Convert a workflowstep to a data frame
#'
#' This method converts a `workflowstep` object into a data frame format, which can be
#' useful for tabular display or exporting.
#' @param x A `workflowstep` object.
#' @param ... Additional arguments (not used).
#' @return A data frame with one row representing the workflow step.
#' @export
as.data.frame.workflowstep <- function(x, ...) {
  data.frame(
    "Entry"         = x$entry,
    "Name"          = x$name,
    "Label"         = x$label,
    "Comments"      = x$comments,
    "Function"      = x$command,
    "Parameters"    = x$args,
    stringsAsFactors = FALSE
  )
}

#' Convert a workflowstep to commands.json record format
#'
#' This method converts a `workflowstep` object into a list format suitable for writing
#' to a `commands.json` file, which is used to define the workflow steps in a structured way.
#' @param x A `workflowstep` object.
#' @param ... Additional arguments (not used).
#' @return A list representing the workflow step for commands.json.
#' @export
as.commands_record.workflowstep <- function(x, ...) {
  list(
    entry    = as.integer(x$entry),
    name     = as.character(x$name),
    label    = as.character(x$label),
    comments = as.character(x$comments),
    command  = as.character(x$command),
    args     = as.character(x$args),
    iteration = as.character(x$iteration %||% x$loop),
    samples  = as.integer(x$samples %||% 1L),
    prompt   = ""
  )
}

map_field <- function() {
  list(
    Entry = "entry",
    Name = "name",
    Label = "label",
    Comments = "comments",
    Function = "command",
    Parameters = "args"
  )
}

#' Get a specific field from a workflowstep
#'
#' @param x A `workflowstep` object.
#' @param field The name of the field to retrieve (e.g., "name", "comments").
#' @param with_map_field Logical, whether to map the field name using `map_field()`. Defaults to `TRUE`.
#' @param ... Additional arguments (not used).
#' @return The value of the specified field from the workflowstep.
#' @export
get_field.workflowstep <- function(x, field, with_map_field = TRUE, ...) {
  if (with_map_field) {
    field <- map_field()[[field]]
  }
  x[[field]]
}

#' Update a workflow step
#'
#' This method allows updating specific fields of a `workflowstep` object, such as its name,
#' label, comments, command, parameters, or loop configuration.
#'
#' @param x The `workflowstep` object to update.
#' @param workflow_file_paths The paths to the workflow files, used for updating related files.
#' @param value The new value to assign to the specified field.
#' @param field The name of the field to update. Must be one of "name", "label", "comments",
#'  "command", "args", "iteration", "samples", or "loop".
#' @param with_map_field Logical, whether to map the field name using `map_field()`.
#'  Defaults to `TRUE`.
#' @param ... Additional arguments (not used).
#' @return The updated `workflowstep` object.
#' @export
update.workflowstep <- function(
  x,
  workflow_file_paths,
  value,
  field,
  with_map_field = TRUE,
  ...
) {
  # validate field
  if (with_map_field) {
    if (!field %in% names(map_field())) {
      allowed_entries <- names(map_field())
      stop(sprintf(
        "Invalid field '%s'. Must be one of %s.",
        field,
        paste(allowed_entries, collapse = ", ")
      ), call. = FALSE)
    }
    field <- map_field()[[field]]
  }
  if (!field %in% c(unlist(map_field(), use.names = FALSE), "iteration", "samples", "loop")) {
    allowed_entries <- c(unlist(map_field(), use.names = FALSE), "iteration", "samples", "loop")
    stop(sprintf(
      "Invalid field '%s'. Must be one of %s.",
      field,
      paste(allowed_entries, collapse = ", ")
    ), call. = FALSE)
  }

  # Backward compatibility for old field name
  if (field == "loop") {
    field <- "iteration"
  }

  # validate value
  if (field == "samples") {
    value <- as.integer(value)
    if (is.na(value) || length(value) != 1L || value < 1L) {
      stop("'samples' must be a single integer >= 1.", call. = FALSE)
    }
  } else if (field != "entry" && (!is.character(value) || length(value) != 1L)) {
    stop(sprintf("'%s' must be a single character string.", field), call. = FALSE)
  }
  if (field == "entry" && (!is.integer(value) || length(value) != 1L)) {
    stop(sprintf("'%s' must be a single integer.", field), call. = FALSE)
  }

  # update the specified field in the workflowstep object
  x[[field]] <- value

  # Keep deprecated alias in sync for compatibility with older internal code paths.
  if (field == "iteration") {
    x$loop <- value
  }

  # update required fields also here, to keep them in sync
  if (field == "args") {
    PEITHO:::logDebug("Parsing required fields from args string for step %d", x$entry)
    required_fields <- parse_required_fields(x$args)

    x$required_inputs <- required_fields$inputs
    x$required_steps  <- required_fields$steps
  }

  # update the commands.json (only if the workflow is file-backed)
  if (!is.null(workflow_file_paths) &&
        length(workflow_file_paths) > 0L &&
        !is.null(workflow_file_paths$commands_path)) {
    # get i-th entry from commands file, update it and write back to file
    commands_list <- read_json_if_exists(path = workflow_file_paths$commands_path)

    # update i-th step
    logDebug(
      "Updating workflow file '%s': step %d, field '%s' with value '%s'",
      basename(workflow_file_paths$commands_path),
      x$entry,
      field,
      value
    )
    commands_list[[x$entry]] <- as.commands_record(x)
    # write back to file
    jsonlite::write_json(
      commands_list,
      path = workflow_file_paths$commands_path,
      auto_unbox = TRUE,
      pretty = TRUE
    )
  }

  # return updated workflowstep
  x
}

resolve_operation <- function(op_name, env, warn_only = FALSE) {
  PEITHO:::logDebug("  Resolving command function: %s", op_name)
  if (!is.character(op_name) || length(op_name) != 1L || !nzchar(op_name)) {
    stop("'command' must be a non-empty character string.", call. = FALSE)
  }
  if (!exists(op_name, mode = "function", envir = env, inherits = TRUE)) {
    msg <- sprintf("Command '%s' not found in given environment.", op_name)
    if (warn_only) {
      PEITHO:::logWarn("%s", msg)
      warning(msg, immediate. = TRUE, call. = FALSE)
      return(NULL)
    } else {
      stop(msg, call. = FALSE)
    }
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
#' @param x  A `workflowstep` object representing the step to execute.
#' @param state A `workflowstate` object representing the current state of the workflow.
#' @param env   An environment to look up the command function. Defaults to the caller's env.
#' @param step_i The number of the step in the workflow, used for logging purposes.
#' @param input_list A list of inputs for argument parsing, loaded from the workflow's inputs file.
#' @param ...   Additional arguments (not used).
#' @return A `workflowsteprun` object recording the execution of the step.
#' @export
run.workflowstep <- function(
  x,
  state,
  env = NULL,            # where to look up function
  step_i = NULL,         # for logging purposes
  step_idx = NULL,       # absolute workflow index for persistence
  input_list = NULL,     # for arguments parsing
  results_path = NULL,   # optional results file path for intermediate writes
  resume_from_sample = NULL, # first sample index still needing execution (NULL = run all)
  resume_from_iteration = NULL, # first iteration index still needing execution (NULL = run all)
  ...
) {
  if (!inherits(state, "workflowstate")) {
    stop("'state' must be a 'workflowstate' object.", call. = FALSE)
  }
  # Resolve environment: use provided env, or fall back to caller's env
  if (is.null(env)) {
    env <- parent.frame()
  }
  # 1) resolve the function
  # for a package you might use: env = asNamespace("PEITHO")
  fn <- resolve_operation(x$command, env)

  # 2) assemble arguments
  PEITHO:::logInfo("Parsing arguments for command %s", x$command)
  params <- make_param_from_arg_loop(
    args_string = x$args,
    loop = x$iteration %||% x$loop,
    step_i = step_i,
    input_list = input_list
  )

  if (!is.list(params)) params <- list()

  args <- list()

  PEITHO:::logDebug("  Extract arguments for command '%s'", x$command)
  for (param in params) {
    if (!inherits(param, "operationparam")) {
      stop("All entries in 'params' must be of class 'operationparam'.", call. = FALSE)
    }

    # unpack params object into args
    arg_list <- extract_arg_list(param, state = state)
    args <- c(args, arg_list)
  }

  if (length(args) == 0L) {
    stop("No parameters found for workflow step.", call. = FALSE)
  }

  sample_total <- as.integer(x$samples %||% 1L)
  if (is.na(sample_total) || sample_total < 1L) {
    stop("'samples' must be a single integer >= 1.", call. = FALSE)
  }

  results_file_name <- NULL
  results_path_to_folder <- NULL
  if (!is.null(results_path) && nzchar(results_path)) {
    results_file_name <- basename(results_path)
    results_path_to_folder <- dirname(results_path)
  }

  detail_step_id <- step_idx %||% step_i %||% x$entry

  prior_detail_records <- NULL
  if (
    (!is.null(resume_from_iteration) || !is.null(resume_from_sample)) &&
      !is.null(results_path_to_folder) && !is.null(results_file_name)
  ) {
    prior_detail_records <- read_iteration_records_for_step(
      run_id = state$run_id,
      step = detail_step_id,
      path_to_folder = results_path_to_folder,
      results_file = results_file_name
    )
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
    configured_iteration <- params[[arg_list_indices[1]]]$iteration %||% params[[arg_list_indices[1]]]$loop
    PEITHO:::logWarn(
      "WARNING! Detected list argument(s) for command '%s', but 'iteration' is set to '%s'.",
      x$command,
      configured_iteration
    )
  }

  # 3) actually call the function, if needed then in a loop
  if (any(is_param_config_loop) || sample_total > 1L) {
    is_iteration_loop <- any(is_param_config_loop)

    PEITHO:::logDebug(
      "  Running command: samples=%d, %s",
      sample_total,
      if (is_iteration_loop) sprintf("WITH ITERATION over argument index %d", loop_param_indices) else "WITHOUT ITERATION"
    )

    loop_index <- if (is_iteration_loop) loop_param_indices[1] else NA_integer_
    loop_values <- if (is_iteration_loop) args[[loop_index]] else list(NULL)
    iteration_total <- length(loop_values)

    step_parts <- vector("list", sample_total * iteration_total)
    resume_sample <- if (!is.null(resume_from_sample)) as.integer(resume_from_sample) else 1L
    resume_iteration <- if (!is.null(resume_from_iteration)) as.integer(resume_from_iteration) else 1L

    for (iter_i in seq_along(loop_values)) {
      for (sample_i in seq_len(sample_total)) {
        part_idx <- (iter_i - 1L) * sample_total + sample_i

        should_skip <-
          (!is.null(resume_from_sample) || !is.null(resume_from_iteration)) &&
          (iter_i < resume_iteration || (iter_i == resume_iteration && sample_i < resume_sample))

        if (should_skip) {
          prior_record <- if (!is.null(prior_detail_records)) {
            Find(function(r) {
              as.integer(r$sample_id %||% 1L) == sample_i &&
                as.integer(r$iteration_id %||% 0L) == iter_i
            }, prior_detail_records)
          } else {
            NULL
          }

          if (!is.null(prior_record)) {
            err_str <- prior_record$error
            step_parts[[part_idx]] <- list(
              output = prior_record$result,
              error  = if (!is.null(err_str) && nzchar(err_str)) err_str else NULL
            )
          } else {
            step_parts[[part_idx]] <- list(output = NULL, error = NULL)
          }
          PEITHO:::logInfo("  Iteration %d, sample %d: skipped (loaded from prior run)", iter_i, sample_i)
          next
        }

        if (is_iteration_loop) {
          v <- loop_values[[iter_i]]
          args[[loop_index]] <- v
        }

        step_part <- run_with_error(fn, args)
        step_parts[[part_idx]] <- step_part

        if (!is.null(results_file_name) && !is.null(results_path_to_folder)) {
          detail_record <- new_iteration_result_record(
            run_id = state$run_id,
            step = detail_step_id,
            workflowstep = x,
            iteration_id = iter_i,
            iteration_total = iteration_total,
            sample_id = sample_i,
            sample_total = sample_total,
            result = step_part$output,
            error = step_part$error
          )
          upsert_results_record(
            record = detail_record,
            path_to_folder = results_path_to_folder,
            results_file = results_file_name
          )
        }
      }
    }

    results <- lapply(step_parts, `[[`, "output")
    errors  <- lapply(step_parts, `[[`, "error")

    PEITHO:::logInfo("  %d sample x iteration runs for command '%s':", length(step_parts), x$command)
    result_lengths <- lengths(results)
    max_result_length <- if (length(result_lengths)) max(result_lengths) else 0L
    if (max_result_length > 1L) {
      PEITHO:::logWarn(
        "     WARNING! Multiple results per iteration! Ensure that downstream steps handle list inputs."
      )
    } else {
      PEITHO:::logInfo("     %d single results.", length(results))
    }

    # return list of results/errors
    steprun <- new_workflowsteprun(
      step   = x,
      args   = args,
      output = results,
      error  = errors,
      run_id = state$run_id,
      is_looped = is_iteration_loop || sample_total > 1L,
      iteration_total = iteration_total,
      sample_total = sample_total,
      completed_iterations = iteration_total
    )
  } else {
    PEITHO:::logDebug("  Running command: NO LOOPING")

    run <- run_with_error(fn, args) # <--- RUN FUNCTION HERE, single run

    # check if result has length > 1 or not
    is_single_result <- length(run$output) == 1L
    PEITHO:::logInfo(
      "  Command '%s': %s result%s",
      x$command,
      if (is_single_result) "single" else length(run$output),
      if (is_single_result) "" else "s"
    )

    # return list of results/errors (to keep structure consistent)
    steprun <- new_workflowsteprun(
      step   = x,
      args   = args,
      output = list(run$output),
      error  = list(run$error),
      run_id = state$run_id,
      is_looped = FALSE,
      iteration_total = 1L
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
    param_iteration <- param$iteration %||% param$loop
    if (is_list) {
      param_iteration %in% c("yes", "auto")
    } else {
      param_iteration == "yes"
    }
  }, params, is_arg_list)
}
