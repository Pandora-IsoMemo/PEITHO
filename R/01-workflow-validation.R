# -------------------------------------------------------------------------
# Validation helpers for workflow construction
# -------------------------------------------------------------------------
# These helpers centralize validations that were previously inline in new_workflow().
# Keeping them here makes new_workflow() easier to read, and ensures consistent checks
# for other constructors/helpers that may create/modify workflows later.

validate_workflow_file_paths <- function(workflow_file_paths) {
  # Minimal structural validation; actual file existence is checked in workflow_steps_from_files()
  if (!is.list(workflow_file_paths)) {
    stop("'workflow_file_paths' must be a list.", call. = FALSE)
  }
  if (length(workflow_file_paths) == 0L) {
    return(invisible(TRUE))
  }
  if (is.null(workflow_file_paths$path_to_folder) || !nzchar(workflow_file_paths$path_to_folder)) {
    stop("'workflow_file_paths$path_to_folder' must be a non-empty string.", call. = FALSE)
  }

  if (!dir.exists(workflow_file_paths$path_to_folder)) {
    stop("Argument 'path_to_folder' does not exist.", call. = FALSE)
  }
  # add check if functions script is not empty here?
  invisible(TRUE)
}

validate_steps_class <- function(steps, error_on_warn = TRUE) {
  if (!is.logical(error_on_warn) || length(error_on_warn) != 1L) {
    stop("'error_on_warn' must be a single logical value.", call. = FALSE)
  }
  if (!is.list(steps)) {
    msg <- "'steps' must be a list."
    if (error_on_warn) {
      stop(msg, call. = FALSE)
    }
    warning(msg, immediate. = TRUE, call. = FALSE)
    return(invisible(TRUE))
  }
  if (length(steps) == 0L) return(invisible(TRUE))

  ok <- vapply(steps, inherits, logical(1), what = "workflowstep")
  if (!all(ok)) {
    msg <- "All elements of 'steps' must be of class 'workflowstep'."
    if (error_on_warn) {
      stop(msg, call. = FALSE)
    }
    warning(msg, immediate. = TRUE, call. = FALSE)
  }
  invisible(TRUE)
}

validate_unique_steps <- function(
  steps,
  id_fields = c("entry", "name"),
  error_on_warn = TRUE
) {
  if (!length(steps)) return(invisible(TRUE))
  if (!is.logical(error_on_warn) || length(error_on_warn) != 1L) {
    stop("'error_on_warn' must be a single logical value.", call. = FALSE)
  }

  for (field in id_fields) {
    vals <- vapply(steps, function(s) as.character(s[[field]]), character(1))
    if (anyDuplicated(vals)) {
      dup <- unique(vals[duplicated(vals)])
      msg <- sprintf(
        "Workflow has duplicate step %s(s): %s",
        field,
        paste(dup, collapse = ", ")
      )
      if (error_on_warn) {
        stop(msg, call. = FALSE)
      }
      warning(msg, immediate. = TRUE, call. = FALSE)
    }
  }
  invisible(TRUE)
}

validate_numeric_entries <- function(steps, error_on_warn = TRUE) {
  if (!length(steps)) return(invisible(TRUE))
  if (!is.logical(error_on_warn) || length(error_on_warn) != 1L) {
    stop("'error_on_warn' must be a single logical value.", call. = FALSE)
  }
  entries <- vapply(steps, function(s) as.integer(s$entry), integer(1))
  if (any(is.na(entries))) {
    msg <- "All steps must have a numeric 'entry' field."
    if (error_on_warn) {
      stop(msg, call. = FALSE)
    }
    warning(msg, immediate. = TRUE, call. = FALSE)
  }
  invisible(TRUE)
}

validate_required_inputs <- function(steps, input_list, error_on_warn = TRUE) {
  if (!length(steps)) return(invisible(TRUE))
  if (!is.logical(error_on_warn) || length(error_on_warn) != 1L) {
    stop("'error_on_warn' must be a single logical value.", call. = FALSE)
  }
  # extract required_fields and check if exist
  required_inputs <- unique(unlist(lapply(steps, function(s) {
    x <- s[["required_inputs"]]
    if (!is.character(x)) {
      msg <- sprintf("'required_inputs' in step '%s' must be a character vector.", s$name)
      if (error_on_warn) {
        stop(msg, call. = FALSE)
      }
      warning(msg, immediate. = TRUE, call. = FALSE)
    }
    x
  }), use.names = FALSE))

  # check if required inputs exist in input_list
  missing_inputs <- setdiff(required_inputs, names(input_list))
  if (length(missing_inputs)) {
    msg <- sprintf("Missing required inputs: %s", paste(missing_inputs, collapse = ", "))
    if (error_on_warn) {
      stop(msg, call. = FALSE)
    }
    warning(msg, immediate. = TRUE, call. = FALSE)
  }
}

validate_required_steps <- function(steps, error_on_warn = TRUE) {
  if (!length(steps)) return(invisible(TRUE))
  if (!is.logical(error_on_warn) || length(error_on_warn) != 1L) {
    stop("'error_on_warn' must be a single logical value.", call. = FALSE)
  }
  required_steps <- unique(unlist(lapply(steps, function(s) {
    x <- s[["required_steps"]]
    if (!is.character(x)) {
      msg <- sprintf("'required_steps' in step '%s' must be a character vector.", s$name)
      if (error_on_warn) {
        stop(msg, call. = FALSE)
      }
      warning(msg, immediate. = TRUE, call. = FALSE)
    }
    x
  }), use.names = FALSE))

  # check if required steps exist in steps
  step_names <- vapply(steps, function(s) s$name, character(1))
  missing_steps <- setdiff(required_steps, step_names)
  if (length(missing_steps)) {
    msg <- sprintf("Missing required steps: %s", paste(missing_steps, collapse = ", "))
    if (error_on_warn) {
      stop(msg, call. = FALSE)
    }
    warning(msg, immediate. = TRUE, call. = FALSE)
  }
}

validate_current_index <- function(current, n_steps) {
  if (is.null(current)) {
    return(if (n_steps) 1L else NA_integer_)
  }
  if (n_steps == 0L) return(NA_integer_)

  current <- as.integer(current)
  if (is.na(current)) return(NA_integer_)

  # clamp to [1, n_steps]
  max(1L, min(current, n_steps))
}

validate_workflow <- function(x, error_on_warn = TRUE) {
  validate_unique_steps(x$steps, error_on_warn = error_on_warn)
  validate_numeric_entries(x$steps, error_on_warn = error_on_warn)
  validate_required_inputs(x$steps, x$input_list, error_on_warn = error_on_warn)
  validate_required_steps(x$steps, error_on_warn = error_on_warn)
}

get_warn_on_validation <- function(dots, default = FALSE) {
  if (!is.list(dots)) {
    stop("'dots' must be a list.", call. = FALSE)
  }
  if (!is.logical(default) || length(default) != 1L) {
    stop("'default' must be a single logical value.", call. = FALSE)
  }

  if ("warn_on_validation" %in% names(dots)) {
    return(isTRUE(dots$warn_on_validation))
  }
  default
}

validate_workflow_with_policy <- function(x, warn_on_validation = FALSE) {
  if (!is.logical(warn_on_validation) || length(warn_on_validation) != 1L) {
    stop("'warn_on_validation' must be a single logical value.", call. = FALSE)
  }

  if (warn_on_validation) {
    validate_workflow(x, error_on_warn = FALSE)
  } else {
    suppressWarnings(validate_workflow(x, error_on_warn = FALSE))
  }
  invisible(TRUE)
}
