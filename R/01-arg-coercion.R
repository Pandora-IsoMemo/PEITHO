supported_arg_types <- function() {
  c("character", "numeric", "integer", "logical")
}

normalize_arg_type <- function(type) {
  if (is.null(type) || length(type) != 1L || is.na(type) || !is.character(type)) {
    stop("Argument 'type' must be a single character string.", call. = FALSE)
  }

  normalized <- tolower(trimws(type))
  if (!normalized %in% supported_arg_types()) {
    stop(
      sprintf(
        "Unsupported argument type '%s'. Supported types are: %s.",
        type,
        paste(supported_arg_types(), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  normalized
}

coerce_arg <- function(x, type, arg) {
  type <- normalize_arg_type(type)

  if (type == "character") {
    return(as.character(x))
  }

  if (type == "numeric") {
    out <- suppressWarnings(as.numeric(x))
    invalid <- is.na(out) & !is.na(x)
    if (any(invalid)) {
      stop(sprintf("Argument '%s' must be numeric.", arg), call. = FALSE)
    }
    return(out)
  }

  if (type == "integer") {
    out_num <- suppressWarnings(as.numeric(x))
    out_int <- suppressWarnings(as.integer(x))

    invalid_non_numeric <- is.na(out_num) & !is.na(x)
    invalid_overflow <- !is.na(out_num) & is.na(out_int)
    invalid_fractional <- !invalid_non_numeric & !invalid_overflow & !is.na(out_num) & !is.na(out_int) & (out_num != out_int)

    if (any(invalid_non_numeric | invalid_overflow | invalid_fractional, na.rm = TRUE)) {
      stop(sprintf("Argument '%s' must be integer.", arg), call. = FALSE)
    }

    return(out_int)
  }

  if (type == "logical") {
    if (is.logical(x)) {
      return(x)
    }

    lx <- tolower(trimws(as.character(x)))
    allowed <- c("true", "false", "t", "f", "1", "0")

    if (any(!lx %in% allowed)) {
      stop(sprintf("Argument '%s' must be logical.", arg), call. = FALSE)
    }

    return(lx %in% c("true", "t", "1"))
  }

  stop(sprintf("Unsupported type '%s' for argument '%s'.", type, arg), call. = FALSE)
}

normalize_arg_types <- function(arg_types, step_label = NULL, args_string = NULL) {
  if (is.null(arg_types) || length(arg_types) == 0L) {
    return(stats::setNames(character(0), character(0)))
  }

  if (is.list(arg_types)) {
    if (is.null(names(arg_types))) {
      stop("'arg_types' must be a named list or named vector.", call. = FALSE)
    }

    values <- vapply(arg_types, function(x) {
      if (is.null(x) || length(x) != 1L || is.na(x)) {
        stop("'arg_types' values must be non-missing scalar strings.", call. = FALSE)
      }
      as.character(x)
    }, character(1))
  } else if (is.atomic(arg_types)) {
    values <- as.character(arg_types)
    if (is.null(names(values))) {
      stop("'arg_types' must be a named list or named vector.", call. = FALSE)
    }
  } else {
    stop("'arg_types' must be a named list or named vector.", call. = FALSE)
  }

  if (any(!nzchar(names(values)))) {
    stop("'arg_types' names must be non-empty argument names.", call. = FALSE)
  }

  if (anyDuplicated(names(values))) {
    stop("'arg_types' contains duplicated argument names.", call. = FALSE)
  }

  normalized_values <- vapply(values, normalize_arg_type, character(1))

  if (!is.null(args_string) && nzchar(args_string)) {
    parsed <- parse_args(args_string)
    named_args <- unique(parsed$names[nzchar(parsed$names)])
    unknown_keys <- setdiff(names(normalized_values), named_args)

    if (length(unknown_keys) > 0L) {
      where <- if (!is.null(step_label) && nzchar(step_label)) {
        sprintf(" in step '%s'", step_label)
      } else {
        ""
      }
      warning(
        sprintf(
          "Unused 'arg_types' keys%s: %s.",
          where,
          paste(unknown_keys, collapse = ", ")
        ),
        immediate. = TRUE,
        call. = FALSE
      )
    }
  }

  normalized_values
}
