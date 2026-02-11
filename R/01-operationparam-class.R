#' Create a new operation parameter object
#'
#' An operation parameter represents a single argument for a step operation.
#' It stores its position in the argument list, a value, and a type.
#'
#' @param step_id  The ID of the step this parameter belongs to.
#' @param position Integer position of this parameter in the argument list.
#' @param name     The name of the parameter (default: `NULL` for unnamed).
#' @param value    The stored value for this parameter (default: "").
#' @param label    A human-readable label for this parameter (default: "").
#' @param type     Type of the parameter. One of:
#'   - "input"  : value comes from user input or external input
#'   - "result" : value refers to a previous step's result
#'   - "literal": value is used as-is (a literal argument)
#' @param loop     Looping behavior for this parameter. One of:
#'   - "no"   : do not loop
#'   - "yes"  : always loop
#'   - "auto" : automatically determine looping, loop if input is a list else not
#' @param ...      Additional metadata (optional).
#'
#' @return A `operationparam` object.
#' @export
new_operationparam <- function(
  step_id,
  position,
  name = NULL,
  value = "",
  label = "",
  type = c("literal", "input", "result"),
  loop = c("no", "yes", "auto"),
  ...
) {
  type <- match.arg(type)
  loop <- match.arg(loop)

  tags <- c(literal = "", input = "@#*I*#@", result = "@#*L*#@")
  tag <- tags[type]

  structure(
    list(
      step_id = step_id,
      position = as.integer(position),
      name     = name,
      value    = value,
      type     = type,
      tag      = tag,
      label    = label,
      loop     = loop,
      dots     = list(...)
    ),
    class = c("operationparam", "list")
  )
}

#' Print method for operationparam objects
#'
#' @param x A `operationparam` object.
#' @param ... Additional arguments (not used).
#' @export
print.operationparam <- function(x, ...) {
  cat("<operationparam>\n")
  cat("  step_id:  ", x$step_id, "\n", sep = "")
  cat("  position: ", x$position, "\n", sep = "")
  cat("  name:     ", x$name, "\n", sep = "")
  cat("  value:    ", if (is.null(x$value)) "NULL" else "...", "\n", sep = "")
  cat("  label:    ", x$label, "\n", sep = "")
  cat("  type:     ", x$type, "\n", sep = "")
  cat("  loop:     ", x$loop, "\n", sep = "")
  invisible(x)
}

extract_arg_list <- function(
  operationparam,
  state,
  ...
) {
  if (!inherits(state, "workflowstate")) {
    stop("'state' must be a 'workflowstate' object.", call. = FALSE)
  }

  if (operationparam$type %in% c("input", "literal")) {
    if (operationparam$name != "") {
      return(stats::setNames(list(operationparam$value), operationparam$name))
    } else {
      return(list(operationparam$value))
    }
  }

  if (operationparam$type == "result") {
    ref <- operationparam$label

    if (is.null(ref) || !nzchar(ref)) {
      stop(
        "Invalid result label: missing 'label' for step_id=",
        operationparam$step_id, ", position=", operationparam$position,
        call. = FALSE
      )
    }

    # Try name lookup first (current JSON convention), then id-as-string
    if (!is.null(state$results_by_name[[ref]])) {
      arg_value <- state$results_by_name[[ref]]
    } else if (!is.null(state$results_by_id[[ref]])) {
      arg_value <- state$results_by_id[[ref]]
    } else {
      stop(
        "Result '", ref, "' not found in workflow state. ",
        "Available results_by_name: [", paste(names(state$results_by_name), collapse = ", "), "].",
        call. = FALSE
      )
    }

    # converting arg_value (previous result) into a list (to convert character vectors)
    arg_value <- as.list(arg_value)

    # check if result is character or list of characters
    if (!(
      is.character(arg_value) ||
        (is.list(arg_value) && all(sapply(arg_value, is.character)))
    )) {
      stop(
        "Stopping workflow because result of step '", operationparam$step_id,
        "' was not character or list of characters."
      )
    }

    # unwrap single-element lists
    if (is.list(arg_value) && length(arg_value) == 1L) {
      arg_value <- arg_value[[1L]]
    }

    if (!is.null(operationparam$name) && nzchar(operationparam$name)) {
      return(stats::setNames(list(arg_value), operationparam$name))
    } else {
      return(list(arg_value))
    }
  }

  stop("Unknown operationparam type '", operationparam$type, "'.", call. = FALSE)
}

