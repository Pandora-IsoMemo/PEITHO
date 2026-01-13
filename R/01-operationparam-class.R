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
  type = c("input", "result", "literal"),
  loop = c("no", "yes", "auto"),
  ...
) {
  type <- match.arg(type)
  loop <- match.arg(loop)

  structure(
    list(
      step_id = step_id,
      position = as.integer(position),
      name     = name,
      value    = value,
      label    = label,
      type     = type,
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
  last_result = list(""),
  ...
) {
  if (operationparam$type %in% c("input", "literal")) {
    if (operationparam$name != "") {
      return(stats::setNames(list(operationparam$value), operationparam$name))
    } else {
      return(list(operationparam$value))
    }
  }

  if (operationparam$type == "result") {
    # check if result is character or list of characters
    if (
      !is.character(last_result) &&
        (is.list(last_result) && !all(sapply(last_result, is.character)))
    ) {
      stop(
        "Stopping workflow because result of step '", operationparam$step_id,
        "' was not character or list of characters."
      )
    }

    arg_value <- last_result

    if (operationparam$name != "") {
      return(stats::setNames(list(arg_value), operationparam$name))
    } else {
      return(list(arg_value))
    }
  }

  stop("Unknown operationparam type '", operationparam$type, "'.", call. = FALSE)
}

