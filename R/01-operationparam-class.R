#' Create a new operation parameter object
#'
#' A operation parameter represents a single argument for a step operation.
#' It stores its position in the argument list, a value, and a type.
#'
#' @param step_id  The ID of the step this parameter belongs to.
#' @param position Integer position of this parameter in the argument list.
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
  cat("  value:    ", if (is.null(x$value)) "NULL" else "…", "\n", sep = "")
  cat("  label:    ", x$label, "\n", sep = "")
  cat("  type:     ", x$type, "\n", sep = "")
  cat("  loop:     ", x$loop, "\n", sep = "")
  invisible(x)
}

#' Extract argument value from operationparam object
#' @param x A `operationparam` object.
#' @param result_path Path to the results JSON file.
#' @param ... Additional arguments (not used).
#' @return A list containing the argument value, named if applicable.
#' @export
extract_arg.operationparam <- function(x, result_path, ...) {
  if (x$type %in% c("input", "literal")) {
    if (x$name != "") {
      return(setNames(list(x$value), x$name))
    } else {
      return(list(x$value))
    }
  }

  if (x$type == "result") {
    # get result from JSON file
    # load result json file

    if (file.exists(result_path)) {
      result_list <- jsonlite::fromJSON(result_path, simplifyVector = FALSE)
    } else {
      result_list <- list()
    }
    
    last_step_id <- as.integer(x$step_id) - 1L
    if (!any(last_step_id %in% sapply(result_list, function(res) res$entry))) {
      stop(
        "Result for previous step '", last_step_id, "' not found in results JSON.",
        call. = FALSE
      )
    }

    res_indx <- which(sapply(result_list, function(res) res$entry) == last_step_id)

    # get results value <- SHOULD THIS BE DONE LATER?
    if (result_list[[res_indx]][["errors"]] != "") {
      stop(
        "Stopping workflow because of error during step '", x$step_id, "': ",
        result_list[[res_indx]][["errors"]]
      )
    }

    last_result <- result_list[[res_indx]][["result"]]
    # check if result is character or list of characters
    if (
      !is.character(last_result) &&
        (is.list(last_result) && !all(sapply(last_result, is.character)))
    ) {
      stop(
        "Stopping workflow because result of step '", x$step_id, "' was not character or list of characters."
      )
    }

    arg_value <- last_result

    if (x$name != "") {
      return(setNames(list(arg_value), x$name))
    } else {
      return(list(arg_value))
    }
  }

  stop("Unknown operationparam type '", x$type, "'.", call. = FALSE)
}
