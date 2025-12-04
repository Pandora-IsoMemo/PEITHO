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
  cat("  position: ", x$position, "\n", sep = "")
  cat("  name:     ", x$name, "\n", sep = "")
  cat("  type:     ", x$type, "\n", sep = "")
  cat("  loop:     ", x$loop, "\n", sep = "")
  invisible(x)
}

#' Extract argument value from operationparam object
#' @param x A `operationparam` object.
#' @param path_to_folder Path to folder containing results.json (default: package's peitho_files).
#' @param result_file Name of the results file (default: "results.json").
#' @param ... Additional arguments (not used).
#' @return A list containing the argument value, named if applicable.
#' @export
extract_arg.operationparam <- function(
  x,
  path_to_folder = system.file("scripts", "peitho_files", package = "PEITHO"),
  result_file = "results.json",
  ...
) {
  if (x$type %in% c("input", "literal")) {
    if (x$name != "") {
      return(setNames(list(x$value), x$name))
    } else {
      return(list(x$value))
    }
  }

  if (x$type == "result") {
    # get result from results.json
    # load result json file
    result_path <- file.path(path_to_folder, result_file)
    if (file.exists(result_path)) {
      result_list <- jsonlite::fromJSON(result_path, simplifyVector = FALSE)
    } else {
      result_list <- list()
    }

    if (!any(x$step_id %in% sapply(result_list, function(res) res$entry))) {
      stop(
        "Result for step '", x$step_id, "' not found in results.json.",
        call. = FALSE
      )
    }

    res_indx <- which(result_list$entry == x$step_id)

    # get results value <- SHOULD THIS BE DONE LATER?

    if (result_list[res_indx][["errors"]] != "") {
      stop(
        "Stopping workflow because of error during step '", x$step_id, "': ",
        result_list[res_indx][["errors"]]
      )
    }

    if (!is.character(result_list[res_indx][["result"]])) {
      stop(
        "Stopping workflow because result of step '", x$step_id, "' was not character."
      )
    }

    arg_value <- result_list[res_indx][["result"]]

    if (x$name != "") {
      return(setNames(list(arg_value), x$name))
    } else {
      return(list(arg_value))
    }
  }



}
