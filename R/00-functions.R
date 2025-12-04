#' Split a single character by a pattern
#' 
#' A simple wrapper around `strsplit` to split a single character string by a specified pattern.
#' @param x A character string to be split.
#' @param pattern A character string containing a regular expression to use for splitting.
#' @param ... Additional arguments passed to `strsplit`.
#' @return A character vector resulting from the split.
#' @export
simple_split <- function(x, pattern, ...) {
  strsplit(x, pattern, ...)[[1]]
}