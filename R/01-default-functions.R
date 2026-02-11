# A simple wrapper around strsplit to return a vector instead of a list
# @param x A character vector to split.
# @param split A character string containing a regular expression to use
#              as the split point.
# @param ... Additional arguments passed to `strsplit()`.
# @return A character vector with the split elements.
simple_split <- function(x, split, ...) {
  strsplit(x, split, ...)[[1]]
}