#' Null coalescing operator
#' Returns left value if not NULL, otherwise right value.
#'
#' @name null-coalescing-operator
#' @rdname null-coalescing-operator
#'
#' @param a Left-hand side value.
#' @param b Right-hand side value.
#' @return Left value if not NULL, otherwise right value.
#' @export
`%||%` <- function(a, b) if (!is.null(a)) a else b