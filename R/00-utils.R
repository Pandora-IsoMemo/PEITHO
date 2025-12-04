#' Null coalescing operator
#' Returns left value if not NULL, otherwise right value.
#' @export
`%||%` <- function(a, b) if (!is.null(a)) a else b