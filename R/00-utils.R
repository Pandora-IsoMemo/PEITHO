#' Null coalescing operator
#' Returns left value if not NULL, otherwise right value.
#'
#' @name null-coalescing-operator
#' @rdname null-coalescing-operator
#'
#' @param a Left-hand side value.
#' @param b Right-hand side value.
#' @return Left value if not NULL or length zero, otherwise right value.
#' @export
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0L) a else b

is_running_online <- function() {
  base::as.logical(base::Sys.getenv("IS_SHINYPROXY") != "", unset = "FALSE")
}

normalize_dt_edit_col_idx <- function(col_idx_raw, n_cols) {
  if (is.null(col_idx_raw) || length(col_idx_raw) != 1L || is.na(col_idx_raw)) {
    return(NA_integer_)
  }

  # DT edit column index can shift with table config (e.g. rownames on/off),
  # so support both 0-based and 1-based indices.
  if (col_idx_raw >= 0 && col_idx_raw < n_cols) {
    return(as.integer(col_idx_raw + 1L))
  }
  if (col_idx_raw >= 1 && col_idx_raw <= n_cols) {
    return(as.integer(col_idx_raw))
  }

  NA_integer_
}