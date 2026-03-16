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

# Build labeled row selector choices while keeping values as row indices.
build_row_selector_choices <- function(df, preferred_name_cols = c("Name", "name")) {
  n <- nrow(df)
  if (n == 0L) return(character(0))

  row_values <- as.character(seq_len(n))
  name_col <- preferred_name_cols[preferred_name_cols %in% names(df)][1]
  if (length(name_col) == 0L || is.na(name_col)) {
    return(stats::setNames(row_values, row_values))
  }

  row_names <- as.character(df[[name_col]])
  row_names[is.na(row_names)] <- ""
  row_labels <- ifelse(
    nzchar(row_names),
    paste0(row_values, " - ", row_names),
    row_values
  )

  stats::setNames(row_values, row_labels)
}

# Keep existing selection when valid; otherwise select the last row by default.
resolve_selected_row <- function(selected, n_rows, default_last = TRUE) {
  if (n_rows <= 0L) return(character(0))

  valid_values <- as.character(seq_len(n_rows))
  if (!is.null(selected) && nzchar(selected) && selected %in% valid_values) {
    return(selected)
  }

  if (default_last) as.character(n_rows) else "1"
}

# Generate a unique input name for a named list.
next_unique_input_name <- function(existing_names, base_name = "new_input") {
  if (!(base_name %in% existing_names)) return(base_name)

  i <- 2L
  candidate <- paste0(base_name, "_", i)
  while (candidate %in% existing_names) {
    i <- i + 1L
    candidate <- paste0(base_name, "_", i)
  }
  candidate
}