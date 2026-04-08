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

peitho_user_agent <- function() {
  ver <- utils::packageVersion("PEITHO")
  sprintf(
    "PEITHO-WebTextFetcher/%s (source: https://github.com/Pandora-IsoMemo/PEITHO)",
    ver
  )
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
  existing_names <- existing_names %||% character(0)
  if (!(base_name %in% existing_names)) return(base_name)

  i <- 2L
  candidate <- paste0(base_name, "_", i)
  while (candidate %in% existing_names) {
    i <- i + 1L
    candidate <- paste0(base_name, "_", i)
  }
  candidate
}

# -----------------------------------------------------------------------------
# Display/meta helpers for default workflow functions (used by the app UI)
#
# These helpers define WHICH package default functions are shown in the workflow
# files module and HOW they are rendered as source text in the read-only editor.
#
# Single source of truth note:
# - `default_workflow_function_names()` controls the visible default functions.
# - `default_workflow_functions_text()` renders function bodies from the package
#   namespace, so no duplicate source file is required for display.
# -----------------------------------------------------------------------------

default_workflow_function_names <- function() {
  c("fetch_WebText", "simple_split")
}

default_workflow_functions_text <- function() {
  ns <- asNamespace("PEITHO")
  fn_names <- default_workflow_function_names()

  chunks <- vapply(fn_names, function(fn_name) {
    if (!exists(fn_name, mode = "function", envir = ns, inherits = FALSE)) {
      return(sprintf("# Missing default workflow function in namespace: %s", fn_name))
    }

    fn <- get(fn_name, mode = "function", envir = ns, inherits = FALSE)
    fn_text <- paste(deparse(fn), collapse = "\n")
    sprintf("%s <- %s", fn_name, fn_text)
  }, character(1))

  paste(chunks, collapse = "\n\n")
}

# Build a data frame of all functions available on the current search path,
# grouped by source package, for display in the function browser DT.
available_functions_df <- function(extra_env = NULL) {
  # Packages to include explicitly (curated subset to avoid noise)
  pkgs <- c("PEITHO", "base", "stats", "utils", "methods")

  rows <- lapply(pkgs, function(pkg) {
    env <- tryCatch(
      if (pkg == "PEITHO") asNamespace("PEITHO") else as.environment(paste0("package:", pkg)),
      error = function(e) NULL
    )
    if (is.null(env)) return(NULL)

    fns <- ls(envir = env)
    fns <- fns[vapply(fns, function(f) {
      tryCatch(is.function(get(f, envir = env, inherits = FALSE)), error = function(e) FALSE)
    }, logical(1))]

    if (length(fns) == 0L) return(NULL)
    data.frame(Function = fns, Source = pkg, stringsAsFactors = FALSE)
  })

  # Add functions from a custom environment (e.g. loaded functions.R)
  if (!is.null(extra_env) && is.environment(extra_env)) {
    fns <- ls(envir = extra_env)
    fns <- fns[vapply(fns, function(f) {
      tryCatch(is.function(get(f, envir = extra_env, inherits = FALSE)), error = function(e) FALSE)
    }, logical(1))]
    if (length(fns) > 0L) {
      rows <- c(
        rows,
        list(data.frame(Function = fns, Source = "custom (functions.R)", stringsAsFactors = FALSE))
      )
    }
  }

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(data.frame(Function = character(0), Source = character(0)))

  result <- do.call(rbind, rows)
  result[order(result$Source, result$Function), ]
}