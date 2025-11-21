#' WebText Class
#' Represents text content fetched from a web page.
#' @param url Character string. The URL of the web page.
#' @param title Character string. The title of the web page.
#' @param text_blocks A character vector. Each element represents a block of text extracted
#'  from the page.
#' @param fetched_at POSIXct. The timestamp when the content was fetched.
#' @param status_code Integer. The HTTP status code of the request.
#' @param warnings Character vector. Any warnings encountered during fetching or parsing.
#' @param errors Character vector. Any errors encountered during fetching or parsing.
#' @return An object of class `WebText`.
#' @export
new_WebText <- function(
  url,
  title,
  text_blocks,
  fetched_at  = Sys.time(),
  status_code = NA_integer_,
  warnings    = character(),
  errors      = character()
) {
  x <- list(
    url        = url,
    title      = title,
    text       = text_blocks,
    fetched_at = fetched_at,
    status_code = status_code,
    warnings   = warnings,
    errors     = errors
  )

  class(x) <- "WebText"
  validate_WebText(x)
}

validate_WebText <- function(x) {
  stopifnot(inherits(x, "WebText"))
  stopifnot(is.character(x$url), length(x$url) == 1)
  stopifnot(is.character(x$title), length(x$title) == 1)
  stopifnot(is.character(x$text))
  stopifnot(inherits(x$fetched_at, "POSIXct"), length(x$fetched_at) == 1)
  stopifnot(is.integer(x$status_code), length(x$status_code) == 1)
  stopifnot(is.character(x$warnings))
  stopifnot(is.character(x$errors))
  invisible(x)
}

is_WebText <- function(x) {
  inherits(x, "WebText")
}

#' @export
print.WebText <- function(x, ...) {
  cat("<WebText>\n")
  cat(" URL: ", x$url, "\n", sep = "")
  cat(" Title: ", x$title, "\n", sep = "")
  cat(" Fetched at: ", format(x$fetched_at), "\n", sep = "")
  cat(" Status code: ", x$status_code, "\n", sep = "")
  cat(" Number of text blocks: ", length(x$text), "\n", sep = "")
  if (length(x$warnings) > 0) {
    cat(" Warnings: ", length(x$warnings), "\n", sep = "")
    cat(" (first warning): ", x$warnings[1], "\n", sep = "")
  }
  if (length(x$errors) > 0) {
    cat(" Errors: ", length(x$errors), "\n", sep = "")
    cat(" (first error): ", x$errors[1], "\n", sep = "")
  }

  cat("\nPreview:\n")
  n_preview <- min(3, length(x$text))
  if (n_preview > 0) {
    for (i in seq_len(n_preview)) {
      block <- x$text[i]
      block_single_line <- paste(block, collapse = " ")
      cat(
        " Block ", i, ": ",
        substr(block_single_line, 1, 100),
        if (nchar(block_single_line) > 100) "..." else "", "\n",
        sep = ""
      )
    }
  } else {
    cat(" No text blocks available.\n")
  }
  invisible(x)
}

WebText_to_tibble <- function(x) {
  stopifnot(inherits(x, "WebText"))
  tibble::tibble(
    url   = x$url,
    title = x$title,
    index = seq_along(x$text),
    text  = x$text
  )
}
