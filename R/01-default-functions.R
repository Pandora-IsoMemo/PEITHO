# A simple wrapper around strsplit to return a vector instead of a list
# @param x A character vector to split.
# @param split A character string containing a regular expression to use
#              as the split point.
# @param ... Additional arguments passed to `strsplit()`.
# @return A list of character strings resulting from splitting `x` by `split`.
simple_split <- function(x, split, ...) {
  as.list(strsplit(x, split, ...)[[1]])
}

#' Fetch and parse web text from a URL
#'
#' This function retrieves the HTML content from the specified URL,
#' parses it to extract the main text content, and returns it as a character vector.
#' It also handles errors and warnings gracefully, providing informative messages if
#' the request fails or if the HTML structure is not as expected.
#' By default, it returns only the extracted text blocks, but it can also return a
#' full `WebText` object containing metadata if `return_text_blocks_only` is set to `FALSE`.
#'
#' @param url A character string specifying the URL to fetch.
#' @param timeout_sec An integer specifying the timeout for the HTTP
#'   request in seconds. Default is `10`.
#' @param user_agent An optional character string specifying the User-Agent
#'   header for the HTTP request. If `NULL`, a default User-Agent
#'   string is used.
#' @param return_text_blocks_only A logical indicating whether to return
#'   only the extracted text blocks as a character vector (`TRUE`),
#'   or a full `WebText` object with metadata (`FALSE`). Default is `TRUE`.
#' @param stop_on_error A logical indicating whether to stop execution
#'   and throw an error if the request fails or if the HTML cannot be parsed.
#'   If `FALSE`, the function will return `NULL` and log a warning instead.
#'   Default is `TRUE`.
#' @return A `WebText` object containing the extracted text and metadata.
#' @export
fetch_WebText <- function(
  url,
  timeout_sec  = 10,
  user_agent   = NULL,
  return_text_blocks_only = TRUE,
  stop_on_error = TRUE,
  max_retries = 3,
  backoff_multiplier = 2,
  initial_delay_sec = 1
) {
  # allow user to set default user agent via options()
  if (is.null(user_agent)) {
    user_agent <- getOption("peitho.user_agent", default = NULL)
  }
  # if still NULL, use package default
  if (is.null(user_agent)) user_agent <- peitho_user_agent()

  warn_msgs <- character()
  err_msgs  <- character()
  status_code <- NA_integer_
  title <- ""
  text_blocks <- character()

  # Try the request
  resp <- tryCatch(
    {
      httr2::request(url) |>
        httr2::req_user_agent(user_agent) |>
        httr2::req_timeout(timeout_sec) |>
        httr2::req_retry(
          max_tries = max_retries + 1,
          backoff = function(attempt) initial_delay_sec * (backoff_multiplier ^ (attempt - 1))
        ) |>
        httr2::req_perform()
    },
    error = function(e) {
      err_msgs <<- c(err_msgs, paste("Request error:", e$message))
      NULL
    }
  )

  if (is.null(resp)) {
    if (stop_on_error) stop(err_msgs)

    PEITHO:::logWarn("  %s: %s", url, paste(err_msgs, collapse = "\n"))
    if (return_text_blocks_only) return(stats::setNames("", url)) else return(NULL)
  }

  status_code <- httr2::resp_status(resp)

  if (status_code >= 400) {
    err_msgs <- c(err_msgs, paste("HTTP error", status_code))
    if (stop_on_error) stop(err_msgs)

    PEITHO:::logWarn("  %s: %s", url, paste(err_msgs, collapse = "\n"))
    if (return_text_blocks_only) return(stats::setNames("", url)) else return(NULL)
  }

  # Parse HTML
  html <- tryCatch(
    httr2::resp_body_html(resp),
    error = function(e) {
      err_msgs <<- c(err_msgs, paste("Failed to parse HTML:", e$message))
      NULL
    }
  )

  if (is.null(html)) {
    if (stop_on_error) stop(err_msgs)

    PEITHO:::logWarn("  %s: %s", url, paste(err_msgs, collapse = "\n"))
    if (return_text_blocks_only) return(stats::setNames("", url)) else return(NULL)
  }

  # Title
  title_node <- rvest::html_element(html, "title")
  if (!is.na(title_node) && !is.null(title_node)) {
    title <- rvest::html_text2(title_node) # gets text without html tags and squishes whitespace
  }

  # Text nodes
  main_node <- rvest::html_element(html, "main")

  if (is.null(main_node)) {
    main_node <- rvest::html_element(html, "article")
  }

  if (!is.null(main_node)) {
    text_blocks <- rvest::html_text2(main_node)
  } else {
    body_node <- rvest::html_element(html, "body")

    if (!is.null(body_node)) {
      # Remove unwanted sections (modifies body_node in place)
      body_node |>
        rvest::html_elements(
          xpath = ".//*[self::nav or self::footer or self::aside or self::script or self::style]"
        ) |>
        xml2::xml_remove()

      text_blocks <- rvest::html_text2(body_node)
    } else {
      warn_msgs <- c(
        warn_msgs,
        "No <body> element found in HTML. Unable to extract text."
      )
      PEITHO:::logWarn("  %s: %s", url, paste(warn_msgs, collapse = "\n"))
    }
  }

  if (length(text_blocks) > 0 && nzchar(text_blocks)) {
    text_blocks <- stringr::str_squish(text_blocks)
  }

  if (return_text_blocks_only) {
    out <- if (length(text_blocks) > 0 && nzchar(text_blocks[1])) text_blocks[1] else ""
    return(stats::setNames(out, url))
  }

  new_WebText(
    url         = url,
    title       = title,
    text_blocks = text_blocks,
    fetched_at  = Sys.time(),
    status_code = as.integer(status_code),
    warnings    = warn_msgs
  )
}

#' Recursively clean a list by removing NULL values
#' This function takes a list and recursively removes any NULL values from it.
#' If the input is not a list, it returns the input unchanged.
#' If the input is NULL, it returns NULL.
#'
#' @param x A list to be cleaned, or any other R object.
#' @return A cleaned list with all NULL values removed, or the original input if it is not a list.
prune_nulls <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.list(x)) {
    cleaned <- lapply(x, prune_nulls)
    cleaned[!vapply(cleaned, is.null, logical(1))]
  } else if (is.atomic(x)) {
    x
  } else {
    stop("Unsupported type in list")
  }
}

#' Generate letter combinations
#'
#' This function generates all possible combinations of lowercase letters of a specified length,
#' starting from a given string and ending at another string. The combinations are generated
#' in lexicographical order. The function validates the input parameters to ensure they are
#' appropriate for generating the desired combinations.
#'
#' @param n_letters An integer specifying the number of letters in each combination.
#'  Must be a positive integer.
#' @param start A character string specifying the starting combination.
#'  Must be of length `n_letters` and consist of lowercase letters.
#'  If `NULL`, defaults to a string of 'a's (e.g., "aaa" for `n_letters = 3`).
#' @param stop A character string specifying the ending combination.
#'  Must be of length `n_letters` and consist of lowercase letters.
#'  If `NULL`, defaults to a string of 'z's (e.g., "zzz" for `n_letters = 3`).
#' @return A character vector containing all combinations of letters from `start` to `stop`,
#'  inclusive, in lexicographical order.
generate_letter_combinations <- function(
  n_letters = 3,
  start = NULL,
  stop = NULL
) {

  # Input validation
  if (!is.numeric(n_letters) ||
      length(n_letters) != 1 ||
      is.na(n_letters) ||
      !is.finite(n_letters) ||
      n_letters < 1 ||
      n_letters %% 1 != 0) {
    stop("n_letters must be a positive integer")
  }
  n_letters <- as.integer(n_letters)

  # Define the alphabet
  letters_vec <- letters # Built-in R constant for lowercase letters

  # Set default start and stop values
  if (is.null(start)) {
    start <- paste(rep("a", n_letters), collapse = "")
  }
  if (is.null(stop)) {
    stop <- paste(rep("z", n_letters), collapse = "")
  }

  # Validate start and stop strings
  if (!is.character(start) || length(start) != 1 || is.na(start)) {
    stop("start must be a single, non-missing character string")
  }
  if (!is.character(stop) || length(stop) != 1 || is.na(stop)) {
    stop("stop must be a single, non-missing character string")
  }

  if (nchar(start, type = "chars") != n_letters) {
    stop(paste0("start must be a string of length ", n_letters))
  }
  if (nchar(stop, type = "chars") != n_letters) {
    stop(paste0("stop must be a string of length ", n_letters))
  }

  # Convert start and stop to numeric indices
  start_idx <- match(strsplit(start, "", fixed = TRUE)[[1]], letters_vec)
  stop_idx <- match(strsplit(stop, "", fixed = TRUE)[[1]], letters_vec)

  # Check for invalid characters
  if (any(is.na(start_idx))) {
    stop("start contains non-letter characters")
  }
  if (any(is.na(stop_idx))) {
    stop("stop contains non-letter characters")
  }

  # Check that start <= stop
  powers <- 26^((n_letters - 1):0)
  start_num <- sum((start_idx - 1) * powers) + 1
  stop_num <- sum((stop_idx - 1) * powers) + 1

  if (start_num > stop_num) {
    stop("start must be lexicographically less than or equal to stop")
  }

  # Generate only the requested range as base-26 numbers
  nums <- seq.int(start_num - 1L, stop_num - 1L)
  n_out <- length(nums)
  chars <- matrix("", nrow = n_out, ncol = n_letters)
  x <- nums

  for (col in n_letters:1) {
    digit <- x %% 26L
    chars[, col] <- letters_vec[digit + 1L]
    x <- x %/% 26L
  }

  result <- do.call(paste0, as.data.frame(chars, stringsAsFactors = FALSE))

  return(result)
}

#' Paste a list of strings with a prefix and suffix
#'
#' This function takes a list of strings and concatenates each string
#' with a specified prefix and suffix.
#' It returns a character vector where each element is the result of pasting the prefix,
#' the string, and the suffix together. The function also includes error handling for
#' invalid inputs and warnings for NA values in the string list.
#'
#' @param string_list A list of strings to be concatenated with the prefix and suffix.
#'  If `NULL`, the function returns an empty character vector.
#' @param prefix A single character string to be prefixed to each element of `string_list`.
#'  Must be a single character string.
#' @param suffix A single character string to be suffixed to each element of `string_list`.
#'  Must be a single character string.
#' @return A character vector where each element is the result of pasting the prefix,
#'  the string from `string_list`, and the suffix together.
#'  If `string_list` is `NULL`, returns an empty character vector.
#'  If `string_list` contains NA values, they will be included as "NA" in the output,
#'  and a warning will be issued.
#' @export
paste_prompt_list <- function(string_list, prefix, suffix) {
  if (is.null(string_list)) return(character(0))

  if (is.null(prefix) || !is.character(prefix) || length(prefix) != 1) {
    stop("prefix must be a single character string")
  }
  if (is.null(suffix) || !is.character(suffix) || length(suffix) != 1) {
    stop("suffix must be a single character string")
  }

  if (anyNA(string_list)) {
    warning("string_list contains NA values; they will appear as \"NA\" in the output")
  }
  string_list <- as.character(string_list)

  # Escape any literal '%' in prefix/suffix so sprintf does not misinterpret them
  safe_prefix <- gsub("%", "%%", prefix, fixed = TRUE)
  safe_suffix <- gsub("%", "%%", suffix, fixed = TRUE)
  full_string <- paste0(safe_prefix, "%s", safe_suffix)

  unname(vapply(
    string_list,
    sprintf,
    character(1),
    fmt = full_string
  ))
}

export_fetched_text <- function(fetched_text, output_dir = "wikipedia_results") {
  dir.create(output_dir, showWarnings = FALSE)

  # Export successful contents to individual text files
  successful <- Filter(function(x) x != "", fetched_text)

  for (fetched_success in successful) {
    # Create a safe filename from URL
    safe_filename <- names(fetched_success) |>
      stringr::str_remove_all("https?://") |>
      stringr::str_remove_all("www\\.") |>
      stringr::str_replace_all("[^a-zA-Z0-9]", "_") |>
      stringr::str_squish() |>
      substr(1, 100)

    file_path <- file.path(output_dir, paste0(safe_filename, ".txt"))
    writeLines(fetched_success, file_path)
  }

  # Export summary CSV
  summary_df <- do.call(rbind, lapply(seq_along(fetched_text), function(i) {
    x <- fetched_text[[i]]
    data.frame(
      url = names(fetched_text[[i]]),
      success = (x != ""),
      char_count = nchar(x),
      word_count = length(strsplit(x, "\\s+")[[1]]),
      stringsAsFactors = FALSE
    )
  }))

  utils::write.csv(summary_df, file.path(output_dir, "fetch_summary.csv"), row.names = FALSE)

  cat(sprintf("\nResults exported to %s/\n", output_dir))
  return(invisible(fetched_text))
}