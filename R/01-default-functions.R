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
#' @return A `WebText` object containing the extracted text and metadata.
#' @export
fetch_WebText <- function(
  url,
  timeout_sec  = 10,
  user_agent   = NULL,
  return_text_blocks_only = TRUE
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
        httr2::req_perform()
    },
    error = function(e) {
      err_msgs <<- c(err_msgs, paste("Request error:", e$message))
      NULL
    }
  )

  if (is.null(resp)) stop(err_msgs)

  status_code <- httr2::resp_status(resp)

  if (status_code >= 400) {
    err_msgs <- c(err_msgs, paste("HTTP error", status_code))
    stop(err_msgs)
  }

  # Parse HTML
  html <- tryCatch(
    httr2::resp_body_html(resp),
    error = function(e) {
      err_msgs <<- c(err_msgs, paste("Failed to parse HTML:", e$message))
      NULL
    }
  )

  if (is.null(html)) stop(err_msgs)

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
      PEITHO:::logWarn(paste(warn_msgs, collapse = "\n"))
    }
  }

  if (length(text_blocks) > 0 && nzchar(text_blocks)) {
    text_blocks <- stringr::str_squish(text_blocks)
  }

  if (return_text_blocks_only) return(text_blocks)

  new_WebText(
    url         = url,
    title       = title,
    text_blocks = text_blocks,
    fetched_at  = Sys.time(),
    status_code = as.integer(status_code),
    warnings    = warn_msgs
  )
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
  if (n_letters < 1 || n_letters %% 1 != 0) {
    stop("n_letters must be a positive integer")
  }

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
  if (nchar(start) != n_letters) {
    stop(paste0("start must be a string of length ", n_letters))
  }
  if (nchar(stop) != n_letters) {
    stop(paste0("stop must be a string of length ", n_letters))
  }

  # Convert start and stop to numeric indices
  start_idx <- match(strsplit(start, "")[[1]], letters_vec)
  stop_idx <- match(strsplit(stop, "")[[1]], letters_vec)

  # Check for invalid characters
  if (any(is.na(start_idx))) {
    stop("start contains non-letter characters")
  }
  if (any(is.na(stop_idx))) {
    stop("stop contains non-letter characters")
  }

  # Check that start <= stop
  start_num <- sum((start_idx - 1) * 26^((n_letters-1):0)) + 1
  stop_num <- sum((stop_idx - 1) * 26^((n_letters-1):0)) + 1

  if (start_num > stop_num) {
    stop("start must be lexicographically less than or equal to stop")
  }

  # Generate all combinations
  all_combinations <- expand.grid(rep(list(letters_vec), n_letters))
  all_combinations <- all_combinations[, n_letters:1] # Reverse to get correct order
  combination_strings <- apply(all_combinations, 1, paste, collapse = "")

  # Filter to the desired range
  result <- combination_strings[start_num:stop_num]

  return(result)
}

paste_prompt_list <- function(string_list, prefix, suffix) {
  full_string <- paste0(prefix, "%s", suffix)

  vapply(
    string_list,
    sprintf,
    character(1),
    fmt = full_string
  )
}