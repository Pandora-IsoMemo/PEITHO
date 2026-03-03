peitho_user_agent <- function() {
  ver <- utils::packageVersion("PEITHO")
  sprintf(
    "PEITHO-WebTextFetcher/%s (source: https://github.com/Pandora-IsoMemo/PEITHO)",
    ver
  )
}

#' Fetch and parse web text from a URL
#'
#' This function retrieves the HTML content from the specified URL,
#' extracts text based on the provided CSS selector, and returns a
#' `WebText` object containing the extracted text and metadata.
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
        rvest::html_elements(xpath = ".//*[self::nav or self::footer or self::aside or self::script or self::style]") |>
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

  if (nzchar(text_blocks)) {
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
