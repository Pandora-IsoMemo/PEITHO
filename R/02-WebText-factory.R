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
#' @param css_selector A character string specifying the CSS selector
#'   to identify text-containing HTML elements. Default is
#'   `"h1, h2, h3, p, li"`.
#' @param timeout_sec An integer specifying the timeout for the HTTP
#'   request in seconds. Default is `10`.
#' @param user_agent An optional character string specifying the User-Agent
#'   header for the HTTP request. If `NULL`, a default User-Agent
#'   string is used.
#' @return A `WebText` object containing the extracted text and metadata.
#' @export
#'
#' @examples
#' # Fetch text from example.com
#' wt <- fetch_WebText("https://example.com")
#' print(wt)
fetch_WebText <- function(
  url,
  css_selector = "h1, h2, h3, p, li",
  timeout_sec  = 10,
  user_agent   = NULL
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

  # Try the request: check if URL is reachable
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

  if (!is.null(resp)) {
    status_code <- httr2::resp_status(resp)

    if (status_code >= 400) {
      err_msgs <- c(err_msgs, paste("HTTP error", status_code))
    } else {
      # Parse HTML
      html <- tryCatch(
        httr2::resp_body_html(resp),
        error = function(e) {
          err_msgs <<- c(err_msgs, paste("Failed to parse HTML:", e$message))
          NULL
        }
      )

      if (!is.null(html)) {
        # Title
        title_node <- rvest::html_element(html, "title")
        if (!is.na(title_node) && !is.null(title_node)) {
          title <- rvest::html_text2(title_node)
        }

        # Text nodes
        nodes <- rvest::html_elements(html, css_selector)
        if (length(nodes) == 0L) {
          warn_msgs <- c(
            warn_msgs,
            paste("No elements matched CSS selector:", css_selector)
          )
        }

        text_blocks <- rvest::html_text2(nodes)
        text_blocks <- stringr::str_squish(text_blocks)
        text_blocks <- text_blocks[nzchar(text_blocks)]
        text_blocks <- unique(text_blocks)
      }
    }
  }

  new_WebText(
    url         = url,
    title       = title,
    text_blocks = text_blocks,
    fetched_at  = Sys.time(),
    status_code = as.integer(status_code),
    warnings    = warn_msgs,
    errors      = err_msgs
  )
}

