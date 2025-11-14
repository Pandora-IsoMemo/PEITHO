fetch_WebText <- function(
  url,
  css_selector = "h1, h2, h3, p, li",
  timeout_sec  = 10
) {
  warn_msgs <- character()
  err_msgs  <- character()
  status_code <- NA_integer_
  title <- ""
  text_blocks <- character()

  # Try the request
  resp <- tryCatch(
    {
      httr2::request(url) |>
        httr2::req_user_agent(NULL) |>
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

