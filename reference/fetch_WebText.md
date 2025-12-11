# Fetch and parse web text from a URL

This function retrieves the HTML content from the specified URL,
extracts text based on the provided CSS selector, and returns a
\`WebText\` object containing the extracted text and metadata.

## Usage

``` r
fetch_WebText(
  url,
  css_selector = "h1, h2, h3, p, li",
  timeout_sec = 10,
  user_agent = NULL,
  return_text_blocks_only = TRUE
)
```

## Arguments

- url:

  A character string specifying the URL to fetch.

- css_selector:

  A character string specifying the CSS selector to identify
  text-containing HTML elements. Default is \`"h1, h2, h3, p, li"\`.

- timeout_sec:

  An integer specifying the timeout for the HTTP request in seconds.
  Default is \`10\`.

- user_agent:

  An optional character string specifying the User-Agent header for the
  HTTP request. If \`NULL\`, a default User-Agent string is used.

- return_text_blocks_only:

  A logical indicating whether to return only the extracted text blocks
  as a character vector (\`TRUE\`), or a full \`WebText\` object with
  metadata (\`FALSE\`). Default is \`TRUE\`.

## Value

A \`WebText\` object containing the extracted text and metadata.
