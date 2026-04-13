# Fetch and parse web text from a URL

This function retrieves the HTML content from the specified URL, parses
it to extract the main text content, and returns it as a character
vector. It also handles errors and warnings gracefully, providing
informative messages if the request fails or if the HTML structure is
not as expected. By default, it returns only the extracted text blocks,
but it can also return a full \`WebText\` object containing metadata if
\`return_text_blocks_only\` is set to \`FALSE\`.

## Usage

``` r
fetch_WebText(
  url,
  timeout_sec = 10,
  user_agent = NULL,
  return_text_blocks_only = TRUE
)
```

## Arguments

- url:

  A character string specifying the URL to fetch.

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
