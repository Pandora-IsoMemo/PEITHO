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
  return_text_blocks_only = TRUE,
  stop_on_error = TRUE,
  max_retries = 3,
  backoff_multiplier = 2,
  initial_delay_sec = 1
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
  as a single character string (collapsed with double newlines) or to
  return a \`WebText\` object containing the text blocks and metadata.
  Default is \`TRUE\`.

- stop_on_error:

  A logical indicating whether to stop execution and throw an error if
  the request fails or if the HTML cannot be parsed. If \`FALSE\`, the
  function will return \`NULL\` and log a warning instead. Default is
  \`TRUE\`.

- max_retries:

  An integer specifying the maximum number of retries for the HTTP
  request in case of transient failures. Default is \`3\`.

- backoff_multiplier:

  A numeric value specifying the multiplier for the exponential backoff
  strategy used during retries. Default is \`2\`.

- initial_delay_sec:

  A numeric value specifying the initial delay in seconds before the
  first retry attempt. The delay will increase exponentially based on
  the \`backoff_multiplier\`. Default is \`1\`.

## Value

If \`return_text_blocks_only\` is \`TRUE\`, a named character vector
containing the extracted text blocks as a single string, with the URL as
the name. If \`return_text_blocks_only\` is \`FALSE\`, a \`WebText\`
object containing the URL, title, text blocks, fetch timestamp, HTTP
status code, and any warnings encountered during the fetch process.
