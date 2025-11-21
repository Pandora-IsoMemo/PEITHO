# WebText Class Represents text content fetched from a web page.

WebText Class Represents text content fetched from a web page.

## Usage

``` r
new_WebText(
  url,
  title,
  text_blocks,
  fetched_at = Sys.time(),
  status_code = NA_integer_,
  warnings = character(),
  errors = character()
)
```

## Arguments

- url:

  Character string. The URL of the web page.

- title:

  Character string. The title of the web page.

- text_blocks:

  A character vector. Each element represents a block of text extracted
  from the page.

- fetched_at:

  POSIXct. The timestamp when the content was fetched.

- status_code:

  Integer. The HTTP status code of the request.

- warnings:

  Character vector. Any warnings encountered during fetching or parsing.

- errors:

  Character vector. Any errors encountered during fetching or parsing.

## Value

An object of class \`WebText\`.
