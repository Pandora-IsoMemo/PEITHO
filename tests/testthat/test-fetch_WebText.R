test_that("fetch_WebText returns valid WebText object for example.com", {
  # Mock httr2 response functions
  local_mocked_bindings(
    resp_status = function(resp) 200L,
    resp_body_html = function(resp) {
      xml2::read_html("<html><head><title>Test</title></head><body>Test content</body></html>")
    },
    .package = "httr2"
  )
  
  obj <- fetch_WebText("https://httpbin.org/html", return_text_blocks_only = FALSE)
  expect_true(is_WebText(obj))
  expect_equal(obj$url, "https://httpbin.org/html")
  expect_type(obj$text, "character")
  expect_s3_class(obj, "WebText")
  expect_true(obj$status_code >= 200)
  expect_type(obj$warnings, "character")
})

test_that("fetch_WebText handles invalid URL gracefully", {
  # Mock to simulate an error during request
  local_mocked_bindings(
    req_perform = function(req, ...) {
      stop("Request error: HTTP 404 Not Found.")
    },
    .package = "httr2"
  )
  
  expect_error(
    fetch_WebText(
      "http://nonexistent.domain.example",
      return_text_blocks_only = FALSE
    )
  )
})

test_that("fetch_WebText can return text blocks only", {
  # Mock httr2 response functions
  local_mocked_bindings(
    resp_status = function(resp) 200L,
    resp_body_html = function(resp) {
      xml2::read_html("<html><body><main>Test content here</main></body></html>")
    },
    .package = "httr2"
  )
  
  text_blocks <- fetch_WebText(
    "https://httpbin.org/html",
    return_text_blocks_only = TRUE
  )
  expect_type(text_blocks, "character")
  expect_true(length(text_blocks) > 0)
})
