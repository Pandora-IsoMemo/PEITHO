test_that("fetch_WebText returns valid WebText object for example.com", {
  obj <- fetch_WebText("https://example.com", return_text_blocks_only = FALSE)
  expect_true(is_WebText(obj))
  expect_equal(obj$url, "https://example.com")
  expect_type(obj$text, "character")
  expect_s3_class(obj, "WebText")
  expect_true(obj$status_code >= 200)
  expect_type(obj$warnings, "character")
})

test_that("fetch_WebText handles invalid URL gracefully", {
  expect_error(
    fetch_WebText(
      "http://nonexistent.domain.example",
      return_text_blocks_only = FALSE
    )
  )
})

test_that("fetch_WebText warns if CSS selector matches nothing", {
  obj <- fetch_WebText(
    "https://example.com",
    css_selector = "notarealcssselector",
    return_text_blocks_only = FALSE
  )
  expect_true(is_WebText(obj))
  expect_true(any(grepl("No elements matched CSS selector", obj$warnings)))
})

test_that("fetch_WebText can return text blocks only", {
  text_blocks <- fetch_WebText(
    "https://example.com",
    return_text_blocks_only = TRUE
  )
  expect_type(text_blocks, "character")
  expect_true(length(text_blocks) > 0)
})
