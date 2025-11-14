test_that("fetch_WebText returns valid WebText object for example.com", {
  obj <- fetch_WebText("https://example.com")
  expect_true(is_WebText(obj))
  expect_equal(obj$url, "https://example.com")
  expect_type(obj$text, "character")
  expect_s3_class(obj, "WebText")
  expect_true(obj$status_code >= 200)
  expect_type(obj$warnings, "character")
  expect_type(obj$errors, "character")
})

test_that("fetch_WebText handles invalid URL gracefully", {
  obj <- fetch_WebText("http://nonexistent.domain.example")
  expect_true(is_WebText(obj))
  expect_true(length(obj$errors) > 0)
})

test_that("fetch_WebText warns if CSS selector matches nothing", {
  obj <- fetch_WebText("https://example.com", css_selector = "notarealcssselector")
  expect_true(is_WebText(obj))
  expect_true(any(grepl("No elements matched CSS selector", obj$warnings)))
})
