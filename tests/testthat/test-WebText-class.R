test_that("new_WebText creates valid WebText object", {
  obj <- new_WebText(
    url = "https://example.com",
    title = "Example Title",
    text_blocks = c("Some text", "More text"),
    fetched_at = as.POSIXct("2025-11-14 12:00:00"),
    status_code = 200L,
    warnings = character()
  )
  expect_true(is_WebText(obj))
  expect_equal(obj$url, "https://example.com")
  expect_equal(obj$title, "Example Title")
  expect_type(obj$text, "character")
  expect_equal(obj$status_code, 200L)
  expect_s3_class(obj, "WebText")
  expect_true(inherits(obj$fetched_at, "POSIXct"))
  expect_type(obj$warnings, "character")
})

test_that("validate_WebText catches invalid input", {
  bad_obj <- list(url = 123, title = TRUE, text = "notalist", fetched_at = "notdate", status_code = "notint", warnings = 1, errors = 2)
  class(bad_obj) <- "WebText"
  expect_error(validate_WebText(bad_obj))
})
