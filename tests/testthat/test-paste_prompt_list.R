test_that("paste_prompt_list wraps each element with prefix and suffix", {
  result <- PEITHO:::paste_prompt_list(list("foo", "bar"), "<<", ">>")
  expect_equal(result, c("<<foo>>", "<<bar>>"))
})

test_that("paste_prompt_list works with empty prefix and suffix", {
  result <- PEITHO:::paste_prompt_list(list("a", "b"), "", "")
  expect_equal(result, c("a", "b"))
})

test_that("paste_prompt_list works with prefix only", {
  result <- PEITHO:::paste_prompt_list(list("x"), "PREFIX: ", "")
  expect_equal(result, "PREFIX: x")
})

test_that("paste_prompt_list works with suffix only", {
  result <- PEITHO:::paste_prompt_list(list("x"), "", " :SUFFIX")
  expect_equal(result, "x :SUFFIX")
})

test_that("paste_prompt_list returns character vector", {
  result <- PEITHO:::paste_prompt_list(list("a", "b"), "p", "s")
  expect_type(result, "character")
  expect_length(result, 2)
})

test_that("paste_prompt_list returns character(0) for NULL string_list", {
  result <- PEITHO:::paste_prompt_list(NULL, "p", "s")
  expect_equal(result, character(0))
})

test_that("paste_prompt_list coerces numeric string_list to character", {
  result <- PEITHO:::paste_prompt_list(list(1, 2, 3), "[", "]")
  expect_equal(result, c("[1]", "[2]", "[3]"))
})

test_that("paste_prompt_list warns on NA in string_list", {
  expect_warning(
    PEITHO:::paste_prompt_list(list("a", NA, "b"), "[", "]"),
    "NA"
  )
})

test_that("paste_prompt_list handles literal % in prefix without error", {
  result <- PEITHO:::paste_prompt_list(list("score"), "80% for ", "!")
  expect_equal(result, "80% for score!")
})

test_that("paste_prompt_list handles literal % in suffix without error", {
  result <- PEITHO:::paste_prompt_list(list("done"), "Task: ", " (100%)")
  expect_equal(result, "Task: done (100%)")
})

test_that("paste_prompt_list handles multiple % in prefix and suffix", {
  result <- PEITHO:::paste_prompt_list(list("x"), "50% & 50% ", " = 100%")
  expect_equal(result, "50% & 50% x = 100%")
})

test_that("paste_prompt_list errors on NULL prefix", {
  expect_error(PEITHO:::paste_prompt_list(list("a"), NULL, "s"), "prefix")
})

test_that("paste_prompt_list errors on NULL suffix", {
  expect_error(PEITHO:::paste_prompt_list(list("a"), "p", NULL), "suffix")
})

test_that("paste_prompt_list errors on non-character prefix", {
  expect_error(PEITHO:::paste_prompt_list(list("a"), 123, "s"), "prefix")
})

test_that("paste_prompt_list errors on non-scalar prefix", {
  expect_error(PEITHO:::paste_prompt_list(list("a"), c("p1", "p2"), "s"), "prefix")
})

test_that("paste_prompt_list errors on non-scalar suffix", {
  expect_error(PEITHO:::paste_prompt_list(list("a"), "p", c("s1", "s2")), "suffix")
})

test_that("paste_prompt_list handles single-element string_list", {
  result <- PEITHO:::paste_prompt_list(list("only"), "START:", ":END")
  expect_equal(result, "START:only:END")
  expect_length(result, 1)
})
