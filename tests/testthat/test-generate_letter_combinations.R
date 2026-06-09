test_that("generate_letter_combinations returns correct default output", {
  result <- PEITHO:::generate_letter_combinations(n_letters = 1)
  expect_equal(result, letters)
  expect_length(result, 26)
})

test_that("generate_letter_combinations returns correct number of combinations for n_letters = 2", {
  result <- PEITHO:::generate_letter_combinations(n_letters = 2)
  expect_length(result, 26^2)
  expect_equal(result[1], "aa")
  expect_equal(result[length(result)], "zz")
})

test_that("generate_letter_combinations respects start and stop parameters", {
  result <- PEITHO:::generate_letter_combinations(n_letters = 2, start = "ab", stop = "ba")
  expect_equal(result[1], "ab")
  expect_equal(result[length(result)], "ba")
})

test_that("generate_letter_combinations returns single element when start equals stop", {
  result <- PEITHO:::generate_letter_combinations(n_letters = 3, start = "abc", stop = "abc")
  expect_equal(result, "abc")
  expect_length(result, 1)
})

test_that("generate_letter_combinations returns combinations in lexicographical order", {
  result <- PEITHO:::generate_letter_combinations(n_letters = 2, start = "aa", stop = "ac")
  expect_equal(result, c("aa", "ab", "ac"))
})

test_that("generate_letter_combinations errors on non-positive n_letters", {
  expect_error(PEITHO:::generate_letter_combinations(n_letters = 0))
  expect_error(PEITHO:::generate_letter_combinations(n_letters = -1))
})

test_that("generate_letter_combinations errors on non-integer n_letters", {
  expect_error(PEITHO:::generate_letter_combinations(n_letters = 1.5))
})

test_that("generate_letter_combinations errors when start length != n_letters", {
  expect_error(PEITHO:::generate_letter_combinations(n_letters = 3, start = "ab"))
  expect_error(PEITHO:::generate_letter_combinations(n_letters = 3, start = "abcd"))
})

test_that("generate_letter_combinations errors when stop length != n_letters", {
  expect_error(PEITHO:::generate_letter_combinations(n_letters = 3, stop = "ab"))
  expect_error(PEITHO:::generate_letter_combinations(n_letters = 3, stop = "abcd"))
})

test_that("generate_letter_combinations errors on non-letter characters in start", {
  expect_error(PEITHO:::generate_letter_combinations(n_letters = 3, start = "ab1"))
  expect_error(PEITHO:::generate_letter_combinations(n_letters = 3, start = "AB1"))
})

test_that("generate_letter_combinations errors on non-letter characters in stop", {
  expect_error(PEITHO:::generate_letter_combinations(n_letters = 3, stop = "zz1"))
})

test_that("generate_letter_combinations errors when start > stop lexicographically", {
  expect_error(PEITHO:::generate_letter_combinations(n_letters = 2, start = "ba", stop = "ab"))
})

test_that("generate_letter_combinations uses correct defaults for start and stop", {
  result_2 <- PEITHO:::generate_letter_combinations(n_letters = 2)
  expect_equal(result_2[1], "aa")
  expect_equal(result_2[length(result_2)], "zz")
})

test_that("generate_letter_combinations returns character vector", {
  result <- PEITHO:::generate_letter_combinations(n_letters = 2, start = "aa", stop = "ac")
  expect_type(result, "character")
})
