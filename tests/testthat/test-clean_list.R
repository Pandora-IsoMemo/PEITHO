test_that("prune_nulls returns NULL for NULL input", {
  expect_null(PEITHO:::prune_nulls(NULL))
})

test_that("prune_nulls returns atomic inputs unchanged", {
  expect_identical(PEITHO:::prune_nulls(42L), 42L)
  expect_identical(PEITHO:::prune_nulls("abc"), "abc")
  expect_identical(PEITHO:::prune_nulls(c(TRUE, FALSE)), c(TRUE, FALSE))
})

test_that("prune_nulls handles empty list", {
  expect_identical(PEITHO:::prune_nulls(list()), list())
})

test_that("prune_nulls removes NULL values recursively", {
  x <- list(
    a = 1,
    b = NULL,
    c = list(
      d = NULL,
      e = 2,
      f = list(NULL, 3)
    )
  )

  out <- PEITHO:::prune_nulls(x)

  expect_equal(
    out,
    list(
      a = 1,
      c = list(
        e = 2,
        f = list(3)
      )
    )
  )
})

test_that("prune_nulls preserves non-NULL nested values", {
  x <- list(
    a = list(b = "x", c = list(d = 7)),
    e = 9
  )
  out <- PEITHO:::prune_nulls(x)

  expect_equal(out, x)
})

test_that("prune_nulls errors on unsupported non-list, non-atomic types", {
  expect_error(
    PEITHO:::prune_nulls(new.env()),
    "Unsupported type in list"
  )
})