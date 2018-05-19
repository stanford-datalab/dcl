context("test-compare.R")

test_that("Identical objects", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- df1

  expect_true(compare::isTRUE(compare(df1, df2)))
})

test_that("Different objects", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- data.frame(x = 2, y = 1)

  expect_false(compare::isTRUE(compare(df1, df2)))
})

test_that("Ignore spec attribute", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- df1
  attr(df2, "spec") <- "a"

  expect_true(compare::isTRUE(compare(df1, df2)))
})

test_that("Do not ignore attributes", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- df1
  attr(df2, "spec") <- "a"

  expect_false(compare::isTRUE(compare(df1, df2, ignore_attrs = NULL)))
})

test_that("Ignore spec and problem attributes", {
  df1 <- data.frame(x = 1, y = 2)
  attr(df1, "spec") <- "a"
  df2 <- df1
  attr(df2, "problems") <- "b"

  expect_true(
    compare::isTRUE(compare(df1, df2, ignore_attrs = c("spec", "problems")))
  )
})
