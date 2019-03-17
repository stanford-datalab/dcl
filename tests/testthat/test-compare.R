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

test_that("Ignore spec and problems attributes", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- df1
  attr(df2, "spec") <- "a"
  attr(df2, "problems") <- "b"

  expect_true(compare::isTRUE(compare(df1, df2)))
})

test_that("Do not ignore attributes", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- df1
  attr(df2, "spec") <- "a"
  attr(df2, "problems") <- "b"

  expect_false(compare::isTRUE(compare(df1, df2, ignore_attrs = NULL)))
})

test_that("Ignore spec_tbl_df class", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- df1
  attr(df2, "class") <- c("spec_tbl_df", attr(df2, "class"))

  expect_true(compare::isTRUE(compare(df1, df2)))
})

test_that("Do not ignore classes", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- df1
  attr(df2, "class") <- c("spec_tbl_df", attr(df2, "class"))

  expect_false(compare::isTRUE(compare(df1, df2, ignore_classes = NULL)))
})

test_that("Ignore foo attribute", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- df1
  attr(df2, "foo") <- "a"

  expect_true(compare::isTRUE(compare(df1, df2, ignore_attrs = "foo")))
})

test_that("Ignore bar class", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- df1
  attr(df2, "class") <- c("bar", attr(df1, "class"))

  expect_true(compare::isTRUE(compare(df1, df2, ignore_classes = "bar")))
})

test_that("Ignore empty attributes", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- df1
  attr(df2, "a") <- logical(0)
  attr(df2, "b") <- integer(0)
  attr(df2, "c") <- double(0)
  attr(df2, "d") <- character(0)

  expect_true(compare::isTRUE(compare(df1, df2)))
})

test_that("Do not ignore empty attributes", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- df1
  attr(df2, "a") <- logical(0)

  expect_false(compare::isTRUE(compare(df1, df2, ignore_empty = FALSE)))
})
