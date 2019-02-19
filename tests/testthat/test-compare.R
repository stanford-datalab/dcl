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

test_that("Ignore problems attribute", {
  df1 <- data.frame(x = 1, y = 2)
  attr(df1, "spec") <- "a"
  df2 <- df1
  attr(df2, "problems") <- "b"

  expect_true(compare::isTRUE(compare(df1, df2, ignore_attrs = "problems")))
})

test_that("Ignore spec and problems attributes", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- df1
  attr(df2, "spec") <- "a"
  attr(df2, "problems") <- "b"

  expect_true(
    compare::isTRUE(compare(df1, df2, ignore_attrs = c("spec", "problems")))
  )
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

test_that("Ignore problems class", {
  df1 <- data.frame(x = 1, y = 2)
  attr(df1, "class") <- c("spec_tbl_df", attr(df1, "class"))
  df2 <- df1
  attr(df2, "class") <- c("problems", attr(df2, "class"))

  expect_true(compare::isTRUE(compare(df1, df2, ignore_classes = "problems")))
})

test_that("Ignore spec and problems classes", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- df1
  attr(df2, "class") <- c("spec_tbl_df", "problems", attr(df2, "class"))

  expect_true(
    compare::isTRUE(
      compare(df1, df2, ignore_classes = c("spec_tbl_df", "problems"))
    )
  )
})
