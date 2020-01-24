test_that("Identical objects", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- df1

  expect_true(compare::isTRUE(compare(df1, df2)))
})

test_that("Identical grouped objects", {
  df1 <- dplyr::group_by(data.frame(x = 1, y = 2), x)
  df2 <- df1

  expect_true(compare::isTRUE(compare(df1, df2)))
})

test_that("Different objects", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- data.frame(x = 2, y = 1)

  expect_false(compare::isTRUE(compare(df1, df2)))
})

test_that("Objects that differ by grouping", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- dplyr::group_by(df1, x)

  expect_false(compare::isTRUE(compare(df1, df2)))
})

test_that("Compare only class, names, and row.names attributes", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- df1
  attr(df2, "foo") <- "a"

  expect_true(compare::isTRUE(compare(df1, df2)))
})

test_that("Compare all attributes", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- df1
  attr(df2, "foo") <- "a"

  expect_false(compare::isTRUE(compare(df1, df2, compare_attrs = NULL)))
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

test_that("Compare attributes with different orders", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- df1
  attributes(df2) <- rev(attributes(df1))

  expect_true(compare::isTRUE(compare(df1, df2)))
})

test_that("Compare foo attribute", {
  df1 <- data.frame(x = 1, y = 2)
  attr(df1, "foo") <- "a"
  attr(df1, "bar") <- "b"
  df2 <- df1
  attr(df2, "bar") <- "c"

  expect_true(compare::isTRUE(compare(df1, df2, compare_attrs = "foo")))
})

test_that("Ignore bar class", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- df1
  attr(df2, "class") <- c("bar", attr(df1, "class"))

  expect_true(compare::isTRUE(compare(df1, df2, ignore_classes = "bar")))
})
