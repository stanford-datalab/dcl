context("test-compare.R")

test_that("Ignore spec attribute", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- data.frame(x = 1, y = 2)
  attr(df2, "spec") <- "blah"

  expect_true(compare(df1, df2, ignore_attr = "spec"))
})
