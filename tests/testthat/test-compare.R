context("test-compare.R")

test_that("Ignore spec attribute", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- data.frame(x = 1, y = 2)
  attr(df2, "spec") <- list("blah")

  cmp <- compare(df1, df2, ignore_attrs = "spec")
  expect_true(cmp$result)
})
