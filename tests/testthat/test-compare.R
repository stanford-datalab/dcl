context("test-compare.R")

test_that("ignore import col_spec", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- data.frame(x = 1, y = 2)
  attr(df2, "col_spec") <- list("bah")

  expect_true(compare(df1, df2))
})
