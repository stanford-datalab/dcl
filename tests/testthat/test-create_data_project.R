old <- "dir"
new <- "dir_new"
file_test <- "test.txt"
test_that("copy_directory() successfully copies a directory", {
  fs::dir_create(old)
  fs::dir_create(new)
  usethis::write_over(path(old, file_test), lines = "test")
  x <- copy_directory(old, new)

  expect_true(fs::dir_exists(new))
  expect_true(fs::file_exists(path(new, file_test)))
})

unlink(old, recursive = TRUE)
unlink(new, recursive = TRUE)


