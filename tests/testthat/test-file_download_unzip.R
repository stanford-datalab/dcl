test_that("file_download_unzip() returns the path to the new directory", {
  x <-
    file_download_unzip(
      url_file = "https://github.com/dcl-docs/project/archive/master.zip",
      path = "tmp"
    )

  expect_true(x == "tmp/project-master")
})

test_that("file_download_unzip() downloads and unzips file", {
  expect_true(dir_exists("tmp/project-master"))
})

test_that("file_download_unzip() removes temporary zip file", {
  expect_true(length(dir_ls("tmp")) == 1)
})

unlink("temp", recursive = TRUE)
