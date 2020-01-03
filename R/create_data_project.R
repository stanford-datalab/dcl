
#' Create project folder
#'
#' Create a project folder with Makefile and sub-folders: data-raw, data,
#' scripts, docs, eda, and reports.
#' Available from RStudio: File > New Project... > New Directory > DCL project
#'
#' @param path Path for project folder.
#' @param template_url Download URL of template repository
#'
#' @export
#'
create_data_project <- function(path,
                                template_url =
                                  "https://github.com/dcl-docs/project/archive/master.zip") {

  dir_temp <- tempdir()
  file_zip <- fs::file_temp(tmp_dir = dir_temp)
  if (utils::download.file(template_url, destfile = file_zip, quiet = TRUE)) {
    stop("Failed to reach template repository. Are you connected to the internet?")
  }

  files <- utils::unzip(file_zip, exdir = dir_temp)

  dir_unzip <-
    files[[1]] %>%
    stringr::str_remove(stringr::str_glue("{dir_temp}/")) %>%
    dirname()

  fs::dir_ls(path = fs::path(dir_temp, dir_unzip)) %>%
    purrr::walk(~ file.copy(from = ., to = path, recursive = TRUE))

  unlink(file_zip, recursive = TRUE)
}
