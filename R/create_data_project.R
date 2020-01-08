
#' Create a project from a template
#'
#' Creates a project from a zipped template file.
#'
#' @param path Path for project folder.
#' @param project Should this be an RStudio project?
#' @param template Download URL of template repository
#' @param rstudio Are you using RStudio?
#' @param open Boolean. If `project` is `TRUE` and `open` is `TRUE`, will
#' open the project in a new session.
#'
#' @export
#'
#' @examples
#' \donttest{create_data_project("path/to/project")}
create_data_project <- function(
  path,
  project = TRUE,
  template = "https://github.com/dcl-docs/project/archive/master.zip",
  rstudio = rstudioapi::isAvailable(),
  open = interactive()
) {

  path <- usethis:::user_path_prep(path)
  name <- path_file(path)

  if (project) {
    usethis:::check_not_nested(path_dir(path), name)
  }

  create_directory_from_template(path, template)

  if (project) {
    old_project <- proj_set(path, force = TRUE)
    on.exit(proj_set(old_project), add = TRUE)
    if (rstudio) {
      use_rstudio()
    }
    if (open & proj_activate(path)) {
      on.exit()
    }
    invisible(proj_get())
  }
}

#' Create and populate a directory from a template
#'
#' @description Recursively creates a directory using a template downloaded from
#' the internet.
#'
#' @param path Path to create directory.
#' @param url_template A GitHub repository download link.
#'
#' @return Prints out messages saying the directory were created.
#' @export
#'
#' @examples
#' \donttest{
#' create_directory_from_template(
#'   "new_dir",
#'   "https://github.com/dcl-docs/project/archive/master.zip"
#' )
#' }
create_directory_from_template <- function(path, url_template) {
  usethis:::create_directory(path)

  dir_temp <- tempdir()
  file_zip <- fs::file_temp(tmp_dir = dir_temp)
  if (utils::download.file(url_template, destfile = file_zip, quiet = TRUE)) {
    stop("Failed to reach template repository. Are you connected to the internet?")
  }

  files <- utils::unzip(file_zip, exdir = dir_temp)

  dir_unzip <-
    files[[1]] %>%
    stringr::str_remove(stringr::str_glue("{dir_temp}/")) %>%
    dirname()

  copy_directory(path = path(dir_temp, dir_unzip), new_path = path)

  unlink(file_zip, recursive = TRUE)
}

#' Recursively copy a directory.
#'
#' @description Recursively copies a directory, printing out messages saying which
#' sub-directories were created.
#'
#' @param path Path of the directory to copy.
#' @param new_path Path to copy directory to.
#' @param all Boolean. If `TRUE`, will copy hidden files as well.
#'
#' @return Prints out messages saying which sub-directories it created.
#' @export
#'
#' @examples
#' \donttest{copy_directory(path = "old/dir", new_path = "new/dir")}
copy_directory <- function(path, new_path, all = TRUE) {
  fs::dir_ls(path, all = all) %>%
    purrr::walk(file.copy, to = new_path, recursive = TRUE, overwrite = TRUE)

  fs::dir_ls(new_path, type = "directory") %>%
    purrr::walk(~ ui_done("Creating {ui_path(basename(.))}"))
}


