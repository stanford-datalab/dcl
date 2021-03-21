#' Create a project from a template
#'
#' Creates a project from a template. By default, creates a project
#' from this template: \url{https://github.com/dcl-docs/project}
#'
#' @param path A path giving the location for the new project. If it does not already exist, it is created.
#' @param project If `TRUE`, creates an RStudio project.
#' @param template Download URL of template repository. Defaults to the dcl-docs
#' project template.
#' @param open If `TRUE` and `project` is `TRUE`, activates the project. If you're
#' using RStudio, this will result in the project opening in a new session.
#'
#' @export
#'
#' @examples
#' \dontrun{create_data_project("path/to/project")}
create_data_project <- function(
  path,
  project = TRUE,
  template = "https://github.com/dcl-docs/project/archive/master.zip",
  open = interactive()
) {

  path <- path_expand(path)
  name <- path_file(path)

  if (project) {
    usethis:::challenge_nested_project(path_dir(path), name)
  }

  create_directory_from_template(path, template)

  if (project) {
    old_project <- proj_set(path, force = TRUE)
    on.exit(proj_set(old_project), add = TRUE)

    if (rstudioapi::isAvailable()) {
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
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' create_directory_from_template(
#'   "new_dir",
#'   "https://github.com/dcl-docs/project/archive/master.zip"
#' )
#' }
create_directory_from_template <- function(path, url_template) {
  usethis:::create_directory(path)

  dir_unzip <-
    file_download_unzip(url_file = url_template, path = path)

  copy_file(dir_unzip, new_path = path)
  unlink(dir_unzip, recursive = TRUE)
}

#' Copies files and recursively copies directories.
#'
#' @description Recursively copies a file, printing out messages saying which
#' sub-directories were created.
#'
#' @param path Path of the file to copy.
#' @param new_path Path to copy file to.
#' @param all Boolean. If `TRUE`, will copy hidden files as well.
#'
#' @return Prints out messages saying which sub-directories it created.
#' @keywords internal
#'
#' @examples
#' \dontrun{copy_file(path = "old/dir", new_path = "new/dir")}
copy_file <- function(path, new_path, all = TRUE) {
  dir_ls(path, all = all) %>%
    walk(file.copy, to = new_path, recursive = TRUE, overwrite = TRUE)

  dir_ls(new_path, type = "file") %>%
    walk(~ ui_done("Writing {ui_path(path_file(.))}"))

  dir_ls(new_path, type = "directory") %>%
    walk(~ ui_done("Creating {ui_path(path_file(.))}"))
}

#' Check if a directory is empty
#'
#' @param path Path to directory
#'
#' @return Boolean.
#' @keywords internal
#'
dir_empty <- function(path) {
  length(dir_ls(path)) == 0
}
