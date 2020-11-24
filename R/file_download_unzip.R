
#' Download and unzip a file.
#'
#' @description Downloads a zipped file from the internet and unzips it to a directory.
#'
#' @param url_file URL of the file to download and unzip.
#' @param path Directory to put the unzipped file.
#'
#' @return Character vector giving the path to the downloaded and unzipped file.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' file_download_unzip(
#'   url_file = "https://github.com/dcl-docs/project/archive/master.zip",
#'   path = "~/my/dir"
#' )
#' }
file_download_unzip <- function(url_file, path) {

  path <- path_expand(path)
  file_zip <- file_temp(tmp_dir = path, ext = ".zip")

  if (!dir_exists(path)) {
    dir_create(path)
  }

  if (
    utils::download.file(
      url_file, destfile = file_zip, quiet = TRUE
    )
  ) {
    stop("Failed to reach template repository. Are you connected to the internet?")
  }

  files <- utils::unzip(file_zip, exdir = path)
  unlink(file_zip, recursive = TRUE)

  path_common(files)
}
