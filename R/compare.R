#' Compare two objects
#'
#' Compare two objects using [compare::compare], ignoring specified attributes.
#'
#' @param model 1
#' @param comparison 2
#' @param ... 3
#'
#' @export
#'
#' @examples
#' # Ignore spec attribute created by readr functions
#' compare(answers$q1, q1, ignore_attr = "spec")
compare <- function(model, comparison, ignore_attr = NULL, ...) {
  for (i in seq_along(ignore_attr)) {
    attr(model, ignore[[i]]) <- NULL
    attr(comparison, ignore[[i]]) <- NULL
  }
  compare::compare(model, comparison, ...)
}
