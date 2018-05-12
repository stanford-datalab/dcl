#' Compare two objects
#'
#' Compare two objects using [compare::compare()], ignoring column order and any
#' attributes specified with `ignore_attr`.
#'
#' @param model       The "correct" object.
#' @param comparison  The object to be compared with `model`.
#' @param ignore_attr String or list of strings specifying attributes to ignore.
#' @param ...         Arguments to be passed to `compare::compare()`.
#'
#' @export
#'
#' @examples
#' # Ignore spec attribute created by readr functions
#' compare(answers$q1, q1, ignore_attr = "spec")
#'
compare <- function(model, comparison, ignore_attr = NULL, ...) {
  for (attr in ignore_attr) {
    attr(model, attr) <- NULL
    attr(comparison, attr) <- NULL
  }
  compare::compare(model, comparison, ignoreColOrder = TRUE, ...)
}
