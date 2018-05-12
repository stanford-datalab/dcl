#' Compare two objects
#'
#' Compare two objects using [compare::compare()], ignoring attributes specified
#' with `ignore_attrs`.
#'
#' @param model        The "correct" object.
#' @param comparison   The object to be compared with `model`.
#' @param ignore_attrs String or list of strings specifying attributes to ignore.
#' @param ...          Arguments to be passed to `compare::compare()`.
#'
#' @export
#'
#' @examples
#' # Ignore spec attribute created by readr functions
#' compare(answers$q1, q1, ignore_attrs = "spec")
#'
compare <- function(model, comparison, ignore_attrs = NULL, ...) {
  for (attr in ignore_attrs) {
    attr(model, attr) <- NULL
    attr(comparison, attr) <- NULL
  }
  compare::compare(model, comparison, ...)
}
