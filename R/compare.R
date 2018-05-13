#' Compare two objects
#'
#' Compare two objects using [compare::compare()], ignoring attributes in
#' `ignore_attrs`.
#'
#' @param model        The "correct" object.
#' @param comparison   The object to be compared with `model`.
#' @param ignore_attrs String or vector of strings of attributes to ignore.
#' @param ...          Arguments to be passed to `compare::compare()`.
#'
#' @export
#'
#' @examples
#' # Ignore `spec` attribute
#' compare(answers$q1, q1, ignore_attrs = "spec")
#'
#' # Ignore `spec` and `problems` attributes
#' compare(answers$q1, q1, ignore_attrs = c("spec", "problems"))
#'
compare <- function(model, comparison, ignore_attrs = NULL, ...) {
  if (!is.null(ignore_attrs) && !is.character(unlist(ignore_attrs))) {
    stop("ignore_attrs must be NULL, a string, or a vector or list of strings")
  }
  for (attr in ignore_attrs) {
    attr(model, attr) <- NULL
    attr(comparison, attr) <- NULL
  }
  compare::compare(model, comparison, ...)
}
