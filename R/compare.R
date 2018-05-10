#' Compare two objects
#'
#' This uses [compare::compare] with some tweaks designed to
#' checking of DCL exercises easier.
#'
#' @param model !!!!!
#' @param comparison
#' @param ...
#'
#' @export
#'
#' @examples
#' # This is a comment
#' compare(mtcars, mtcars)
compare <- function(model, comparison, ...) {
  attr(model, "col_spec") <- NULL
  attr(comparison, "col_spec") <- NULL

  compare::compare(model, comparison, ...)
}
