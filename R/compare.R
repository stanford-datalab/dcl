#' Compare two objects
#'
#' This uses [compare::compare] with some tweaks designed to
#' checking of DCL exercises easier.
#'
#' @param model
#' @param comparison
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
compare <- function(model, comparison, ...) {
  compare::compare(model, comparison, ...)
}
