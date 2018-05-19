#' Compare two objects
#'
#' Compare two objects using [compare::compare()], ignoring `spec` attribute by
#' default.
#'
#' @param model        The "correct" object.
#' @param comparison   The object to be compared with `model`.
#' @param ignore_attrs String or vector of strings of attributes to ignore.
#' Default is to ignore `spec` attribute. Use `NULL` to not ignore attributes.
#' @param ...          Arguments to be passed to `compare::compare()`.
#'
#' @export
#'
#' @examples
#' df1 <- data.frame(x = 1, y = 2)
#' df2 <- df1
#' attr(df2, "spec") <- "a"
#' df3 <- df2
#' attr(df3, "problems") <- "b"
#'
#' # Ignore `spec` attribute by default
#' compare(df1, df2)
#'
#' # Do not ignore attributes
#' compare(df1, df2, ignore_attrs = NULL)
#'
#' Ignore `spec` and `problems` attributes
#' compare(df1, df3, ignore_attrs = c("spec", "problems"))
#'
compare <- function(model, comparison, ignore_attrs = "spec", ...) {
  if (!is.null(ignore_attrs) && !is.character(unlist(ignore_attrs))) {
    stop("ignore_attrs must be NULL, a string, or a vector of strings")
  }
  for (attr in ignore_attrs) {
    attr(model, attr) <- NULL
    attr(comparison, attr) <- NULL
  }
  compare::compare(model, comparison, ...)
}
