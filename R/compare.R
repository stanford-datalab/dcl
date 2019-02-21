#' Compare two objects
#'
#' Compare two objects using [compare::compare()], ignoring `spec` and
#' `problems` attributes and `spec_tbl_df` class by default.
#'
#' @param model        The "correct" object.
#' @param comparison   The object to be compared with `model`.
#' @param ignore_attrs String or vector of strings of attributes to ignore.
#' Default is to ignore `spec` and `problems` attributes. Use `NULL` to not
#' ignore attributes.
#' @param ignore_classes String or vector of strings of classes to ignore.
#' Default is to ignore `spec_tbl_df` class. Use `NULL` to not ignore classes.
#' @param ...          Arguments to be passed to `compare::compare()`.
#'
#' @export
#'
#' @examples
#' df1 <- data.frame(x = 1, y = 2)
#' df2 <- df1
#' attr(df2, "spec") <- "a"
#' attr(df2, "problems") <- "b"
#' attr(df2, "class") <- c("spec_tbl_df", attr(df1, "class"))
#' df3 <- df1
#' attr(df3, "foo") <- "a"
#' attr(df3, "class") <- c("bar", attr(df1, "class"))
#'
#' # Ignore spec and problems attributes and spec_tbl_df class by default
#' compare(df1, df2)
#'
#' # Do not ignore attributes or classes
#' compare(df1, df2, ignore_attrs = NULL, ignore_classes = NULL)
#'
#' # Ignore foo attribute
#' compare(df2, df3, ignore_attrs = "foo")
#'
#' # Ignore bar class
#' compare(df1, df3, ignore_classes = "bar")
#'
compare <- function(
  model, comparison, ignore_attrs = c("spec", "problems"),
  ignore_classes = "spec_tbl_df", ...
) {
  if (!is.null(ignore_attrs) && !is.character(unlist(ignore_attrs))) {
    stop("ignore_attrs must be NULL, a string, or a vector of strings")
  }
  if (!is.null(ignore_classes) && !is.character(unlist(ignore_classes))) {
    stop("ignore_classes must be NULL, a string, or a vector of strings")
  }
  for (attr in ignore_attrs) {
    attr(model, attr) <- NULL
    attr(comparison, attr) <- NULL
  }
  attr(model, "class") <- dplyr::setdiff(attr(model, "class"), ignore_classes)
  attr(comparison, "class") <-
    dplyr::setdiff(attr(comparison, "class"), ignore_classes)
  compare::compare(model, comparison, ...)
}
