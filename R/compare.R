#' Compare two objects
#'
#' Compare two objects using [compare::compare()], comparing only the attributes
#' in `compare_attrs` and ignoring classes in `ignore_classes`.
#'
#' @param model          The "correct" object.
#' @param comparison     The object to be compared with `model`.
#' @param compare_attrs  String or vector of strings of attributes to compare.
#' Default is to compare only `class`, `groups`, `names`, and `row.names`
#' attributes. Use `NULL` to compare all attributes.
#' @param ignore_classes String or vector of strings of classes to ignore.
#' Default is to ignore `spec_tbl_df` class. Use `NULL` to not ignore classes.
#' @param ...            Arguments to be passed to `compare::compare()`.
#'
#' @export
#'
#' @examples
#' df1 <- data.frame(x = 1, y = 2)
#' df2 <- df1
#' attr(df2, "class") <- c("spec_tbl_df", attr(df1, "class"))
#' attr(df2, "foo") <- "bar"
#'
#' # Compare objects
#' compare(df1, df2)
#'
compare <- function(
  model, comparison, compare_attrs = c("class", "names", "groups", "row.names"),
  ignore_classes = "spec_tbl_df", ...
) {
  if (!is.null(compare_attrs) && !is.character(unlist(compare_attrs))) {
    stop("compare_attrs must be NULL, a string, or a vector of strings")
  }
  if (!is.null(ignore_classes) && !is.character(unlist(ignore_classes))) {
    stop("ignore_classes must be NULL, a string, or a vector of strings")
  }
  attr(model, "class") <- setdiff(attr(model, "class"), ignore_classes)
  attr(comparison, "class") <-
    setdiff(attr(comparison, "class"), ignore_classes)
  if (!is.null(compare_attrs)) {
    attrs <- attributes(model)
    attributes(model) <- attrs[sort(intersect(names(attrs), compare_attrs))]
    attrs <- attributes(comparison)
    attributes(comparison) <-
      attrs[sort(intersect(names(attrs), compare_attrs))]
  }
  compare::compare(model, comparison, ...)
}
