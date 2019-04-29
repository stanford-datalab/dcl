#' Compare attributes
#'
#' Compares attributes of two objects, comparing only the attributes in
#' `compare_attrs` and ignoring classes in `ignore_classes`.
#'
#' @param x,y            Objects for which to compare attributes.
#' @param compare_attrs  String or vector of strings of attributes to compare.
#' Default is to compare only `class`, `names`, and `row.names` attributes. Use
#' `NULL` to compare all attributes.
#' @param ignore_classes String or vector of strings of classes to ignore.
#' Default is to ignore `spec_tbl_df` class. Use `NULL` to not ignore classes.
#'
#' @export
#'
compare_attributes <- function(
  x, y, compare_attrs = c("class", "names", "row.names"),
  ignore_classes = "spec_tbl_df"
) {
  if (!is.null(compare_attrs) && !is.character(unlist(compare_attrs))) {
    stop("compare_attrs must be NULL, a string, or a vector of strings")
  }
  if (!is.null(ignore_classes) && !is.character(unlist(ignore_classes))) {
    stop("ignore_classes must be NULL, a string, or a vector of strings")
  }
  attr(x, "class") <- setdiff(attr(x, "class"), ignore_classes)
  attr(y, "class") <- setdiff(attr(y, "class"), ignore_classes)
  if (!is.null(compare_attrs)) {
    attrs <- attributes(x)
    attributes(x) <- attrs[sort(intersect(names(attrs), compare_attrs))]
    attrs <- attributes(y)
    attributes(y) <- attrs[sort(intersect(names(attrs), compare_attrs))]
  }
  attrs_x <- attributes(x)
  attrs_y <- attributes(y)
  if (identical(attrs_x, attrs_y)) {
    cat("x and y have identical attributes\n")
  } else {
    attr_names_x <- names(attrs_x)
    attr_names_y <- names(attrs_y)
    for (attr_name in union(attr_names_x, attr_names_y) %>% sort()) {
      if ((attr_name %in% attr_names_x) && (attr_name %in% attr_names_y)) {
        if (identical(attr(x, attr_name), attr(y, attr_name))) {
          cat(str_glue("{attr_name}: identical\n\n"))
        } else {
          cat(str_glue("{attr_name}: \n\n"))
          cat(" x:\n")
          glimpse(attr(x, attr_name))
          cat(" y:\n")
          glimpse(attr(y, attr_name))
        }
      } else if
      ((attr_name %in% attr_names_x) && !(attr_name %in% attr_names_y)) {
        cat(str_glue("{attr_name}: in x, not in y\n\n"))
      } else if
      (!(attr_name %in% attr_names_x) && (attr_name %in% attr_names_y)) {
        cat(str_glue("{attr_name}: not in x, in y\n\n"))
      }
    }
  }
  invisible(NULL)
}
