#' Compare attributes
#'
#' Compares attributes of two objects.
#'
#' @param x,y  Objects for which to compare attributes.
#'
#' @export
#'
compare_attributes <- function(x, y) {
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
