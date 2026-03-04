#' Print an ABM_Field object
#'
#' Prints a compact summary of an \code{"ABM_Field"} object.
#'
#' @param x An \code{"ABM_Field"} object.
#' @param ... Unused.
#'
#' @return \code{x}, invisibly.
#' @export
print.ABM_Field <- function(x, ...) {
  cat("<ABM_Field: ", x$category, ">\n", sep = "")
  cat("$", x$name, "\n", sep = "")
  print(x$value)
  invisible(x)
}
