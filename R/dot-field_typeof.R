#' Classify a field value by a coarse type label
#'
#' Internal helper used for field registries and printing.
#'
#' @param x Any R object.
#' @return A length-1 character label.
#' @keywords internal
.field_typeof <- function(x) {
  if (is.function(x)) {
    "function"

  } else if (is.data.frame(x)) {
    "data.frame"

  } else if (is.matrix(x)) {
    "matrix"

  } else if (is.array(x)) {
    "array"

  } else if (inherits(x, "R6")) {
    "R6"

  } else if (is.environment(x)) {
    "environment"

  } else if (is.atomic(x) && length(x) == 1) {
    "scalar"

  } else if (is.atomic(x)) {
    "vector"

    # plain list"
  } else if (is.list(x) && !is.object(x)) {
    "list"

    # list-like objects (e.g., igraph)
  } else if (is.list(x) && is.object(x)) {
    cls <- class(x)
    if (length(cls)) cls[1] else "list"

  } else {
    cls <- class(x)
    if (length(cls)) cls[1] else typeof(x)
  }
}

