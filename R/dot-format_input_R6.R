#' Validate and normalize input for R6 initialization
#'
#' Internal helper used in R6 `initialize()` methods to validate inputs.
#'
#' @param x An input object expected to be a named list.
#'
#' @return A named list.
#'
#' @keywords internal
.format_input_R6 <- function(x) {
  if (is.null(x)) return(list())

  if (!is.list(x) || is.object(x)) {
    stop("In R6 initialize(), input must be a named list (and not an object).")
  }

  # allow empty list
  if (length(x) == 0L) return(x)

  nms <- names(x)
  if (is.null(nms) || anyNA(nms) || any(nms == "")) {
    stop("Put names to all elements.")
  }

  x
}
