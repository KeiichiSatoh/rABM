#' Normalize user input into a named list
#'
#' Internal utility function used to normalize user inputs for API functions
#' such as [`Game()`].
#'
#' - A single object is wrapped into a list.
#' - Unnamed single objects may have their name inferred from the call.
#' - Unnamed multiple inputs are automatically named using a prefix.
#'
#' @param x An input object to be normalized.
#' @param expr The unevaluated expression corresponding to `x`
#' (typically obtained via `match.call()`).
#' @param prefix A prefix used to generate names when automatic naming is required.
#'
#' @return A named list.
#'
#' @keywords internal
.format_input <- function(x, expr, prefix = "Z") {
  if (is.null(x)) return(NULL)

  # normalize to list
  if (is.data.frame(x)) {
    x_list <- list(x)
  } else if (is.list(x) && !is.object(x)) {
    x_list <- x
  } else {
    x_list <- list(x)
  }

  # name check / fill
  x_names <- names(x_list)
  if (!is.null(x_names)) {
    if (anyNA(x_names) || any(x_names == "")) {
      stop("Put names to all elements.")
    }
  } else if (length(x_list) == 1L && is.symbol(expr)) {
    nm <- deparse(expr)
    if (length(nm) > 1L) nm <- paste(nm, collapse = "")
    names(x_list) <- nm
  } else {
    names(x_list) <- paste0(prefix, seq_along(x_list))
  }
  x_list
}
