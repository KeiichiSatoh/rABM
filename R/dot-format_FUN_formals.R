#' Normalize function formals for ABM execution
#'
#' This internal helper standardizes the formal arguments of a function
#' so that it explicitly takes \code{G} and \code{E} as the first arguments.
#'
#' If the input function already defines arguments named \code{G} or \code{E},
#' they are removed before prepending standardized \code{G} and \code{E}
#' symbols. The symbols are not evaluated here; they are expected to be
#' resolved in the function's enclosing environment at execution time.
#'
#' This utility is mainly used to ensure a consistent calling convention
#' for user-supplied or internally generated functions (e.g., \code{act_FUN},
#' \code{global_FUN}) within the ABM framework.
#'
#' @param fun A function whose formal arguments are to be reformatted.
#'
#' @return A function identical to \code{fun} except that its formals are
#'   modified to begin with \code{G} and \code{E}.
#'
#' @details
#' The function operates as follows:
#' \enumerate{
#'   \item Extract existing formals using \code{formals()}.
#'   \item Remove arguments named \code{G} or \code{E} if present.
#'   \item Prepend \code{G} and \code{E} as symbolic arguments.
#' }
#'
#' No validation of the function body is performed.
#'
#' @keywords internal

.format_FUN_formals = function(fun, include_self = FALSE) {
  f <- formals(fun)
  if (is.null(f)) f <- pairlist()

  # drop existing G/E (if any)
  nms <- names(f)

  # include self?
  if(isTRUE(include_self)){
    if (!is.null(nms)) {
      f <- f[!nms %in% c("self")]
    }
    formals(fun) <- c(alist(self = self), f)
  }

  fun
}
