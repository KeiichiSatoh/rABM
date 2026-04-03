#' Capture a Code Chunk Without Evaluating It
#'
#' @description
#' `Chunk()` is a helper function that captures an expression
#' without evaluating it. It is primarily designed for use with
#' `Series()`, where code blocks (chunks) are stored as `ABM_Chunk`
#' objects and executed later in a controlled environment.
#'
#' This allows users to write ordinary R code blocks using `{}` syntax,
#' while ensuring that the code is stored as a language object
#' (i.e., not evaluated immediately).
#'
#' @param expr An R expression. For single-line expressions, \code{\{\}} is
#'   optional — bare expressions are automatically wrapped internally.
#'   For multi-line expressions, \code{\{\}} is required; omitting it
#'   will result in a parse error.
#' @return A language object of class \code{ABM_Chunk} representing
#'   the captured expression.
#'
#' @details
#' The function internally uses `substitute()` to prevent evaluation,
#' and attaches the class \code{"ABM_Chunk"} to the result.
#' If the expression is not already wrapped in a \code{\{\}} block,
#' it is wrapped automatically. For example, `Chunk(x <- 1)` will be treated as `Chunk({x <- 1})`
#'
#' Note that multi-line expressions must always be wrapped in a \code{\{\}} block.
#' A bare expression spanning multiple lines will cause a parse error
#' before `Chunk()` is even called:
#'
#' ```
#' # This will cause a parse error — use {} instead
#' Chunk(
#'   x <- 1
#'   y <- x + 1
#' )
#'
#' # Correct
#' Chunk({
#'   x <- 1
#'   y <- x + 1
#' })
#' ```
#'
#' The class is used by `Series()` to verify that all arguments
#' have been properly wrapped with `Chunk()`.
#'
#' The returned object can later be evaluated using `eval()`
#' in a specified environment.
#'
#' @examples
#' # Create a chunk without executing it
#' ch <- Chunk({
#'   x <- 1
#'   y <- x * 2
#' })
#'
#' # {} is optional — bare expressions are automatically wrapped
#' ch2 <- Chunk(x <- 1)
#' identical(ch2, Chunk({ x <- 1 }))  # TRUE
#'
#' @seealso [Series()]
#' @export
Chunk <- function(expr) {
  result <- substitute(expr)

  # Wrap with {} if not already a {} block
  if (!is.call(result) || !identical(result[[1]], as.symbol("{"))) {
    result <- call("{", result)
  }

  class(result) <- c("ABM_Chunk", class(result))
  result
}


#' @rdname Chunk
#' @export
print.ABM_Chunk <- function(x, ...) {
  cat("<ABM_Chunk>\n")
  cat(deparse(x, width.cutoff = 60L), sep = "\n")
  invisible(x)
}
