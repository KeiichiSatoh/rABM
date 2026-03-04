#' Capture a Code Chunk Without Evaluating It
#'
#' @description
#' `chunk()` is a helper function that captures an expression
#' without evaluating it. It is primarily designed for use with
#' `ABM_Series()`, where code blocks (chunks) are stored and
#' executed later by `run_Series()` in a controlled environment.
#'
#' This allows users to write ordinary R code blocks using `{}` syntax,
#' while ensuring that the code is stored as a language object
#' (i.e., not evaluated immediately).
#'
#' @param expr An R expression, typically written as a `{}` block.
#' The expression is captured but not evaluated.
#'
#' @return A language object (call) representing the captured expression.
#'
#' @details
#' The function internally uses `substitute()` to prevent evaluation.
#' For example:
#'
#' ```
#' chunk({
#'   x <- 1
#'   y <- x * 2
#' })
#' ```
#'
#' stores the code block as a language object rather than executing it.
#'
#' The returned object can later be evaluated using `eval()`
#' in a specified environment.
#'
#' @examples
#' # Create a chunk without executing it
#' ch <- chunk({
#'   x <- 1
#'   y <- x * 2
#' })
#'
#' # Inspect structure
#' str(ch)
#'
#' # Evaluate later
#' env <- new.env()
#' eval(ch, envir = env)
#' env$x
#' env$y
#'
#' @seealso [ABM_Series()]
#' @export
chunk <- function(expr) {
  substitute(expr)
}
