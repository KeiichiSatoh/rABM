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
#' \strong{Passing an existing \code{ABM_Chunk} back into \code{Chunk()}:}
#' if \code{expr} is a single bare variable reference (a symbol) whose value
#' is already an \code{ABM_Chunk} object (e.g. \code{step1 <- Chunk({x <- 1})}
#' followed by \code{Chunk(step1)}), that would normally produce a new,
#' unrelated chunk whose body is just \code{\{step1\}} — i.e. "evaluate the
#' variable \code{step1} at execution time" — rather than reusing
#' \code{step1}'s original body \code{\{x <- 1\}}. This is almost never what
#' is intended (the correct way to reuse an existing chunk is to pass it
#' directly, e.g. \code{Series(step1)}, without wrapping it in \code{Chunk()}
#' again), and left unchecked it can silently do the wrong thing whenever an
#' unrelated variable of the same name happens to exist. To guard against
#' this, \code{Chunk()} special-cases exactly this situation: when \code{expr}
#' is a single symbol and evaluating it (in the caller's environment) yields
#' an object already of class \code{"ABM_Chunk"}, that object is returned
#' as-is (idempotently), instead of being wrapped again. This lookup only
#' happens for a bare symbol, so it never evaluates — and therefore never
#' triggers side effects from — any other kind of expression, preserving the
#' "does not evaluate \code{expr}" contract for ordinary use.
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
#' # Passing an existing ABM_Chunk symbol back into Chunk() is a no-op:
#' # it returns the same object rather than wrapping it in a new chunk.
#' identical(Chunk(ch), ch)  # TRUE
#'
#' @seealso [Series()]
#' @export
Chunk <- function(expr) {
  result <- substitute(expr)

  # Guard: if 'expr' is a single bare symbol whose value is already an
  # ABM_Chunk, return it as-is instead of wrapping it again (see Details).
  # Evaluation is only attempted for a bare symbol, so this never triggers
  # side effects for any other kind of expression.
  if (is.symbol(result)) {
    val <- tryCatch(
      eval(result, envir = parent.frame()),
      error = function(e) NULL
    )
    if (inherits(val, "ABM_Chunk")) {
      return(val)
    }
  }

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
