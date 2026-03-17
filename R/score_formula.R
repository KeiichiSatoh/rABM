# =========================================================
# score_formula
# =========================================================

#' Calculate scores from a one-sided formula
#'
#' These functions evaluate a one-sided scoring formula by automatically
#' injecting input data into term functions.
#'
#' `calc_score_formula()` directly evaluates the formula once in the current
#' formula environment.
#'
#' `make_score_formula()` interprets the formula once and returns a reusable
#' evaluator for repeated use. In this case, objects referenced by the formula
#' other than `data` are frozen at creation time, while `data` itself is supplied
#' dynamically each time the returned function is called.
#'
#' Bare function symbols such as `recip` are interpreted as
#' `recip(data = data)`, while explicit calls such as
#' `recip(symmetric = TRUE)` are interpreted as
#' `recip(data = data, symmetric = TRUE)`.
#'
#' Ordinary variables such as `a` or `alpha` are left unchanged.
#' General functions listed in `exclude_funs` (e.g. `log`, `sum`, `mean`)
#' are not treated as term functions.
#'
#' @param formula A one-sided formula such as `~ 2 * recip + a`.
#' @param data Input data passed to term functions.
#' @param data_arg A single character string giving the argument name through
#'   which `data` is supplied to term functions.
#' @param exclude_funs A character vector of function names that should never
#'   be treated as term functions.
#'
#' @return
#' `calc_score_formula()` returns the evaluated score.
#'
#' `make_score_formula()` returns a function with argument `data` that evaluates
#' the compiled score formula.
#'
#' @details
#' `calc_score_formula()` is intended for one-off evaluation.
#'
#' `make_score_formula()` is intended for repeated evaluation, such as in
#' simulations. It freezes non-data objects referenced by the formula at the
#' time the evaluator is created. Thus, later changes to external variables
#' such as `alpha` do not affect an existing evaluator, whereas changes in the
#' supplied `data` are reflected each time the evaluator is called.
#'
#' @examples
#' recip <- function(data, symmetric = FALSE) {
#'   if (symmetric) {
#'     data + t(data)
#'   } else {
#'     t(data)
#'   }
#' }
#'
#' alpha <- 2
#' a <- 10
#' mat <- matrix(1:4, 2, 2)
#'
#' # one-off evaluation
#' calc_score_formula(~ 2 * recip + a, data = mat)
#' calc_score_formula(~ alpha * recip + 2, data = mat)
#' calc_score_formula(~ 2 * recip(symmetric = TRUE), data = mat)
#' calc_score_formula(~ log(alpha * recip + 1), data = mat)
#'
#' # reusable evaluator
#' f_score <- make_score_formula(~ alpha * recip + 2)
#' f_score(mat)
#'
#' # later changes to alpha do not affect the existing evaluator
#' alpha <- 10
#' f_score(mat)
#'
#' @name score_formula
NULL


# =========================================================
# calc_score_formula
# =========================================================

#' @rdname score_formula
#' @export
calc_score_formula <- function(formula,
                               data,
                               data_arg = "data",
                               exclude_funs = c(
                                 "+", "-", "*", "/", "^", "%%", "%/%", "%*%",
                                 "&", "|", "&&", "||", "!",
                                 "<", "<=", ">", ">=", "==", "!=",
                                 "(", "[", "[[", "{",
                                 "c", "list", "seq", "rep",
                                 "sum", "mean", "min", "max",
                                 "abs", "sqrt", "log", "exp",
                                 "round", "floor", "ceiling",
                                 "ifelse"
                               )) {
  expr <- .interpret_formula(
    formula = formula,
    data_arg = data_arg,
    env = environment(formula),
    exclude_funs = exclude_funs
  )

  eval_env <- environment(formula)
  if (is.null(eval_env)) {
    eval_env <- parent.frame()
  }

  eval(
    expr,
    envir = setNames(list(data), data_arg),
    enclos = eval_env
  )
}


# =========================================================
# .collect_formula_symbols
# =========================================================

#' Collect symbol names appearing in an expression
#'
#' Internal helper used by `make_score_formula()` to identify external
#' objects that should be frozen into a private evaluation environment.
#'
#' @param x An R expression.
#'
#' @return A character vector of unique symbol names.
#' @noRd
.collect_formula_symbols <- function(x) {
  unique(all.names(x, functions = TRUE))
}


# =========================================================
# make_score_formula
# =========================================================

#' @rdname score_formula
#' @export
make_score_formula <- function(formula,
                               data_arg = "data",
                               exclude_funs = c(
                                 "+", "-", "*", "/", "^", "%%", "%/%", "%*%",
                                 "&", "|", "&&", "||", "!",
                                 "<", "<=", ">", ">=", "==", "!=",
                                 "(", "[", "[[", "{",
                                 "c", "list", "seq", "rep",
                                 "sum", "mean", "min", "max",
                                 "abs", "sqrt", "log", "exp",
                                 "round", "floor", "ceiling",
                                 "ifelse"
                               )) {
  expr <- .interpret_formula(
    formula = formula,
    data_arg = data_arg,
    env = environment(formula),
    exclude_funs = exclude_funs
  )

  eval_env <- environment(formula)
  if (is.null(eval_env)) {
    eval_env <- parent.frame()
  }

  symbol_names <- .collect_formula_symbols(expr)
  symbol_names <- setdiff(
    symbol_names,
    c(
      data_arg,
      "TRUE", "FALSE", "NULL", "NA", "NaN", "Inf", "...",
      exclude_funs
    )
  )

  frozen_env <- new.env(parent = baseenv())

  for (nm in symbol_names) {
    if (exists(nm, envir = eval_env, inherits = TRUE)) {
      frozen_env[[nm]] <- get(nm, envir = eval_env, inherits = TRUE)
    }
  }

  function(data) {
    data_env <- new.env(parent = frozen_env)
    data_env[[data_arg]] <- data
    eval(expr, envir = data_env)
  }
}
