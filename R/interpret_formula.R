#' Interpret a one-sided formula by injecting a data argument
#'
#' Internal helper that rewrites a one-sided formula such as
#' `~ 2 * recip + a` into an expression where bare function symbols are
#' converted to function calls with a data argument, e.g.
#' `2 * recip(data = data) + a`.
#'
#' Explicit term calls are also supported. For example,
#' `~ 2 * recip(symmetric = TRUE)` is rewritten to
#' `2 * recip(data = data, symmetric = TRUE)`.
#'
#' A symbol or call head is interpreted as a term function only when it
#' resolves to a function in the supplied environment and is not listed in
#' `exclude_funs`. Ordinary variables such as `a` or `alpha` are left
#' unchanged.
#'
#' This helper is used internally by [calc_score_formula()] and
#' [make_score_formula()].
#'
#' @param formula A one-sided formula such as `~ 2 * recip + a`.
#' @param data_arg A single character string giving the name of the data
#'   argument to inject into term functions.
#' @param env Environment used to resolve symbols. Defaults to
#'   `environment(formula)`.
#' @param exclude_funs A character vector of function names that should never
#'   be treated as term functions.
#'
#' @return An expression object.
#'
#' @keywords internal
#' @noRd
.interpret_formula <- function(formula,
                               data_arg = "data",
                               env = environment(formula),
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
  stopifnot(
    "'formula' must be a one-sided formula such as ~ 2 * recip + a." =
      inherits(formula, "formula") && length(formula) == 2L,
    "'data_arg' must be a single character string." =
      is.character(data_arg) && length(data_arg) == 1L && !is.na(data_arg),
    "'exclude_funs' must be a character vector." =
      is.character(exclude_funs)
  )

  if (is.null(env)) {
    env <- parent.frame()
  }

  expr <- formula[[2L]]

  special_symbols <- c(
    "TRUE", "FALSE", "NULL", "NA", "NaN", "Inf", "...", data_arg
  )

  is_term_function <- function(x) {
    if (!is.symbol(x)) {
      return(FALSE)
    }

    nm <- as.character(x)

    if (nm %in% special_symbols || nm %in% exclude_funs) {
      return(FALSE)
    }

    obj <- tryCatch(
      get(nm, envir = env, inherits = TRUE),
      error = function(e) NULL
    )

    is.function(obj)
  }

  has_named_arg <- function(call_obj, arg_name) {
    nms <- names(as.list(call_obj))
    if (is.null(nms)) {
      return(FALSE)
    }
    arg_name %in% nms[-1L]
  }

  add_data_arg <- function(call_obj) {
    if (has_named_arg(call_obj, data_arg)) {
      return(call_obj)
    }

    parts <- as.list(call_obj)
    nms <- names(parts)

    out <- c(
      list(parts[[1L]]),
      setNames(list(as.name(data_arg)), data_arg),
      parts[-1L]
    )

    out_nms <- c(
      if (is.null(nms)) "" else nms[1L],
      data_arg,
      if (length(parts) >= 2L) {
        if (is.null(nms)) rep("", length(parts) - 1L) else nms[-1L]
      } else {
        character(0)
      }
    )

    names(out) <- out_nms
    as.call(out)
  }

  rewrite <- function(x, head = FALSE) {
    if (is.atomic(x) || is.null(x)) {
      return(x)
    }

    if (is.symbol(x)) {
      if (!head && is_term_function(x)) {
        return(as.call(
          setNames(
            list(x, as.name(data_arg)),
            c("", data_arg)
          )
        ))
      }
      return(x)
    }

    if (is.call(x)) {
      parts <- as.list(x)
      head0 <- parts[[1L]]

      parts[[1L]] <- rewrite(head0, head = TRUE)

      if (length(parts) >= 2L) {
        parts[-1L] <- lapply(parts[-1L], rewrite)
      }

      out <- as.call(parts)

      if (is_term_function(head0)) {
        out <- add_data_arg(out)
      }

      return(out)
    }

    x
  }

  rewrite(expr)
}
