#-------------------------------------------------------------------------------
# Field constructors (S3)
#-------------------------------------------------------------------------------

#' Field constructors for rABM objects
#'
#' The helper constructors \code{State()}, \code{Active()}, \code{Act()},
#' \code{Stop()}, \code{Report()}, and \code{Plot()} create an
#' \code{"ABM_Field"} object that stores a value, its name, and a category label.
#'
#' \code{ABM_Field()} and the internal helpers are intended for internal use.
#' Users should create fields using one of the constructors listed above.
#'
#' @details
#' \itemize{
#'   \item \code{State()}, \code{Active()}, \code{Act()}, \code{Stop()},
#'   \code{Report()}, and \code{Plot()} each accept \code{name = NULL}
#'   (the default) or an explicit \code{name}.
#'   \item If \code{name} is \code{NULL}, the field name is inferred from the
#'   expression passed to \code{x} (via \code{substitute(x)}) at the
#'   top-level helper (so it works for \code{State(y)} etc.).
#'   \item For function categories (\code{Active()}, \code{Act()},
#'   \code{Stop()}, \code{Report()}, \code{Plot()}), a call input like
#'   \code{fun(a = 3)} is allowed: it is converted into a new function whose
#'   default arguments are updated according to the call (partial application
#'   by updating defaults, not by fixing values).
#'   \item \code{State()} does not accept call inputs when \code{name} is
#'   \code{NULL}.
#'   \item For \code{Active()}, a call-input default (e.g.
#'   \code{Active(fn(v = 1))}) only affects arguments that are not used to
#'   detect reads vs. writes. If the underlying function distinguishes reads
#'   from writes via that argument (typically with \code{missing()}),
#'   supplying a call-input default for it has no effect.
#' }
#'
#' Each constructor stores its field under a fixed category label:
#' \code{State()} uses \code{"state"}, \code{Active()} uses
#' \code{"active_state"}, and \code{Act()}, \code{Stop()}, \code{Report()},
#' \code{Plot()} use \code{"act_FUN"}, \code{"stop_FUN"}, \code{"report_FUN"},
#' \code{"plot_FUN"} respectively.
#'
#' \code{"active_state"} is named after its access pattern (accessed like a
#' state, via \code{self$x}, without parentheses) rather than following the
#' \code{"_FUN"} suffix pattern used by the other function categories,
#' because it is implemented as an R6 active binding rather than a callable
#' method.
#'
#' @param x A value (for \code{State()}) or a function (for \code{Active()},
#'   \code{Act()}, \code{Stop()}, \code{Report()}, \code{Plot()}) to be
#'   stored in the field.
#' @param name An optional single character string giving the field name.
#'   If \code{NULL} (the default), the name is inferred from \code{x}.
#'
#' @return An object of class \code{"ABM_Field"} (a list with elements
#' \code{value}, \code{name}, and \code{category}).
#'
#' @examples
#' # State (name inferred from the object name)
#' y <- 1
#' State(y)            # name = "y"
#' State(1, name="y")  # literal allowed when name is provided
#'
#' # Function fields
#' get_older <- function(){self$age <- self$age + 1}
#' Act(get_older)
#'
#' get_older2 <- function(a = 1){self$age <- self$age + a}        # with arguments
#' Act(get_older2(a = 2))
#'
#' Act(function(){self$age <- self$age + 1}, name = "get_older")  # anonymous OK with explicit name
#'
#' @name Field
#' @rdname Field
NULL

#-------------------------------------------------------------------------------
# Internal constructor
#-------------------------------------------------------------------------------

#' Construct an ABM_Field object (internal)
#'
#' @param x A value or function.
#' @param name Field name (single character string).
#' @param category One of \code{"state"}, \code{"active_state"}, \code{"act_FUN"},
#'   \code{"stop_FUN"}, \code{"report_FUN"}, or \code{"plot_FUN"}. See
#'   \link{Field} for the constructor that produces each category.
#'
#' @return An object of class \code{"ABM_Field"}.
#' @keywords internal
ABM_Field <- function(x, name, category = c("state", "active_state",
                                            "act_FUN", "stop_FUN",
                                            "plot_FUN", "report_FUN")) {
  category <- match.arg(category)

  structure(
    list(value = x, name = name, category = category),
    class = "ABM_Field"
  )
}

#-------------------------------------------------------------------------------
# Internal helpers
#-------------------------------------------------------------------------------

#' Validate a field name (internal)
#'
#' @param name A candidate field name.
#' @return A trimmed name (character scalar).
#' @keywords internal
.validate_name1 <- function(name) {
  stopifnot(
    "'name' must be a single character string." =
      is.character(name) && length(name) == 1L && !is.na(name)
  )
  name <- trimws(name)
  if (identical(name, "")) stop("'name' must not be empty.", call. = FALSE)
  name
}

#' Create an ABM_Field with validation (internal)
#'
#' @param x A value or function.
#' @param category Field category string.
#' @param must_be_function Logical scalar or \code{NULL}. If \code{TRUE}, \code{x}
#'   must be a function; if \code{FALSE}, \code{x} must not be a function.
#' @param label Label used in error messages (e.g. \code{"State()"},
#'   \code{"Active()"}). Every current caller passes this explicitly.
#' @param name Field name (required).
#'
#' @return An object of class \code{"ABM_Field"}.
#' @keywords internal
.make_field <- function(x, category,
                        must_be_function = NULL,
                        label,
                        name) {
  # validate function-ness
  if (!is.null(must_be_function)) {
    stopifnot(
      "'must_be_function' must be TRUE/FALSE or NULL." =
        is.logical(must_be_function) && length(must_be_function) == 1L
    )

    if (isTRUE(must_be_function) && !is.function(x)) {
      stop(sprintf("'x' must be a function for %s.", label), call. = FALSE)
    }
    if (identical(must_be_function, FALSE) && is.function(x)) {
      stop(sprintf("'x' must not be a function for %s.", label), call. = FALSE)
    }
  }

  nm <- .validate_name1(name)
  ABM_Field(x = x, name = nm, category = category)
}

#' Coerce a call like fun(a=3) into a function with updated defaults (internal)
#'
#' @param x_call A call object (e.g., \code{fun(a = 3)}).
#' @param envir Environment used to evaluate the call head (function object).
#' @return A list with elements \code{fun} and \code{name_from_call}.
#' @keywords internal
.coerce_call_to_FUN <- function(x_call, envir = parent.frame()) {
  stopifnot(is.call(x_call))

  f_expr <- x_call[[1L]]
  FUN <- eval(f_expr, envir = envir)
  stopifnot("Call head must evaluate to a function." = is.function(FUN))

  mc <- match.call(definition = FUN, call = x_call, expand.dots = FALSE)
  args <- as.list(mc)[-1L]  # drop function name

  fm <- formals(FUN)

  # override defaults by supplied args (keep expressions unevaluated)
  for (nm in names(args)) {
    if (!is.null(nm) && nzchar(nm) && (nm %in% names(fm))) {
      fm[[nm]] <- args[[nm]]
    }
  }

  out <- as.function(c(fm, body(FUN)))
  environment(out) <- environment(FUN)

  nm_call <- if (is.symbol(f_expr)) as.character(f_expr) else deparse(f_expr)

  list(fun = out, name_from_call = nm_call)
}

#' Resolve the field name from an expression (internal)
#'
#' Used when \code{name} is \code{NULL}: infers the field name from the
#' expression originally passed to \code{x} at the top-level constructor.
#' Call expressions (e.g. \code{fun(a = 3)}) are never resolved here; they
#' are handled upstream by \code{\link{.coerce_call_to_FUN}} before this
#' function is reached.
#'
#' @param x_sbs Expression from \code{substitute(x)} at the top-level constructor.
#' @param name Optional explicit name.
#' @return A character scalar name.
#' @keywords internal
.resolve_field_name <- function(x_sbs, name = NULL) {
  if (!is.null(name)) return(.validate_name1(name))

  if (is.symbol(x_sbs)) return(deparse(x_sbs))

  stop("'x' must be an object name when 'name' is NULL.", call. = FALSE)
}

#' Create a function-category field with call support (internal)
#'
#' @param x Input to the user-facing constructor.
#' @param x_sbs Expression from \code{substitute(x)} at the top level.
#' @param name Optional explicit name.
#' @param category Category string.
#' @param label Label used in error messages (e.g. \code{"Active()"}).
#' @param envir Environment used to evaluate the call head.
#' @return An \code{"ABM_Field"}.
#' @keywords internal
.make_FUN_field <- function(x, x_sbs, name = NULL,
                            category,
                            label,
                            envir = parent.frame()) {

  # A function literal (e.g. `function() 1` or `\(x) x`) parses as a call
  # whose head is the symbol `function` or `\`. This is NOT a call-input
  # like `fun(a = 3)` and must be treated as a plain function value, not
  # routed through .coerce_call_to_FUN().
  is_fn_literal <- is.call(x_sbs) &&
    is.symbol(x_sbs[[1L]]) &&
    as.character(x_sbs[[1L]]) %in% c("function", "\\")

  if (is.call(x_sbs) && !is_fn_literal) {
    out <- .coerce_call_to_FUN(x_sbs, envir = envir)
    x <- out$fun
    if (is.null(name)) name <- out$name_from_call
  } else {
    if (is.null(name)) {
      name <- .resolve_field_name(x_sbs, name = NULL)
    } else {
      name <- .validate_name1(name)
    }
  }

  .make_field(
    x = x,
    category = category,
    must_be_function = TRUE,
    label = label,
    name = name
  )
}

#-------------------------------------------------------------------------------
# User-facing API
#-------------------------------------------------------------------------------

#' @rdname Field
#' @export
State <- function(x, name = NULL) {
  x_sbs <- substitute(x)

  # State: call is NOT allowed when name is NULL (and usually nonsensical)
  if (is.null(name) && is.call(x_sbs)) {
    stop("'x' must not be a call for State() when 'name' is NULL.", call. = FALSE)
  }

  nm <- .resolve_field_name(x_sbs, name = name)

  .make_field(
    x = x,
    category = "state",
    must_be_function = FALSE,
    label = "State()",
    name = nm
  )
}

#' @rdname Field
#' @export
Active <- function(x, name = NULL) {
  x_sbs <- substitute(x)
  .make_FUN_field(x, x_sbs, name = name, category = "active_state", label = "Active()",
                  envir = parent.frame())
}

#' @rdname Field
#' @export
Act <- function(x, name = NULL) {
  x_sbs <- substitute(x)
  .make_FUN_field(x, x_sbs, name = name, category = "act_FUN", label = "Act()",
                  envir = parent.frame())
}

#' @rdname Field
#' @export
Stop <- function(x, name = NULL) {
  x_sbs <- substitute(x)
  .make_FUN_field(x, x_sbs, name = name, category = "stop_FUN", label = "Stop()",
                  envir = parent.frame())
}

#' @rdname Field
#' @export
Report <- function(x, name = NULL) {
  x_sbs <- substitute(x)
  .make_FUN_field(x, x_sbs, name = name, category = "report_FUN", label = "Report()",
                  envir = parent.frame())
}

#' @rdname Field
#' @export
Plot <- function(x, name = NULL) {
  x_sbs <- substitute(x)
  .make_FUN_field(x, x_sbs, name = name, category = "plot_FUN", label = "Plot()",
                  envir = parent.frame())
}


#===============================================================================
# Print method (S3)
#===============================================================================

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
  cat("<Field: ", x$category, ">\n", sep = "")
  cat("$", x$name, "\n", sep = "")
  print(x$value)
  invisible(x)
}
