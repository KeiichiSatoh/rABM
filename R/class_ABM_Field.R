#-------------------------------------------------------------------------------
# Field constructors (S3)
#-------------------------------------------------------------------------------

#' Field constructors for rABM objects
#'
#' \code{Field()} and the helper constructors (\code{State()}, \code{Active()},
#' \code{Act()}, \code{Stop()}, \code{Report()}, \code{Plot()}) create an
#' \code{"ABM_Field"} object that stores a value, its name, and a category label.
#'
#' \code{ABM_Field()} and the internal helpers are intended for internal use.
#' Most users should use \code{Field()} or one of the helper constructors.
#'
#' @details
#' \itemize{
#'   \item \code{Field()} is a general constructor where you provide \code{name}
#'   explicitly.
#'   \item \code{State()}, \code{Active()}, \code{Act()}, \code{Stop()},
#'   \code{Report()}, and \code{Plot()} are convenience constructors that accept
#'   \code{name = NULL}.
#'   \item For helper constructors, if \code{name} is \code{NULL}, the field name
#'   is inferred from the expression passed to \code{x} (via \code{substitute(x)})
#'   at the top-level helper (so it works for \code{State(y)} etc.).
#'   \item For function categories (Active/Act/Stop/Report/Plot), a call input
#'   like \code{fun(a = 3)} is allowed: it is converted into a new function whose
#'   default arguments are updated according to the call (partial application by
#'   updating defaults, not by fixing values).
#'   \item \code{State()} does not accept call inputs when \code{name} is \code{NULL}.
#' }
#'
#' The category labels are currently:
#' \code{"state"}, \code{"active_state"}, \code{"act_FUN"}, \code{"stop_FUN"},
#' \code{"plot_FUN"}, and \code{"report_FUN"}.
#'
#' @param x A value or a function to be stored in the field.
#' @param name A single character string giving the field name.
#'   \code{Field()} requires \code{name}. For helper constructors,
#'   \code{name = NULL} will infer the name from \code{x}.
#' @param category A single character string specifying the field category.
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
#' fun <- function(a = 1, b = 2) c(a, b)
#' Plot(fun)           # name = "fun"
#' Plot(fun(a = 3))    # function(a = 3, b = 2) ...
#' Plot(function() 1, name = "my_plot")  # anonymous OK with explicit name
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
#' @param category Field category.
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
#' IMPORTANT: This helper does NOT infer names from \code{substitute(x)}.
#' Name inference must be done at the top-level user-facing constructors.
#'
#' @param x A value or function.
#' @param category Field category string.
#' @param must_be_function Logical scalar or \code{NULL}. If \code{TRUE}, \code{x}
#'   must be a function; if \code{FALSE}, \code{x} must not be a function.
#' @param label Label used in error messages (defaults to \code{category}).
#' @param name Field name (required).
#'
#' @return An object of class \code{"ABM_Field"}.
#' @keywords internal
.make_field <- function(x, category,
                        must_be_function = NULL,
                        label = NULL,
                        name) {
  if (is.null(label)) label <- category

  # validate function-ness
  if (!is.null(must_be_function)) {
    stopifnot(
      "'must_be_function' must be TRUE/FALSE or NULL." =
        is.logical(must_be_function) && length(must_be_function) == 1L
    )

    if (isTRUE(must_be_function) && !is.function(x)) {
      stop(sprintf("'x' must be a function for '%s'.", label), call. = FALSE)
    }
    if (identical(must_be_function, FALSE) && is.function(x)) {
      stop(sprintf("'x' must not be a function for '%s'.", label), call. = FALSE)
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
#' @param x_sbs Expression from \code{substitute(x)} at the top-level constructor.
#' @param name Optional explicit name.
#' @param allow_call Logical; whether call expressions are allowed for name inference.
#' @return A character scalar name.
#' @keywords internal
.resolve_field_name <- function(x_sbs, name = NULL, allow_call = FALSE) {
  if (!is.null(name)) return(.validate_name1(name))

  if (is.symbol(x_sbs)) return(deparse(x_sbs))

  if (isTRUE(allow_call) && is.call(x_sbs)) {
    f_expr <- x_sbs[[1L]]
    if (is.symbol(f_expr)) return(as.character(f_expr))
    return(deparse(f_expr))
  }

  stop("'x' must be an object name when 'name' is NULL.", call. = FALSE)
}

#' Create a function-category field with call support (internal)
#'
#' @param x Input to the user-facing constructor.
#' @param x_sbs Expression from \code{substitute(x)} at the top level.
#' @param name Optional explicit name.
#' @param category Category string.
#' @param label Label used in error messages.
#' @param envir Environment used to evaluate the call head.
#' @return An \code{"ABM_Field"}.
#' @keywords internal
.make_FUN_field <- function(x, x_sbs, name = NULL,
                            category,
                            label = category,
                            envir = parent.frame()) {

  if (is.call(x_sbs)) {
    out <- .coerce_call_to_FUN(x_sbs, envir = envir)
    x <- out$fun
    if (is.null(name)) name <- out$name_from_call
  } else {
    if (is.null(name)) {
      name <- .resolve_field_name(x_sbs, name = NULL, allow_call = FALSE)
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
Field <- function(x, name, category = c("state", "active_state",
                                        "act_FUN", "stop_FUN",
                                        "plot_FUN", "report_FUN")) {
  ABM_Field(x = x, name = .validate_name1(name), category = category)
}

#' @rdname Field
#' @export
State <- function(x, name = NULL) {
  x_sbs <- substitute(x)

  # State: call is NOT allowed when name is NULL (and usually nonsensical)
  if (is.null(name) && is.call(x_sbs)) {
    stop("'x' must not be a call for 'state' when 'name' is NULL.", call. = FALSE)
  }

  nm <- .resolve_field_name(x_sbs, name = name, allow_call = FALSE)

  .make_field(
    x = x,
    category = "state",
    must_be_function = FALSE,
    label = "state",
    name = nm
  )
}

#' @rdname Field
#' @export
Active <- function(x, name = NULL) {
  x_sbs <- substitute(x)
  .make_FUN_field(x, x_sbs, name = name, category = "active_state", label = "active_state",
                  envir = parent.frame())
}

#' @rdname Field
#' @export
Act <- function(x, name = NULL) {
  x_sbs <- substitute(x)
  .make_FUN_field(x, x_sbs, name = name, category = "act_FUN", label = "act_FUN",
                  envir = parent.frame())
}

#' @rdname Field
#' @export
Stop <- function(x, name = NULL) {
  x_sbs <- substitute(x)
  .make_FUN_field(x, x_sbs, name = name, category = "stop_FUN", label = "stop_FUN",
                  envir = parent.frame())
}

#' @rdname Field
#' @export
Report <- function(x, name = NULL) {
  x_sbs <- substitute(x)
  .make_FUN_field(x, x_sbs, name = name, category = "report_FUN", label = "report_FUN",
                  envir = parent.frame())
}

#' @rdname Field
#' @export
Plot <- function(x, name = NULL) {
  x_sbs <- substitute(x)
  .make_FUN_field(x, x_sbs, name = name, category = "plot_FUN", label = "plot_FUN",
                  envir = parent.frame())
}

