#' Create an ABM game object
#'
#' `Game()` is the user-facing constructor for an [`ABM_Game`] object,
#' the core object of the **rABM** package.
#' It wraps the internal R6 class [`ABM_Game`] so that users do not need to interact
#' with R6 directly.
#'
#' @details
#' The `ABM_Game` object manages several field categories:
#' - `"state"`: non-function global fields (e.g., parameters, data objects)
#' - `"active_state"`: active bindings (functions evaluated on access)
#' - `"act_FUN"`, `"stop_FUN"`, `"report_FUN"`, `"plot_FUN"`:
#'   functions registered as model-level methods
#'
#' Field names must be unique across all categories.
#'
#' `Game()` internally calls `ABM_Game$new()` after normalizing inputs via
#' `.format_input()`.
#'
#' ## Notes on input
#' When providing inputs for each category, users are encouraged to supply a named list,
#' because the names are used as field names.
#' An exception is when a user supplies a single object; in that case, the name may be
#' inferred from the call and used as the field name.
#' As a safer option, users can always supply a named list explicitly.
#'
#' @param state Fields to be stored as `"state"`.
#' Functions are not allowed.
#' @param active_state Functions registered as active bindings (`"active_state"`).
#' @param act_FUN Functions registered as model-level methods (`"act_FUN"`).
#' @param stop_FUN Functions registered as model-level methods (`"stop_FUN"`).
#' @param report_FUN Functions registered as model-level methods (`"report_FUN"`).
#' @param plot_FUN Functions registered as model-level methods (`"plot_FUN"`).
#' @param time A positive integer time step. If `NULL`, the default (`1`) is used.
#' @param log A list of saved snapshots (default: `NULL`).
#' @param notes A list of notes (default: `NULL`).
#'
#' @return An [`ABM_Game`] object.
#'
#' @seealso [`ABM_Game`], [`as.Game()`]
#'
#' @export
#' @examples
#' # Basic example
#' stage1 <- 1
#' stage2 <- 2
#' gfun1 <- function() 1
#' gfun2 <- function() 2
#'
#' Game(
#'   stage = list(stage1 = stage1, stage2 = stage2),
#'   global_FUN = list(gfun1 = gfun1, gfun2 = gfun2)
#' )
#'
#' # A single object can be supplied directly
#' Game(stage = stage1, global_FUN = gfun1)
Game <- function(...,
                 time = NULL, log = NULL, notes = NULL){
  ABM_Game$new(...,
               time = time,
               log = log,
               notes = notes)
}

#' Coerce an object to an ABM game
#'
#' `as.Game()` coerces an object into an [`ABM_Game`] object.
#' If `x` is already an [`ABM_Game`], it is returned unchanged.
#' Otherwise, `x` is treated as `"stage"` and a new game object is created.
#'
#' @param x An object to be coerced into an [`ABM_Game`].
#' @param ... Reserved for future extensions.
#'
#' @return An [`ABM_Game`] object.
#'
#' @seealso [`Game()`], [`ABM_Game`]
#'
#' @export
as.Game <- function(x, ...) UseMethod("as.Game")

#' @export
as.Game.ABM_Game <- function(x, ...) x


#-------------------------
# internal: list -> ABM_Field list
#-------------------------
.fields_from_named_list <- function(x, category) {
  if (is.null(x)) return(list())
  if (is.list(x) && !is.object(x)) {
    if (!length(x)) return(list())
    nms <- names(x)
    if (is.null(nms) || anyNA(nms) || any(nms == "")) {
      stop("For category '", category, "', supply a *named* list.", call. = FALSE)
    }
    out <- Map(function(val, nm) ABM_Field(x = val, name = nm, category = category),
               x, nms)
    return(unname(out))
  }

  # single object (not list): cannot reliably infer name here
  stop("For category '", category, "', supply a *named* list (or use Field/State/etc.).",
       call. = FALSE)
}

#-------------------------
# internal: collect ABM_Field from ...
#-------------------------
.collect_fields_from_dots <- function(...) {
  dots <- list(...)
  if (!length(dots)) return(list())

  ok <- vapply(dots, inherits, logical(1), what = "ABM_Field")
  if (!all(ok)) {
    stop("All elements in '...' must be ABM_Field objects.", call. = FALSE)
  }
  dots
}

#' @export
as.Game.default <- function(
    x,
    ...,
    # legacy-style optional inputs (kept for convenience/backward-compat)
    state = NULL,
    active_state = NULL,
    act_FUN = NULL,
    stop_FUN = NULL,
    report_FUN = NULL,
    plot_FUN = NULL,
    time = NULL,
    log = NULL,
    notes = NULL
) {
  # Case 1: x itself is ABM_Field
  if (inherits(x, "ABM_Field")) {
    fields <- c(list(x), .collect_fields_from_dots(...))
    return(do.call(ABM_Game$new, c(fields, list(time = time, log = log, notes = notes))))
  }

  # Case 2: x is a list of ABM_Field
  if (is.list(x) && !is.object(x) && length(x) &&
      all(vapply(x, inherits, logical(1), what = "ABM_Field"))) {
    fields <- c(x, .collect_fields_from_dots(...))
    return(do.call(ABM_Game$new, c(fields, list(time = time, log = log, notes = notes))))
  }

  # Otherwise: treat x + legacy inputs as sources to build fields
  # x is interpreted as 'state' source if provided
  # - if x is NULL -> ignore
  # - if x is named list -> becomes state fields
  # - otherwise -> reject (name inference unavailable here)
  state_from_x <- NULL
  if (!missing(x) && !is.null(x)) {
    state_from_x <- x
  }

  fields <- c(
    .fields_from_named_list(state_from_x, "state"),
    .fields_from_named_list(state, "state"),
    .fields_from_named_list(active_state, "active_state"),
    .fields_from_named_list(act_FUN, "act_FUN"),
    .fields_from_named_list(stop_FUN, "stop_FUN"),
    .fields_from_named_list(report_FUN, "report_FUN"),
    .fields_from_named_list(plot_FUN, "plot_FUN"),
    .collect_fields_from_dots(...)
  )

  do.call(ABM_Game$new, c(fields, list(time = time, log = log, notes = notes)))
}

