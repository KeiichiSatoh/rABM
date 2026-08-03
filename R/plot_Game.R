#' Plot an ABM_Game object (current state or log replay)
#'
#' Execute one or more \code{plot_FUN} functions stored in an \code{ABM_Game}
#' object. You can either plot the current state (\code{log = NULL}) or replay
#' plots over stored snapshots in \code{x$log}.
#'
#' \code{plot_Game(G, ...)} is a plain-function alias for
#' \code{plot(G, ...)}: both call the exact same underlying implementation
#' (\code{plot.ABM_Game()}), with \code{plot_Game()}'s \code{G} argument
#' passed straight through as \code{plot.ABM_Game()}'s \code{x}. Use
#' whichever calling style you prefer; there is no difference in behavior.
#'
#' @param x An \code{ABM_Game} object.
#' @param name Name of a \code{plot_FUN} field in \code{x}. If \code{log} is
#'   \code{NULL} (the current state), \code{name} may also be \code{NULL}, in
#'   which case all fields categorized as \code{"plot_FUN"} are executed
#'   sequentially. If \code{log} is provided (log replay), \code{name} is
#'   required and cannot be \code{NULL} -- replaying every \code{plot_FUN}
#'   over every selected log entry is not supported, since the combined
#'   output can grow very large. In either mode, \code{name} (when supplied)
#'   must refer to a field registered under the \code{"plot_FUN"} category in
#'   \code{x} (see \code{x$.get_category()}); any other field name (including
#'   a valid field of a different category) raises an error.
#' @param log Which log entries to replay. If \code{NULL} (the default), the
#'   plot is produced using the current state of \code{x} (\code{name} may be
#'   \code{NULL} in this case). If provided, \code{name} is required, and
#'   \code{log} must be one of the forms accepted by \code{\link{value_of_log}}:
#'   \code{"all"}, a character vector matching \code{names(x$log)}, or a
#'   numeric vector of positions within \code{seq_len(length(x$log))}.
#'   Resolution of \code{log} (including validation of \code{x$log} and of
#'   the requested entries) is delegated entirely to
#'   \code{\link{value_of_log}}.
#' @param verbose Logical. If \code{TRUE} (the default), a message of the
#'   form \code{plotting: "<field_name>"} (current state) or
#'   \code{plotting: "<field_name>" on t = <time>} (log replay) is printed to
#'   the console before each plot is drawn. This information is written to
#'   the console rather than onto the plot itself (e.g. via
#'   \code{graphics::title()}), so it never collides with a subtitle the
#'   underlying \code{plot_FUN} may set on its own.
#' @param pause Logical. If \code{TRUE} (the default), execution stops after
#'   each plot and waits for \verb{<ENTER>} before drawing the next one,
#'   except after the last plot in the sequence, which is never followed by
#'   a pause. If \code{FALSE}, all plots are drawn back-to-back with no
#'   pausing. Applies uniformly to both multi-plot cases: running all
#'   \code{"plot_FUN"} fields on the current state (\code{log = NULL, name =
#'   NULL}), and replaying a single \code{plot_FUN} over multiple selected
#'   log entries. Has no effect when exactly one plot is produced (a single
#'   \code{name} on the current state, or \code{log} resolving to a single
#'   entry).
#' @param ... Additional arguments passed to the underlying \code{plot_FUN}.
#'
#' @return \code{NULL}, invisibly.
#'
#' @details
#' \itemize{
#'   \item \strong{Current state} (\code{log = NULL}): if \code{name} is also
#'   \code{NULL}, every field registered as \code{"plot_FUN"} in \code{x} is
#'   executed in turn; if \code{name} is supplied, only that one
#'   \code{plot_FUN} is executed directly on the current state of \code{x}.
#'   \item \strong{Log replay} (\code{log} supplied): \code{name} is
#'   required and \strong{cannot} be \code{NULL}, unlike the current-state
#'   case above -- there is intentionally no "all \code{plot_FUN} \eqn{\times}
#'   all log entries" mode, since for a non-trivial log this would produce a
#'   combinatorial explosion of plots. The \code{"time"} value of each
#'   selected log entry is retrieved via \code{value_of_log(x, "time", log =
#'   log)}, which also determines which entries of \code{x$log} are replayed
#'   and in what order. The \code{plot_FUN} named by \code{name} is then
#'   called once per selected entry, with \code{self} temporarily bound to
#'   that log entry (a snapshot list, not the live \code{x}) instead of
#'   \code{x} itself, so the function body's \code{self$...} references
#'   reflect the state at that logged time step.
#'   \item Whenever more than one plot is produced -- either the "all
#'   \code{plot_FUN}" case above, or log replay over more than one entry --
#'   \code{pause} controls whether execution stops between plots (see
#'   \code{pause}), and \code{verbose} controls whether a
#'   \code{plotting: "<field_name>"} / \code{plotting: "<field_name>" on t =
#'   <time>} message is printed to the console before each one (see
#'   \code{verbose}). These two options are independent of each other.
#'   \item Neither mode writes anything onto the plot device itself (e.g. no
#'   automatic subtitle); see \code{verbose} for the console-based progress
#'   messages instead.
#'   \item In both modes, \code{name} (when supplied) is validated to be a
#'   field registered under the \code{"plot_FUN"} category in \code{x};
#'   supplying a field of a different category, or a name not present in
#'   \code{x} at all, raises an error before any plotting is attempted.
#'   \item \code{plot(x, ...)} and \code{plot_Game(x, ...)} are two names for
#'   the same call: \code{plot_Game} exists only so the plotting method can
#'   be invoked as an ordinary function (useful e.g. inside pipes or when
#'   \code{plot()}'s S3 dispatch is inconvenient), and does not add or change
#'   any behavior.
#' }
#'
#' @seealso \code{\link{value_of_log}}, \code{\link{Plot}}
#'
#' @method plot ABM_Game
#' @export
#'
#' @examples
#' # A wealth model with no inter-agent interaction, where inequality still
#' # emerges purely from independent, multiplicative random growth rates
#' # (Gibrat's law), with a floor to keep wealth from going non-positive.
#' set.seed(1)
#' wealth <- rep(10, 20)
#' grow_wealth <- function() {
#'   rate <- rnorm(length(self$wealth), mean = 0.01, sd = 0.05)
#'   self$wealth <- pmax(self$wealth * (1 + rate), 0.1)
#' }
#' plot_hist    <- function() { graphics::hist(self$wealth, main = "wealth") }
#' plot_boxplot <- function() { graphics::boxplot(self$wealth, main = "wealth") }
#'
#' G <- run_Game(
#'   Game(State(wealth), Act(grow_wealth),
#'        Plot(plot_hist), Plot(plot_boxplot)),
#'   plan  = c("grow_wealth"),
#'   times = 20
#' )
#'
#' # Current state
#' plot(G, name = "plot_hist")
#'
#' # Replay without pausing between frames (safe to run non-interactively)
#' plot(G, name = "plot_hist", log = "all", pause = FALSE)
#'
#' # Suppress the console progress messages as well
#' plot(G, name = "plot_hist", log = "all", verbose = FALSE, pause = FALSE)
#'
#' # plot_Game() is the same call as plot(), just as a plain function
#' plot_Game(G, name = "plot_hist", log = "all", pause = FALSE)
#'
#' \dontrun{
#' # Interactive-only: pauses for <ENTER> between each plot
#' plot(G)                                  # runs all registered plot_FUN
#' plot(G, name = "plot_hist", log = "all") # pauses between log frames
#' }

plot.ABM_Game <- function(x, name = NULL, log = NULL, verbose = TRUE, pause = TRUE, ...) {

  # from log or current?
  if (is.null(log)) {
    # name = NULL --> execute all plot_FUN
    if (is.null(name)) {
      fl <- x$.get_category()
      pl_names <- names(fl)[fl == "plot_FUN"]

      if (!length(pl_names)) return(invisible(NULL))

      for (i in seq_along(pl_names)) {
        nm <- pl_names[i]
        if (isTRUE(verbose)) {
          cat(sprintf('plotting: "%s"\n', nm))
        }
        x[[nm]](...)
        if (isTRUE(pause) && i < length(pl_names)) {
          .pause_for_enter()
        }
      }
      return(invisible(NULL))
    }

    # validation
    .validate_field_name(field_name = name)
    .assert_plot_field(x, name)
    if (isTRUE(verbose)) {
      cat(sprintf('plotting: "%s"\n', name))
    }
    x[[name]](...)
    return(invisible(NULL))
  }

  # From log ===================================================

  # validation (fail fast, before doing any log resolution work)
  stopifnot(
    "'name' is required when 'log' is provided (only 'log = NULL', i.e. the current state, supports 'name = NULL' to run all 'plot_FUN' fields at once)." =
      !is.null(name)
  )
  .validate_field_name(field_name = name)
  .assert_plot_field(x, name)

  # log replay --------------------------------------------------------------
  # Resolve both "which entries" and "their time values" via value_of_log(),
  # instead of a separate .resolve_log_idx() call plus manual
  # x$log[[t]]$time indexing. value_of_log()'s index resolution
  # (.resolve_collection_idx()) follows the same NULL/"all"/character/numeric
  # contract as .resolve_log_idx() and preserves the order of the 'log'
  # argument, so log_idx derived from names(times_ls) refers to the same
  # entries in the same order .resolve_log_idx(x, log) would have returned.
  times_ls <- value_of_log(x, field_name = "time", log = log, simplify = FALSE)
  times    <- vapply(times_ls, function(v) v, numeric(1))
  log_idx  <- match(names(times_ls), names(x$log))

  # create a wrapper that accepts 'self' (snapshot) and keeps FUN's environment
  FUN <- x[[name]]
  plot_log <- function() {}
  fm <- formals(FUN)

  if (!("self" %in% names(fm))) {
    fm <- c(alist(self = NULL), fm)  # add self with no default
  }

  formals(plot_log) <- fm
  body(plot_log) <- body(FUN)
  environment(plot_log) <- environment(FUN)

  for (i in seq_along(log_idx)) {
    if (isTRUE(verbose)) {
      cat(sprintf('plotting: "%s" on t = %s\n', name, times[i]))
    }
    t <- log_idx[i]
    plot_log(self = x$log[[t]], ...)
    if (isTRUE(pause) && i < length(log_idx)) {
      .pause_for_enter()
    }
  }

  invisible(NULL)
}

#' Ensure a field name refers to a registered plot_FUN field (internal)
#'
#' @param x An \code{ABM_Game} object.
#' @param name A single field name to check.
#' @return \code{TRUE}, invisibly, if \code{name} is a registered
#'   \code{"plot_FUN"} field in \code{x}; otherwise stops with an error.
#' @keywords internal
.assert_plot_field <- function(x, name) {
  fl <- x$.get_category()
  if (!(name %in% names(fl)) || !identical(unname(fl[name]), "plot_FUN")) {
    stop(sprintf("'%s' is not a registered 'plot_FUN' field in 'x'.", name),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Prompt the user to press <ENTER> before proceeding (internal)
#'
#' Thin wrapper around \code{readline()}, extracted purely so it can be
#' mocked in tests. Base R functions such as \code{readline()} cannot be
#' mocked directly via \code{testthat::local_mocked_bindings()}, because
#' base's namespace is locked; wrapping it in an ordinary function defined
#' in this package gives tests something mockable.
#'
#' @param prompt Passed through to \code{readline()}.
#' @return \code{invisible(NULL)}.
#' @keywords internal
.pause_for_enter <- function(prompt = "hit <ENTER> to proceed") {
  readline(prompt = prompt)
  invisible(NULL)
}

#===============================================================================
# plot_Game
#===============================================================================

#' @rdname plot.ABM_Game
#' @export
plot_Game <- function(G, name = NULL, log = NULL, verbose = TRUE, pause = TRUE, ...) {
  plot.ABM_Game(x = G, name = name, log = log, verbose = verbose, pause = pause, ...)
}
