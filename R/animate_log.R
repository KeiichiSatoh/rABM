#' Create an animated GIF from stored log snapshots
#'
#' Replays stored snapshots in \code{G$log} and generates an animated GIF by
#' calling \code{\link{plot.ABM_Game}} once per selected log entry.
#'
#' @param G An \code{ABM_Game} object.
#' @param name A field name in \code{G} indicating the \code{plot_FUN} to use
#'   for each frame (see \code{\link{plot.ABM_Game}}).
#' @param log Which log entries to replay. \code{NULL} (the default) uses all
#'   entries (\code{"all"}); otherwise this is passed directly to
#'   \code{\link{value_of_log}} (via \code{\link{plot.ABM_Game}}) and accepts
#'   the same forms: \code{"all"}, a character vector matching
#'   \code{names(G$log)}, or a numeric vector of positions.
#' @param delay Delay between frames in seconds. Passed to
#'   \code{gifski::save_gif()}.
#' @param width,height Width/height of the GIF in pixels.
#' @param res Resolution (ppi). Passed to \code{gifski::save_gif()}.
#' @param add_time_label Logical. If \code{TRUE}, add a subtitle indicating
#'   the \code{time} of each frame (via \code{graphics::title(sub = ...)}).
#' @param file Output GIF file path.
#' @param play Logical. If \code{TRUE}, open the resulting file after creation.
#' @param check Logical. If \code{TRUE} (the default), stop when the number of
#'   frames would exceed 500 (after thinning by \code{interval}), rather than
#'   silently building a very large GIF.
#' @param interval A positive integer. Use every \code{interval}-th selected
#'   frame (\code{1} = use all of them).
#' @param plot_args A list of additional arguments passed to
#'   \code{\link{plot.ABM_Game}} for each frame -- and from there, ultimately
#'   to the underlying \code{plot_FUN} named by \code{name}. Use this (rather
#'   than \code{...}) to pass arguments through to the plotting function.
#' @param ... Additional arguments passed to \code{gifski::save_gif()} (e.g.
#'   \code{loop}, \code{progress}). These do \strong{not} reach \code{plot_FUN};
#'   use \code{plot_args} for that.
#'
#' @return Invisibly returns \code{file}.
#'
#' @section Notes:
#' At replay time, only values stored in \code{G$log} are available via
#' \code{self} (see \code{\link{plot.ABM_Game}}). If your \code{plot_FUN}
#' requires additional fields, ensure those fields were saved into the log
#' when running the simulation (see \code{run_Game(fields_to_save = ...)}).
#'
#' @seealso \code{\link{plot.ABM_Game}}, \code{\link[gifski]{save_gif}}
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' wealth <- rep(10, 20)
#' grow_wealth <- function() {
#'   rate <- rnorm(length(self$wealth), mean = 0.01, sd = 0.05)
#'   self$wealth <- pmax(self$wealth * (1 + rate), 0.1)
#' }
#' # Fixed bin edges (rather than a target count) so every frame uses
#' # exactly the same bins -- otherwise hist() recomputes bin width from
#' # each frame's own data range, making bins drift across frames.
#' wealth_breaks <- seq(0, 40, by = 4)
#' plot_hist <- function() {
#'   graphics::hist(self$wealth, main = "wealth", xlim = c(0, 40),
#'                 breaks = wealth_breaks)
#' }
#'
#' G <- run_Game(
#'   Game(State(wealth), Act(grow_wealth), Plot(plot_hist)),
#'   plan  = c("grow_wealth"),
#'   times = 20
#' )
#'
#' animate_log(G, name = "plot_hist", add_time_label = TRUE, file = "wealth.gif")
#'
#' # Pass an argument through to plot_hist() itself -- still fixed bin
#' # edges, just resolved via plot_args instead of a closure variable.
#' plot_hist2 <- function(breaks = seq(0, 40, by = 4)) {
#'   graphics::hist(self$wealth, main = "wealth", xlim = c(0, 40), breaks = breaks)
#' }
#' G2 <- run_Game(
#'   Game(State(wealth), Act(grow_wealth), Plot(plot_hist2)),
#'   plan  = c("grow_wealth"),
#'   times = 20
#' )
#' animate_log(
#'   G2, name = "plot_hist2",
#'   plot_args = list(breaks = seq(0, 40, by = 2)),
#'   file = "wealth2.gif"
#' )
#' }
#' @export
animate_log <- function(G, name, log = NULL,
                        delay = 1,
                        width = 800, height = 600, res = 96,
                        add_time_label = FALSE,
                        file = "temp.gif",
                        play = TRUE,
                        check = TRUE,
                        interval = 1,
                        plot_args = list(),
                        ...) {
  # 'gifski' is in Suggests (not Imports); check availability first.
  .require_gifski()

  # input validation
  stopifnot(inherits(G, "ABM_Game"))
  .validate_field_name(name)
  .assert_plot_field(G, name)

  stopifnot("'plot_args' must be a list." = is.list(plot_args))

  if (is.null(log)) log <- "all"

  stopifnot(
    "'interval' must be a single positive integer." =
      is.numeric(interval) && length(interval) == 1L && !is.na(interval) &&
      interval >= 1 && (interval %% 1 == 0)
  )
  interval <- as.integer(interval)

  stopifnot("'file' must be a character of length 1." =
              is.character(file) && length(file) == 1L)

  # Resolve which entries to replay, and their 'time' labels, via
  # value_of_log() -- the same mechanism plot.ABM_Game() uses for log
  # replay. This also means: (a) requesting a nonexistent entry now errors
  # here rather than silently falling through, and (b) an entry lacking
  # '$time' now errors rather than silently falling back to its position.
  times_ls    <- value_of_log(G, field_name = "time", log = log, simplify = FALSE)
  entry_names <- names(times_ls)
  times       <- vapply(times_ls, function(v) v, numeric(1))

  # thinning (stable: keep every 'interval'-th selected entry, in order;
  # duplicate entries in 'log' are allowed and each renders its own frame)
  keep_idx <- seq(1L, length(entry_names), by = interval)

  if (isTRUE(check) && length(keep_idx) > 500L) {
    stop(
      "The length of log is longer than 500 (", length(keep_idx), "). ",
      "Set 'interval' larger (e.g., 2, 5, 10) or set 'check = FALSE'."
    )
  }

  entry_names <- entry_names[keep_idx]
  times       <- times[keep_idx]

  gifski::save_gif(
    expr = {
      for (i in seq_along(entry_names)) {
        # Delegate frame rendering entirely to plot.ABM_Game(): field
        # validation, self-binding to the log entry, and single-entry log
        # resolution are all handled there (see plot.ABM_Game.R).
        do.call(
          plot.ABM_Game,
          c(
            list(x = G, name = name, log = entry_names[i],
                 verbose = FALSE, pause = FALSE),
            plot_args
          )
        )

        if (isTRUE(add_time_label)) {
          graphics::title(sub = paste0("t = ", times[i]))
        }
      }
    },
    gif_file = file,
    width = width,
    height = height,
    res = res,
    delay = delay,
    ...
  )

  if (isTRUE(play)) {
    .browse_gif(file)
  }

  invisible(file)
}

#' Check that the 'gifski' package is available (internal)
#'
#' Thin wrapper around \code{requireNamespace("gifski", quietly = TRUE)},
#' extracted purely so the "package not installed" path can be mocked in
#' tests without requiring an environment where 'gifski' is actually absent.
#'
#' @return \code{invisible(TRUE)} if 'gifski' is available; otherwise stops
#'   with an informative error.
#' @keywords internal
.require_gifski <- function() {
  if (!requireNamespace("gifski", quietly = TRUE)) {
    stop(
      "Package 'gifski' is required for animate_log(). ",
      "Install it with install.packages('gifski').",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Open the generated GIF in a browser, if requested (internal)
#'
#' Thin wrapper around \code{utils::browseURL()}, extracted purely so it can
#' be mocked in tests (\code{utils} is a base package and its bindings
#' cannot be mocked directly via \code{testthat::local_mocked_bindings()}).
#'
#' @param file Path to the GIF file to open.
#' @return \code{invisible(NULL)}.
#' @keywords internal
.browse_gif <- function(file) {
  utils::browseURL(normalizePath(file, winslash = "/", mustWork = FALSE))
  invisible(NULL)
}
