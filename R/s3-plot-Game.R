#' Plot an ABM_Game object (current state or log replay)
#'
#' Execute one or more \code{plot_FUN} functions stored in an \code{ABM_Game}
#' object. You can either plot the current state (\code{log = NULL}) or replay
#' plots over stored snapshots in \code{x$log}.
#'
#' @param x An \code{ABM_Game} object `G`.
#' @param name Name of a \code{plot_FUN} field in \code{x}. If \code{NULL}, all
#'   fields categorized as \code{"plot_FUN"} are executed sequentially.
#' @param log Which log entries to replay. If \code{NULL}, the plot is produced
#'   using the current state of \code{x}. If provided, it must be either a
#'   character vector matching \code{names(x$log)} or a numeric vector of indices
#'   within \code{seq_len(length(x$log))}.
#' @param ... Additional arguments passed to the underlying \code{plot_FUN}.
#'
#' @return \code{NULL} invisibly.
#'
#' @details
#' See \code{plot.ABM_Game()} for details.
#'
#' @method plot ABM_Game
#' @export
#'
#' @examples
#' \dontrun{
#' plot(G)
#' plot(G, name = "plot_map")
#' plot(G, name = "plot_map", log = 1:5)
#' plot(G, name = "plot_map", log = c("t1", "t5"))
#' }

plot.ABM_Game <- function(x, name = NULL, log = NULL, verbose = TRUE, ...) {

  # from log or current?
  if(is.null(log)){
    # name = NULL --> execute all plot_FUN
    if (is.null(name)) {
      fl <- x$.get_category()
      pl_names <- names(fl)[fl == "plot_FUN"]

      if (!length(pl_names)) return(invisible(NULL))

      for (nm in pl_names) {
        x[[nm]](...)
        graphics::title(sub = paste0("plot = ", nm))
        readline(prompt = "hit <ENTER> to proceed")
      }
      return(invisible(NULL))
    }

    # validation
    .validate_field_name(field_name = name)
    x[[name]](...)
    return(invisible(NULL))
  }

  # From log ===================================================

  # log replay
  log_idx <- .resolve_log_idx(x = x, log = log)

  # validation
  stopifnot("Provide 'name' for plotting from the 'log'" = !is.null(name))
  .validate_field_name(field_name = name)

  # create a wrapper that accepts 'self' (snapshot) and keeps FUN's environment
  FUN <- G[[name]]
  plot_log <- function() {}
  fm <- formals(FUN)

  if (!("self" %in% names(fm))) {
    fm <- c(alist(self = NULL), fm)  # add self with no default
  }

  formals(plot_log) <- fm
  body(plot_log) <- body(FUN)
  environment(plot_log) <- environment(FUN)

  times <- vapply(log_idx, function(t) x$log[[t]]$time, numeric(1))

  for (i in seq_along(log_idx)) {
    if(isTRUE(verbose)){
      cat("printing: ", i, "/", length(log_idx), "\n", sep = "")
    }
    t <- log_idx[i]
    plot_log(self = x$log[[t]], ...)
    graphics::title(sub = paste0("t = ", times[i]))
  }

  invisible(NULL)
}


#===============================================================================
# plot_Game
#===============================================================================

#' @rdname plot.ABM_Game
#' @export
plot_Game <- function(G, name = NULL, log = NULL, ...) {
  plot.ABM_Game(x = G, name = name, log = log, ...)
}
