#' Create an animated GIF from stored log snapshots
#'
#' This function replays stored snapshots in \code{G$log} and generates an
#' animated GIF by repeatedly calling a plotting function (\code{plot_FUN})
#' stored in the \code{ABM_Game} object.
#'
#' The plotting function is executed with \code{G} and \code{E} from the current
#' game object, while a snapshot for each frame is injected via the \code{self}
#' argument.
#'
#' @param G An \code{ABM_Game} object.
#' @param name A field name in \code{G} indicating the plotting function to use.
#'   The field must be a function (typically categorized as \code{"plot_FUN"}).
#' @param log Which log entries to replay. This is passed to
#'   \code{.resolve_log_idx()} and can be a character vector (e.g.,
#'   \code{names(G$log)}) or a numeric index vector.
#' @param delay Delay between frames in seconds. Passed to \code{gifski::save_gif()}.
#' @param width,height Width/height of the GIF in pixels.
#' @param res Resolution (ppi). Passed to \code{gifski::save_gif()}.
#' @param add_time_label Logical. If \code{TRUE}, add a subtitle indicating time
#'   (uses \code{snapshot$time} if available; otherwise uses the log index).
#' @param file Output GIF file path.
#' @param play Logical. If \code{TRUE}, open the resulting file after creation.
#' @param check Logical. If \code{TRUE}, stop when the number of frames would
#'   exceed 500 (after thinning by \code{interval}).
#' @param interval A positive integer. Use every \code{interval}-th frame
#'   (1 = use all frames).
#' @param ... Additional arguments passed to the plotting function \code{name}.
#'
#' @return Invisibly returns \code{file}.
#'
#' @section Notes:
#' At replay time, only values stored in \code{G$log} are available via
#' \code{self}. If your plotting function requires additional fields, you must
#' ensure that those fields were saved into the log when running the simulation.
#'
#' @seealso \code{\link[gifski]{save_gif}}
#'
#' @examples
#' \dontrun{
#' G <- run_Game(G, plan = ..., save_log = TRUE)
#' animate_log(G, name = "plot_agents", log = NULL, file = "anim.gif")
#' }
animate_log <- function(G, name, log = NULL,
                        delay = 1,
                        width = 800, height = 600, res = 96,
                        add_time_label = FALSE,
                        file = "temp.gif",
                        play = TRUE,
                        check = TRUE,
                        interval = 1,
                        ...) {
  # input validation
  stopifnot(inherits(G, "ABM_Game"))
  .validate_field_name(name)
  if(is.null(log)){
    log <- 1:length(G$log)
  }
  log_idx <- .resolve_log_idx(x = G, log = log)

  stopifnot(
    "'interval' must be a single positive integer." =
      is.numeric(interval) && length(interval) == 1L && !is.na(interval) &&
      interval >= 1 && (interval %% 1 == 0)
  )
  interval <- as.integer(interval)

  stopifnot("'file' must be a character of length 1." =
              is.character(file) && length(file) == 1L)

  if (!requireNamespace("gifski", quietly = TRUE)) {
    stop("Package 'gifski' is required. Install it with install.packages('gifski').")
  }

  # validate field name and category
  FUN <- G[[name]]
  fc <- G$.get_category()
  stopifnot(
    "'name' must indicate a 'plot_FUN' function in G." =
      (name %in% names(fc)) &&
      identical(unname(fc[[name]]), "plot_FUN") &&
      is.function(FUN)
  )

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


  # decide which frames to use (stable thinning)
  log_idx <- as.integer(log_idx)
  log_idx <- log_idx[!is.na(log_idx)]
  log_idx <- unique(log_idx)

  if (isTRUE(check)) {
    log_idx2 <- log_idx[seq(1L, length(log_idx), by = interval)]
    if (length(log_idx2) > 500L) {
      stop(
        "The length of log is longer than 500 (", length(log_idx2), "). ",
        "Set 'interval' larger (e.g., 2, 5, 10) or set 'check = FALSE'."
      )
    }
    log_idx <- log_idx2
  } else if (interval > 1L) {
    log_idx <- log_idx[seq(1L, length(log_idx), by = interval)]
  }

  # precompute time labels if needed (robust)
  times <- lapply(log_idx, function(i) {
    s <- G$log[[i]]
    if (!is.null(s$time)) s$time else i
  })

  gifski::save_gif(
    expr = {
      for (i in seq_along(log_idx)) {
        k <- log_idx[i]

        plot_log(
          self = G$log[[k]]
        )

        if (isTRUE(add_time_label)) {
          graphics::title(sub = paste0("t = ", times[[i]]))
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
    utils::browseURL(normalizePath(file, winslash = "/", mustWork = FALSE))
  }

  invisible(file)
}
