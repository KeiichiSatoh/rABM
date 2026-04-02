#' Resolve log index for ABM objects
#'
#' Convert a user-specified log identifier into numeric indices referring to
#' elements of \code{x$log}. The \code{log} argument can be given as the
#' special string \code{"all"}, as character names of log entries, or as
#' numeric indices. This helper validates the input and returns the
#' corresponding numeric indices.
#'
#' @param x An object containing a \code{log} field (e.g., an \code{ABM_Game}
#'   object) whose logs are stored as a named list.
#' @param log A specification of the log entries to retrieve. One of:
#'   \itemize{
#'     \item \code{NULL} — returns \code{NULL}.
#'     \item \code{"all"} — returns indices for all log entries.
#'     \item A character vector matching names of \code{x$log}.
#'     \item A numeric vector of positions within \code{x$log}.
#'   }
#'
#' @return A numeric vector of indices corresponding to the specified log
#'   entries, or \code{NULL} if \code{log} is \code{NULL}.
#'
#' @details
#' This function is intended for internal use to standardize access to stored
#' logs in ABM-related objects. It performs the following checks in order:
#' \itemize{
#'   \item If \code{log} is \code{NULL}, returns \code{NULL} immediately.
#'   \item If \code{log} is \code{"all"}, returns all valid indices.
#'   \item If \code{log} is a character vector, each element must match a name
#'         in \code{x$log}; an error is raised for any unmatched name.
#'   \item If \code{log} is a numeric vector, all values must lie within
#'         \code{seq_len(length(x$log))}; an error is raised otherwise.
#'   \item Any other type raises an error.
#' }
#'
#' @keywords internal
#'
.resolve_log_idx <- function(x, log) {
  if (is.null(log)) return(NULL)

  if (is.character(log)) {
    if (identical(log, "all")) return(seq_len(length(x$log)))
    idx <- match(log, names(x$log))
    stopifnot("'log' contains names not found in 'x$log'." = !any(is.na(idx)))
    return(idx)
  }

  if (is.numeric(log)) {
    stopifnot("'log' contains indices outside the range of 'x$log'." =
                all(log %in% seq_len(length(x$log))))
    return(log)
  }

  stop("'log' must be NULL, \"all\", a character vector, or a numeric vector.")
}
