#' Resolve log index for ABM objects
#'
#' Convert a user-specified log identifier into numeric indices referring to
#' elements of \code{x$log}. The \code{log} argument can be given either as
#' character names of log entries or as numeric indices. This helper function
#' validates the input and returns the corresponding numeric indices.
#'
#' @param x An object that contains a \code{log} field (e.g., an \code{ABM_Game}
#'   object) where logs are stored as a named list.
#' @param log A specification of the log entries to retrieve. Either:
#'   \itemize{
#'     \item A character vector matching the names of \code{x$log}, or
#'     \item A numeric vector indicating positions within \code{x$log}.
#'   }
#'
#' @return A numeric vector of indices corresponding to the specified log entries.
#'   Returns \code{NULL} if \code{log} is \code{NULL}.
#'
#' @details
#' This function is intended for internal use to standardize access to stored logs
#' in ABM-related objects. It performs the following checks:
#' \itemize{
#'   \item If \code{log} is \code{NULL}, \code{NULL} is returned.
#'   \item If \code{log} is character, it must match existing names in \code{x$log}.
#'   \item If \code{log} is numeric, all values must fall within the valid index
#'   range of \code{x$log}.
#'   \item Otherwise, an error is thrown.
#' }
#'
#' @keywords internal
#'
.resolve_log_idx <- function(x, log) {
  if (is.null(log)) return(NULL)

  if (is.character(log)) {
    idx <- match(log, names(x$log))
    stopifnot("'log' does not match the existing 'log' names in 'x'." = !any(is.na(idx)))
    return(idx)
  }
  if (is.numeric(log)) {
    idx <- log
    stopifnot("The range of 'log' does not match the 'log' in 'x'." =
                all(idx %in% seq_len(length(x$log))))
    return(idx)
  }
  stop("The input 'log' must be either 'character' or 'numeric' type.")
}

.resolve_log_idx <- function(x, log) {
  if (is.null(log)) return(NULL)

  if (is.character(log)) {
    idx <- match(log, names(x$log))
    stopifnot("'log' does not match the existing 'log' names in 'x'." = !any(is.na(idx)))
    return(idx)
  }
  if (is.numeric(log)) {
    idx <- log
    stopifnot("The range of 'log' does not match the 'log' in 'x'." =
                all(idx %in% seq_len(length(x$log))))
    return(idx)
  }
  stop("The input 'log' must be either 'character' or 'numeric' type.")
}
