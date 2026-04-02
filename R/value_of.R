#' Retrieve a field value from an ABM object
#'
#' Extract the value of a named field from an \code{ABM_Game} object, either
#' from its current state or from stored log entries. An optional
#' transformation function can be applied to the extracted value(s).
#'
#' @param G An \code{ABM_Game} object.
#' @param field_name A single character string naming the field to retrieve.
#'   Must be a valid field recognised by \code{G$.get_category()}.
#' @param log A specification of the log entries to retrieve. Either
#'   \code{NULL} (default, returns the current state), \code{"all"} (all log
#'   entries), a character vector matching names of \code{G$log}, or a numeric
#'   vector of positions within \code{G$log}. See \code{.resolve_log_idx()} for
#'   details.
#' @param return_FUN An optional function applied to the extracted value(s)
#'   before returning. When \code{log} is \code{NULL} it is called once as
#'   \code{return_FUN(value, ...)}; otherwise it is applied to each log entry
#'   individually via \code{lapply()}. The output type depends entirely on this
#'   function and is the caller's responsibility. Defaults to \code{NULL}
#'   (no transformation).
#' @param ... Additional arguments passed to \code{return_FUN}.
#'
#' @return
#' \itemize{
#'   \item If \code{log} is \code{NULL}: the value of \code{field_name} in the
#'     current state of \code{G}, optionally transformed by \code{return_FUN}.
#'     The type matches that of the field (or the output of \code{return_FUN}).
#'   \item If \code{log} is specified: a named list of values, one element per
#'     selected log entry, optionally transformed by \code{return_FUN}. Names
#'     correspond to the names of the selected entries in \code{G$log}.
#' }
#'
#' @details
#' The function dispatches on whether log entries are requested:
#' \itemize{
#'   \item \strong{Current state} (\code{log = NULL}): returns
#'     \code{G[[field_name]]} directly.
#'   \item \strong{Log entries}: extracts \code{field_name} from each selected
#'     entry in \code{G$log} and returns the results as a named list.
#' }
#' Input validation is performed by \code{.validate_field_name()},
#' \code{.validate_return_FUN()}, and \code{.resolve_log_idx()} before any
#' extraction takes place.
#'
#' @seealso \code{\link{.resolve_log_idx}}
#'
#' @examples
#' \dontrun{
#' # Current state
#' value_of(G, "agent_wealth")
#'
#' # All log entries
#' value_of(G, "agent_wealth", log = "all")
#'
#' # Specific log entries by name
#' value_of(G, "agent_wealth", log = c(1,3))
#'
#' # With a transformation function
#' value_of(G, "agent_wealth", log = "all", return_FUN = mean)
#' }
#'
#' @export
value_of <- function(G,
                     field_name,
                     log = NULL,
                     return_FUN = NULL, ...) {

  .validate_field_name(field_name)
  .validate_return_FUN(return_FUN)

  log_idx <- .resolve_log_idx(G, log)

  # (A) current stage
  if (is.null(log_idx)) {
    value <- G[[field_name]]
    if (!is.null(return_FUN)) value <- return_FUN(value, ...)
    return(value)
  }

  # (B) log
  value <- lapply(log_idx, function(t) G$log[[t]][[field_name]])
  names(value) <- names(G$log)[log_idx]
  if (!is.null(return_FUN)) value <- lapply(value, function(x) return_FUN(x, ...))
  return(value)
}


#===============================================================================
# helpers (internal)
#===============================================================================

.validate_return_FUN <- function(return_FUN) {
  if (!is.null(return_FUN)) {
    stopifnot("'return_FUN' must be a function." = is.function(return_FUN))
  }
  invisible(return_FUN)
}



# value -> must be length-1 atomic for vec mode
.as_vec1 <- function(v, where = "") {
  ok <- is.atomic(v) && length(v) == 1L && !is.null(v)
  if (!ok) {
    stop("Cannot coerce to 'vec': values must be length-1 atomic.", where)
  }
  v
}

# single value -> 1-row df
.df_one <- function(value, field_name) {
  data.frame(
    setNames(list(I(list(value))), field_name),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}



