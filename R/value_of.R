#' Extract field values from ABM objects
#'
#' Extract the value(s) of a specified field from ABM objects.
#' The output format is controlled by \code{return_as}. Optionally, apply a
#' post-processing function via \code{return_FUN}.
#'
#' @param G an `ABM_Game` object.
#' @param field_name A single non-empty character string. The name of the field
#'   to extract.
#' @param return_as Output format. One of \code{"list"}, \code{"vec"}, or
#'   \code{"df"}.
#' @param log Optional. Which log entry/entries to extract from.
#'   Can be a character vector of log names (i.e., \code{names(G$log)}) or a
#'   numeric vector of indices. If \code{NULL}, extraction uses the current
#'   state (not the log).
#' @param return_FUN An optional function applied to the extracted output.
#'   If provided, it is applied after \code{return_as} conversion.
#' @param ... Additional arguments passed to \code{return_FUN}.
#'
#' @details
#' The meaning of \code{return_as} is standardized across ABM classes:
#'
#' \itemize{
#' \item \code{"list"}: returns the extracted value(s) as a value or a named list.
#' \item \code{"vec"}: returns a (named) atomic vector only when all extracted
#'   values are length-1 atomic. Otherwise, an error is thrown.
#' \item \code{"df"}: returns a \code{data.frame}. For single-object extraction,
#'   a one-row \code{data.frame} is returned. For group/log extraction, a
#'   long-format \code{data.frame} with identifier columns is returned.
#' }
#'
#' Class-specific behavior:
#' \itemize{
#' \item \code{ABM_Game}: extracts from one of the following contexts depending on
#'   \code{group_name} and \code{log}:
#'   \enumerate{
#'     \item current state (\code{group_name = NULL}, \code{log = NULL})
#'     \item state log (\code{group_name = NULL}, \code{log != NULL})
#'   }
#' }
#'
#' @return
#' Depends on the value of \code{return_as}:
#'
#' \itemize{
#' \item \code{"list"}: a value or a named list.
#' \item \code{"vec"}: a (named) atomic vector (or a list of such vectors for
#'   group-log extraction in \code{ABM_Game}).
#' \item \code{"df"}: a \code{data.frame}.
#' }
#'
#' @export
#'
#' @examples
#' # G <- Game()
#' # value_of(G, "time", return_as = "vec")
#' # value_of(G, "a", group_name = "group1", return_as = "df")

#===============================================================================
# ABM_Game
#===============================================================================
#' @export
value_of <- function(G,
                     field_name,
                     return_as = c("list", "vec", "df"),
                     log = NULL,
                     fast = FALSE,
                     return_FUN = NULL, ...) {

  .validate_field_name(field_name)
  .validate_return_FUN(return_FUN)
  return_as <- match.arg(return_as)

  # log index
  log_idx <- .resolve_log_idx(G, log)

  # validate the fieldname
  fc <- G$.get_category()
  stopifnot("'field_name' does not exist in 'G'." = field_name %in% names(fc))

  #---------------------------------
  # dispatch
  #---------------------------------

  # (A) current stage
  if (is.null(log_idx)) {
    value <- G[[field_name]]
    out <- switch(
      return_as,
      "list" = value,
      "vec"  = .as_vec1(value),
      "df"   = .df_one(value, field_name)
    )
    if (!is.null(return_FUN)) out <- return_FUN(out, ...)
    return(out)
  }

  # (B) log
  if (!is.null(log_idx)) {
    value <- lapply(log_idx, function(t) G$log[[t]][[field_name]])
    names(value) <- names(G$log)[log_idx]
    time <- vapply(log_idx, function(t) G$log[[t]]$time, FUN.VALUE = numeric(1))

    out <- switch(
      return_as,
      "list" = value,
      "vec"  = {
        vec <- vapply(value, function(z) .as_vec1(z), FUN.VALUE = NA)
        names(vec) <- names(value)
        vec
      },
      "df"   = data.frame(
        time = time,
        setNames(list(I(value)), field_name),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    )
    if (!is.null(return_FUN)) out <- return_FUN(out, ...)
    return(out)
  }

  stop("Invalid combination of arguments.")
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



