#' Retrieve field values from an object's log
#'
#' Generic function for extracting the values of a named field from the log
#' entries of an object. Methods are implemented per object class; see
#' \code{\link{value_of_log.ABM_Game}} for the \code{ABM_Game} method.
#'
#' @param G An object for which a \code{value_of_log} method is implemented
#'   (e.g. an \code{ABM_Game} object).
#' @param field_name A single character string naming the field to retrieve.
#' @param log A specification of which log entries to retrieve.
#'   Interpretation is method-specific; see the relevant method's
#'   documentation.
#' @param return_FUN An optional function applied to the extracted value(s)
#'   before returning.
#' @param simplify Logical. Whether a single, explicitly selected log entry
#'   should be unwrapped to its raw value rather than returned as a named
#'   list of length 1. Interpretation is method-specific; see the relevant
#'   method's documentation. Default \code{FALSE}.
#' @param ... Additional arguments passed to methods and/or \code{return_FUN}.
#'
#' @seealso \code{\link{value_of_log.ABM_Game}}, \code{\link{value_of}}
#'
#' @export
value_of_log <- function(G, field_name, log = "all", return_FUN = NULL,
                         simplify = FALSE, ...) {
  UseMethod("value_of_log")
}

#' @rdname value_of_log
#' @export
value_of_log.default <- function(G, field_name, log = "all", return_FUN = NULL,
                                 simplify = FALSE, ...) {
  stop(
    "'value_of_log()' is not implemented for objects of class '",
    paste(class(G), collapse = "/"), "'."
  )
}

#' Retrieve field values from an ABM_Game object's log
#'
#' Extract the value of a named field from one or more log entries stored in
#' \code{G$log}. An optional transformation function can be applied to the
#' extracted value(s).
#'
#' @param G An \code{ABM_Game} object.
#' @param field_name A single character string naming the field to retrieve.
#'   Must be a valid field recognised by \code{G$.get_category()}.
#' @param log A specification of the log entries to retrieve. Either
#'   \code{"all"} (default, all log entries, always returned as a list), a
#'   single character string or single numeric value naming/positioning one
#'   entry of \code{G$log}, or a character/numeric vector of length
#'   \code{>= 2} specifying multiple entries. Whether a single
#'   explicitly-named/positioned entry is unwrapped or still returned as a
#'   list of length 1 is controlled by \code{simplify} (see below); this has
#'   no effect when \code{log = "all"} or when \code{length(log) >= 2}, both
#'   of which are always returned as a named list.
#' @param return_FUN An optional function applied to the extracted value(s)
#'   before returning. When the result is unwrapped (a single log entry with
#'   \code{simplify = TRUE}), \code{return_FUN} is called once as
#'   \code{return_FUN(value, ...)}. Otherwise, \code{return_FUN} is applied to
#'   each log entry individually via \code{lapply()}. The output type depends
#'   entirely on this function and is the caller's responsibility. Defaults
#'   to \code{NULL} (no transformation).
#' @param simplify Logical. Controls how a \strong{single}, explicitly
#'   selected log entry (i.e. \code{log} is a length-1 character/numeric
#'   value other than \code{"all"}) is returned. If \code{FALSE} (the
#'   default), the result is still a named list of length 1, so the return
#'   type is always a list regardless of how many log entries were
#'   requested -- this is the safer default, since it avoids silently
#'   mis-indexing downstream code (e.g. \code{result[[1]]}) that assumes a
#'   list, which is especially easy to get wrong when the extracted field is
#'   itself list-like (such as a \code{data.frame}, since \code{is.list()}
#'   is \code{TRUE} for data frames too). If \code{TRUE}, a single selected
#'   log entry is unwrapped and the raw value is returned directly, which
#'   can be more convenient for interactive, one-off lookups. Has no effect
#'   when \code{log = "all"} or when \code{length(log) >= 2}: both of those
#'   cases always return a named list irrespective of \code{simplify}.
#' @param ... Additional arguments passed to \code{return_FUN}.
#'
#' @return
#' \itemize{
#'   \item If \code{log} identifies a single entry (a single character name
#'     or a single numeric position, other than \code{"all"}) and
#'     \code{simplify = TRUE}: the raw value of \code{field_name} extracted
#'     from that log entry, optionally transformed by \code{return_FUN}, not
#'     wrapped in a list.
#'   \item Otherwise (\code{log = "all"}; a character/numeric vector of
#'     length \code{>= 2}; or a single log entry with \code{simplify =
#'     FALSE}, the default): a named list of values, one element per
#'     selected log entry, optionally transformed by \code{return_FUN}.
#'     Names correspond to the names of the selected entries in \code{G$log}.
#' }
#'
#' @details
#' \itemize{
#'   \item \strong{\code{log = "all"}}: always returns a named list,
#'     regardless of how many entries \code{G$log} contains and regardless
#'     of \code{simplify}.
#'   \item \strong{Single log entry, \code{simplify = TRUE}} (\code{log} is a
#'     length-1 character or numeric value, and is not \code{"all"}):
#'     returns the extracted value directly, unwrapped, for convenience.
#'   \item \strong{Single log entry, \code{simplify = FALSE}} (the default):
#'     returns a named list of length 1, so that the return type is uniform
#'     regardless of how many entries were requested.
#'   \item \strong{Multiple log entries} (\code{log = "all"}, or a
#'     character/numeric vector of length \code{>= 2}): extracts
#'     \code{field_name} from each selected entry in \code{G$log} and returns
#'     the results as a named list, irrespective of \code{simplify}.
#' }
#' Input validation is performed by \code{.validate_field_name()} and
#' \code{.validate_return_FUN()} before extraction takes place. In addition,
#' if \code{G$log} does not exist (or is \code{NULL}), if \code{G$log} has no
#' names, or if any requested log entry (or the requested field within it) is
#' not present in \code{G$log}, the function stops with an error rather than
#' silently returning \code{NULL} or \code{NA}-named results.
#'
#' For retrieving the \emph{current} value of a field (i.e. not from the
#' log), use \code{\link{value_of}} instead.
#'
#' @examples
#' age <- c(1, 2, 3)
#' get_older <- function() { self$age <- self$age + 1 }
#' G <- run_Game(
#'   Game(State(age), Act(get_older)),
#'   plan = c("get_older"),
#'   times = 3
#' )
#'
#' # All log entries (always a named list)
#' value_of_log(G, "age")
#' value_of_log(G, "age", log = "all")
#'
#' # A single log entry: a named list of length 1 by default (simplify = FALSE)
#' value_of_log(G, "age", log = 2)
#' value_of_log(G, "age", log = "t2")
#'
#' # A single log entry, unwrapped to the raw value (simplify = TRUE)
#' value_of_log(G, "age", log = 2, simplify = TRUE)
#'
#' # Multiple specific log entries (named list; simplify has no effect here)
#' value_of_log(G, "age", log = c(1, 3))
#'
#' # With a transformation function
#' value_of_log(G, "age", log = "all", return_FUN = mean)
#'
#' @rdname value_of_log
#' @export
value_of_log.ABM_Game <- function(G, field_name, log = "all", return_FUN = NULL,
                                  simplify = FALSE, ...) {
  .validate_field_name(field_name)
  .validate_return_FUN(return_FUN)

  if (is.null(G$log)) {
    stop("'G$log' does not exist: no log entries are available.")
  }

  resolved <- .resolve_collection_idx(G$log, log, container_label = "'G$log'")

  .extract_from_collection(
    entries      = G$log,
    entry_names  = resolved$names,
    field_name   = field_name,
    idx          = resolved$idx,
    is_all       = resolved$is_all,
    simplify     = simplify,
    return_FUN   = return_FUN,
    ...,
    entry_label  = "log entry"
  )
}
