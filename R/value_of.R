#' Retrieve the current value of a field from an object
#'
#' Generic function for extracting the current value of a named field from an
#' object. Methods are implemented per object class; see
#' \code{\link{value_of.ABM_Game}} and \code{\link{value_of.list}}.
#'
#' @param x An object for which a \code{value_of} method is implemented (e.g.
#'   an \code{ABM_Game} object, or a named \code{list}).
#' @param field_name A single character string naming the field to retrieve.
#' @param return_FUN An optional function applied to the extracted value(s)
#'   before returning.
#' @param ... Additional arguments passed to methods and/or \code{return_FUN}.
#'
#' @details
#' \code{value_of()} has no concept of a log or history: it returns the
#' value(s) currently held by \code{x}. To retrieve values from an
#' \code{ABM_Game} object's log (i.e. across multiple past time points), use
#' \code{\link{value_of_log}} instead.
#'
#' @seealso \code{\link{value_of.ABM_Game}}, \code{\link{value_of.list}},
#'   \code{\link{value_of_log}}
#'
#' @export
value_of <- function(x, field_name, return_FUN = NULL, ...) {
  UseMethod("value_of")
}

#' @rdname value_of
#' @export
value_of.default <- function(x, field_name, return_FUN = NULL, ...) {
  stop(
    "'value_of()' is not implemented for objects of class '",
    paste(class(x), collapse = "/"), "'."
  )
}

#' Retrieve the current value of a field from an ABM_Game object
#'
#' Extract the value of a named field from the current state of an
#' \code{ABM_Game} object (i.e. \code{x[[field_name]]}). An optional
#' transformation function can be applied to the extracted value.
#'
#' @param x An \code{ABM_Game} object.
#' @param field_name A single character string naming the field to retrieve.
#'   Any field of \code{x} may be requested -- not only \code{"state"} /
#'   \code{"active_state"} fields registered via \code{x$.get_category()},
#'   but also public fields such as \code{"time"}, \code{"log"}, or
#'   \code{"notes"}. If \code{field_name} does not exist on \code{x} at all,
#'   a warning is issued and \code{NULL} is returned (see Details).
#' @param return_FUN An optional function applied to the extracted value
#'   before returning, called as \code{return_FUN(value, ...)}. The output
#'   type depends entirely on this function and is the caller's
#'   responsibility. Defaults to \code{NULL} (no transformation).
#' @param ... Additional arguments passed to \code{return_FUN}.
#'
#' @return The value of \code{field_name} in the current state of \code{x},
#'   optionally transformed by \code{return_FUN}. The type matches that of
#'   the field (or the output of \code{return_FUN}). If \code{field_name}
#'   does not exist on \code{x}, \code{NULL} is returned (with a warning).
#'
#' @details
#' This method always reflects the \emph{current} state of \code{x}; it does
#' not consult \code{x$log}. Input validation is performed by
#' \code{.validate_field_name()} and \code{.validate_return_FUN()} before
#' extraction takes place.
#'
#' Unlike \code{x$.get_category()}, which only lists \code{"state"} /
#' \code{"active_state"} / function-category fields explicitly registered on
#' the game, \code{value_of.ABM_Game()} allows retrieving \emph{any} field
#' present on \code{x} -- including public fields such as \code{x$time},
#' \code{x$log}, or \code{x$notes} that are not tracked by
#' \code{.get_category()}. If \code{field_name} does not correspond to any
#' field on \code{x} (checked via \code{exists(field_name, envir = x,
#' inherits = FALSE)}), a warning is raised and \code{NULL} is returned,
#' rather than silently returning \code{NULL} with no indication that the
#' field does not exist.
#'
#' To retrieve values from \code{x}'s log (i.e. from past time points), use
#' \code{\link{value_of_log}} instead.
#'
#' @seealso \code{\link{value_of_log.ABM_Game}}
#'
#' @examples
#' \dontrun{
#' value_of(G, "agent_wealth")
#' value_of(G, "agent_wealth", return_FUN = mean)
#' value_of(G, "log")   # any public field can be retrieved, not just state
#' value_of(G, "typo_field_name")  # warns and returns NULL
#' }
#'
#' @rdname value_of
#' @export
value_of.ABM_Game <- function(x, field_name, return_FUN = NULL, ...) {
  .validate_field_name(field_name)
  .validate_return_FUN(return_FUN)

  if (!exists(field_name, envir = x, inherits = FALSE)) {
    warning(
      sprintf("'%s' does not exist as a field of 'x'; returning NULL.", field_name),
      call. = FALSE
    )
  }

  value <- x[[field_name]]
  if (!is.null(return_FUN)) value <- return_FUN(value, ...)
  value
}

#' Retrieve field values from the entries of a list
#'
#' Extract the value of a named field from one or more entries of a list,
#' where each entry is itself a list containing \code{field_name} (e.g.
#' \code{list(t1 = list(a = 1), t2 = list(a = 2))}). An optional
#' transformation function can be applied to the extracted value(s).
#'
#' Unlike \code{\link{value_of.ABM_Game}}, \code{x} here is treated as a
#' collection of entries (analogous to \code{$log} for an \code{ABM_Game}),
#' not as a single object whose top-level elements are the fields. There is
#' no notion of a "current state" for a plain list, and the entry names
#' (e.g. \code{"t1"}, \code{"t2"}) carry no implied ordering or time
#' semantics; they are treated purely as labels.
#'
#' @param x A named \code{list} of entries, each itself containing
#'   \code{field_name}.
#' @param field_name A single character string naming the field to retrieve
#'   from each entry.
#' @param which A specification of which entries of \code{x} to retrieve
#'   from. Either \code{"all"} (default, all entries, always returned as a
#'   list), a single character string or single numeric value
#'   naming/positioning one entry of \code{x}, or a character/numeric vector
#'   of length \code{>= 2} specifying multiple entries. Whether a single
#'   explicitly-named/positioned entry is unwrapped or still returned as a
#'   list of length 1 is controlled by \code{simplify} (see below); this has
#'   no effect when \code{which = "all"} or when \code{length(which) >= 2},
#'   both of which are always returned as a named list.
#' @param return_FUN An optional function applied to the extracted value(s)
#'   before returning. When the result is unwrapped (a single entry with
#'   \code{simplify = TRUE}), \code{return_FUN} is called once as
#'   \code{return_FUN(value, ...)}. Otherwise, \code{return_FUN} is applied to
#'   each entry individually via \code{lapply()}. Defaults to \code{NULL} (no
#'   transformation).
#' @param simplify Logical. Controls how a \strong{single}, explicitly
#'   selected entry (i.e. \code{which} is a length-1 character/numeric value
#'   other than \code{"all"}) is returned. If \code{FALSE} (the default), the
#'   result is still a named list of length 1, so the return type is always a
#'   list regardless of how many entries were requested -- this is the safer
#'   default, since it avoids silently mis-indexing downstream code (e.g.
#'   \code{result[[1]]}) that assumes a list, which is especially easy to get
#'   wrong when the extracted field is itself list-like (such as a
#'   \code{data.frame}, since \code{is.list()} is \code{TRUE} for data
#'   frames too). If \code{TRUE}, a single selected entry is unwrapped and
#'   the raw value is returned directly, which can be more convenient for
#'   interactive, one-off lookups. Has no effect when \code{which = "all"} or
#'   when \code{length(which) >= 2}: both of those cases always return a
#'   named list irrespective of \code{simplify}.
#' @param ... Additional arguments passed to \code{return_FUN}.
#'
#' @return
#' \itemize{
#'   \item If \code{which} identifies a single entry (a single character
#'     name or a single numeric position, other than \code{"all"}) and
#'     \code{simplify = TRUE}: the raw value of \code{field_name} extracted
#'     from that entry, optionally transformed by \code{return_FUN}, not
#'     wrapped in a list.
#'   \item Otherwise (\code{which = "all"}; a character/numeric vector of
#'     length \code{>= 2}; or a single entry with \code{simplify = FALSE},
#'     the default): a named list of values, one element per selected entry,
#'     optionally transformed by \code{return_FUN}. Names correspond to the
#'     names of the selected entries in \code{x}.
#' }
#'
#' @details
#' \code{field_name} must be present in each selected entry; otherwise the
#' function stops with an error rather than silently returning \code{NULL}.
#' \code{which = "all"} always returns a list, even if \code{x} happens to
#' contain only one entry, since it represents a request for "all entries"
#' rather than a specific single entry; this is unaffected by \code{simplify}.
#'
#' @examples
#' \dontrun{
#' sample_list <- list(t1 = list(a = 1), t2 = list(a = 2), t3 = list(a = 3))
#'
#' # All entries (always a named list)
#' value_of(sample_list, "a")
#'
#' # A single entry: a named list of length 1 by default (simplify = FALSE)
#' value_of(sample_list, "a", which = "t2")
#' value_of(sample_list, "a", which = 2)
#'
#' # A single entry, unwrapped to the raw value (simplify = TRUE)
#' value_of(sample_list, "a", which = "t2", simplify = TRUE)
#'
#' # Multiple specific entries (named list; simplify has no effect here)
#' value_of(sample_list, "a", which = c("t1", "t3"))
#' }
#'
#' @rdname value_of
#' @export
value_of.list <- function(x, field_name, which = "all", return_FUN = NULL,
                          simplify = FALSE, ...) {
  .validate_return_FUN(return_FUN)

  resolved <- .resolve_collection_idx(x, which, container_label = "'x'")

  .extract_from_collection(
    entries      = x,
    entry_names  = resolved$names,
    field_name   = field_name,
    idx          = resolved$idx,
    is_all       = resolved$is_all,
    simplify     = simplify,
    return_FUN   = return_FUN,
    ...,
    entry_label  = "entry"
  )
}
