#===============================================================================
# utils-collection.R
# Internal helpers shared by value_of.list() and value_of_log.ABM_Game() for
# extracting a named field from a collection of entries.
#===============================================================================
# Common extraction logic: given a named collection of entries, pull
# `field_name` out of the entries selected by `idx`.
#
# Wrapping behaviour is controlled by `simplify`:
#   - `is_all = TRUE` (the caller requested "all" entries): ALWAYS returns a
#     named list, regardless of `simplify` and regardless of how many entries
#     that happens to be. This "all" contract is intentionally not affected
#     by `simplify`.
#   - `is_all = FALSE` and exactly one entry was explicitly selected: the raw
#     value is returned unwrapped only if `simplify = TRUE`; otherwise (the
#     default, `simplify = FALSE`) it is still wrapped in a named list of
#     length 1, so callers can rely on a consistent list-based return type
#     regardless of how many entries were requested.
#   - Two or more explicitly selected entries: always a named list.
.extract_from_collection <- function(entries, entry_names, field_name,
                                     idx, is_all, simplify = FALSE, return_FUN, ...,
                                     entry_label = "entry") {
  extract_one <- function(t) {
    entry <- entries[[t]]
    if (!field_name %in% names(entry)) {
      stop(
        "Field '", field_name, "' was not found in ", entry_label, " '",
        entry_names[t], "'."
      )
    }
    entry[[field_name]]
  }
  # single, explicitly-selected entry (not "all") with simplify = TRUE -> raw value
  if (!is_all && isTRUE(simplify) && length(idx) == 1L) {
    value <- extract_one(idx)
    if (!is.null(return_FUN)) value <- return_FUN(value, ...)
    return(value)
  }
  # "all", multiple entries, or simplify = FALSE -> named list
  value <- lapply(idx, extract_one)
  names(value) <- entry_names[idx]
  if (!is.null(return_FUN)) value <- lapply(value, function(x) return_FUN(x, ...))
  value
}
# Resolve a `which`/`log`-style selector (character "all", or a
# character/numeric vector) against a named collection, returning integer
# indices. Stops with an error if any requested entry cannot be found, or if
# a numeric selector contains non-whole numbers (e.g. 1.5).
.resolve_collection_idx <- function(x, selector, container_label = "'x'") {
  if (length(x) == 0L) {
    stop(container_label, " is empty: no entries are available.")
  }
  x_names <- names(x)
  if (is.null(x_names)) {
    stop(container_label, " has no names: cannot label the requested entries.")
  }
  is_all <- identical(selector, "all")
  if (is_all) {
    idx <- seq_along(x)
  } else if (is.numeric(selector)) {
    stopifnot(
      "'selector' must contain whole numbers." =
        all(selector == as.integer(selector))
    )
    idx <- as.integer(selector)
  } else if (is.character(selector)) {
    idx <- match(selector, x_names)
  } else {
    stop("Selector must be \"all\" or a character/numeric vector.")
  }
  missing <- is.na(idx) | idx < 1L | idx > length(x)
  if (any(missing)) {
    bad <- if (is.character(selector)) selector[missing] else idx[missing]
    stop(
      "The following entries were not found in ", container_label, ": ",
      paste(bad, collapse = ", ")
    )
  }
  list(idx = idx, names = x_names, is_all = is_all)
}
