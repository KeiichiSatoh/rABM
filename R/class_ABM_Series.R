#' Create an ABM_Series Object
#'
#' @description
#' `Series()` is a constructor for the `"ABM_Series"` class.
#' It stores a set of code chunks (as `ABM_Chunk` objects) and default objects
#' (values and/or functions) that will be used to initialize the execution
#' environment in `run_Series()`.
#'
#' This class is designed to support iterative ABM workflows where users
#' define a sequence of steps as `ABM_Chunk` objects, which are then
#' executed later in a controlled environment by `run_Series()`.
#'
#' @param ... Named or unnamed `ABM_Chunk` objects created by `Chunk()`.
#'   All arguments must be `ABM_Chunk` objects; passing other types will
#'   result in an error. Chunk names must be unique. Names are resolved as
#'   follows, in order:
#'   \enumerate{
#'     \item If an argument is passed with an explicit name (e.g.
#'     `Series(init = step1)`), that name is used.
#'     \item Otherwise, if the argument is a bare variable reference (e.g.
#'     `Series(step1)`), the variable name itself is used.
#'     \item Otherwise (e.g. an inline `Series(Chunk({...}))`, where there is
#'     no variable name to fall back on), an automatic name of the form
#'     `"Chunk1"`, `"Chunk2"`, ... is assigned, numbered in the order such
#'     arguments appear among `...` (not counting explicitly named or
#'     bare-variable arguments).
#'   }
#' @param default An optional list of default objects (values and/or functions)
#'   to be injected into the execution environment before running chunks.
#'   These can be overridden at runtime via `run_Series(..., input = ...)`.
#'   If `NULL` (the default), an empty default list is stored. If supplied,
#'   `default` must be a plain list with every element explicitly named
#'   (e.g. `default = list(add2 = function(a, b) a + b)`); unlike
#'   `run_Series()`'s `input` argument, no automatic naming is attempted
#'   here, so an unnamed or partially named list raises an error rather
#'   than being auto-named.
#' @param x An `"ABM_Series"` object (for `print.ABM_Series()`).
#' @param contents Logical; passed to `print.ABM_Series()`. If `FALSE` (the
#'   default), only metadata is shown -- the number of chunks/defaults and
#'   their names -- without previewing each chunk's or default's actual
#'   content. If `TRUE`, each chunk's body and each default's value are also
#'   previewed (subject to truncation via `max_lines`). Mirrors the
#'   lightweight-by-default design of `print.ABM_Game(fields = FALSE)`.
#' @param max_lines Passed to `print.ABM_Series()`. A single non-negative
#'   integer controlling how many lines of each chunk's/default's preview
#'   are shown before truncating, when `contents = TRUE`. Ignored when
#'   `contents = FALSE`.
#'
#' @return
#' An object of class `"ABM_Series"`, a list with the following elements:
#' \describe{
#'   \item{chunks}{A named list of `ABM_Chunk` objects representing code chunks.}
#'   \item{default}{A named list of default objects (values and/or functions).}
#' }
#'
#' @details
#' Chunks are created using `Chunk()`, which captures code blocks without
#' evaluating them. Each chunk is stored as an `ABM_Chunk` object and
#' executed later by `run_Series()` in a shared environment, so objects
#' created in one chunk are available in subsequent chunks.
#'
#' ```
#' step1 <- Chunk({ x <- 1 })
#' step2 <- Chunk({ y <- x + 1 })
#'
#' S <- Series(step1, step2)
#' ```
#'
#' Named arguments can be used to override the variable name as the key:
#'
#' ```
#' S <- Series(init = step1, calc = step2)
#' ```
#'
#' Passing an inline, unnamed chunk (i.e. not a bare variable reference)
#' falls back to automatic sequential naming, since there is no variable
#' name available:
#'
#' ```
#' S <- Series(step1, Chunk({ z <- y + 1 }))
#' # chunk names: "step1", "Chunk1"
#' ```
#'
#' The `default` slot represents the initial state of the execution environment.
#' Users can later override these defaults at runtime via
#' `run_Series(..., input = ...)`.
#'
#' @examples
#' step1 <- Chunk({ x <- 1 })
#' step2 <- Chunk({ y <- x + 1 })
#'
#' # Basic usage
#' S <- Series(step1, step2)
#' str(S)
#'
#' # Default print(): metadata only (chunk/default names and counts)
#' S
#'
#' # Preview actual chunk/default contents
#' print(S, contents = TRUE)
#'
#' # With explicit names
#' S <- Series(init = step1, calc = step2)
#'
#' # Inline, unnamed chunk gets an automatic "Chunk<N>" name
#' S <- Series(step1, Chunk({ z <- y + 1 }))
#'
#' # With default objects
#' S <- Series(
#'   step1,
#'   step2,
#'   default = list(
#'     add2 = function(a, b) a + b
#'   )
#' )
#'
#' @seealso [run_Series()], [Chunk()]
#' @export
Series <- function(..., default = NULL) {

  #=========== chunk_list ==========================

  # Capture the unevaluated expressions passed to '...' via base R only
  # (no rlang dependency): substitute(list(...)) yields a call of the form
  # list(step1, step2) or list(init = step1, calc = step2); dropping the
  # leading `list` symbol gives a (possibly named) list/pairlist of the
  # argument expressions, in the same order as '...'.
  dot_exprs <- as.list(substitute(list(...)))[-1L]
  n <- length(dot_exprs)

  nm0 <- names(dot_exprs)
  if (is.null(nm0)) nm0 <- rep_len("", n)

  # Resolve names: explicit name > bare-symbol variable name >
  # automatic "Chunk<N>" (numbered only among the arguments that need it).
  nms <- character(n)
  auto_idx <- 0L
  for (i in seq_len(n)) {
    if (nzchar(nm0[i])) {
      nms[i] <- nm0[i]
    } else if (is.symbol(dot_exprs[[i]])) {
      nms[i] <- as.character(dot_exprs[[i]])
    } else {
      auto_idx <- auto_idx + 1L
      nms[i] <- paste0("Chunk", auto_idx)
    }
  }

  # Evaluate '...' normally to retrieve the underlying ABM_Chunk objects.
  chunk_list <- list(...)

  # Verify that all arguments are ABM_Chunk objects
  is_chunk <- vapply(chunk_list, inherits, logical(1), what = "ABM_Chunk")

  if (!all(is_chunk)) {
    bad <- nms[!is_chunk]
    stop(
      "All arguments to Series() must be wrapped with Chunk(). ",
      "Invalid argument(s): ", paste(bad, collapse = ", ")
    )
  }

  # Assign names and check for duplicates
  chunk_list <- setNames(chunk_list, nms)

  if (anyDuplicated(names(chunk_list))) {
    dups <- unique(names(chunk_list)[duplicated(names(chunk_list))])
    stop(
      "Chunk names must be unique. ",
      "Duplicated name(s): ", paste(dups, collapse = ", ")
    )
  }

  #=========== default_list ==========================

  # Unlike run_Series()'s 'input' (which uses .format_input() to support
  # convenient auto-naming of a single unnamed object), 'default' requires
  # an explicit, fully named plain list: it is meant to be written out by
  # the user as e.g. default = list(add2 = function(a, b) a + b), so there
  # is no variable-name expression to fall back on the way there is for a
  # bare 'input' argument, and silently auto-naming a mistakenly-unnamed
  # 'default' element would hide what is almost certainly a user error.
  if (is.null(default)) {
    default_list <- list()
  } else {
    if (!is.list(default) || is.object(default)) {
      stop("'default' must be NULL or a plain named list of objects.")
    }
    nm_d <- names(default)
    if (is.null(nm_d) || anyNA(nm_d) || any(nm_d == "")) {
      stop("'default' must be a fully named list (every element must have a name).")
    }
    default_list <- default
  }

  if (anyDuplicated(names(default_list))) {
    dups <- unique(names(default_list)[duplicated(names(default_list))])
    stop(
      "Default names must be unique. ",
      "Duplicated name(s): ", paste(dups, collapse = ", ")
    )
  }

  #=========== output ==========================

  out <- list(
    chunks  = chunk_list,
    default = default_list
  )

  structure(out, class = "ABM_Series")
}


#' @rdname Series
#' @export
print.ABM_Series <- function(x, contents = FALSE, max_lines = 6L, ...) {

  stopifnot(
    "'contents' must be a single logical value." =
      is.logical(contents) && length(contents) == 1L && !is.na(contents)
  )
  stopifnot(
    "'max_lines' must be a single non-negative integer" =
      is.numeric(max_lines) &&
      length(max_lines) == 1L &&
      !is.na(max_lines) &&
      max_lines >= 0 &&
      max_lines == floor(max_lines)
  )
  max_lines <- as.integer(max_lines)
  truncated_any <- FALSE

  # Helper: truncate lines to max_lines
  .truncate <- function(lines, max_lines) {
    if (max_lines == 0L) {
      return(list(lines = character(0), truncated = length(lines) > 0))
    }
    if (length(lines) > max_lines) {
      return(list(
        lines = c(lines[seq_len(max_lines)], "  ---- (truncated) ----"),
        truncated = TRUE
      ))
    }
    list(lines = lines, truncated = FALSE)
  }

  # Helper: preview a single element
  .preview <- function(elem, max_lines, ...) {
    if (inherits(elem, "ABM_Chunk")) {
      # Print Chunk contents directly without the <ABM_Chunk> header
      lines <- deparse(elem, width.cutoff = 60L)
    } else if (is.function(elem)) {
      lines <- deparse(elem)
    } else {
      lines <- capture.output(base::print(elem, ...))
      if (!length(lines)) lines <- capture.output(utils::str(elem))
    }
    out <- .truncate(lines, max_lines)
    if (length(out$lines)) {
      cat(paste(out$lines, collapse = "\n"), "\n", sep = "")
    }
    out$truncated
  }

  # Header
  cat("<Series>\n")

  if (isTRUE(contents)) {
    # [chunks]
    cat("[chunks]\n")
    for (nm in names(x$chunks)) {
      cat("$", nm, "\n", sep = "")
      if (.preview(x$chunks[[nm]], max_lines, ...)) truncated_any <- TRUE
      cat("\n")
    }

    # [default]
    cat("[default]\n")
    if (length(x$default) == 0L) {
      cat("  (none)\n")
    } else {
      for (nm in names(x$default)) {
        cat("$", nm, "\n", sep = "")
        if (.preview(x$default[[nm]], max_lines, ...)) truncated_any <- TRUE
        cat("\n")
      }
    }
  }

  # Summary footer (always shown, regardless of 'contents')
  chunk_names   <- names(x$chunks)
  default_names <- names(x$default)

  cat("-------------------\n")
  cat("n of chunks  :", length(chunk_names),  "\n")
  cat("  chunks     :", paste(chunk_names,   collapse = ", "), "\n")
  cat("n of defaults:", length(default_names), "\n")
  cat("  defaults   :", paste(default_names, collapse = ", "), "\n")
  cat("-------------------\n")

  if (isTRUE(contents) && isTRUE(truncated_any)) {
    cat(
      "*Some elements are truncated. ",
      "Increase 'max_lines' to display more.\n",
      sep = ""
    )
  }
  if (!isTRUE(contents)) {
    cat("*Chunk/default contents are hidden by default. Use print(contents = TRUE) to preview them.\n")
  }

  invisible(x)
}
