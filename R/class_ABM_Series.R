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
#'   If no name is provided, the variable name is used as the key.
#'   All arguments must be `ABM_Chunk` objects; passing other types will
#'   result in an error. Chunk names must be unique.
#' @param default An optional list of default objects (values and/or functions)
#'   to be injected into the execution environment before running chunks.
#'   These can be overridden at runtime via `run_Series(..., input = ...)`.
#'   If `NULL`, an empty default list is stored.
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
#' # With explicit names
#' S <- Series(init = step1, calc = step2)
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

  dot_exprs <- rlang::enexprs(...)

  # Supplement missing names with the deparsed symbol names
  nms <- ifelse(
    names(dot_exprs) == "",
    sapply(dot_exprs, deparse),
    names(dot_exprs)
  )

  # Evaluate each symbol to retrieve the underlying object
  chunk_list <- lapply(dot_exprs, rlang::eval_tidy)

  # Verify that all arguments are ABM_Chunk objects
  is_chunk <- sapply(chunk_list, inherits, "ABM_Chunk")

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

  default_list <- .format_input(
    x = default,
    expr = substitute(default),
    prefix = "default"
  )
  if (is.null(default_list)) default_list <- list()

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
as.Series <- function(x, ...) {
  UseMethod("as.Series")
}

#' @rdname Series
#' @export
as.Series.ABM_Series <- function(x, ...) {
  x
}

#' @rdname Series
#' @export
as.Series.default <- function(x, ...) {
  stop(
    "Cannot coerce an object of class '",
    paste(class(x), collapse = ", "),
    "' to ABM_Series."
  )
}


#' @rdname Series
#' @export
print.ABM_Series <- function(x, max_lines = 6L, ...) {

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

  # Summary footer
  chunk_names   <- names(x$chunks)
  default_names <- names(x$default)

  cat("-------------------\n")
  cat("n of chunks  :", length(chunk_names),  "\n")
  cat("  chunks     :", paste(chunk_names,   collapse = ", "), "\n")
  cat("n of defaults:", length(default_names), "\n")
  cat("  defaults   :", paste(default_names, collapse = ", "), "\n")
  cat("-------------------\n")

  if (isTRUE(truncated_any)) {
    cat(
      "*Some elements are truncated. ",
      "Increase 'max_lines' to display more.\n",
      sep = ""
    )
  }

  invisible(x)
}

