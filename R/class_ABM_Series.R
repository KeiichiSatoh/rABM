#' Create an ABM_Series Object
#'
#' @description
#' `ABM_Series()` is a constructor for the `"ABM_Series"` class.
#' It stores a set of code chunks (as language objects) and default objects
#' (values and/or functions) that will be used to initialize the execution
#' environment in `run_Series()`.
#'
#' This class is designed to support iterative ABM workflows where users repeatedly
#' modify settings, generate a `Game` object, run simulations, and post-process results.
#' Each step can be stored as a chunk and executed later in an arbitrary order.
#'
#' @param chunk_expr A list (or a single object) of chunks to store.
#' Each element must be a language object (e.g., `quote({ ... })`,
#' `quote(x <- 1)`, or the output of [chunk()]). If unnamed, names are generated.
#' If `NULL`, an empty chunk list is stored.
#' @param default A list (or a single object) of default objects to store.
#' These objects are injected into the execution environment before running chunks.
#' Functions are allowed. If unnamed, names are generated. If `NULL`, an empty
#' default list is stored.
#'
#' @return
#' An object of class `"ABM_Series"`, a list with the following elements:
#' \describe{
#'   \item{chunks}{A named list of language objects representing code chunks.}
#'   \item{default}{A named list of default objects (values and/or functions).}
#' }
#'
#' @details
#' Chunks are typically created using [chunk()], which captures code blocks
#' without evaluating them. For example:
#'
#' ```
#' chunk({
#'   x <- 1
#'   y <- x * 2
#' })
#' ```
#'
#' The `default` slot is intended to represent the initial state of the execution
#' environment. Users can later override these defaults at runtime via
#' `run_Series(..., input = ...)`.
#'
#' @examples
#' S <- ABM_Series(
#'   chunk_expr = list(
#'     init = chunk({ x <- 1; y <- 2 }),
#'     calc = chunk({ z <- add2(x, y) })
#'   ),
#'   default = list(
#'     add2 = function(a, b) a + b
#'   )
#' )
#'
#' str(S)
#'
#' @seealso [run_Series()], [chunk()]
#' @export
ABM_Series <- function(chunk_expr = NULL,
                       default = NULL) {

  #----------------------------
  # chunks
  #----------------------------
  chunk_list <- .format_input(
    x = chunk_expr,
    expr = substitute(chunk_expr),
    prefix = "chunk"
  )
  if (is.null(chunk_list)) chunk_list <- list()

  stopifnot(
    "All elements in 'chunk_expr' must be a type of 'language'." =
      all(vapply(chunk_list, is.language, logical(1)))
  )

  chunk_names <- names(chunk_list)
  if (anyDuplicated(chunk_names)) {
    dups <- unique(chunk_names[duplicated(chunk_names)])
    stop("Duplicated chunk names: ", paste(dups, collapse = ", "))
  }

  #----------------------------
  # default (values and/or functions)
  #----------------------------
  default_list <- .format_input(
    x = default,
    expr = substitute(default),
    prefix = "default"
  )
  if (is.null(default_list)) default_list <- list()

  default_names <- names(default_list)
  if (anyDuplicated(default_names)) {
    dups <- unique(default_names[duplicated(default_names)])
    stop("Duplicated default names: ", paste(dups, collapse = ", "))
  }

  #----------------------------
  # output
  #----------------------------
  out <- list(
    chunks  = chunk_list,
    default = default_list
  )

  structure(out, class = "ABM_Series")
}
