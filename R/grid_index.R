#-------------------------------------------------------------------------------
# grid_index(): convert between linear and (row, col) grid indices
#-------------------------------------------------------------------------------

#' Convert between linear and matrix grid indices
#'
#' Fast, bidirectional conversion between linear indices and \code{(row,
#' col)} matrix coordinates under R's column-major ordering.
#'
#' If \code{index} is a two-column matrix, it is interpreted as \code{(row,
#' col)} coordinates and converted to linear indices. If \code{index} is a
#' numeric vector, it is interpreted as linear indices and converted to a
#' two-column matrix with columns \code{row} and \code{col}.
#'
#' This function assumes column-major order (R's default):
#' \deqn{linear = (col - 1) \times n\_row + row}
#'
#' The computational core is implemented in C++ for speed (see
#' \code{grid_index_rc2lin_cpp()} / \code{grid_index_lin2rc_cpp()}); this
#' function only validates \code{index}'s type/shape and dispatches to the
#' appropriate backend. Out-of-bounds coordinates are only checked when
#' \code{n_col} is supplied.
#'
#' @param index A numeric vector of linear indices, or a two-column matrix
#'   of \code{(row, col)} coordinates.
#' @param n_row Integer. Number of rows in the grid.
#' @param n_col Integer, optional. Number of columns in the grid. If
#'   supplied, out-of-bounds coordinates raise an error. If \code{NULL}
#'   (the default), this check is skipped.
#'
#' @return
#' If \code{index} is a matrix, an integer vector of linear indices.
#' If \code{index} is a vector, a two-column integer matrix with columns
#' \code{row} and \code{col}.
#'
#' @examples
#' # (row, col) -> linear
#' rc <- cbind(row = c(1, 3), col = c(2, 4))
#' grid_index(rc, n_row = 5)
#'
#' # linear -> (row, col)
#' id <- c(1, 6, 10)
#' grid_index(id, n_row = 5)
#'
#' # with bounds checking (n_col supplied)
#' grid_index(id, n_row = 5, n_col = 3)
#'
#' @export
grid_index <- function(index, n_row, n_col = NULL) {
  if (!is.numeric(index)) {
    stop("'index' must be a numeric vector or a 2-column numeric matrix.")
  }

  check_bounds <- !is.null(n_col)
  n_col_arg <- if (check_bounds) as.integer(n_col) else NA_integer_

  if (!is.null(dim(index))) {
    if (ncol(index) != 2L) {
      stop("'index' must have exactly 2 columns: (row, col).")
    }
    return(grid_index_rc2lin_cpp(index, as.integer(n_row), n_col_arg, check_bounds))
  }

  grid_index_lin2rc_cpp(index, as.integer(n_row), n_col_arg, check_bounds)
}
