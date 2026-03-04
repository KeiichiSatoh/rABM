#' Convert between linear and matrix grid indices
#'
#' Fast conversion between linear indices and (row, col) matrix coordinates
#' under R's column-major ordering.
#'
#' If \code{index} is a two-column matrix, it is interpreted as
#' \code{(row, col)} coordinates and converted to linear indices.
#' If \code{index} is a numeric vector, it is interpreted as linear indices
#' and converted to a two-column matrix \code{(row, col)}.
#'
#' This function assumes column-major order (R default):
#' \deqn{linear = (col - 1) * n_row + row}
#'
#' No extensive input validation is performed for speed.
#'
#' @param index A numeric vector of linear indices, or a two-column matrix
#'   of \code{(row, col)} coordinates.
#' @param n_row Integer. Number of rows in the grid.
#'
#' @return
#' If \code{index} is a matrix, returns an integer vector of linear indices.
#' If \code{index} is a vector, returns a two-column matrix with columns
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
#' @export
grid_index <- function(index, n_row) {
  if (is.matrix(index)) {
    return((index[, 2L] - 1L) * n_row + index[, 1L])
  }

  col <- (index - 1L) %/% n_row + 1L
  row <- index - (col - 1L) * n_row
  cbind(row = row, col = col)
}




grid_index <- function(index, n_row) {
  # auto-detect:
  #   matrix(,2) -> linear
  #   numeric vector -> (row, col)
  # column-major (R default)

  if (!is.null(dim(index))) {
    # (row, col) -> linear
    return((index[, 2L] - 1L) * n_row + index[, 1L])
  }

  # linear -> (row, col)
  col <- (index - 1L) %/% n_row + 1L
  row <- index - (col - 1L) * n_row
  cbind(row = row, col = col)
}


?which

mat <- matrix(1:12, 4, 3)

posit <- matrix(c(1,1,
                  2,3), 2, 2, byrow = TRUE)

n_row <- 4
n_col <- 3

posit <- c(1,4)

function(posit, n_row, n_col, change_to = c("lin", "arr")){
  out <- switch(change_to,
                "lin" = {(posit[,2] - 1)*n_row + posit[,1]},
                "arr" = {
                  y <- (posit - 1) %/% n_row + 1
                  x <- posit - (y - 1)*n_row
                  cbind(x, y)
                })
 out
}





grid_index <- function(index, n_row, n_col = NULL) {
  # auto: (row,col) matrix -> linear,  or linear vector -> (row,col)
  # column-major (R default): linear = (col-1)*n_row + row

  # ---- matrix: (row, col) -> linear
  if (!is.null(dim(index))) {
    # fastest minimal sanity: 2 columns
    if (ncol(index) != 2L) stop("'index' must have 2 columns: (row, col).")

    # assume integer-ish; keep it fast
    return((index[, 2L] - 1L) * n_row + index[, 1L])
  }

  # ---- vector: linear -> (row, col)
  # (avoid heavy checks; just require numeric/integer)
  if (!is.numeric(index)) stop("'index' must be a numeric vector or a 2-col matrix.")

  col <- (index - 1L) %/% n_row + 1L
  row <- index - (col - 1L) * n_row

  # optional bound check if n_col provided (still cheap-ish)
  if (!is.null(n_col)) {
    if (any(row < 1L | row > n_row | col < 1L | col > n_col)) {
      stop("Index out of bounds.")
    }
  }

  cbind(row = row, col = col)
}
