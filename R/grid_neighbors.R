#-------------------------------------------------------------------------------
# grid_neighbors(): Moore / von Neumann neighborhood lookup on a grid
#-------------------------------------------------------------------------------

#' Retrieve the values of a cell's grid neighbors
#'
#' \code{grid_neighbors()} looks up the values of the cells surrounding one
#' or more positions in a matrix-shaped grid, using either the 8-direction
#' Moore neighborhood or the 4-direction von Neumann neighborhood. It is
#' intended as a building block for spatial \pkg{rABM} models, where agents
#' need to inspect the state of the cells around them.
#'
#' @details
#' Two backends are used, selected automatically based on the type of
#' \code{mat}:
#' \itemize{
#'   \item For numeric, integer, or logical \code{mat}, the lookup runs
#'   through a compiled C++ backend (\code{grid_neighbors_core()}), which
#'   is fast enough for repeated use inside a simulation loop.
#'   \item For other atomic types (most commonly \strong{character}
#'   matrices), there is no compiled backend, so a pure R fallback
#'   (\code{.grid_neighbors_generic()}) is used instead. It implements the
#'   exact same neighborhood structure and edge/\code{NA}/torus-wrapping
#'   behavior, but is considerably slower for large \code{posit} -- this
#'   path exists for correctness and convenience, not performance.
#' }
#'
#' @param posit A numeric vector of positions to query. By default this is
#'   interpreted as 1-based linear (column-major) indices into \code{mat},
#'   i.e. the same indexing used by \code{mat[posit]}. If \code{arr_ind =
#'   TRUE}, \code{posit} is instead a two-column matrix of \code{(row,
#'   col)} coordinates, resolved via \code{\link{grid_index}}.
#' @param mat A matrix giving the grid to query. Numeric, integer, and
#'   logical matrices are processed through a fast C++ backend; other
#'   atomic types (e.g. character) go through a slower, pure R fallback.
#'   See Details.
#' @param arr_ind Logical. If \code{TRUE}, \code{posit} is a two-column
#'   \code{(row, col)} matrix and is first converted to linear indices via
#'   \code{\link{grid_index}}. Defaults to \code{FALSE} (\code{posit} is
#'   already a vector of linear indices).
#' @param grid_type One of \code{"moore"} (8 neighbors: up, down, left,
#'   right, and the four diagonals) or \code{"neumann"} (4 neighbors: up,
#'   down, left, right only). Partial matching is supported via
#'   \code{\link{match.arg}}.
#' @param include_self Logical. If \code{TRUE}, an additional \code{"self"}
#'   column holding \code{mat[posit]} is appended to the result. Defaults
#'   to \code{FALSE}.
#' @param torus Logical. If \code{TRUE}, the grid wraps around at the
#'   edges (periodic boundary conditions), so every cell has a full set of
#'   neighbors. If \code{FALSE} (the default), neighbors that would fall
#'   outside the grid are \code{NA}.
#' @param simplify Logical. If \code{TRUE}, the result is flattened with
#'   \code{\link{as.vector}} before being returned. Defaults to
#'   \code{FALSE}.
#'
#' @return By default, a matrix (numeric, or the same type as \code{mat}
#'   when \code{mat} is not numeric/integer/logical) with one row per
#'   element of \code{posit} and one column per neighbor direction
#'   (\code{"U"}, \code{"D"}, \code{"L"}, \code{"R"}, and, for
#'   \code{grid_type = "moore"}, also \code{"UL"}, \code{"UR"},
#'   \code{"DL"}, \code{"DR"}; plus \code{"self"} when \code{include_self =
#'   TRUE}). Row names are \code{posit} coerced to character. If
#'   \code{simplify = TRUE}, the result is coerced to a plain vector via
#'   \code{\link{as.vector}}.
#'
#' @seealso \code{\link{grid_index}}
#'
#' @examples
#' m <- matrix(1:100, nrow = 10)
#'
#' # A single, non-edge cell
#' grid_neighbors(posit = 25, mat = m)
#'
#' # Multiple positions at once
#' grid_neighbors(posit = c(1, 25, 100), mat = m)
#'
#' # von Neumann neighborhood; edge cells get NA for out-of-range neighbors
#' grid_neighbors(posit = 1, mat = m, grid_type = "neumann")
#'
#' # Torus (periodic) boundary: no NAs, edges wrap around
#' grid_neighbors(posit = 1, mat = m, torus = TRUE)
#'
#' # A 2-dimensional position, given as (row, col) coordinates
#' rc <- cbind(row = c(1, 5), col = c(1, 5))
#' grid_neighbors(posit = rc, mat = m, arr_ind = TRUE)
#'
#' # Row-wise summary computed afterwards
#' rowMeans(grid_neighbors(posit = 1:100, mat = m), na.rm = TRUE)
#'
#' # simplify = TRUE flattens the result to a plain vector
#' grid_neighbors(posit = 25, mat = m, grid_type = "neumann", simplify = TRUE)
#'
#' # Character grids are supported through a slower, pure R fallback
#' m_chr <- matrix(letters[1:100], nrow = 10)
#' grid_neighbors(posit = 25, mat = m_chr, grid_type = "neumann")
#'
#' @export
grid_neighbors <- function(
  posit,
  mat,
  arr_ind = FALSE,
  grid_type = c("moore", "neumann"),
  include_self = FALSE,
  torus = FALSE,
  simplify = FALSE){

  grid_type <- match.arg(grid_type)

  # --- lightweight validation (kept cheap since this may be called
  #     repeatedly inside a simulation loop) ---
  stopifnot(
    "'mat' must be a matrix." = is.matrix(mat),
    "'mat' must be an atomic matrix." = is.atomic(mat),
    "'posit' must be numeric." = is.numeric(posit),
    "'include_self' must be TRUE/FALSE." =
      is.logical(include_self) && length(include_self) == 1L && !is.na(include_self),
    "'torus' must be TRUE/FALSE." =
      is.logical(torus) && length(torus) == 1L && !is.na(torus)
  )

  mat_dim <- dim(mat)
  n_row   <- mat_dim[1]
  n_col   <- mat_dim[2]
  n_cell  <- n_row * n_col

  if (arr_ind) {
    posit <- grid_index(index = posit, n_row = n_row, n_col = n_col)
  }

  if (anyNA(posit) || any(posit < 1L | posit > n_cell)) {
    stop("'posit' contains an index out of the range of 'mat'.")
  }

  moore <- identical(grid_type, "moore")
  posit_int <- as.integer(posit)

  if (is.numeric(mat) || is.logical(mat)) {
    # Fast path: compiled C++ backend (numeric/integer/logical grids).
    mat_num <- if (is.double(mat)) mat else matrix(as.double(mat), nrow = n_row, ncol = n_col)
    out <- grid_neighbors_core(
      mat          = mat_num,
      posit        = posit_int,
      moore        = moore,
      include_self = include_self,
      torus        = torus
    )
  } else {
    # Generic fallback: pure R, works for any atomic type (e.g. character).
    out <- .grid_neighbors_generic(
      mat = mat, posit = posit_int,
      moore = moore, include_self = include_self, torus = torus,
      n_row = n_row, n_col = n_col
    )
  }

  if (simplify) {
    out <- as.vector(out)
  }

  out
}

#-------------------------------------------------------------------------------
# .grid_neighbors_generic(): pure-R fallback for non-numeric/logical grids
#-------------------------------------------------------------------------------

#' Compute grid neighbor values for non-numeric matrices (internal)
#'
#' Pure R fallback used by \code{\link{grid_neighbors}} when \code{mat} is
#' not numeric or logical (most commonly a character matrix), since the
#' compiled C++ backend only supports numeric data. Mirrors the exact same
#' neighborhood structure and edge/\code{NA}/torus-wrapping behavior as
#' \code{grid_neighbors_core()}, at the cost of speed (an R-level loop
#' over directions instead of a single C++ pass).
#'
#' @param mat An atomic matrix.
#' @param posit An integer vector of 1-based linear indices into \code{mat}.
#' @param moore Logical. \code{TRUE} for the Moore neighborhood,
#'   \code{FALSE} for the von Neumann neighborhood.
#' @param include_self Logical. If \code{TRUE}, append a \code{"self"}
#'   column.
#' @param torus Logical. If \code{TRUE}, wrap around at the edges.
#' @param n_row,n_col Grid dimensions.
#'
#' @return A matrix of the same type as \code{mat}, with one row per
#'   element of \code{posit} and one column per direction.
#' @noRd
.grid_neighbors_generic <- function(mat, posit, moore, include_self, torus, n_row, n_col) {
  row <- ((posit - 1L) %% n_row) + 1L
  col <- ((posit - 1L) %/% n_row) + 1L

  shift <- function(dr, dc) {
    new_row <- row + dr
    new_col <- col + dc
    if (torus) {
      new_row <- ((new_row - 1L) %% n_row) + 1L
      new_col <- ((new_col - 1L) %% n_col) + 1L
    } else {
      out_of_range <- new_row < 1L | new_row > n_row | new_col < 1L | new_col > n_col
      new_row[out_of_range] <- NA
      new_col[out_of_range] <- NA
    }
    (new_col - 1L) * n_row + new_row
  }

  dirs <- if (moore) {
    list(U = c(-1, 0), D = c(1, 0), L = c(0, -1), R = c(0, 1),
         UL = c(-1, -1), UR = c(-1, 1), DL = c(1, -1), DR = c(1, 1))
  } else {
    list(U = c(-1, 0), D = c(1, 0), L = c(0, -1), R = c(0, 1))
  }

  col_values <- lapply(dirs, function(d) mat[shift(d[1L], d[2L])])
  if (include_self) col_values$self <- mat[posit]

  out <- do.call(cbind, col_values)
  rownames(out) <- as.character(posit)
  out
}
