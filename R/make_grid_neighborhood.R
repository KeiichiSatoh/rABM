#' Create a Cell-Adjacency Neighbor Index Matrix for a 2D Grid
#'
#' @description
#' `make_grid_neighborhood` builds a neighbor index matrix for a 2D
#' cellular-automaton style grid. Each grid cell is mapped to an
#' **R-style linear index** (column-major order), and the function
#' returns, for every cell, the linear indices of its neighboring cells
#' or an adjacency matrix representing cell connectivity.
#'
#' Neighbor directions are returned in a **fixed column order** with
#' column names:
#' \itemize{
#'   \item \code{type = "neumann"}: \code{N, S, W, E}
#'     (and \code{C} if \code{include_self = TRUE})
#'   \item \code{type = "moore"}  : \code{N, S, W, E, NW, NE, SW, SE}
#'     (and \code{C} if \code{include_self = TRUE})
#' }
#'
#' Boundary cells have out-of-bounds neighbors; these entries are
#' returned as \code{NA_integer_}.
#'
#' @details
#' **Cell ordering / indexing**
#'
#' The rows of the output correspond to cells in the order:
#' \code{(r, c) = (1,1), (2,1), ..., (n_row,1), (1,2), ..., (n_row,n_col)}.
#' This matches the standard linear indexing of an R matrix.
#'
#' Linear indices are computed as:
#' \deqn{idx = (c - 1) \times n\_row + r}
#'
#' **Output shape (\code{return_as = "node_list"})**
#'
#' Let \code{N = n_row * n_col}. Let \code{S} be the number of neighbor
#' directions:
#' \itemize{
#'   \item Neumann: \code{S = 4} (or \code{5} if \code{include_self = TRUE})
#'   \item Moore  : \code{S = 8} (or \code{9} if \code{include_self = TRUE})
#' }
#' The result is an integer matrix of size \code{N x S}.
#'
#' **Output shape (\code{return_as = "network"})**
#'
#' The result is a binary integer matrix of size \code{N x N}, where
#' entry \code{[i, j] = 1} indicates that cell \code{j} is a neighbor
#' of cell \code{i}. When \code{include_self = TRUE}, the diagonal
#' entries are set to \code{1}.
#'
#' @param n_row Integer (>= 1). Number of rows in the grid.
#' @param n_col Integer (>= 1). Number of columns in the grid.
#' @param type Character string. Neighborhood type. One of
#'   \code{"moore"} (default) or \code{"neumann"}.
#' @param include_self Logical. If \code{TRUE}, include the cell itself
#'   as an additional direction named \code{C} (center), appended as
#'   the last column. Default is \code{FALSE}.
#' @param return_as Character string. Output format. One of
#'   \code{"node_list"} (default) or \code{"network"}.
#'   \code{"node_list"} returns an \code{N x S} integer matrix of
#'   neighbor indices. \code{"network"} returns an \code{N x N} binary
#'   adjacency matrix representing cell connectivity.
#'
#' @return
#' If \code{return_as = "node_list"}: an integer matrix of size
#' \code{(n_row * n_col) x S}. Each row corresponds to a grid cell
#' (in R's column-major linear order), and each column corresponds to
#' a neighbor direction with fixed names: \code{N, S, W, E} (plus
#' diagonals \code{NW, NE, SW, SE} for Moore; and \code{C} if
#' requested). Out-of-bounds neighbors are \code{NA_integer_}.
#'
#' If \code{return_as = "network"}: a binary integer matrix of size
#' \code{(n_row * n_col) x (n_row * n_col)}, where entry
#' \code{[i, j] = 1} indicates that cell \code{j} is a neighbor of
#' cell \code{i}.
#'
#' @examples
#' # 3x3 grid, von Neumann neighborhood (N, S, W, E)
#' nb4 <- make_ca_neighborhood(3, 3, type = "neumann")
#' dim(nb4)       # 9 x 4
#' colnames(nb4)  # "N" "S" "W" "E"
#'
#' # 3x3 grid, Moore neighborhood (N, S, W, E, NW, NE, SW, SE)
#' nb8 <- make_ca_neighborhood(3, 3, type = "moore")
#' dim(nb8)       # 9 x 8
#' colnames(nb8)
#'
#' # Include self as "C" (center) appended last
#' nb5 <- make_ca_neighborhood(3, 3, type = "neumann", include_self = TRUE)
#' colnames(nb5)  # "N" "S" "W" "E" "C"
#'
#' # Neighbors of the center cell (r=2, c=2) in a 3x3 grid:
#' # linear index = (2-1)*3 + 2 = 5
#' center <- 5L
#' nb8[center, ]
#'
#' # Return as adjacency matrix
#' net <- make_ca_neighborhood(3, 3, type = "neumann", return_as = "network")
#' dim(net)  # 9 x 9
#'
#' @export
make_grid_neighborhood <- function(n_row, n_col,
                         type = c("moore", "neumann"),
                         include_self = FALSE,
                         return_as = c("node_list", "network")) {
  type      <- match.arg(type)
  return_as <- match.arg(return_as)

  # ---- input validation ----
  stopifnot(
    "'n_row' must be a single positive integer." =
      is.numeric(n_row) && length(n_row) == 1L && is.finite(n_row) &&
      !is.na(n_row) && n_row >= 1 && (n_row %% 1 == 0),
    "'n_col' must be a single positive integer." =
      is.numeric(n_col) && length(n_col) == 1L && is.finite(n_col) &&
      !is.na(n_col) && n_col >= 1 && (n_col %% 1 == 0),
    "'include_self' must be TRUE or FALSE." =
      is.logical(include_self) && length(include_self) == 1L && !is.na(include_self)
  )
  n <- as.integer(n_row)
  m <- as.integer(n_col)

  # ---- all cells (column-major order) ----
  rowv <- rep(seq_len(n), times = m)
  colv <- rep(seq_len(m), each  = n)
  N <- length(rowv)

  # ---- steps (fixed order + labels) ----
  if (type == "neumann") {
    steps <- matrix(
      c(-1L,  0L,   # N
        1L,  0L,   # S
        0L, -1L,   # W
        0L,  1L),  # E
      ncol = 2L,
      byrow = TRUE
    )
    dir_names <- c("N", "S", "W", "E")
  } else {
    steps <- matrix(
      c(-1L,  0L,   # N
        1L,  0L,   # S
        0L, -1L,   # W
        0L,  1L,   # E
        -1L, -1L,   # NW
        -1L,  1L,   # NE
        1L, -1L,   # SW
        1L,  1L),  # SE
      ncol = 2L,
      byrow = TRUE
    )
    dir_names <- c("N", "S", "W", "E", "NW", "NE", "SW", "SE")
  }
  if (include_self) {
    steps <- rbind(steps, c(0L, 0L))
    dir_names <- c(dir_names, "C")
  }
  S <- nrow(steps)

  # ---- vectorized neighbor coordinates ----
  neib_r <- rep(rowv, each = S) + rep(steps[, 1L], times = N)
  neib_c <- rep(colv, each = S) + rep(steps[, 2L], times = N)

  # ---- out-of-bounds -> NA ----
  valid <- (neib_r >= 1L & neib_r <= n & neib_c >= 1L & neib_c <= m)
  neib_r[!valid] <- NA_integer_
  neib_c[!valid] <- NA_integer_

  # ---- linear index (column-major) ----
  lin <- (neib_c - 1L) * n + neib_r

  # ---- output ----
  out <- matrix(lin, nrow = N, ncol = S, byrow = TRUE)
  colnames(out) <- dir_names

  if (return_as == "node_list") {
    return(out)
  }

  # ---- Option: return as network ----
  n_place <- nrow(out)
  net <- matrix(0, nrow = n_place, ncol = n_place,
                dimnames = list(seq_len(n_place), seq_len(n_place)))
  for (i in seq_len(n_place)) {
    net[i, na.exclude(out[i, ])] <- 1
  }
  return(net)
}


