// grid_neighbors.cpp
//
// Rcpp backend for grid_neighbors(): retrieves the values of the Moore
// (8-neighbor) or von Neumann (4-neighbor) neighborhood for a set of
// linear indices into a matrix, with optional torus (periodic boundary)
// wrapping.
//
// This file is the internal computational core. Input validation,
// arr_ind resolution, FUN post-processing, and simplify are all handled
// on the R side in grid_neighbors() (R/grid_neighbors.R); this function
// assumes its inputs are already well-formed.

#include <Rcpp.h>
#include <string>
using namespace Rcpp;

//' Compute grid neighbor values (internal Rcpp backend)
//'
//' Internal C++ backend for \code{\link{grid_neighbors}}. Not intended to
//' be called directly by users; \code{grid_neighbors()} performs input
//' validation and post-processing (\code{FUN}, \code{simplify}) around
//' this function.
//'
//' @param mat A numeric (double) matrix to read neighbor values from.
//' @param posit An integer vector of 1-based linear (column-major)
//'   indices into \code{mat}.
//' @param moore Logical. \code{TRUE} for the 8-direction Moore
//'   neighborhood, \code{FALSE} for the 4-direction von Neumann
//'   neighborhood.
//' @param include_self Logical. If \code{TRUE}, an extra \code{"self"}
//'   column holding \code{mat[posit]} is appended.
//' @param torus Logical. If \code{TRUE}, out-of-bounds neighbors wrap
//'   around to the opposite edge (periodic boundary conditions). If
//'   \code{FALSE}, out-of-bounds neighbors are \code{NA}.
//'
//' @return A numeric matrix with \code{length(posit)} rows and one column
//'   per direction (\code{"U"}, \code{"D"}, \code{"L"}, \code{"R"}, and,
//'   for the Moore neighborhood, also \code{"UL"}, \code{"UR"},
//'   \code{"DL"}, \code{"DR"}; plus \code{"self"} when
//'   \code{include_self = TRUE}). Row names are \code{posit} coerced to
//'   character.
//' @noRd
// [[Rcpp::export]]
NumericMatrix grid_neighbors_core(NumericMatrix mat,
                                   IntegerVector posit,
                                   bool moore,
                                   bool include_self,
                                   bool torus) {
  const int n_row = mat.nrow();
  const int n_col = mat.ncol();
  const int n     = posit.size();
  const int n_dir = moore ? 8 : 4;
  const int n_out = n_dir + (include_self ? 1 : 0);

  // Direction offsets (row, col), in the same order as the output columns.
  static const int dr8[8] = {-1, 1, 0, 0, -1, -1,  1,  1};
  static const int dc8[8] = { 0, 0,-1, 1, -1,  1, -1,  1};
  static const int dr4[4] = {-1, 1, 0, 0};
  static const int dc4[4] = { 0, 0,-1, 1};
  const int* dr = moore ? dr8 : dr4;
  const int* dc = moore ? dc8 : dc4;

  NumericMatrix out(n, n_out);

  for (int i = 0; i < n; ++i) {
    // Convert the 1-based linear index to a 0-based (row, col) pair.
    const int p   = posit[i] - 1;
    const int row = p % n_row;
    const int col = p / n_row;

    for (int d = 0; d < n_dir; ++d) {
      int nr = row + dr[d];
      int nc = col + dc[d];

      if (torus) {
        nr = ((nr % n_row) + n_row) % n_row;
        nc = ((nc % n_col) + n_col) % n_col;
        out(i, d) = mat[nc * n_row + nr];
      } else if (nr < 0 || nr >= n_row || nc < 0 || nc >= n_col) {
        out(i, d) = NA_REAL;
      } else {
        out(i, d) = mat[nc * n_row + nr];
      }
    }

    if (include_self) {
      out(i, n_dir) = mat[p];
    }
  }

  CharacterVector col_labels = moore
    ? CharacterVector::create("U", "D", "L", "R", "UL", "UR", "DL", "DR")
    : CharacterVector::create("U", "D", "L", "R");
  if (include_self) col_labels.push_back("self");

  CharacterVector row_labels(n);
  for (int i = 0; i < n; ++i) row_labels[i] = std::to_string(posit[i]);

  out.attr("dimnames") = List::create(row_labels, col_labels);

  return out;
}
