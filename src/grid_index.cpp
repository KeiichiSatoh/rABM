// grid_index.cpp
//
// Rcpp backend for grid_index(): converts between linear indices and
// (row, col) coordinates under R's column-major ordering. The two
// directions are implemented as separate exported functions so each stays
// a tight, branch-free loop; grid_index() (R/grid_index.R) picks the
// right one based on whether 'index' is a matrix or a vector, and also
// handles input-type validation.

#include <Rcpp.h>
using namespace Rcpp;

//' Convert (row, col) coordinates to linear indices (internal Rcpp backend)
//'
//' Internal C++ backend for \code{\link{grid_index}}. Not intended to be
//' called directly by users.
//'
//' @param rc A two-column numeric matrix of \code{(row, col)} coordinates.
//' @param n_row Number of rows in the grid.
//' @param n_col Number of columns in the grid. Only used when
//'   \code{check_bounds = TRUE}.
//' @param check_bounds Logical. If \code{TRUE}, out-of-bounds coordinates
//'   raise an error.
//' @return An integer vector of linear indices.
//' @noRd
// [[Rcpp::export]]
IntegerVector grid_index_rc2lin_cpp(NumericMatrix rc, int n_row, int n_col, bool check_bounds) {
  const int n = rc.nrow();
  IntegerVector out(n);

  for (int i = 0; i < n; ++i) {
    int row = (int) rc(i, 0);
    int col = (int) rc(i, 1);

    if (check_bounds && (row < 1 || row > n_row || col < 1 || col > n_col)) {
      stop("'index' contains out-of-bounds (row, col) coordinates.");
    }

    out[i] = (col - 1) * n_row + row;
  }

  return out;
}

//' Convert linear indices to (row, col) coordinates (internal Rcpp backend)
//'
//' Internal C++ backend for \code{\link{grid_index}}. Not intended to be
//' called directly by users.
//'
//' @param idx A numeric vector of linear indices.
//' @param n_row Number of rows in the grid.
//' @param n_col Number of columns in the grid. Only used when
//'   \code{check_bounds = TRUE}.
//' @param check_bounds Logical. If \code{TRUE}, out-of-bounds indices
//'   raise an error.
//' @return A two-column integer matrix with columns \code{row} and
//'   \code{col}.
//' @noRd
// [[Rcpp::export]]
IntegerMatrix grid_index_lin2rc_cpp(NumericVector idx, int n_row, int n_col, bool check_bounds) {
  const int n = idx.size();
  IntegerMatrix out(n, 2);
  colnames(out) = CharacterVector::create("row", "col");

  for (int i = 0; i < n; ++i) {
    int id = (int) idx[i];

    if (check_bounds && (id < 1 || id > n_row * n_col)) {
      stop("'index' contains out-of-bounds linear indices.");
    }

    int col = (id - 1) / n_row + 1;
    int row = id - (col - 1) * n_row;
    out(i, 0) = row;
    out(i, 1) = col;
  }

  return out;
}
