# tests/testthat/test-grid_neighbors.R
#
# Reference grid used throughout: a 4x3 matrix whose values equal their own
# linear (column-major) index, which makes expected neighbor values easy to
# derive and verify by hand.
#
#      col1 col2 col3
# row1    1    5    9
# row2    2    6   10
# row3    3    7   11
# row4    4    8   12

m <- matrix(1:12, nrow = 4, ncol = 3)

test_that("grid_neighbors() computes the Moore neighborhood of an interior cell", {
  # posit = 6 -> (row = 2, col = 2), a fully interior cell
  out <- grid_neighbors(posit = 6, mat = m, grid_type = "moore")

  expect_equal(colnames(out), c("U", "D", "L", "R", "UL", "UR", "DL", "DR"))
  expect_equal(rownames(out), "6")
  expect_equal(
    unname(out[1, ]),
    unname(c(U = 5, D = 7, L = 2, R = 10, UL = 1, UR = 9, DL = 3, DR = 11))
  )
})

test_that("grid_neighbors() computes the von Neumann neighborhood of an interior cell", {
  out <- grid_neighbors(posit = 6, mat = m, grid_type = "neumann")

  expect_equal(colnames(out), c("U", "D", "L", "R"))
  expect_equal(unname(out[1, ]), c(5, 7, 2, 10))
})

test_that("grid_neighbors() returns NA for out-of-range neighbors at an edge (non-torus)", {
  # posit = 1 -> (row = 1, col = 1), the top-left corner
  out <- grid_neighbors(posit = 1, mat = m, grid_type = "moore", torus = FALSE)

  expect_equal(
    unname(out[1, ]),
    unname(c(U = NA_real_, D = 2, L = NA_real_, R = 5,
             UL = NA_real_, UR = NA_real_, DL = NA_real_, DR = 6))
  )
})

test_that("grid_neighbors() wraps around the edges when torus = TRUE", {
  out <- grid_neighbors(posit = 1, mat = m, grid_type = "moore", torus = TRUE)

  expect_equal(
    unname(out[1, ]),
    unname(c(U = 4, D = 2, L = 9, R = 5, UL = 12, UR = 8, DL = 10, DR = 6))
  )
  expect_false(anyNA(out))
})

test_that("grid_neighbors() appends a 'self' column when include_self = TRUE", {
  out <- grid_neighbors(posit = 6, mat = m, grid_type = "neumann", include_self = TRUE)

  expect_equal(colnames(out), c("U", "D", "L", "R", "self"))
  expect_equal(unname(out[1, "self"]), 6)
})

test_that("grid_neighbors() vectorizes over multiple positions consistently", {
  posit <- c(1, 6, 12)
  out_batched <- grid_neighbors(posit = posit, mat = m, grid_type = "moore", torus = TRUE)

  expect_equal(nrow(out_batched), length(posit))
  expect_equal(rownames(out_batched), as.character(posit))

  for (i in seq_along(posit)) {
    out_single <- grid_neighbors(posit = posit[i], mat = m, grid_type = "moore", torus = TRUE)
    expect_equal(unname(out_batched[i, ]), unname(out_single[1, ]))
  }
})

test_that("grid_neighbors() supports arr_ind = TRUE with (row, col) coordinates", {
  rc <- cbind(row = 2, col = 2)  # same cell as posit = 6

  out_arr_ind <- grid_neighbors(posit = rc, mat = m, arr_ind = TRUE, grid_type = "moore")
  out_linear  <- grid_neighbors(posit = 6, mat = m, grid_type = "moore")

  expect_equal(unname(out_arr_ind), unname(out_linear))
})

test_that("grid_neighbors() flattens the result when simplify = TRUE", {
  out <- grid_neighbors(
    posit = 6, mat = m, grid_type = "neumann", simplify = TRUE
  )

  expect_false(is.matrix(out))
  expect_equal(out, c(5, 7, 2, 10))
})

test_that("grid_neighbors() coerces non-double numeric/logical matrices via the fast path", {
  m_int <- matrix(1:12, nrow = 4, ncol = 3)
  m_lgl <- matrix(c(TRUE, FALSE), nrow = 4, ncol = 3)

  expect_equal(
    grid_neighbors(posit = 6, mat = m_int),
    grid_neighbors(posit = 6, mat = matrix(as.double(1:12), nrow = 4, ncol = 3))
  )
  expect_no_error(grid_neighbors(posit = 1, mat = m_lgl))
})

test_that("grid_neighbors() validates its inputs", {
  expect_error(grid_neighbors(posit = 1, mat = 1:12), "matrix")
  expect_error(grid_neighbors(posit = "a", mat = m), "numeric")
  expect_error(grid_neighbors(posit = 999, mat = m), "range")
  expect_error(grid_neighbors(posit = 1, mat = m, include_self = "yes"), "TRUE/FALSE")
  expect_error(grid_neighbors(posit = 1, mat = m, torus = c(TRUE, TRUE)), "TRUE/FALSE")
  expect_error(grid_neighbors(posit = 1, mat = m, grid_type = "bogus"))

  m_list <- matrix(list(1, 2, 3, 4), nrow = 2, ncol = 2)
  expect_error(grid_neighbors(posit = 1, mat = m_list), "atomic")
})

# --- character matrices: pure-R generic fallback ---------------------------

test_that("grid_neighbors() supports character matrices via the generic fallback", {
  m_num <- matrix(1:12, nrow = 4, ncol = 3)
  m_chr <- matrix(as.character(1:12), nrow = 4, ncol = 3)

  out_num <- grid_neighbors(posit = 6, mat = m_num, grid_type = "moore", torus = TRUE)
  out_chr <- grid_neighbors(posit = 6, mat = m_chr, grid_type = "moore", torus = TRUE)

  expect_true(is.character(out_chr))
  expect_equal(unname(out_chr[1, ]), as.character(unname(out_num[1, ])))
})

test_that("grid_neighbors() generic fallback handles NA at edges (non-torus) and include_self", {
  m_chr <- matrix(letters[1:12], nrow = 4, ncol = 3)

  out <- grid_neighbors(
    posit = 1, mat = m_chr, grid_type = "moore", torus = FALSE, include_self = TRUE
  )

  expect_equal(colnames(out), c("U", "D", "L", "R", "UL", "UR", "DL", "DR", "self"))
  expect_equal(
    unname(out[1, ]),
    c(NA_character_, "b", NA_character_, "e",
      NA_character_, NA_character_, NA_character_, "f", "a")
  )
})

test_that("grid_neighbors() generic fallback vectorizes over multiple positions consistently", {
  m_chr <- matrix(letters[1:12], nrow = 4, ncol = 3)
  posit <- c(1, 6, 12)

  out_batched <- grid_neighbors(posit = posit, mat = m_chr, grid_type = "moore", torus = TRUE)

  expect_equal(nrow(out_batched), length(posit))
  for (i in seq_along(posit)) {
    out_single <- grid_neighbors(posit = posit[i], mat = m_chr, grid_type = "moore", torus = TRUE)
    expect_equal(unname(out_batched[i, ]), unname(out_single[1, ]))
  }
})

test_that("grid_neighbors() generic fallback respects grid_type, simplify, and arr_ind", {
  m_chr <- matrix(letters[1:12], nrow = 4, ncol = 3)

  out_neumann <- grid_neighbors(posit = 6, mat = m_chr, grid_type = "neumann")
  expect_equal(colnames(out_neumann), c("U", "D", "L", "R"))
  expect_equal(unname(out_neumann[1, ]), c("e", "g", "b", "j"))

  out_simplify <- grid_neighbors(posit = 6, mat = m_chr, grid_type = "neumann", simplify = TRUE)
  expect_false(is.matrix(out_simplify))
  expect_equal(out_simplify, c("e", "g", "b", "j"))

  rc <- cbind(row = 2, col = 2)  # same cell as posit = 6
  out_arr_ind <- grid_neighbors(posit = rc, mat = m_chr, arr_ind = TRUE, grid_type = "neumann")
  expect_equal(unname(out_arr_ind), unname(out_neumann))
})
