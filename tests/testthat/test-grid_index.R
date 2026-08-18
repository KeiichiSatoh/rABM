# tests/testthat/test-grid_index.R

test_that("grid_index() converts linear indices to (row, col) correctly", {
  n_row <- 4L
  id <- c(1L, 4L, 5L, 12L)

  expected <- cbind(row = c(1L, 4L, 1L, 4L), col = c(1L, 1L, 2L, 3L))

  result <- grid_index(id, n_row = n_row)

  expect_true(is.matrix(result))
  expect_type(result, "integer")
  expect_equal(unname(result), unname(expected))
  expect_equal(colnames(result), c("row", "col"))
})

test_that("grid_index() converts (row, col) coordinates to linear indices correctly", {
  n_row <- 4L
  rc <- cbind(row = c(1L, 4L, 1L, 4L), col = c(1L, 1L, 2L, 3L))

  result <- grid_index(rc, n_row = n_row)

  expect_false(is.matrix(result))
  expect_type(result, "integer")
  expect_equal(result, c(1L, 4L, 5L, 12L))
})

test_that("grid_index() round-trips linear -> (row, col) -> linear", {
  n_row <- 7L
  n_col <- 5L
  id <- as.integer(sample.int(n_row * n_col, 15))

  rc   <- grid_index(id, n_row = n_row)
  back <- grid_index(rc, n_row = n_row)

  expect_equal(back, id)
})

test_that("grid_index() agrees with base R's which(..., arr.ind = TRUE)", {
  n_row <- 6L
  n_col <- 4L
  id <- sort(c(1L, 7L, 13L, 24L))

  m <- matrix(FALSE, n_row, n_col)
  m[id] <- TRUE
  ref <- which(m, arr.ind = TRUE)
  ref <- ref[order(ref[, "row"], ref[, "col"]), , drop = FALSE]

  result <- grid_index(id, n_row = n_row)

  expect_equal(unname(result[, "row"]), unname(ref[, "row"]))
  expect_equal(unname(result[, "col"]), unname(ref[, "col"]))
})

test_that("grid_index() checks bounds only when n_col is supplied", {
  n_row <- 4L
  n_col <- 3L

  # 13 is one past the last valid linear index (4 * 3 = 12)
  expect_error(grid_index(13, n_row = n_row, n_col = n_col), "out-of-bounds")
  expect_no_error(grid_index(13, n_row = n_row))

  # row = 5 is out of range when n_row = 4
  rc_bad <- cbind(row = 5L, col = 1L)
  expect_error(grid_index(rc_bad, n_row = n_row, n_col = n_col), "out-of-bounds")
  expect_no_error(grid_index(rc_bad, n_row = n_row))
})

test_that("grid_index() validates input type and shape", {
  expect_error(grid_index("a", n_row = 4), "numeric")
  expect_error(grid_index(cbind(1:3, 2:4, 3:5), n_row = 4), "2 columns")
})

test_that("grid_index() returns the documented types", {
  res_rc <- grid_index(c(1L, 2L), n_row = 3)
  expect_true(is.matrix(res_rc))
  expect_type(res_rc, "integer")
  expect_equal(colnames(res_rc), c("row", "col"))

  res_lin <- grid_index(cbind(row = 1L, col = 1L), n_row = 3)
  expect_true(is.integer(res_lin))
  expect_false(is.matrix(res_lin))
})
