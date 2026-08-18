# Tests for make_group_labels()
#
# These use the testthat package (3rd edition style `test_that()` blocks).
# If this file lives under tests/testthat/ in the rABM package, testthat
# will source R/make_group_labels.R automatically via devtools::test() /
# testthat::test_dir(). To run this file standalone instead, uncomment the
# two lines below and point source() at the function file.
#
# library(testthat)
# source("make_group_labels.R")

# ---------------------------------------------------------------------------
# Basic allocation (no adjustment needed)
# ---------------------------------------------------------------------------

test_that("named proportions produce exact, correctly labeled counts", {
  out <- make_group_labels(10, c(A = 0.4, B = 0.6))
  expect_length(out, 10)
  expect_setequal(unique(out), c("A", "B"))
  expect_equal(unname(table(out)["A"]), 4)
  expect_equal(unname(table(out)["B"]), 6)
})

test_that("unnamed proportions are labeled as 1-based integers", {
  out <- make_group_labels(10, c(0.4, 0.6))
  expect_type(out, "integer")
  expect_length(out, 10)
  expect_equal(unname(table(out)["1"]), 4)
  expect_equal(unname(table(out)["2"]), 6)
})

test_that("a single group returns that label n times", {
  out <- make_group_labels(5, c(A = 1))
  expect_equal(out, rep("A", 5))
})

test_that("n = 0 returns a zero-length vector", {
  out <- make_group_labels(0, c(A = 0.5, B = 0.5))
  expect_length(out, 0)
})

# ---------------------------------------------------------------------------
# adjustment_by
# ---------------------------------------------------------------------------

test_that("adjustment_by corrects a deficit onto the specified group", {
  # normalized prop = 1/3 each; n * prop = 3.333.. -> round to 3 each -> sum 9,
  # a deficit of 1 against n = 10.
  out <- make_group_labels(10, c(A = 1, B = 1, C = 1), adjustment_by = "B")
  expect_length(out, 10)
  tab <- table(out)
  expect_equal(unname(tab["A"]), 3)
  expect_equal(unname(tab["B"]), 4)
  expect_equal(unname(tab["C"]), 3)
})

test_that("adjustment_by errors on an unknown group label, even when rounding needs no adjustment", {
  # 0.4/0.6 at n = 10 rounds exactly to (4, 6) -- sum already equals n, so no
  # adjustment is actually performed. adjustment_by must still be validated:
  # previously this validation only ran inside the "sum(gr_n) != n" branch,
  # so an invalid label silently succeeded whenever rounding happened to be exact.
  expect_error(
    make_group_labels(10, c(A = 0.4, B = 0.6), adjustment_by = "Z"),
    "valid group label"
  )
})

test_that("adjustment_by errors on an unknown group label when adjustment is actually needed", {
  # 5 equal groups; n * prop = 0.6 each -> rounds to 1 each -> sum 5 vs n = 3,
  # so the adjustment path itself is also exercised.
  expect_error(
    make_group_labels(3, rep(1, 5), adjustment_by = "Z"),
    "valid group label"
  )
})

test_that("adjustment_by errors instead of driving a group's count negative", {
  # 5 equal groups; n * prop = 0.6 each -> rounds to 1 each -> sum 5, excess of 2
  # against n = 3. Group 1 only has a count of 1, so removing 2 would go negative.
  expect_error(
    make_group_labels(3, rep(1, 5), adjustment_by = 1),
    "below zero"
  )
})

# ---------------------------------------------------------------------------
# random_adjustment
# ---------------------------------------------------------------------------

test_that("random_adjustment (default) always returns a vector of length n", {
  set.seed(42)
  for (i in 1:20) {
    out <- make_group_labels(10, c(A = 1, B = 1, C = 1))
    expect_length(out, 10)
    expect_true(all(names(table(out)) %in% c("A", "B", "C")))
  }
})

test_that("random_adjustment handles a large excess without erroring or going negative", {
  # 5 equal groups rounded up to 1 each (sum 5) against a much smaller n = 3;
  # this forces several decrement steps, previously a source of crashes
  # when a group's count could be pushed below zero.
  set.seed(123)
  for (i in 1:20) {
    out <- make_group_labels(3, rep(1, 5))
    expect_length(out, 3)
    expect_true(all(out %in% 1:5))
  }
})

test_that("adjustment_by == NULL and random_adjustment == FALSE warns and skips adjustment", {
  expect_warning(
    out <- make_group_labels(10, c(A = 1, B = 1, C = 1), random_adjustment = FALSE),
    "no adjustment"
  )
  # gr_n rounds to (3, 3, 3) = 9, uncorrected -> length 9, not 10
  expect_length(out, 9)
})

# ---------------------------------------------------------------------------
# from_zero
# ---------------------------------------------------------------------------

test_that("from_zero relabels two groups as 0 / 1", {
  out <- make_group_labels(10, c(A = 0.4, B = 0.6), from_zero = TRUE)
  expect_length(out, 10)
  expect_setequal(unique(out), c(0, 1))
  expect_equal(unname(table(out)["0"]), 4)
  expect_equal(unname(table(out)["1"]), 6)
})

test_that("from_zero relabels more than two groups as 0, 1, 2, ...", {
  out <- make_group_labels(10, c(A = 0.2, B = 0.3, C = 0.5), from_zero = TRUE)
  expect_length(out, 10)
  expect_setequal(unique(out), c(0, 1, 2))
})

test_that("from_zero combined with adjustment_by uses the 0-based label", {
  out <- make_group_labels(10, c(A = 1, B = 1, C = 1), adjustment_by = 1, from_zero = TRUE)
  expect_length(out, 10)
  tab <- table(out)
  expect_equal(unname(tab["0"]), 3)
  expect_equal(unname(tab["1"]), 4)
  expect_equal(unname(tab["2"]), 3)
})

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("invalid 'n' values are rejected", {
  expect_error(make_group_labels(-1, c(A = 0.5, B = 0.5)), "non-negative integer")
  expect_error(make_group_labels(2.5, c(A = 0.5, B = 0.5)), "non-negative integer")
  expect_error(make_group_labels(NA, c(A = 0.5, B = 0.5)), "non-negative integer")
  expect_error(make_group_labels(c(1, 2), c(A = 0.5, B = 0.5)), "non-negative integer")
})

test_that("invalid 'prop' values are rejected", {
  expect_error(make_group_labels(10, numeric(0)), "non-empty numeric vector")
  expect_error(make_group_labels(10, c(A = NA, B = 0.5)), "negative values or NAs")
  expect_error(make_group_labels(10, c(A = -0.5, B = 1.5)), "negative values or NAs")
  expect_error(make_group_labels(10, c(A = 0, B = 0)), "must not sum to zero")
})

test_that("duplicate group labels are rejected", {
  expect_error(make_group_labels(10, c(A = 0.5, A = 0.5)), "unique")
})
