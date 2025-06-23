# tests/testthat/test-sample2.R

test_that("sample2 returns expected values for scalar input", {
  expect_equal(sample2(3), 3)
  expect_equal(sample2(100), 100)
})

test_that("sample2 returns elements from the input vector", {
  x <- letters[1:5]
  out <- sample2(x, size = 3)
  expect_true(all(out %in% x))
  expect_length(out, 3)
})

test_that("sample2 handles replacement correctly", {
  x <- 1:3
  out <- sample2(x, size = 10, replace = TRUE)
  expect_true(all(out %in% x))
  expect_length(out, 10)
})

test_that("sample2 respects probability weights", {
  x <- c("a", "b", "c")
  prob <- c(1, 0, 0)  # Always sample "a"
  out <- sample2(x, size = 5, replace = TRUE, prob = prob)
  expect_true(all(out == "a"))
})

