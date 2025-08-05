test_that(".remove_code_comments removes comments but keeps roxygen", {
  script <- "
# This is a comment
x <- 1  # inline comment
#' Title: This is a title
#' @param x A parameter
y <- x + 1
"
  cleaned <- .remove_code_comments(script)
  expect_match(cleaned, "x <- 1")
  expect_match(cleaned, "#' Title: This is a title")
  expect_false(grepl("# This is a comment", cleaned))
  expect_false(grepl("# inline comment", cleaned))
})

test_that(".roxygen_to_natural transforms roxygen comments to plain text", {
  roxy <- "#' @title Sample Title\n#' @param x An input\n#' @return An output"
  result <- .roxygen_to_natural(roxy)
  expect_match(result, "Title: Sample Title")
  expect_match(result, "Parameter 'x'")
  expect_match(result, "Returns: An output")
})

# test_that("safe_readLines handles invalid URLs", {
#  result <- safe_readLines("https://invalid.url/test.R")
#  expect_type(result, "character")
#  expect_equal(result, "")
# })

