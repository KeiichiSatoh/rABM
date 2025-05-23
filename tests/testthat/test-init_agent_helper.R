
#===============================================================================
# TEST of ".shape_agent_attr"
#===============================================================================
test_that(".shape_agent_attr returns NULL when attr is NULL", {
  x <- NULL
  attr_sbs <- substitute(x)
  result <- .shape_agent_attr(attr = x, attr_sbs = attr_sbs)
  expect_null(result)
})

test_that(".shape_agent_attr works with vector input and assigns correct column name", {
  y <- 1:5
  result <- .shape_agent_attr(attr = y, attr_sbs = quote(y))

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 1)
  expect_equal(result[[1]], 1:5)
  expect_equal(colnames(result), "y")
})

test_that(".shape_agent_attr works with unnamed vector input and fallback name", {
  attr_sbs <- quote(1:5)  # symbolでないので fallback の "X" を使う
  result <- .shape_agent_attr(attr = 1:5, attr_sbs = attr_sbs)

  expect_s3_class(result, "data.frame")
  expect_equal(colnames(result), "X")
})

test_that(".shape_agent_attr returns data.frame unchanged if input is already data.frame", {
  df <- data.frame(a = 1:5, b = letters[1:5])
  attr_sbs <- substitute(df)
  result <- .shape_agent_attr(attr = df, attr_sbs = attr_sbs)

  expect_identical(result, df)
})

test_that(".shape_agent_attr throws error for invalid input (e.g., list)", {
  bad_input <- list(a = 1:3)
  attr_sbs <- substitute(bad_input)

  expect_error(
    .shape_agent_attr(attr = bad_input, attr_sbs = attr_sbs),
    "attr must be either vector or data.frame."
  )
})
