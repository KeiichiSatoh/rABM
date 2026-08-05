# Tests for Series() / print.ABM_Series() (class_ABM_Series.R)

# ---- naming resolution -----------------------------------------------------

test_that("bare symbol arguments use the variable name as the chunk name", {
  step1 <- Chunk({ x <- 1 })
  step2 <- Chunk({ y <- 2 })

  S <- Series(step1, step2)

  expect_identical(names(S$chunks), c("step1", "step2"))
  expect_identical(S$chunks$step1, step1)
  expect_identical(S$chunks$step2, step2)
})

test_that("explicit names override the variable-name inference", {
  step1 <- Chunk({ x <- 1 })
  step2 <- Chunk({ y <- 2 })

  S <- Series(init = step1, calc = step2)

  expect_identical(names(S$chunks), c("init", "calc"))
})

test_that("named and unnamed (bare-symbol) arguments can be mixed", {
  step1 <- Chunk({ x <- 1 })
  step2 <- Chunk({ y <- 2 })

  S <- Series(step1, calc = step2)

  expect_identical(names(S$chunks), c("step1", "calc"))
})

test_that("an inline, unnamed non-symbol chunk gets an automatic 'Chunk<N>' name", {
  step1 <- Chunk({ x <- 1 })

  S <- Series(step1, Chunk({ z <- 1 }))

  expect_identical(names(S$chunks), c("step1", "Chunk1"))
})

test_that("multiple inline chunks are numbered sequentially", {
  S <- Series(Chunk({ a <- 1 }), Chunk({ b <- 2 }))

  expect_identical(names(S$chunks), c("Chunk1", "Chunk2"))
})

test_that("automatic numbering only counts the arguments that actually need it", {
  step1 <- Chunk({ x <- 1 })

  S <- Series(
    step1,                 # -> "step1"      (bare symbol)
    Chunk({ a <- 1 }),      # -> "Chunk1"     (inline, 1st auto)
    named = Chunk({ b <- 2 }), # -> "named"   (explicit)
    Chunk({ c <- 1 })       # -> "Chunk2"     (inline, 2nd auto)
  )

  expect_identical(names(S$chunks), c("step1", "Chunk1", "named", "Chunk2"))
})

test_that("regression: Series() with all-unnamed bare-symbol arguments no longer errors", {
  # This is the case that used to fail with
  # "'names' attribute [0] must be the same length as the vector [2]"
  # because names(dot_exprs) was NULL (not a "" vector) when nothing was
  # named.
  step1 <- Chunk({ x <- 1 })
  step2 <- Chunk({ y <- 2 })

  expect_no_error(S <- Series(step1, step2))
  expect_identical(names(S$chunks), c("step1", "step2"))
})

test_that("Series() with no chunk arguments succeeds with an empty chunk list", {
  S <- Series()
  expect_length(S$chunks, 0)
})

test_that("passing an existing ABM_Chunk back through Chunk() falls back to auto-naming", {
  step1 <- Chunk({ x <- 1 })

  # Chunk(step1) returns step1 itself (idempotency guard), but the
  # *expression* Series() sees is `Chunk(step1)` -- a call, not a bare
  # symbol -- so it cannot recover "step1" as the name and falls back to
  # automatic numbering instead.
  S <- Series(Chunk(step1))

  expect_identical(names(S$chunks), "Chunk1")
  expect_identical(S$chunks$Chunk1, step1)
})

# ---- validation -------------------------------------------------------------

test_that("duplicated chunk names raise an error", {
  step1 <- Chunk({ x <- 1 })
  expect_error(Series(step1, step1), "Duplicated")
})

test_that("non-ABM_Chunk arguments raise an error naming the offending argument", {
  step1 <- Chunk({ x <- 1 })
  expect_error(Series(step1, 5), "Invalid argument")
})

# ---- default validation ------------------------------------------------------

test_that("default is an empty list when not supplied", {
  step1 <- Chunk({ x <- 1 })
  S <- Series(step1)
  expect_identical(S$default, list())
})

test_that("default accepts a fully named plain list", {
  step1 <- Chunk({ x <- 1 })
  add2 <- function(a, b) a + b

  S <- Series(step1, default = list(a = 1, f = add2))

  expect_identical(names(S$default), c("a", "f"))
  expect_identical(S$default$a, 1)
  expect_identical(S$default$f, add2)
})

test_that("default rejects a fully unnamed list", {
  step1 <- Chunk({ x <- 1 })
  expect_error(Series(step1, default = list(1, 2)), "fully named")
})

test_that("default rejects a partially named list", {
  step1 <- Chunk({ x <- 1 })
  expect_error(Series(step1, default = list(a = 1, 2)), "fully named")
})

test_that("default rejects non-list input", {
  step1 <- Chunk({ x <- 1 })
  expect_error(Series(step1, default = 5), "plain named list")
})

test_that("default rejects a data.frame (an S3 object, not a plain list)", {
  step1 <- Chunk({ x <- 1 })
  expect_error(Series(step1, default = data.frame(x = 1)), "plain named list")
})

test_that("duplicated default names raise an error", {
  step1 <- Chunk({ x <- 1 })
  expect_error(Series(step1, default = list(a = 1, a = 2)), "Duplicated")
})

# ---- print.ABM_Series --------------------------------------------------------

test_that("print.ABM_Series shows only metadata by default (contents = FALSE)", {
  step1 <- Chunk({ x <- 1 })
  step2 <- Chunk({ y <- 2 })
  S <- Series(step1, step2, default = list(a = 1))

  out <- capture.output(print(S))

  expect_true(any(grepl("^<Series>$", out)))
  expect_false(any(grepl("^\\[chunks\\]$", out)))
  expect_false(any(grepl("^\\[default\\]$", out)))
  expect_true(any(grepl("n of chunks  : 2", out, fixed = TRUE)))
  expect_true(any(grepl("n of defaults: 1", out, fixed = TRUE)))
  expect_true(any(grepl("hidden by default", out)))
})

test_that("print.ABM_Series(contents = TRUE) previews chunk/default contents", {
  step1 <- Chunk({ x <- 1 })
  step2 <- Chunk({ y <- 2 })
  S <- Series(step1, step2, default = list(a = 1))

  out <- capture.output(print(S, contents = TRUE))

  expect_true(any(grepl("^<Series>$", out)))
  expect_true(any(grepl("^\\[chunks\\]$", out)))
  expect_true(any(grepl("^\\[default\\]$", out)))
  expect_true(any(grepl("n of chunks  : 2", out, fixed = TRUE)))
  expect_true(any(grepl("n of defaults: 1", out, fixed = TRUE)))
  expect_false(any(grepl("hidden by default", out)))
})

test_that("print.ABM_Series(contents = TRUE) shows '(none)' when there are no defaults", {
  step1 <- Chunk({ x <- 1 })
  S <- Series(step1)

  out <- capture.output(print(S, contents = TRUE))
  expect_true(any(grepl("\\(none\\)", out)))
})

test_that("print.ABM_Series(contents = TRUE) truncates long previews and reports it", {
  long_chunk <- Chunk({
    a <- 1; b <- 2; c <- 3; d <- 4; e <- 5; f <- 6; g <- 7; h <- 8
  })
  S <- Series(long_chunk)

  out <- capture.output(print(S, contents = TRUE, max_lines = 2))
  expect_true(any(grepl("truncated", out)))
})

test_that("print.ABM_Series validates 'contents'", {
  step1 <- Chunk({ x <- 1 })
  S <- Series(step1)

  expect_error(print(S, contents = "yes"))
  expect_error(print(S, contents = c(TRUE, FALSE)))
  expect_error(print(S, contents = NA))
})

test_that("print.ABM_Series validates 'max_lines'", {
  step1 <- Chunk({ x <- 1 })
  S <- Series(step1)

  expect_error(print(S, contents = TRUE, max_lines = -1))
  expect_error(print(S, contents = TRUE, max_lines = 1.5))
  expect_error(print(S, contents = TRUE, max_lines = c(1, 2)))
})

test_that("print.ABM_Series returns x invisibly", {
  step1 <- Chunk({ x <- 1 })
  S <- Series(step1)

  ret <- withVisible(print(S))
  expect_false(ret$visible)
  expect_identical(ret$value, S)
})
