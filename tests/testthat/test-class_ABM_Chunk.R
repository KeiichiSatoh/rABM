# Tests for Chunk() / print.ABM_Chunk() (class_ABM_Chunk.R)

test_that("Chunk() wraps a multi-line block and tags it with class 'ABM_Chunk'", {
  ch <- Chunk({
    x <- 1
    y <- 2
  })

  expect_s3_class(ch, "ABM_Chunk")
  expect_true(is.call(ch))
  expect_identical(ch[[1]], as.symbol("{"))
})

test_that("Chunk() auto-wraps a bare single-line expression in {}", {
  ch1 <- Chunk(x <- 1)
  ch2 <- Chunk({ x <- 1 })
  expect_identical(ch1, ch2)
})

test_that("Chunk() does not evaluate its argument", {
  # A bare call (not a symbol) must never be evaluated at Chunk()-call time.
  counter <- 0
  side_effect_fn <- function() {
    counter <<- counter + 1
    TRUE
  }

  ch <- Chunk(side_effect_fn())
  expect_equal(counter, 0)
  expect_s3_class(ch, "ABM_Chunk")

  # stop() inside the captured expression must not fire either.
  expect_no_error(Chunk(stop("should never run")))
})

test_that("Chunk() is idempotent: passing an existing ABM_Chunk symbol returns it unchanged", {
  step1 <- Chunk({ x <- 1 })
  ch2 <- Chunk(step1)

  expect_identical(ch2, step1)
})

test_that("Chunk() only special-cases a bare symbol referring to an ABM_Chunk", {
  # A symbol whose value is *not* an ABM_Chunk is wrapped normally, not
  # short-circuited.
  some_number <- 5
  ch <- Chunk(some_number)

  expected <- quote({some_number})
  class(expected) <- c("ABM_Chunk", class(expected))

  expect_identical(ch, expected)
})

test_that("Chunk() tolerates a symbol that does not exist in the calling scope", {
  # Referencing an undefined variable must not error at Chunk()-call time
  # (only later, when/if the chunk is actually evaluated).
  expect_no_error(ch <- Chunk(this_var_does_not_exist_anywhere_12345))
  expect_s3_class(ch, "ABM_Chunk")
})

test_that("Chunk() does not special-case non-symbol expressions even if they would evaluate to an ABM_Chunk", {
  step1 <- Chunk({ x <- 1 })
  # `(step1)` parses as a call to `(`, not a bare symbol, so the idempotency
  # guard must not fire here.
  ch <- Chunk((step1))
  expect_false(identical(ch, step1))
  expect_s3_class(ch, "ABM_Chunk")
})

test_that("print.ABM_Chunk() prints a header and the deparsed body, invisibly returning x", {
  ch <- Chunk({ x <- 1 })
  out <- capture.output(print(ch))

  expect_true(any(grepl("^<ABM_Chunk>$", out)))
  expect_true(any(grepl("x <- 1", out, fixed = TRUE)))

  ret <- withVisible(print(ch))
  expect_false(ret$visible)
  expect_identical(ret$value, ch)
})
