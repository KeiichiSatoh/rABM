# Tests for run_Series() and its internal helper .format_input() (run_Series.R)

# ---- basic execution ---------------------------------------------------------

test_that("run_Series executes chunks in order and accumulates state", {
  step1 <- Chunk({ x <- 1; y <- 2 })
  step2 <- Chunk({ z <- add2(x, y) })
  S <- Series(step1, step2, default = list(add2 = function(a, b) a + b))

  out <- run_Series(S, verbose = FALSE)

  expect_equal(out$values$z, 3)
  expect_equal(out$series_plan, c("step1", "step2"))
})

test_that("series_plan = NULL runs all chunks in their stored order", {
  step1 <- Chunk({ x <- 1 })
  step2 <- Chunk({ y <- x + 1 })
  S <- Series(step1, step2)

  out <- run_Series(S, verbose = FALSE)

  expect_equal(out$series_plan, c("step1", "step2"))
  expect_equal(out$values$y, 2)
})

test_that("series_plan by character name runs only the requested subset", {
  step1 <- Chunk({ x <- 1 })
  step2 <- Chunk({ y <- 99 })
  S <- Series(step1, step2)

  out <- run_Series(S, series_plan = "step1", verbose = FALSE)

  expect_equal(out$series_plan, "step1")
  expect_equal(out$values$x, 1)
  expect_null(out$values$y)
})

test_that("an unknown chunk name in series_plan raises an error", {
  step1 <- Chunk({ x <- 1 })
  S <- Series(step1)

  expect_error(
    run_Series(S, series_plan = "nope", verbose = FALSE),
    "No corresponding chunk"
  )
})

# ---- series_plan numeric validation (bug fix) --------------------------------

test_that("series_plan accepts whole-number numeric indices, including doubles", {
  step1 <- Chunk({ x <- 1 })
  step2 <- Chunk({ y <- 2 })
  S <- Series(step1, step2)

  out1 <- run_Series(S, series_plan = 2, verbose = FALSE)
  expect_equal(out1$series_plan, "step2")

  out2 <- run_Series(S, series_plan = 2.0, verbose = FALSE)
  expect_equal(out2$series_plan, "step2")
})

test_that("series_plan rejects non-integer numeric indices instead of silently truncating", {
  step1 <- Chunk({ x <- 1 })
  step2 <- Chunk({ y <- 2 })
  S <- Series(step1, step2)

  expect_error(
    run_Series(S, series_plan = 1.5, verbose = FALSE),
    "whole-number"
  )
})

test_that("series_plan rejects out-of-range indices", {
  step1 <- Chunk({ x <- 1 })
  S <- Series(step1)

  expect_error(
    run_Series(S, series_plan = 5, verbose = FALSE),
    "out of range"
  )
})

# ---- input / default interaction --------------------------------------------

test_that("input overrides matching default values", {
  step1 <- Chunk({ z <- x })
  S <- Series(step1, default = list(x = 1))

  out <- run_Series(S, input = list(x = 99), verbose = FALSE)

  expect_equal(out$values$z, 99)
})

test_that("an explicit NULL in input keeps the name bound to NULL (bug fix)", {
  step1 <- Chunk({
    found_x <- exists("x", inherits = FALSE)
    val_x   <- x
  })
  S <- Series(step1, default = list(x = 1))

  out <- run_Series(S, input = list(x = NULL), verbose = FALSE)

  expect_true(out$values$found_x)
  expect_null(out$values$val_x)
})

test_that("a single unnamed object passed to input is auto-named from its variable", {
  step1 <- Chunk({ doubled <- my_val * 2 })
  S <- Series(step1)

  my_val <- 21
  out <- run_Series(S, input = my_val, verbose = FALSE)

  expect_equal(out$values$doubled, 42)
})

# ---- on_error handling (bug fix) ---------------------------------------------

test_that("on_error = 'continue' logs the error and keeps running later chunks", {
  step1     <- Chunk({ x <- 1 })
  step_bad  <- Chunk({ stop("boom") })
  step_after <- Chunk({ ran_after <- TRUE })
  S <- Series(step1, step_bad, step_after)

  out <- run_Series(S, on_error = "continue", verbose = FALSE)

  expect_length(out$error_log, 1)
  expect_true(out$values$ran_after)
})

test_that("on_error = 'stop' halts remaining chunks but run_Series() still returns normally", {
  step1     <- Chunk({ x <- 1 })
  step_bad  <- Chunk({ stop("boom") })
  step_after <- Chunk({ ran_after <- TRUE })
  S <- Series(step1, step_bad, step_after)

  expect_no_error(
    out <- run_Series(S, on_error = "stop", verbose = FALSE)
  )

  expect_length(out$error_log, 1)
  expect_equal(out$values$x, 1)
  expect_null(out$values$ran_after)
})

test_that("on_error = 'stop' records the error's message in error_log", {
  step_bad <- Chunk({ stop("boom") })
  S <- Series(step_bad)

  out <- run_Series(S, on_error = "stop", verbose = FALSE)

  expect_length(out$error_log, 1)
  expect_true(grepl("boom", conditionMessage(out$error_log[[1]])))
})

# ---- output shape -------------------------------------------------------------

test_that("simplify_output = TRUE returns only the resulting values", {
  step1 <- Chunk({ x <- 1 })
  S <- Series(step1)

  out <- run_Series(S, simplify_output = TRUE, verbose = FALSE)

  expect_true(is.list(out))
  expect_equal(out$x, 1)
  expect_null(out$series_plan)
})

test_that("simplify_output = FALSE returns values plus run metadata", {
  step1 <- Chunk({ x <- 1 })
  S <- Series(step1)

  out <- run_Series(S, simplify_output = FALSE, verbose = FALSE)

  expect_true(all(c("values", "series_plan", "series_seed",
                     "error_log", "implementation_took") %in% names(out)))
})

test_that("keep subsets the returned values and warns on missing names", {
  step1 <- Chunk({ x <- 1; y <- 2 })
  S <- Series(step1)

  expect_warning(
    out <- run_Series(S, keep = c("x", "nonexistent"), verbose = FALSE),
    "not found"
  )
  expect_identical(names(out$values), "x")
})

test_that("keep works together with simplify_output = TRUE", {
  step1 <- Chunk({ x <- 1; y <- 2 })
  S <- Series(step1)

  out <- run_Series(S, keep = "x", simplify_output = TRUE, verbose = FALSE)
  expect_identical(names(out), "x")
})

# ---- seed handling (bug fix) --------------------------------------------------

test_that("series_seed makes a run reproducible", {
  step1 <- Chunk({ r <- sample(1:1000, 1) })
  S <- Series(step1)

  out1 <- run_Series(S, series_seed = 42, verbose = FALSE)
  out2 <- run_Series(S, series_seed = 42, verbose = FALSE)

  expect_equal(out1$values$r, out2$values$r)
  expect_equal(out1$series_seed, 42)
})

test_that("an auto-generated series_seed is recorded and is a valid integer-ish value", {
  step1 <- Chunk({ x <- 1 })
  S <- Series(step1)

  out <- run_Series(S, verbose = FALSE)

  expect_true(is.numeric(out$series_seed))
  expect_true(out$series_seed >= 1 && out$series_seed <= .Machine$integer.max)
})

# ---- environment / parent handling --------------------------------------------

test_that("chunks can access objects from the calling environment via the default 'parent'", {
  helper_fn <- function() {
    local_val <- 7
    step1 <- Chunk({ out_val <- local_val + 1 })
    S <- Series(step1)
    run_Series(S, verbose = FALSE)
  }

  out <- helper_fn()
  expect_equal(out$values$out_val, 8)
})

test_that("parent = baseenv() isolates chunks from the calling environment", {
  outer_val <- 999
  step1 <- Chunk({ out_val <- outer_val + 1 })
  S <- Series(step1)

  out <- run_Series(S, parent = baseenv(), on_error = "continue", verbose = FALSE)

  expect_length(out$error_log, 1)
  expect_true(grepl("outer_val", conditionMessage(out$error_log[[1]])))
})

# ---- input validation ----------------------------------------------------------

test_that("run_Series validates that 'S' is an ABM_Series object", {
  expect_error(run_Series("not a series"), "ABM_Series")
})

test_that("run_Series validates S$chunks is a properly named list", {
  bad_S <- structure(list(chunks = "not a list", default = list()),
                      class = "ABM_Series")
  expect_error(run_Series(bad_S, verbose = FALSE), "must be a list")

  bad_S2 <- structure(list(chunks = list(a = 1, 2), default = list()),
                       class = "ABM_Series")
  expect_error(run_Series(bad_S2, verbose = FALSE), "named list")
})

test_that("run_Series validates 'keep'", {
  step1 <- Chunk({ x <- 1 })
  S <- Series(step1)

  expect_error(run_Series(S, keep = 5, verbose = FALSE))
  expect_error(run_Series(S, keep = "", verbose = FALSE))
})

test_that("verbose = TRUE prints the plan and step progress without erroring", {
  step1 <- Chunk({ x <- 1 })
  S <- Series(step1)

  expect_output(run_Series(S, verbose = TRUE), "Series plan")
})

# ---- .format_input() (internal helper) -----------------------------------------

test_that(".format_input() returns NULL for NULL input", {
  expect_null(.format_input(NULL, quote(x)))
})

test_that(".format_input() passes a fully named list through unchanged", {
  out <- .format_input(list(a = 1, b = 2), quote(y))
  expect_equal(out, list(a = 1, b = 2))
})

test_that(".format_input() errors only on a partially named list", {
  # names(list(a = 1, 2)) is c("a", "") (not NULL), so the "any element
  # missing a name" check applies and raises an error.
  expect_error(.format_input(list(a = 1, 2), quote(y)), "Put names")
})

test_that(".format_input() auto-names a fully unnamed list rather than erroring", {
  # names(list(1, 2)) is NULL (not c("", "")), so this does NOT hit the
  # "Put names to all elements." check -- it falls through to prefix-based
  # auto-naming instead, the same as a single unnamed object would.
  out <- .format_input(list(1, 2), quote(y), prefix = "input")
  expect_equal(names(out), c("input1", "input2"))
  expect_equal(unname(out), list(1, 2))
})

test_that(".format_input() infers a name from a single bare-symbol argument", {
  z <- 5
  out <- .format_input(z, quote(z))
  expect_equal(out, list(z = 5))
})

test_that(".format_input() falls back to prefix-based auto-naming otherwise", {
  out <- .format_input(5, quote(1 + 1), prefix = "input")
  expect_equal(names(out), "input1")
  expect_equal(out[["input1"]], 5)
})
