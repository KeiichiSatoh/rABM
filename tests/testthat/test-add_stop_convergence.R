# tests/testthat/test-add_stop_convergence.R
#
# Assumes the package defining Game(), State(), Act(), Stop(), add_field(),
# copy_obj(), append_to_body(), and add_stop_convergence() is already
# loaded (e.g. via devtools::load_all() / library(rABM)).

# ---- helpers ----------------------------------------------------------

make_G <- function(x0 = 10){
  x <- x0
  Game(State(x))
}

# ==========================================================================
# append_to_body()
# ==========================================================================

test_that("append_to_body() appends a block after the existing body by default", {
  # NB: function(){ 1 } (braced) is used deliberately -- a brace-less body
  # like `function() 1` has body(f) == 1 (a bare literal, not a `{`-call),
  # so as.list(body(f))[-1] would drop it entirely.
  f <- function(){ 1 }
  new_f <- append_to_body(f, quote({ 2 }))
  expect_equal(new_f(), 2)                 # last statement wins
  expect_equal(length(body(new_f)), 3)      # `{`, `1`, `2`
})

test_that("append_to_body() inserts a block before the existing body when posit = 'before'", {
  log <- c()
  f <- function(){ log <<- c(log, "orig"); "orig_result" }
  new_f <- append_to_body(f, quote({ log <<- c(log, "extra") }), posit = "before")
  result <- new_f()
  expect_equal(log, c("extra", "orig"))
  expect_equal(result, "orig_result")
})

test_that("append_to_body() works with bquote()-substituted blocks", {
  nm <- "foo"
  blk <- bquote({ y <- .(nm) })
  f <- function() NULL
  new_f <- append_to_body(f, blk)
  # deparse should contain the literal substituted string, not the variable name
  expect_true(grepl('"foo"', paste(deparse(body(new_f)), collapse = " ")))
})

test_that("append_to_body() errors on an invalid 'posit'", {
  f <- function() 1
  expect_error(append_to_body(f, quote({ 2 }), posit = "middle"))
})

test_that("append_to_body() does not mutate the original function", {
  f <- function() 1
  new_f <- append_to_body(f, quote({ 2 }))
  expect_equal(f(), 1)
  expect_equal(new_f(), 2)
})

# ==========================================================================
# add_stop_convergence() -- input validation
# ==========================================================================

test_that("add_stop_convergence() validates 'watching_field'", {
  G <- make_G()
  expect_error(add_stop_convergence(G, watching_field = 1))
  expect_error(add_stop_convergence(G, watching_field = c("x", "y")))
  expect_error(add_stop_convergence(G, watching_field = ""))
  expect_error(add_stop_convergence(G, watching_field = "not_a_field"),
               regexp = "not a field")
})

test_that("add_stop_convergence() validates 'state_name' and 'stop_FUN_name'", {
  G <- make_G()
  expect_error(add_stop_convergence(G, watching_field = "x", state_name = ""))
  expect_error(add_stop_convergence(G, watching_field = "x", state_name = NA_character_))
  expect_error(add_stop_convergence(G, watching_field = "x", stop_FUN_name = character(0)))
})

test_that("add_stop_convergence() validates 'eval_by'", {
  G <- make_G()
  expect_error(add_stop_convergence(G, watching_field = "x", eval_by = "not_a_method"))
})

test_that("add_stop_convergence() validates 'eval_length'", {
  G <- make_G()
  expect_error(add_stop_convergence(G, watching_field = "x", eval_length = 0))
  expect_error(add_stop_convergence(G, watching_field = "x", eval_length = -1))
  expect_error(add_stop_convergence(G, watching_field = "x", eval_length = 2.5))
  expect_error(add_stop_convergence(G, watching_field = "x", eval_length = NA))
})

test_that("add_stop_convergence() requires eval_length >= 2 for diff/sd-based criteria", {
  G <- make_G()
  expect_error(
    add_stop_convergence(G, watching_field = "x", eval_by = "absdiff", eval_length = 1),
    regexp = "eval_length"
  )
  expect_error(
    add_stop_convergence(G, watching_field = "x", eval_by = "reldiff", eval_length = 1),
    regexp = "eval_length"
  )
  expect_error(
    add_stop_convergence(G, watching_field = "x", eval_by = "sd", eval_length = 1),
    regexp = "eval_length"
  )
  # "lower"/"upper"/"range" are fine with eval_length = 1
  expect_no_error(
    suppressMessages(
      add_stop_convergence(G, watching_field = "x", eval_by = "lower",
                           eval_length = 1, thresh = 0)
    )
  )
})

test_that("add_stop_convergence() validates 'thresh' for eval_by = 'range'", {
  G <- make_G()
  expect_error(add_stop_convergence(G, watching_field = "x", eval_by = "range", thresh = 1))
  expect_error(add_stop_convergence(G, watching_field = "x", eval_by = "range", thresh = c(1, 2, 3)))
  expect_error(add_stop_convergence(G, watching_field = "x", eval_by = "range", thresh = c(2, 1)))
  expect_error(add_stop_convergence(G, watching_field = "x", eval_by = "range", thresh = c("a", "b")))
  expect_no_error(
    suppressMessages(
      add_stop_convergence(G, watching_field = "x", eval_by = "range", thresh = c(1, 2))
    )
  )
})

test_that("add_stop_convergence() validates 'thresh' for non-range eval_by", {
  G <- make_G()
  expect_error(add_stop_convergence(G, watching_field = "x", eval_by = "absdiff", thresh = c(1, 2)))
  expect_error(add_stop_convergence(G, watching_field = "x", eval_by = "absdiff", thresh = "a"))
  expect_error(add_stop_convergence(G, watching_field = "x", eval_by = "absdiff", thresh = NA_real_))
})

test_that("add_stop_convergence() validates 'include_max_times' and 'max_times'", {
  G <- make_G()
  expect_error(add_stop_convergence(G, watching_field = "x", include_max_times = "yes"))
  expect_error(add_stop_convergence(G, watching_field = "x", include_max_times = TRUE, max_times = -1))
  expect_error(add_stop_convergence(G, watching_field = "x", include_max_times = TRUE, max_times = 1.5))
  # max_times is not validated when include_max_times = FALSE
  expect_no_error(
    suppressMessages(
      add_stop_convergence(G, watching_field = "x", eval_by = "lower", thresh = 0,
                           include_max_times = FALSE, max_times = -1)
    )
  )
})

test_that("add_stop_convergence() validates 'FUN' when supplied", {
  G <- make_G()
  expect_error(add_stop_convergence(G, watching_field = "x", FUN = "not_a_function"))
})

# ==========================================================================
# add_stop_convergence() -- field registration
# ==========================================================================

test_that("add_stop_convergence() registers the state and stop_FUN fields with the requested names", {
  G <- make_G()
  G2 <- suppressMessages(
    add_stop_convergence(G, watching_field = "x", state_name = "conv",
                         stop_FUN_name = "my_stop", eval_by = "lower", thresh = 0)
  )
  flist <- G2$.get_flist()
  expect_true("conv" %in% flist$name)
  expect_true("my_stop" %in% flist$name)
  expect_equal(flist$category[flist$name == "conv"], "state")
  expect_equal(flist$category[flist$name == "my_stop"], "stop_FUN")
})

test_that("add_stop_convergence() does not mutate the original G (deep copy)", {
  G <- make_G()
  flist_before <- G$.get_flist()$name
  invisible(suppressMessages(
    add_stop_convergence(G, watching_field = "x", eval_by = "lower", thresh = 0)
  ))
  expect_equal(G$.get_flist()$name, flist_before)
})

test_that("add_stop_convergence() stores max_times only when include_max_times = TRUE", {
  G <- make_G()
  G_with <- suppressMessages(
    add_stop_convergence(G, watching_field = "x", eval_by = "lower", thresh = 0,
                         include_max_times = TRUE, max_times = 42)
  )
  expect_equal(G_with$convergence$max_times, 42)

  G_without <- suppressMessages(
    add_stop_convergence(G, watching_field = "x", eval_by = "lower", thresh = 0,
                         include_max_times = FALSE)
  )
  expect_null(G_without$convergence$max_times)
})

test_that("add_stop_convergence() emits an informative message", {
  G <- make_G()
  expect_message(
    add_stop_convergence(G, watching_field = "x", state_name = "conv",
                         stop_FUN_name = "my_stop", eval_by = "lower", thresh = 0),
    regexp = "conv.*my_stop"
  )
})

# ==========================================================================
# add_stop_convergence() -- generated stop_FUN behavior
# ==========================================================================
#
# Fields registered as stop_FUN are already re-bound to `self` (see
# ABM_Game$.add_method()), so the generated function can be invoked directly
# as G$<stop_FUN_name>() once G has been mutated (e.g. G$x <- ...).

test_that("'lower' criterion converges once the watched value drops below thresh", {
  G <- make_G(x0 = 10)
  G2 <- suppressMessages(
    add_stop_convergence(G, watching_field = "x", eval_by = "lower",
                         eval_length = 1, thresh = 0, max_times = 500)
  )

  expect_false(G2$converged())  # x = 10: first call just records the value
  G2$x <- 10
  expect_false(G2$converged())  # still above thresh
  G2$x <- -1
  expect_true(G2$converged())   # now below thresh
})

test_that("'upper' criterion converges once the watched value exceeds thresh", {
  G <- make_G(x0 = 0)
  G2 <- suppressMessages(
    add_stop_convergence(G, watching_field = "x", eval_by = "upper",
                         eval_length = 1, thresh = 5, max_times = 500)
  )

  expect_false(G2$converged())  # x = 0: warm-up
  G2$x <- 0
  expect_false(G2$converged())  # still <= thresh
  G2$x <- 10
  expect_true(G2$converged())   # now above thresh
})

test_that("'absdiff' criterion converges once consecutive differences are small", {
  G <- make_G(x0 = 10)
  G2 <- suppressMessages(
    add_stop_convergence(G, watching_field = "x", eval_by = "absdiff",
                         eval_length = 2, thresh = 0.5, max_times = 500)
  )

  expect_false(G2$converged())  # values: [] -> [10], warm-up (length 0 < 2)
  G2$x <- 20
  expect_false(G2$converged())  # values: [10] -> [10, 20], warm-up (length 1 < 2)
  G2$x <- 100
  expect_false(G2$converged())  # values: [10,20] -> [20,100], |diff| = 80 >= thresh
  G2$x <- 100.05
  expect_true(G2$converged())   # values: [20,100] -> [100,100.05], |diff| = 0.05 < thresh
})

test_that("'sd' criterion converges once the window's standard deviation is small", {
  G <- make_G(x0 = 0)
  G2 <- suppressMessages(
    add_stop_convergence(G, watching_field = "x", eval_by = "sd",
                         eval_length = 3, thresh = 0.1, max_times = 500)
  )

  expect_false(G2$converged())  # values: [] -> [0], warm-up (length 0 < 3)
  G2$x <- 0
  expect_false(G2$converged())  # values: [0] -> [0,0], warm-up (length 1 < 3)
  G2$x <- 0
  expect_false(G2$converged())  # values: [0,0] -> [0,0,0], warm-up (length 2 < 3)
  G2$x <- 10
  expect_false(G2$converged())  # values: [0,0,0] -> [0,0,10], sd is large
  G2$x <- 10
  expect_false(G2$converged())  # values: [0,0,10] -> [0,10,10], sd still large
  G2$x <- 10
  expect_true(G2$converged())   # values: [0,10,10] -> [10,10,10], sd = 0 < thresh
})

test_that("'range' criterion converges once all recent values fall within bounds", {
  G <- make_G(x0 = 100)
  G2 <- suppressMessages(
    add_stop_convergence(G, watching_field = "x", eval_by = "range",
                         eval_length = 1, thresh = c(0, 1), max_times = 500)
  )

  expect_false(G2$converged())  # x = 100, warm-up
  G2$x <- 100
  expect_false(G2$converged())  # out of [0, 1]
  G2$x <- 0.5
  expect_true(G2$converged())   # within [0, 1]
})

test_that("'reldiff' criterion converges once relative change is small", {
  G <- make_G(x0 = 100)
  G2 <- suppressMessages(
    add_stop_convergence(G, watching_field = "x", eval_by = "reldiff",
                         eval_length = 2, thresh = 0.01, max_times = 500)
  )

  expect_false(G2$converged())  # values: [] -> [100], warm-up (length 0 < 2)
  G2$x <- 200
  expect_false(G2$converged())  # values: [100] -> [100,200], warm-up (length 1 < 2)
  G2$x <- 400
  expect_false(G2$converged())  # values: [100,200] -> [200,400], |diff|/200 = 1.0 >= thresh
  G2$x <- 400.01
  expect_true(G2$converged())   # values: [200,400] -> [400,400.01], relative diff ~0.000025 < thresh
})

test_that("simulation stops once self$time reaches max_times, regardless of eval_by", {
  G <- make_G(x0 = 0)
  G2 <- suppressMessages(
    add_stop_convergence(G, watching_field = "x", eval_by = "lower",
                         eval_length = 1, thresh = -999, max_times = 3,
                         include_max_times = TRUE)
  )
  G2$time <- 3
  expect_true(G2$converged())  # max_times reached, even though 'lower' would say FALSE
})

test_that("include_max_times = FALSE never stops on time alone", {
  G <- make_G(x0 = 0)
  G2 <- suppressMessages(
    add_stop_convergence(G, watching_field = "x", eval_by = "lower",
                         eval_length = 1, thresh = -999, max_times = 1,
                         include_max_times = FALSE)
  )
  G2$time <- 999
  expect_false(G2$converged())  # thresh (-999) never satisfied by x = 0
})

# ==========================================================================
# add_stop_convergence() -- custom FUN
# ==========================================================================

test_that("a custom FUN's body runs before the convergence check", {
  G <- make_G(x0 = 10)
  custom <- function(){
    if (isTRUE(self$notes$force_stop)) return(TRUE)
  }
  G2 <- suppressMessages(
    add_stop_convergence(G, watching_field = "x", eval_by = "lower",
                         eval_length = 1, thresh = -999, FUN = custom,
                         include_max_times = FALSE)
  )
  G2$notes <- list(force_stop = TRUE)
  expect_true(G2$converged())  # short-circuits via the custom FUN, not convergence
})

test_that("state_name works with a name other than the default 'convergence'", {
  G <- make_G(x0 = 10)
  G2 <- suppressMessages(
    add_stop_convergence(G, watching_field = "x", state_name = "my_state",
                         eval_by = "lower", eval_length = 1, thresh = 0)
  )
  expect_true("my_state" %in% G2$.get_flist()$name)
  expect_false("convergence" %in% G2$.get_flist()$name)

  expect_false(G2$converged())  # warm-up (length 0 < 1)
  G2$x <- 10
  expect_false(G2$converged())  # still above thresh
  G2$x <- -1
  expect_true(G2$converged())   # now below thresh
  expect_equal(G2$my_state$watching, "x")
})
