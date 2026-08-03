# test-plot.ABM_Game.R

#-------------------------------------------------------------------------------
# Test fixture helper
#-------------------------------------------------------------------------------
# Builds a minimal ABM_Game with one State field ("val"), one Act field
# ("bump"), and one Plot field ("do_plot"). "do_plot" does not call any real
# graphics function -- it just records that it was called and what self$val
# was at the time, via <<- into the environment returned alongside G. This
# lets us assert on call counts, call order, and which self was seen
# (live x vs. a log-entry snapshot) without needing a graphics device.
make_test_game <- function() {
  val <- 1
  plot_calls <- 0
  plot_seen  <- list()

  bump <- function() { self$val <- self$val + 1 }
  do_plot <- function() {
    plot_calls <<- plot_calls + 1
    plot_seen[[length(plot_seen) + 1]] <<- self$val
  }

  G <- Game(
    State(val),
    Act(bump, name = "bump"),
    Plot(do_plot, name = "do_plot")
  )

  list(G = G, env = environment())
}

# Builds a game with two Plot fields ("do_plot_a", "do_plot_b"), each
# recording its own call order into a shared vector, so we can test the
# "name = NULL -> run all plot_FUN" branch and its ordering.
make_test_game_multi_plot <- function() {
  val <- 1
  call_order <- character(0)

  bump <- function() { self$val <- self$val + 1 }
  do_plot_a <- function() { call_order <<- c(call_order, "a") }
  do_plot_b <- function() { call_order <<- c(call_order, "b") }

  G <- Game(
    State(val),
    Act(bump, name = "bump"),
    Plot(do_plot_a, name = "do_plot_a"),
    Plot(do_plot_b, name = "do_plot_b")
  )

  list(G = G, env = environment())
}

# Attaches a manually constructed log to G, with one entry per (name, time,
# val) triple given. Bypasses run_Game() entirely so test times/values are
# fully controlled. Entry naming ("t" + time) matches what run_Game()
# actually produces (see names(log) <- paste0("t", log_time) in run_Game.R).
attach_log <- function(G, entries) {
  # entries: list of list(name = "t1", time = 1, val = 10), ...
  log <- setNames(
    lapply(entries, function(e) list(val = e$val, time = e$time)),
    vapply(entries, function(e) e$name, character(1))
  )
  G$log <- log
  G
}

#-------------------------------------------------------------------------------
# .assert_plot_field()
#-------------------------------------------------------------------------------

test_that(".assert_plot_field() accepts a registered plot_FUN field", {
  fx <- make_test_game()
  expect_true(.assert_plot_field(fx$G, "do_plot"))
})

test_that(".assert_plot_field() rejects a field of a different category", {
  fx <- make_test_game()
  expect_error(.assert_plot_field(fx$G, "val"), "not a registered 'plot_FUN' field")
  expect_error(.assert_plot_field(fx$G, "bump"), "not a registered 'plot_FUN' field")
})

test_that(".assert_plot_field() rejects a name not present in x at all", {
  fx <- make_test_game()
  expect_error(.assert_plot_field(fx$G, "nonexistent"), "not a registered 'plot_FUN' field")
})

#-------------------------------------------------------------------------------
# Current state, name = NULL (run all plot_FUN)
#-------------------------------------------------------------------------------

test_that("name = NULL, log = NULL runs every plot_FUN field, in category order", {
  fx <- make_test_game_multi_plot()
  plot(fx$G, pause = FALSE, verbose = FALSE)
  expect_identical(fx$env$call_order, c("a", "b"))
})

test_that("name = NULL, log = NULL with no plot_FUN fields is a no-op", {
  val <- 1
  G <- Game(State(val))
  expect_null(plot(G, pause = FALSE, verbose = FALSE))
})

test_that("name = NULL: pause = TRUE pauses between plots but not after the last one", {
  fx <- make_test_game_multi_plot()
  calls <- 0
  testthat::local_mocked_bindings(
    .pause_for_enter = function(...) { calls <<- calls + 1; invisible(NULL) }
  )
  plot(fx$G, pause = TRUE, verbose = FALSE)
  expect_identical(fx$env$call_order, c("a", "b"))
  expect_equal(calls, 1L)  # 2 plots -> 1 pause
})

test_that("name = NULL: pause = FALSE never calls .pause_for_enter()", {
  fx <- make_test_game_multi_plot()
  calls <- 0
  testthat::local_mocked_bindings(
    .pause_for_enter = function(...) { calls <<- calls + 1; invisible(NULL) }
  )
  plot(fx$G, pause = FALSE, verbose = FALSE)
  expect_equal(calls, 0L)
})

test_that("name = NULL: verbose = TRUE prints one message per plot_FUN", {
  fx <- make_test_game_multi_plot()
  out <- capture.output(plot(fx$G, pause = FALSE, verbose = TRUE))
  expect_identical(out, c('plotting: "do_plot_a"', 'plotting: "do_plot_b"'))
})

test_that("name = NULL: verbose = FALSE prints nothing", {
  fx <- make_test_game_multi_plot()
  out <- capture.output(plot(fx$G, pause = FALSE, verbose = FALSE))
  expect_identical(out, character(0))
})

#-------------------------------------------------------------------------------
# Current state, name supplied
#-------------------------------------------------------------------------------

test_that("name supplied, log = NULL runs that single plot_FUN exactly once", {
  fx <- make_test_game()
  plot(fx$G, name = "do_plot", pause = FALSE, verbose = FALSE)
  expect_equal(fx$env$plot_calls, 1L)
  expect_equal(fx$env$plot_seen[[1]], 1)  # current val, not from any log
})

test_that("name supplied, log = NULL: verbose message uses the current-state format", {
  fx <- make_test_game()
  out <- capture.output(plot(fx$G, name = "do_plot", pause = FALSE, verbose = TRUE))
  expect_identical(out, 'plotting: "do_plot"')
})

test_that("name supplied but not a plot_FUN field errors before plotting", {
  fx <- make_test_game()
  expect_error(
    plot(fx$G, name = "val", pause = FALSE, verbose = FALSE),
    "not a registered 'plot_FUN' field"
  )
  expect_equal(fx$env$plot_calls, 0L)
})

test_that("name supplied that does not exist at all errors before plotting", {
  fx <- make_test_game()
  expect_error(
    plot(fx$G, name = "nonexistent", pause = FALSE, verbose = FALSE),
    "not a registered 'plot_FUN' field"
  )
})

#-------------------------------------------------------------------------------
# Log replay
#-------------------------------------------------------------------------------

test_that("log supplied without name errors with the documented message", {
  fx <- make_test_game()
  fx$G <- attach_log(fx$G, list(list(name = "t1", time = 1, val = 10)))
  expect_error(
    plot(fx$G, log = "all", pause = FALSE, verbose = FALSE),
    "'name' is required when 'log' is provided"
  )
})

test_that("log supplied with a non-plot_FUN name errors before log resolution", {
  fx <- make_test_game()
  fx$G <- attach_log(fx$G, list(list(name = "t1", time = 1, val = 10)))
  expect_error(
    plot(fx$G, name = "val", log = "t99", pause = FALSE, verbose = FALSE),
    "not a registered 'plot_FUN' field"
  )
  # confirms the plot_FUN-category check runs before log = "t99" is resolved
  # (which would otherwise fail with a log-related error instead)
})

test_that("log replay binds 'self' to the log entry, not the live x", {
  fx <- make_test_game()
  fx$G <- attach_log(fx$G, list(
    list(name = "t1", time = 1, val = 10),
    list(name = "t2", time = 2, val = 20)
  ))
  # live x$val is unrelated to the logged snapshots
  fx$G$val <- 999

  plot(fx$G, name = "do_plot", log = "all", pause = FALSE, verbose = FALSE)

  expect_equal(fx$env$plot_calls, 2L)
  expect_equal(unlist(fx$env$plot_seen), c(10, 20))
})

test_that("log replay preserves the requested order, not x$log's storage order", {
  fx <- make_test_game()
  fx$G <- attach_log(fx$G, list(
    list(name = "t1", time = 1, val = 10),
    list(name = "t2", time = 2, val = 20)
  ))

  plot(fx$G, name = "do_plot", log = c("t2", "t1"), pause = FALSE, verbose = FALSE)

  expect_equal(unlist(fx$env$plot_seen), c(20, 10))
})

test_that("log replay: verbose message includes field name and time", {
  fx <- make_test_game()
  fx$G <- attach_log(fx$G, list(
    list(name = "t1", time = 1, val = 10),
    list(name = "t2", time = 2, val = 20)
  ))

  out <- capture.output(
    plot(fx$G, name = "do_plot", log = "all", pause = FALSE, verbose = TRUE)
  )
  expect_identical(out, c(
    'plotting: "do_plot" on t = 1',
    'plotting: "do_plot" on t = 2'
  ))
})

test_that("log replay: pause = TRUE pauses between entries but not after the last", {
  fx <- make_test_game()
  fx$G <- attach_log(fx$G, list(
    list(name = "t1", time = 1, val = 10),
    list(name = "t2", time = 2, val = 20),
    list(name = "t3", time = 3, val = 30)
  ))

  calls <- 0
  testthat::local_mocked_bindings(
    .pause_for_enter = function(...) { calls <<- calls + 1; invisible(NULL) }
  )
  plot(fx$G, name = "do_plot", log = "all", pause = TRUE, verbose = FALSE)

  expect_equal(fx$env$plot_calls, 3L)
  expect_equal(calls, 2L)  # 3 entries -> 2 pauses
})

test_that("log replay: a single selected entry never pauses, even with pause = TRUE", {
  fx <- make_test_game()
  fx$G <- attach_log(fx$G, list(
    list(name = "t1", time = 1, val = 10),
    list(name = "t2", time = 2, val = 20)
  ))

  calls <- 0
  testthat::local_mocked_bindings(
    .pause_for_enter = function(...) { calls <<- calls + 1; invisible(NULL) }
  )
  plot(fx$G, name = "do_plot", log = "t1", pause = TRUE, verbose = FALSE)

  expect_equal(fx$env$plot_calls, 1L)
  expect_equal(calls, 0L)
})

test_that("log replay: requesting an unknown log entry errors (delegated to value_of_log())", {
  fx <- make_test_game()
  fx$G <- attach_log(fx$G, list(list(name = "t1", time = 1, val = 10)))
  expect_error(
    plot(fx$G, name = "do_plot", log = "t99", pause = FALSE, verbose = FALSE)
  )
})

#-------------------------------------------------------------------------------
# plot_Game() alias
#-------------------------------------------------------------------------------

test_that("plot_Game() behaves identically to plot() for the current state", {
  fx <- make_test_game_multi_plot()
  plot_Game(fx$G, pause = FALSE, verbose = FALSE)
  expect_identical(fx$env$call_order, c("a", "b"))
})

test_that("plot_Game() behaves identically to plot() for log replay", {
  fx <- make_test_game()
  fx$G <- attach_log(fx$G, list(
    list(name = "t1", time = 1, val = 10),
    list(name = "t2", time = 2, val = 20)
  ))
  plot_Game(fx$G, name = "do_plot", log = "all", pause = FALSE, verbose = FALSE)
  expect_equal(unlist(fx$env$plot_seen), c(10, 20))
})
