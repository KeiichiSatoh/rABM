# test-animate_log.R

testthat::skip_if_not_installed("gifski")

#-------------------------------------------------------------------------------
# Fixtures (kept self-contained in this file)
#-------------------------------------------------------------------------------

make_animate_test_game <- function() {
  val <- 1
  plot_calls <- 0
  plot_seen  <- list()
  plot_args_seen <- list()

  bump <- function() { self$val <- self$val + 1 }
  do_plot <- function(scale = 1) {
    plot_calls <<- plot_calls + 1
    plot_seen[[length(plot_seen) + 1]] <<- self$val
    plot_args_seen[[length(plot_args_seen) + 1]] <<- scale
  }

  G <- Game(
    State(val),
    Act(bump, name = "bump"),
    Plot(do_plot, name = "do_plot")
  )

  list(G = G, env = environment())
}

attach_log <- function(G, entries) {
  log <- setNames(
    lapply(entries, function(e) list(val = e$val, time = e$time)),
    vapply(entries, function(e) e$name, character(1))
  )
  G$log <- log
  G
}

# Returns a save_gif() replacement that force-evaluates 'expr' (running the
# real per-frame loop) instead of encoding an actual GIF, optionally
# recording the other arguments it was called with. This is a plain factory
# -- it does NOT call local_mocked_bindings() itself -- because
# local_mocked_bindings() must be called directly from within each
# test_that() block to pick up the correct scope for teardown.
make_mock_save_gif <- function(recorded_args = NULL) {
  function(expr, gif_file, ...) {
    if (!is.null(recorded_args)) {
      assign("last_args", list(gif_file = gif_file, ...), envir = recorded_args)
    }
    force(expr)
    gif_file
  }
}

#-------------------------------------------------------------------------------
# Dependency guard: .require_gifski()
#-------------------------------------------------------------------------------

test_that(".require_gifski() passes silently when gifski is installed", {
  expect_true(.require_gifski())
})

test_that("animate_log() stops immediately if gifski is unavailable, before any other validation", {
  testthat::local_mocked_bindings(
    .require_gifski = function() stop(
      "Package 'gifski' is required for animate_log(). ",
      "Install it with install.packages('gifski').",
      call. = FALSE
    )
  )
  # G/name are deliberately invalid here, to confirm the gifski check runs
  # *before* any other validation.
  expect_error(
    animate_log(G = list(), name = character(0)),
    "Package 'gifski' is required"
  )
})

#-------------------------------------------------------------------------------
# Input validation (gifski present from here on)
#-------------------------------------------------------------------------------

test_that("animate_log() requires G to be an ABM_Game object", {
  expect_error(animate_log(G = list(), name = "do_plot"))
})

test_that("animate_log() rejects a name that is not a registered plot_FUN field", {
  fx <- make_animate_test_game()
  fx$G <- attach_log(fx$G, list(list(name = "t1", time = 1, val = 10)))
  expect_error(
    animate_log(fx$G, name = "val", file = tempfile(fileext = ".gif"), play = FALSE),
    "not a registered 'plot_FUN' field"
  )
})

test_that("animate_log() requires 'plot_args' to be a list", {
  fx <- make_animate_test_game()
  fx$G <- attach_log(fx$G, list(list(name = "t1", time = 1, val = 10)))
  expect_error(
    animate_log(fx$G, name = "do_plot", plot_args = "not a list",
                file = tempfile(fileext = ".gif"), play = FALSE),
    "'plot_args' must be a list"
  )
})

test_that("animate_log() requires 'interval' to be a single positive integer", {
  fx <- make_animate_test_game()
  fx$G <- attach_log(fx$G, list(list(name = "t1", time = 1, val = 10)))
  expect_error(
    animate_log(fx$G, name = "do_plot", interval = 0,
                file = tempfile(fileext = ".gif"), play = FALSE),
    "'interval' must be a single positive integer"
  )
  expect_error(
    animate_log(fx$G, name = "do_plot", interval = 1.5,
                file = tempfile(fileext = ".gif"), play = FALSE),
    "'interval' must be a single positive integer"
  )
})

test_that("animate_log() requires 'file' to be a single character string", {
  fx <- make_animate_test_game()
  fx$G <- attach_log(fx$G, list(list(name = "t1", time = 1, val = 10)))
  expect_error(
    animate_log(fx$G, name = "do_plot", file = 123, play = FALSE),
    "'file' must be a character"
  )
})

test_that("animate_log() propagates value_of_log()'s error for an unknown log entry", {
  fx <- make_animate_test_game()
  fx$G <- attach_log(fx$G, list(list(name = "t1", time = 1, val = 10)))
  expect_error(
    animate_log(fx$G, name = "do_plot", log = "t99",
                file = tempfile(fileext = ".gif"), play = FALSE)
  )
})

#-------------------------------------------------------------------------------
# Frame selection and delegation to plot.ABM_Game()
#-------------------------------------------------------------------------------

test_that("log = NULL defaults to 'all' and renders one frame per log entry, in order", {
  fx <- make_animate_test_game()
  fx$G <- attach_log(fx$G, list(
    list(name = "t1", time = 1, val = 10),
    list(name = "t2", time = 2, val = 20),
    list(name = "t3", time = 3, val = 30)
  ))

  testthat::local_mocked_bindings(save_gif = make_mock_save_gif(), .package = "gifski")
  animate_log(fx$G, name = "do_plot", file = tempfile(fileext = ".gif"), play = FALSE)

  expect_equal(fx$env$plot_calls, 3L)
  expect_equal(unlist(fx$env$plot_seen), c(10, 20, 30))
})

test_that("log resolves specific entries in the requested order", {
  fx <- make_animate_test_game()
  fx$G <- attach_log(fx$G, list(
    list(name = "t1", time = 1, val = 10),
    list(name = "t2", time = 2, val = 20)
  ))

  testthat::local_mocked_bindings(save_gif = make_mock_save_gif(), .package = "gifski")
  animate_log(fx$G, name = "do_plot", log = c("t2", "t1"),
              file = tempfile(fileext = ".gif"), play = FALSE)

  expect_equal(unlist(fx$env$plot_seen), c(20, 10))
})

test_that("duplicate entries in 'log' are allowed and each renders its own frame", {
  fx <- make_animate_test_game()
  fx$G <- attach_log(fx$G, list(
    list(name = "t1", time = 1, val = 10),
    list(name = "t2", time = 2, val = 20)
  ))

  testthat::local_mocked_bindings(save_gif = make_mock_save_gif(), .package = "gifski")
  animate_log(fx$G, name = "do_plot", log = c("t1", "t1", "t2"),
              file = tempfile(fileext = ".gif"), play = FALSE)

  expect_equal(fx$env$plot_calls, 3L)
  expect_equal(unlist(fx$env$plot_seen), c(10, 10, 20))
})

test_that("'interval' keeps every interval-th selected frame, preserving order", {
  fx <- make_animate_test_game()
  fx$G <- attach_log(fx$G, list(
    list(name = "t1", time = 1, val = 10),
    list(name = "t2", time = 2, val = 20),
    list(name = "t3", time = 3, val = 30),
    list(name = "t4", time = 4, val = 40)
  ))

  testthat::local_mocked_bindings(save_gif = make_mock_save_gif(), .package = "gifski")
  animate_log(fx$G, name = "do_plot", log = "all", interval = 2,
              file = tempfile(fileext = ".gif"), play = FALSE)

  expect_equal(unlist(fx$env$plot_seen), c(10, 30))
})

test_that("check = TRUE stops when the thinned frame count exceeds 500", {
  fx <- make_animate_test_game()
  entries <- lapply(1:501, function(i) list(name = paste0("t", i), time = i, val = i))
  fx$G <- attach_log(fx$G, entries)

  testthat::local_mocked_bindings(save_gif = make_mock_save_gif(), .package = "gifski")
  expect_error(
    animate_log(fx$G, name = "do_plot", log = "all",
                file = tempfile(fileext = ".gif"), play = FALSE),
    "longer than 500"
  )
  expect_equal(fx$env$plot_calls, 0L)  # fails before any frame is rendered
})

test_that("check = FALSE allows more than 500 frames", {
  fx <- make_animate_test_game()
  entries <- lapply(1:501, function(i) list(name = paste0("t", i), time = i, val = i))
  fx$G <- attach_log(fx$G, entries)

  testthat::local_mocked_bindings(save_gif = make_mock_save_gif(), .package = "gifski")
  animate_log(fx$G, name = "do_plot", log = "all", check = FALSE,
              file = tempfile(fileext = ".gif"), play = FALSE)

  expect_equal(fx$env$plot_calls, 501L)
})

test_that("plot_args are forwarded through to the underlying plot_FUN", {
  fx <- make_animate_test_game()
  fx$G <- attach_log(fx$G, list(list(name = "t1", time = 1, val = 10)))

  testthat::local_mocked_bindings(save_gif = make_mock_save_gif(), .package = "gifski")
  animate_log(fx$G, name = "do_plot", plot_args = list(scale = 99),
              file = tempfile(fileext = ".gif"), play = FALSE)

  expect_equal(fx$env$plot_args_seen[[1]], 99)
})

test_that("frames use verbose = FALSE, pause = FALSE regardless of defaults", {
  fx <- make_animate_test_game()
  fx$G <- attach_log(fx$G, list(
    list(name = "t1", time = 1, val = 10),
    list(name = "t2", time = 2, val = 20)
  ))

  testthat::local_mocked_bindings(save_gif = make_mock_save_gif(), .package = "gifski")
  out <- capture.output(
    animate_log(fx$G, name = "do_plot", file = tempfile(fileext = ".gif"), play = FALSE)
  )
  expect_identical(out, character(0))
  expect_equal(fx$env$plot_calls, 2L)
})

#-------------------------------------------------------------------------------
# gifski::save_gif() call itself
#-------------------------------------------------------------------------------

test_that("delay/width/height/res and file are forwarded to gifski::save_gif()", {
  fx <- make_animate_test_game()
  fx$G <- attach_log(fx$G, list(list(name = "t1", time = 1, val = 10)))

  captured <- new.env()
  testthat::local_mocked_bindings(save_gif = make_mock_save_gif(captured), .package = "gifski")

  out_file <- tempfile(fileext = ".gif")
  animate_log(fx$G, name = "do_plot", file = out_file,
              delay = 0.5, width = 400, height = 300, res = 72, play = FALSE)

  expect_equal(captured$last_args$gif_file, out_file)
  expect_equal(captured$last_args$delay, 0.5)
  expect_equal(captured$last_args$width, 400)
  expect_equal(captured$last_args$height, 300)
  expect_equal(captured$last_args$res, 72)
})

test_that("'...' is forwarded to gifski::save_gif(), not to the plot_FUN", {
  fx <- make_animate_test_game()
  fx$G <- attach_log(fx$G, list(list(name = "t1", time = 1, val = 10)))

  captured <- new.env()
  testthat::local_mocked_bindings(save_gif = make_mock_save_gif(captured), .package = "gifski")

  animate_log(fx$G, name = "do_plot", loop = FALSE,
              file = tempfile(fileext = ".gif"), play = FALSE)

  expect_equal(captured$last_args$loop, FALSE)
  # confirm it did NOT leak into the plot_FUN's arguments (default 'scale' = 1 was used)
  expect_equal(fx$env$plot_args_seen[[1]], 1)
})

#-------------------------------------------------------------------------------
# add_time_label
#-------------------------------------------------------------------------------

test_that("add_time_label = TRUE runs without error when plot_FUN draws a real plot", {
  val <- 1
  bump <- function() { self$val <- self$val + 1 }
  do_plot_real <- function() { graphics::plot(1, main = "x") }

  G <- Game(State(val), Act(bump, name = "bump"), Plot(do_plot_real, name = "do_plot_real"))
  G <- attach_log(G, list(
    list(name = "t1", time = 1, val = 10),
    list(name = "t2", time = 2, val = 20)
  ))

  testthat::local_mocked_bindings(save_gif = make_mock_save_gif(), .package = "gifski")

  grDevices::pdf(nullfile())
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_no_error(
    animate_log(G, name = "do_plot_real", add_time_label = TRUE,
                file = tempfile(fileext = ".gif"), play = FALSE)
  )
})

#-------------------------------------------------------------------------------
# play
#-------------------------------------------------------------------------------

test_that("play = TRUE calls .browse_gif() with the file path; play = FALSE does not", {
  fx <- make_animate_test_game()
  fx$G <- attach_log(fx$G, list(list(name = "t1", time = 1, val = 10)))
  testthat::local_mocked_bindings(save_gif = make_mock_save_gif(), .package = "gifski")

  browsed <- character(0)
  testthat::local_mocked_bindings(
    .browse_gif = function(file) { browsed <<- c(browsed, file); invisible(NULL) }
  )

  out_file <- tempfile(fileext = ".gif")
  animate_log(fx$G, name = "do_plot", file = out_file, play = TRUE)
  expect_equal(browsed, out_file)

  browsed <- character(0)
  animate_log(fx$G, name = "do_plot", file = out_file, play = FALSE)
  expect_equal(browsed, character(0))
})
