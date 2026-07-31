#-------------------------------------------------------------------------------
# Helpers
#-------------------------------------------------------------------------------

# A minimal counter-based Game, used across most tests below.
.make_counter_game <- function(start = 0) {
  count <- start
  increment <- function(step = 1) { self$count <- self$count + step }
  Game(State(count), Act(increment))
}


#-------------------------------------------------------------------------------
# Basic execution
#-------------------------------------------------------------------------------

test_that("run_Game() runs 'plan' once per step, 'times' times", {
  G  <- .make_counter_game(start = 0)
  G2 <- run_Game(G = G, plan = "increment", times = 3, verbose = FALSE, save_log = FALSE)

  expect_s3_class(G2, "ABM_Game")
  expect_equal(G2$count, 3)
  expect_equal(G2$time, G$time + 3)
})

test_that("run_Game() executes multiple plan elements in the given order, every step", {
  count <- 1
  double_it <- function() { self$count <- self$count * 2 }
  add_one   <- function() { self$count <- self$count + 1 }
  G <- Game(State(count), Act(double_it), Act(add_one))

  # double, then +1: (1 * 2) + 1 = 3
  G2 <- run_Game(G = G, plan = c("double_it", "add_one"), times = 1,
                 verbose = FALSE, save_log = FALSE)
  expect_equal(G2$count, 3)

  # +1, then double: (1 + 1) * 2 = 4
  G3 <- run_Game(G = G, plan = c("add_one", "double_it"), times = 1,
                 verbose = FALSE, save_log = FALSE)
  expect_equal(G3$count, 4)
})

test_that("run_Game() does not modify the original G (deep clone)", {
  G  <- .make_counter_game(start = 0)
  t0 <- G$time

  G2 <- run_Game(G = G, plan = "increment", times = 5, verbose = FALSE, save_log = FALSE)

  expect_equal(G$count, 0)
  expect_equal(G$time, t0)
  expect_equal(G2$count, 5)

  # the original G's act_FUN must still be independently callable/correct
  G$increment()
  expect_equal(G$count, 1)
})


#-------------------------------------------------------------------------------
# 'plan' with call-style argument overrides (regression test for the
# .replace_FUN_args() / G$.replace() fix: passing an ABM_Field, not a bare
# name=/x= pair)
#-------------------------------------------------------------------------------

test_that("run_Game() applies inline argument overrides from 'plan' for that run only", {
  G <- .make_counter_game(start = 0)

  G2 <- run_Game(G = G, plan = "increment(step = 5)", times = 2,
                 verbose = FALSE, save_log = FALSE)
  expect_equal(G2$count, 10)

  # the override must not leak back into the original G's default
  G3 <- run_Game(G = G, plan = "increment", times = 2, verbose = FALSE, save_log = FALSE)
  expect_equal(G3$count, 2)
})


#-------------------------------------------------------------------------------
# Input validation
#-------------------------------------------------------------------------------

test_that("run_Game() requires G to be an ABM_Game object", {
  expect_error(run_Game(G = list(), plan = "increment", times = 1))
})

test_that("run_Game() requires 'plan' to be a character vector", {
  G <- .make_counter_game()
  expect_error(run_Game(G = G, plan = 123, times = 1))
})

test_that("run_Game() requires every 'plan' element to be an act_FUN field", {
  G <- .make_counter_game()
  # 'count' is a 'state' field, not an 'act_FUN'
  expect_error(
    run_Game(G = G, plan = "count", times = 1, verbose = FALSE),
    "act_FUN"
  )
})

test_that("run_Game() requires 'nm_stop_FUN' to name a 'stop_FUN' field", {
  G <- .make_counter_game()
  # 'increment' is an 'act_FUN' field, not a 'stop_FUN'
  expect_error(
    run_Game(G = G, plan = "increment", nm_stop_FUN = "increment", verbose = FALSE),
    "stop_FUN"
  )
})

test_that("run_Game() validates 'times'", {
  G <- .make_counter_game()
  expect_error(run_Game(G = G, plan = "increment", times = 0, verbose = FALSE))
  expect_error(run_Game(G = G, plan = "increment", times = -1, verbose = FALSE))
  expect_error(run_Game(G = G, plan = "increment", times = 1.5, verbose = FALSE))
})

test_that("run_Game() validates 'save_interval'", {
  G <- .make_counter_game()
  expect_error(run_Game(G = G, plan = "increment", times = 3, save_interval = 0, verbose = FALSE))
  expect_error(run_Game(G = G, plan = "increment", times = 3, save_interval = 1.5, verbose = FALSE))
})

test_that("run_Game() validates 'RDS_file_name' when saveRDS_inbetween = TRUE", {
  G <- .make_counter_game()
  expect_error(
    run_Game(G = G, plan = "increment", times = 1, verbose = FALSE,
             saveRDS_inbetween = TRUE, RDS_file_name = 123)
  )
})


#-------------------------------------------------------------------------------
# nm_stop_FUN / times precedence
#-------------------------------------------------------------------------------

test_that("run_Game() stops on nm_stop_FUN's condition, and ignores 'times' entirely", {
  count <- 0
  increment <- function() { self$count <- self$count + 1 }
  reached_3 <- function() { self$count >= 3 }
  G <- Game(State(count), Act(increment), Stop(reached_3))

  # 'times' is nonsensical here (negative) but must simply be ignored,
  # since 'nm_stop_FUN' takes precedence.
  G2 <- run_Game(G = G, plan = "increment", nm_stop_FUN = "reached_3",
                 times = -999, verbose = FALSE, save_log = FALSE)

  expect_equal(G2$count, 3)
  expect_equal(G2$time, G$time + 3)
})


#-------------------------------------------------------------------------------
# add_tryCatch
#-------------------------------------------------------------------------------

test_that("run_Game() catches act_FUN errors and continues when add_tryCatch = TRUE", {
  count <- 0
  risky <- function() {
    if (self$count == 1) stop("boom")
    self$count <- self$count + 1
  }
  G <- Game(State(count), Act(risky))

  # step 1: count 0 -> 1 (no error)
  # step 2: count == 1 -> errors, caught, count stays at 1
  # step 3: count == 1 -> errors again, caught, count stays at 1
  expect_message(
    G2 <- run_Game(G = G, plan = "risky", times = 3, verbose = FALSE, save_log = FALSE),
    "boom"
  )
  expect_equal(G2$count, 1)
  expect_equal(G2$time, G$time + 3)  # the run still completes all 'times' steps
})

test_that("run_Game() propagates act_FUN errors when add_tryCatch = FALSE", {
  count <- 0
  risky <- function() {
    if (self$count == 1) stop("boom")
    self$count <- self$count + 1
  }
  G <- Game(State(count), Act(risky))

  expect_error(
    run_Game(G = G, plan = "risky", times = 3, verbose = FALSE, save_log = FALSE,
             add_tryCatch = FALSE),
    "boom"
  )
})


#-------------------------------------------------------------------------------
# Logging: save_log / save_interval / fields_to_save
#-------------------------------------------------------------------------------

test_that("run_Game() logs one snapshot per step by default (save_interval = 1)", {
  G  <- .make_counter_game()
  G2 <- run_Game(G = G, plan = "increment", times = 3, verbose = FALSE)

  # initial snapshot + one per step
  expect_length(G2$log, 4)
  expect_equal(unname(vapply(G2$log, function(s) s$time, numeric(1))),
               c(G$time, G$time + 1, G$time + 2, G$time + 3))
})

test_that("run_Game() does not populate log when save_log = FALSE", {
  G  <- .make_counter_game()
  G2 <- run_Game(G = G, plan = "increment", times = 3, verbose = FALSE, save_log = FALSE)
  expect_null(G2$log)
})

test_that("run_Game() saves only every 'save_interval' steps", {
  G  <- .make_counter_game()
  G2 <- run_Game(G = G, plan = "increment", times = 4, save_interval = 2, verbose = FALSE)

  # initial (t=1) + t=3 + t=5
  expect_length(G2$log, 3)
  expect_equal(unname(vapply(G2$log, function(s) s$time, numeric(1))),
               c(G$time, G$time + 2, G$time + 4))
})

test_that("run_Game() restricts saved fields via fields_to_save", {
  count <- 0
  label <- "unchanged"
  increment <- function() { self$count <- self$count + 1 }
  G <- Game(State(count), State(label), Act(increment))

  G2 <- run_Game(G = G, plan = "increment", times = 1,
                 fields_to_save = "count", verbose = FALSE)
  expect_equal(names(G2$log[[1]]), c("count", "time"))

  G3 <- run_Game(G = G, plan = "increment", times = 1, verbose = FALSE)
  expect_true(all(c("count", "label") %in% names(G3$log[[1]])))
})

test_that("run_Game() errors with the actual missing field name in fields_to_save", {
  G <- .make_counter_game()
  # regression test: the error must name the missing field itself, not "NA"
  expect_error(
    run_Game(G = G, plan = "increment", times = 1, verbose = FALSE,
             fields_to_save = "not_a_real_field"),
    "not_a_real_field"
  )
})


#-------------------------------------------------------------------------------
# return_update_FUN
#-------------------------------------------------------------------------------

test_that("run_Game() records update_FUN's body in notes only when requested", {
  G <- .make_counter_game()

  G2 <- run_Game(G = G, plan = "increment", times = 1, verbose = FALSE,
                 save_log = FALSE, return_update_FUN = TRUE)
  expect_type(G2$notes$update_FUN_used, "character")
  expect_true(length(G2$notes$update_FUN_used) > 0)

  G3 <- run_Game(G = G, plan = "increment", times = 1, verbose = FALSE,
                 save_log = FALSE, return_update_FUN = FALSE)
  expect_null(G3$notes$update_FUN_used)
})


#-------------------------------------------------------------------------------
# seed
#-------------------------------------------------------------------------------

test_that("run_Game() reproduces results when seed is supplied explicitly", {
  count <- 0
  add_random <- function() { self$count <- self$count + sample(1:100, 1) }
  G <- Game(State(count), Act(add_random))

  G_a <- run_Game(G = G, plan = "add_random", times = 5, seed = 42,
                  verbose = FALSE, save_log = FALSE)
  G_b <- run_Game(G = G, plan = "add_random", times = 5, seed = 42,
                  verbose = FALSE, save_log = FALSE)

  expect_equal(G_a$count, G_b$count)
  expect_equal(G_a$notes$seed, 42)
  expect_equal(G_b$notes$seed, 42)
})

test_that("run_Game() auto-generates and records a valid seed when seed = NULL", {
  G  <- .make_counter_game()
  G2 <- run_Game(G = G, plan = "increment", times = 1, verbose = FALSE, save_log = FALSE)

  expect_true(is.numeric(G2$notes$seed))
  expect_true(G2$notes$seed >= 1 && G2$notes$seed <= .Machine$integer.max)
})


#-------------------------------------------------------------------------------
# notes$simulation_took
#-------------------------------------------------------------------------------

test_that("run_Game() records the elapsed time in 'hh:mm:ss.mmm' format", {
  G  <- .make_counter_game()
  G2 <- run_Game(G = G, plan = "increment", times = 1, verbose = FALSE, save_log = FALSE)

  expect_match(G2$notes$simulation_took, "^\\d{2}:\\d{2}:\\d{2}\\.\\d{3}$")
})


#-------------------------------------------------------------------------------
# verbose
#-------------------------------------------------------------------------------

test_that("run_Game() prints progress messages only when verbose = TRUE", {
  G <- .make_counter_game()

  expect_silent(
    run_Game(G = G, plan = "increment", times = 1, verbose = FALSE, save_log = FALSE)
  )
  expect_output(
    run_Game(G = G, plan = "increment", times = 1, verbose = TRUE, save_log = FALSE),
    "\\[plan\\]"
  )
})


#-------------------------------------------------------------------------------
# saveRDS_inbetween
#-------------------------------------------------------------------------------

test_that("run_Game() writes an RDS snapshot at each step when saveRDS_inbetween = TRUE", {
  G <- .make_counter_game()
  rds_path <- tempfile(fileext = ".rds")
  on.exit(unlink(rds_path), add = TRUE)

  run_Game(G = G, plan = "increment", times = 2, verbose = FALSE, save_log = TRUE,
           saveRDS_inbetween = TRUE, RDS_file_name = rds_path)

  expect_true(file.exists(rds_path))
  saved <- readRDS(rds_path)
  expect_s3_class(saved, "ABM_Game")
})


#-------------------------------------------------------------------------------
# beep
#-------------------------------------------------------------------------------

test_that("run_Game() plays a notification sound only when beep = TRUE", {
  skip_if_not_installed("beepr")
  testthat::skip_if_not(
    exists("local_mocked_bindings", where = asNamespace("testthat")),
    "testthat version does not support local_mocked_bindings()"
  )

  called <- FALSE
  testthat::local_mocked_bindings(beep = function(...) { called <<- TRUE }, .package = "rABM")

  G <- .make_counter_game()
  run_Game(G = G, plan = "increment", times = 1, verbose = FALSE, save_log = FALSE, beep = TRUE)

  expect_true(called)
})
