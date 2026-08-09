# tests/testthat/test-batch_Chunk.R
#
# Tests for batch_Chunk().
# Chunk()/ABM_Chunk and Game()/State()/Act()/run_Game() are defined
# elsewhere in the package (class_ABM_*.R, run_Game.R), so this file only
# needs to call them.
# Intended to be run via devtools::test() / testthat::test_file().

test_that("feed = NULL runs exactly once without variable substitution", {
  chunk_noFeed <- Chunk({
    z <- 42
  })
  res <- batch_Chunk(chunk_noFeed, keep = "z", verbose = FALSE)

  expect_equal(length(res$values_out), 1)
  expect_equal(res$batch_label, "B1_1")
  expect_equal(res$values_out[["B1_1"]]$z, 42)
})

test_that("feed as data.frame creates one scenario per row", {
  chunk_add1 <- Chunk({
    y <- x + 1
  })
  res <- batch_Chunk(chunk_add1, feed = data.frame(x = 1:3), keep = "y", verbose = FALSE)

  expect_equal(length(res$values_out), 3)
  expect_equal(res$batch_label, c("B1_1", "B2_1", "B3_1"))
  expect_equal(unname(vapply(res$values_out, function(v) v$y, numeric(1))), c(2, 3, 4))
})

test_that("feed as list works the same way as data.frame", {
  chunk_add1 <- Chunk({
    y <- x + 1
  })
  res <- batch_Chunk(chunk_add1,
                     feed = list(list(x = 1), list(x = 2), list(x = 3)),
                     feed_type = "list", keep = "y", verbose = FALSE)

  expect_equal(length(res$values_out), 3)
  expect_equal(unname(vapply(res$values_out, function(v) v$y, numeric(1))), c(2, 3, 4))
})

test_that("n_each_run repeats each scenario and batch_label follows scenario-major order", {
  chunk_echo <- Chunk({
    out <- x
  })
  res <- batch_Chunk(chunk_echo, feed = data.frame(x = c(10, 20)),
                     n_each_run = 2, keep = c("x", "out"), verbose = FALSE)

  expect_equal(length(res$values_out), 4)
  # batch_label should follow scenario(k)-slow, run(l)-fast ordering
  expect_equal(res$batch_label, c("B1_1", "B1_2", "B2_1", "B2_2"))

  x_by_run <- vapply(res$values_out, function(v) v$x, numeric(1))
  expect_equal(unname(x_by_run), c(10, 10, 20, 20))
})

test_that("keep drops unrequested names and warns about missing ones", {
  chunk_add1 <- Chunk({
    y <- x + 1
  })
  expect_warning(
    res <- batch_Chunk(chunk_add1, feed = data.frame(x = 1:2),
                       keep = c("y", "does_not_exist"), verbose = FALSE),
    "not found"
  )
  expect_true(all(vapply(res$values_out, function(v) identical(names(v), "y"), logical(1))))
})

test_that("keep = NULL returns every object created in the chunk", {
  chunk_add1 <- Chunk({
    y <- x + 1
  })
  res <- batch_Chunk(chunk_add1, feed = data.frame(x = 1), verbose = FALSE)
  nm <- names(res$values_out[[1]])
  expect_true(all(c("x", "y") %in% nm))
})

test_that("feed variables can be accessed explicitly via V$<name>", {
  chunk_v_style <- Chunk({
    y <- V$x + 1
  })
  res <- batch_Chunk(chunk_v_style, feed = data.frame(x = 1:3), keep = "y", verbose = FALSE)

  expect_equal(unname(vapply(res$values_out, function(v) v$y, numeric(1))), c(2, 3, 4))
})

test_that("bare names and V$<name> access refer to the same values", {
  chunk_both_styles <- Chunk({
    y_bare <- x + 1
    y_v    <- V$x + 1
  })
  res <- batch_Chunk(chunk_both_styles, feed = data.frame(x = 1:3),
                     keep = c("y_bare", "y_v"), verbose = FALSE)

  for (v in res$values_out) {
    expect_equal(v$y_bare, v$y_v)
  }
})

test_that("the self-referential V binding does not leak into the returned values", {
  chunk_add1 <- Chunk({
    y <- x + 1
  })
  # keep = NULL returns every object created/visible during the run, so
  # this would surface an accidental 'V' entry if the internal cleanup
  # (rm("V", envir = V)) were missing.
  res <- batch_Chunk(chunk_add1, feed = data.frame(x = 1), verbose = FALSE)

  expect_false("V" %in% names(res$values_out[[1]]))
})

test_that("explicit seed makes results reproducible", {
  chunk_rand <- Chunk({
    r <- sample.int(1e6, 1)
  })
  res1 <- batch_Chunk(chunk_rand, feed = data.frame(x = 1:3),
                      seed = c(101, 102, 103), keep = "r", verbose = FALSE)
  res2 <- batch_Chunk(chunk_rand, feed = data.frame(x = 1:3),
                      seed = c(101, 102, 103), keep = "r", verbose = FALSE)

  expect_identical(res1$values_out, res2$values_out)
})

test_that("chunk must be an ABM_Chunk object", {
  expect_error(batch_Chunk("not_a_chunk", feed = data.frame(x = 1)))
})

test_that("n_each_run validation", {
  chunk_add1 <- Chunk({ y <- x + 1 })
  expect_error(batch_Chunk(chunk_add1, feed = data.frame(x = 1), n_each_run = 0))
  expect_error(batch_Chunk(chunk_add1, feed = data.frame(x = 1), n_each_run = 1.5))
  expect_error(batch_Chunk(chunk_add1, feed = data.frame(x = 1), n_each_run = c(1, 2)))
})

test_that("seed validation", {
  chunk_add1 <- Chunk({ y <- x + 1 })
  # length mismatch (n_total_run = 2)
  expect_error(batch_Chunk(chunk_add1, feed = data.frame(x = 1:2), seed = c(1, 2, 3)))
  # not positive
  expect_error(batch_Chunk(chunk_add1, feed = data.frame(x = 1:2), seed = c(-1, 2)))
  # not integer
  expect_error(batch_Chunk(chunk_add1, feed = data.frame(x = 1:2), seed = c(1.5, 2)))
})

test_that("feed/feed_type mismatch is rejected", {
  chunk_add1 <- Chunk({ y <- x + 1 })
  # feed_type = "data.frame" but feed is not a data.frame -> stopifnot fails immediately
  expect_error(
    batch_Chunk(chunk_add1, feed = list(list(x = 1)), feed_type = "data.frame")
  )
  # Passing a data.frame while feed_type = "list":
  # is.list(data.frame(...)) is TRUE, so the input check itself passes, but
  # each scenario then gets interpreted per "column vector", which breaks
  # the shape expected by list2env(), so it errors out at runtime anyway
  # (a slightly weak spot in the input validation).
  expect_error(
    batch_Chunk(chunk_add1, feed = data.frame(x = 1), feed_type = "list")
  )
})

test_that("on_error = 'continue' logs the error and keeps going", {
  chunk_err <- Chunk({
    if (x == 2) stop("boom")
    y <- x + 1
  })

  expect_warning(
    res <- batch_Chunk(chunk_err, feed = data.frame(x = 1:3),
                       keep = "y", on_error = "continue", verbose = FALSE),
    "not found"
  )

  expect_equal(length(res$values_out), 3)
  expect_null(res$error_log[["B1_1"]])
  expect_s3_class(res$error_log[["B2_1"]], "error")
  expect_null(res$error_log[["B3_1"]])
  # The x == 2 run stops before y is assigned, so the kept result is empty
  expect_length(res$values_out[["B2_1"]], 0)
  expect_equal(res$values_out[["B1_1"]]$y, 2)
  expect_equal(res$values_out[["B3_1"]]$y, 4)
})

test_that("on_error = 'stop' aborts immediately (sequential)", {
  chunk_err <- Chunk({
    if (x == 2) stop("boom")
    y <- x + 1
  })
  expect_error(
    batch_Chunk(chunk_err, feed = data.frame(x = 1:3), on_error = "stop", verbose = FALSE),
    "boom"
  )
})

test_that("output_only returns just values_out", {
  chunk_add1 <- Chunk({ y <- x + 1 })
  res <- batch_Chunk(chunk_add1, feed = data.frame(x = 1:2), keep = "y",
                     output_only = TRUE, verbose = FALSE)
  expect_type(res, "list")
  expect_equal(names(res), c("B1_1", "B2_1"))
  # Not nested -- the contents of values_out are returned directly
  expect_false("values_out" %in% names(res))
})

test_that("return_with_feed attaches the feed used for each run (data.frame)", {
  chunk_add1 <- Chunk({ y <- x + 1 })
  res <- batch_Chunk(chunk_add1, feed = data.frame(x = 1:2), n_each_run = 2,
                     keep = "y", return_with_feed = TRUE, verbose = FALSE)

  expect_true("feed" %in% names(res))
  expect_equal(res$feed$batch, res$batch_label)
  expect_equal(res$feed$x, c(1, 1, 2, 2))
})

test_that("return_with_feed attaches the feed used for each run (list)", {
  chunk_add1 <- Chunk({ y <- x + 1 })
  res <- batch_Chunk(chunk_add1,
                     feed = list(list(x = 1), list(x = 2)),
                     feed_type = "list", n_each_run = 2,
                     keep = "y", return_with_feed = TRUE, verbose = FALSE)

  expect_equal(names(res$feed), res$batch_label)
  expect_equal(
    vapply(res$feed, function(f) f$x, numeric(1)),
    c(1, 1, 2, 2),
    ignore_attr = TRUE
  )
})

test_that("return_with_feed defaults to TRUE", {
  chunk_add1 <- Chunk({ y <- x + 1 })
  # 'return_with_feed' intentionally left unset, to check the default.
  res <- batch_Chunk(chunk_add1, feed = data.frame(x = 1:2), n_each_run = 2,
                     keep = "y", verbose = FALSE)

  expect_true("feed" %in% names(res))
  expect_equal(res$feed$batch, res$batch_label)
  expect_equal(res$feed$x, c(1, 1, 2, 2))
})

test_that("parallel = TRUE (multisession) gives the same results as sequential execution", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  chunk_add1 <- Chunk({ y <- x + 1 })

  res_seq <- batch_Chunk(chunk_add1, feed = data.frame(x = 1:4), n_each_run = 2,
                         seed = 1:8, keep = "y", verbose = FALSE)
  res_par <- batch_Chunk(chunk_add1, feed = data.frame(x = 1:4), n_each_run = 2,
                         seed = 1:8, keep = "y", verbose = FALSE,
                         parallel = TRUE, n_cores = 2, plan_type = "multisession")

  expect_equal(res_par$batch_label, res_seq$batch_label)
  expect_equal(res_par$values_out, res_seq$values_out)
})

test_that("parallel = TRUE (multicore) gives the same results as sequential execution", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  skip_if_not(future::supportsMulticore(), "multicore is not supported on this platform")

  chunk_add1 <- Chunk({ y <- x + 1 })

  res_seq <- batch_Chunk(chunk_add1, feed = data.frame(x = 1:4), n_each_run = 2,
                         seed = 1:8, keep = "y", verbose = FALSE)
  res_par <- batch_Chunk(chunk_add1, feed = data.frame(x = 1:4), n_each_run = 2,
                         seed = 1:8, keep = "y", verbose = FALSE,
                         parallel = TRUE, n_cores = 2, plan_type = "multicore")

  expect_equal(res_par$batch_label, res_seq$batch_label)
  expect_equal(res_par$values_out, res_seq$values_out)
})

test_that("parallel = TRUE propagates errors when on_error = 'stop'", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  chunk_err <- Chunk({
    if (x == 2) stop("boom")
    y <- x + 1
  })

  expect_error(
    batch_Chunk(chunk_err, feed = data.frame(x = 1:3), on_error = "stop",
               parallel = TRUE, n_cores = 2, verbose = FALSE),
    "boom"
  )
})

test_that("parallel = TRUE with inherit_env picks up objects from the caller's environment", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  helper_add <- function(a, b) a + b   # an object living in the caller's environment
  chunk_use_helper <- Chunk({
    y <- helper_add(x, 1)
  })

  res <- batch_Chunk(chunk_use_helper, feed = data.frame(x = 1:2), keep = "y",
                     verbose = FALSE, parallel = TRUE, n_cores = 2)

  expect_equal(unname(vapply(res$values_out, function(v) v$y, numeric(1))), c(2, 3))
})

test_that("integration: batch_Chunk works with Game()/run_Game() inside a chunk", {
  chunk_game <- Chunk({
    b <- b

    wealth <- rep(1, 5)
    build_wealth <- function(){
      id <- sample(1:5, 1)
      self$wealth[id] <- self$wealth[id] + self$b
    }

    G <- Game(State(wealth), State(b), Act(build_wealth))
    G2 <- run_Game(G, "build_wealth", times = 5, verbose = FALSE)
    wealth_out <- G2$wealth
  })

  res <- batch_Chunk(chunk_game, feed = data.frame(b = 1:3), n_each_run = 2,
                     keep = "wealth_out", seed = 1:6, verbose = FALSE)

  expect_equal(length(res$values_out), 6)
  expect_true(all(vapply(res$values_out, function(v) length(v$wealth_out) == 5, logical(1))))

  # Each call to build_wealth() always increases total wealth by exactly b,
  # so after 5 runs the total gain should always be b * 5 (an invariant that
  # doesn't depend on which agent gets picked).
  total_gain <- vapply(res$values_out, function(v) sum(v$wealth_out) - 5, numeric(1))
  expected_gain <- rep(c(1, 2, 3) * 5, each = 2)
  expect_equal(unname(total_gain), expected_gain)
})

test_that("integration: a feed variable can override a run_Game() plan argument via paste0()", {
  # 'G' is defined in the caller's environment (not inside the chunk), and
  # 'b' is the feed variable being swept, spliced into the 'plan' string to
  # override add_money()'s default argument for that run only.
  add_money <- function(b = 1) {
    self$money[self$selected_agent] <- self$money[self$selected_agent] + b
  }
  select_agent <- function() {
    self$selected_agent <- sample(1:5, size = 2)
  }
  money          <- 1:5
  selected_agent <- 1:5
  G <- Game(State(money), State(selected_agent), Act(add_money), Act(select_agent))

  sim_chunk <- Chunk({
    G_i <- run_Game(G, plan = c("select_agent", paste0("add_money(b = ", b, ")")),
                    times = 10, verbose = FALSE)
    total_money <- sum(G_i$money)
  })

  res <- batch_Chunk(
    sim_chunk,
    feed = data.frame(b = c(1, 2, 5)),
    n_each_run = 2,
    keep = "total_money",
    return_with_feed = TRUE,
    seed = 1:6,
    verbose = FALSE
  )

  expect_equal(length(res$values_out), 6)
  expect_equal(res$feed$b, rep(c(1, 2, 5), each = 2))

  # add_money() always adds exactly b per step to the 2 selected agents, and
  # each step runs select_agent then add_money once, so total money always
  # grows by exactly 2 * b * times, regardless of which agents get picked.
  total_gain <- vapply(res$values_out, function(v) v$total_money - sum(1:5), numeric(1))
  expected_gain <- rep(c(1, 2, 5) * 2 * 10, each = 2)
  expect_equal(unname(total_gain), expected_gain)
})

test_that("implementation_took is present and correctly formatted", {
  chunk_add1 <- Chunk({ y <- x + 1 })
  res <- batch_Chunk(chunk_add1, feed = data.frame(x = 1:2), keep = "y", verbose = FALSE)

  expect_true("implementation_took" %in% names(res))
  expect_match(res$implementation_took, "^\\d{2}:\\d{2}:\\d{2}\\.\\d{3}$")
})

test_that("implementation_took reflects actual elapsed time", {
  chunk_slow <- Chunk({
    Sys.sleep(0.1)
    y <- x + 1
  })
  res <- batch_Chunk(chunk_slow, feed = data.frame(x = 1:2), keep = "y", verbose = FALSE)

  # "hh:mm:ss.mmm" -> c(hours, minutes, seconds, milliseconds)
  parts <- as.numeric(strsplit(gsub("\\.", ":", res$implementation_took), ":")[[1]])
  elapsed_secs <- parts[1] * 3600 + parts[2] * 60 + parts[3] + parts[4] / 1000

  # Two runs of >= 0.1s each -> total elapsed time should be at least ~0.2s;
  # 0.15s is used as a safe margin to avoid flakiness.
  expect_gte(elapsed_secs, 0.15)
})

test_that("implementation_took is included regardless of return_with_feed", {
  chunk_add1 <- Chunk({ y <- x + 1 })

  res_default <- batch_Chunk(chunk_add1, feed = data.frame(x = 1:2), keep = "y",
                             verbose = FALSE)
  res_with_feed <- batch_Chunk(chunk_add1, feed = data.frame(x = 1:2), keep = "y",
                               return_with_feed = TRUE, verbose = FALSE)

  expect_true("implementation_took" %in% names(res_default))
  expect_true("implementation_took" %in% names(res_with_feed))
})

test_that("implementation_took is not included when output_only = TRUE", {
  chunk_add1 <- Chunk({ y <- x + 1 })
  res <- batch_Chunk(chunk_add1, feed = data.frame(x = 1:2), keep = "y",
                     output_only = TRUE, verbose = FALSE)

  expect_false("implementation_took" %in% names(res))
})

test_that("implementation_took is present for parallel execution", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  chunk_add1 <- Chunk({ y <- x + 1 })
  res <- batch_Chunk(chunk_add1, feed = data.frame(x = 1:2), keep = "y",
                     verbose = FALSE, parallel = TRUE, n_cores = 2)

  expect_true("implementation_took" %in% names(res))
  expect_match(res$implementation_took, "^\\d{2}:\\d{2}:\\d{2}\\.\\d{3}$")
})
