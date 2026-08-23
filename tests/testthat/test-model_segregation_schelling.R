# tests/testthat/test-model_segregation_schelling.R
#
# Tests for model_segregation_schelling(). Assumes this file lives under
# tests/testthat/ in the rABM package (so Game(), State(), Act(), Plot(),
# Report(), add_field(), run_Game(), add_stop_convergence(), and
# model_segregation_schelling() are all in scope, exactly as in any other
# testthat file in this package).
#
# Runs that go through run_Game() are wrapped in capture.output() to
# suppress its (default) verbose console output; grid sizes are kept small
# and 'minimum_same_prop' lenient so full runs converge quickly.

test_that("input validation rejects out-of-range / malformed arguments", {
  expect_error(
    model_segregation_schelling(vacant_prop = 0, max_times = 0),
    "vacant_prop"
  )
  expect_error(
    model_segregation_schelling(vacant_prop = 1, max_times = 0),
    "vacant_prop"
  )
  expect_error(
    model_segregation_schelling(vacant_prop = -0.1, max_times = 0),
    "vacant_prop"
  )
  expect_error(
    model_segregation_schelling(n_row = 0, max_times = 0),
    "n_row"
  )
  expect_error(
    model_segregation_schelling(n_row = 5.5, max_times = 0),
    "n_row"
  )
  expect_error(
    model_segregation_schelling(n_col = c(5, 5), max_times = 0),
    "n_col"
  )
  expect_error(
    model_segregation_schelling(group_prop = 0.5, max_times = 0),
    "group_prop"
  )
  expect_error(
    model_segregation_schelling(group_prop = c(-0.2, 1.2), max_times = 0),
    "group_prop"
  )
  expect_error(
    model_segregation_schelling(group_prop = c(0.3, 0.3), max_times = 0),
    "sum to 1"
  )
  expect_error(
    model_segregation_schelling(
      n_row = 5, n_col = 5, vacant_prop = 0.2,
      minimum_same_prop = c(0.2, 0.4), max_times = 0
    ),
    "minimum_same_prop"
  )
  expect_error(
    model_segregation_schelling(minimum_same_prop = 1.5, max_times = 0),
    "minimum_same_prop"
  )
})

test_that("max_times = 0 returns an unrun model with the expected structure", {
  G <- model_segregation_schelling(
    vacant_prop = 0.2, group_prop = c(0.5, 0.5),
    minimum_same_prop = 0.3, n_row = 6, n_col = 6, max_times = 0
  )

  expect_s3_class(G, "ABM_Game")
  expect_identical(G$time, 1)

  fl <- G$.get_category()
  expect_setequal(
    names(fl)[fl == "state"],
    c("agent", "settings", "city", "same_group_prop",
      "unhappy_agent", "unhappy_agent_prop",
      "convergence") # bookkeeping state added by add_stop_convergence()
  )
  expect_setequal(
    names(fl)[fl == "act_FUN"],
    c("move", "update_city", "update_same_group_prop", "update_unhappy_agent")
  )
  expect_identical(names(fl)[fl == "plot_FUN"], "plot_city")
  expect_identical(names(fl)[fl == "report_FUN"], "report_stats")
  expect_identical(names(fl)[fl == "stop_FUN"], "converged")

  n_agent_expected <- floor(6 * 6 * (1 - 0.2))
  expect_equal(nrow(G$agent), n_agent_expected) # nrow() is integer, n_agent_expected is double
  expect_identical(G$settings$n_agent, n_agent_expected)
  expect_identical(G$settings$n_groups, 2L) # length(group_prop) == 2

  expect_setequal(
    names(G$agent),
    c("ID", "group", "place", "minimum_same_prop")
  )
  expect_null(G$settings$minimum_same_prop)

  expect_true(all(G$unhappy_agent %in% G$agent$ID))
  expect_identical(
    G$unhappy_agent_prop,
    length(G$unhappy_agent) / G$settings$n_agent
  )
  expect_true(G$unhappy_agent_prop >= 0 && G$unhappy_agent_prop <= 1)
  expect_true(all(G$same_group_prop >= 0 & G$same_group_prop <= 1))
})

test_that("minimum_same_prop is recycled correctly onto agent$minimum_same_prop", {
  G <- model_segregation_schelling(
    n_row = 6, n_col = 6, vacant_prop = 0.2,
    minimum_same_prop = 0.4, max_times = 0
  )
  expect_true(all(G$agent$minimum_same_prop == 0.4))
  expect_identical(length(G$agent$minimum_same_prop), nrow(G$agent))
})

test_that("a per-agent minimum_same_prop vector is accepted and stored as given", {
  n_agent_expected <- floor(6 * 6 * (1 - 0.2))
  custom_thresh <- rep(c(0.2, 0.6), length.out = n_agent_expected)

  G <- model_segregation_schelling(
    n_row = 6, n_col = 6, vacant_prop = 0.2,
    minimum_same_prop = custom_thresh, max_times = 0
  )
  expect_identical(G$agent$minimum_same_prop, custom_thresh)
})

test_that("agent$group only takes values within the configured number of groups", {
  G <- model_segregation_schelling(
    n_row = 8, n_col = 8, vacant_prop = 0.3,
    group_prop = c(0.2, 0.3, 0.5), max_times = 0
  )
  expect_true(all(G$agent$group %in% seq_along(c(0.2, 0.3, 0.5))))
  expect_identical(G$settings$n_groups, 3L)
})

test_that("running the model advances time and reaches its own convergence condition", {
  out <- capture.output(
    G <- model_segregation_schelling(
      vacant_prop = 0.3, group_prop = c(0.5, 0.5),
      minimum_same_prop = 0.2, n_row = 6, n_col = 6,
      max_times = 500, convergence_thresh = 0.05, convergence_eval_by = "sd"
    )
  )

  expect_s3_class(G, "ABM_Game")
  expect_gt(G$time, 1)
  expect_true(isTRUE(G$converged()))

  # unhappy_agent_prop in the final state must be internally consistent
  expect_identical(
    G$unhappy_agent,
    G$agent$ID[G$same_group_prop < G$agent$minimum_same_prop]
  )
  expect_identical(
    G$unhappy_agent_prop,
    length(G$unhappy_agent) / G$settings$n_agent
  )
})

test_that("report_stats() with log = NULL prints stats and returns them invisibly", {
  out <- capture.output(
    G <- model_segregation_schelling(
      vacant_prop = 0.3, minimum_same_prop = 0.2,
      n_row = 6, n_col = 6, max_times = 0
    )
  )

  expect_output(
    res <- withVisible(G$report_stats()),
    "Proportion of unhappy agents"
  )
  expect_false(res$visible)
  expect_setequal(names(res$value), c("time", "prop_unhappy", "avg_same_group_neib"))
  expect_identical(unname(res$value["time"]), G$time)
})

test_that("report_stats(log = 'all') returns a data.frame matching the log length", {
  out <- capture.output(
    G <- model_segregation_schelling(
      vacant_prop = 0.3, group_prop = c(0.5, 0.5),
      minimum_same_prop = 0.2, n_row = 6, n_col = 6,
      max_times = 500
    )
  )

  expect_message(
    res <- withVisible(G$report_stats(log = "all", show_plot = FALSE)),
    "calculated and returned"
  )
  expect_false(res$visible)
  df <- res$value
  expect_s3_class(df, "data.frame")
  expect_setequal(names(df), c("time", "prop_unhappy", "avg_same_group_neib"))
  expect_identical(nrow(df), length(G$log))
  expect_true(all(df$prop_unhappy >= 0 & df$prop_unhappy <= 1))
})

test_that("plot_city runs without error on the current state", {
  G <- model_segregation_schelling(
    vacant_prop = 0.3, minimum_same_prop = 0.2,
    n_row = 6, n_col = 6, max_times = 0
  )

  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit({
    grDevices::dev.off()
    unlink(tmp)
  }, add = TRUE)

  expect_no_error(plot(G, name = "plot_city"))
})

test_that("the same seed reproduces the same initial configuration", {
  set.seed(42)
  G1 <- model_segregation_schelling(
    vacant_prop = 0.2, group_prop = c(0.5, 0.5),
    minimum_same_prop = 0.3, n_row = 6, n_col = 6, max_times = 0
  )
  set.seed(42)
  G2 <- model_segregation_schelling(
    vacant_prop = 0.2, group_prop = c(0.5, 0.5),
    minimum_same_prop = 0.3, n_row = 6, n_col = 6, max_times = 0
  )

  expect_identical(G1$agent, G2$agent)
  expect_identical(G1$same_group_prop, G2$same_group_prop)
})
