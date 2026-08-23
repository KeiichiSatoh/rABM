# tests/testthat/test-model_opinion_jager.R
#
# Tests for model_opinion_jager(). These assume the file lives under
# tests/testthat/ in the rABM package and is run via devtools::test() (or
# testthat::test_dir()), so that Game(), State(), Act(), Report(), Plot(),
# run_Game(), value_of_log(), sample2(), add_field(), and copy_obj() are all
# already available from the loaded package namespace.
#
# NOTE: I was not able to execute R in this session (no R runtime is
# installed here, and there is no network access to install one), so these
# tests are written and reasoned through by hand rather than run against the
# actual package. Please run `devtools::test()` locally and let me know if
# any expected error-message pattern doesn't match wording elsewhere in the
# package (e.g. if `sample2()`'s actual contract differs from what the
# reviewed code implies: return the single element unchanged when
# `length(x) == 1`, otherwise behave like `sample()`).

# ---- helpers -----------------------------------------------------------

make_opinion_df <- function(n) {
  data.frame(x1 = runif(n, -1, 1), x2 = runif(n, -1, 1))
}

# ==========================================================================
# Input validation
# ==========================================================================

test_that("agent_opinion values must fall within [-1, 1]", {
  op <- data.frame(x1 = c(0.2, 1.5), x2 = c(0, 0))
  expect_error(
    model_opinion_jager(n_agent = 2, agent_opinion = op, sim_times = 0),
    "range from -1 to 1"
  )
})

test_that("agent_opinion must have exactly 2 columns", {
  op <- data.frame(x1 = c(0.1, 0.2))
  expect_error(
    model_opinion_jager(n_agent = 2, agent_opinion = op, sim_times = 0),
    "number of columns"
  )
})

test_that("agent_opinion row count must match n_agent", {
  op <- make_opinion_df(3)
  expect_error(
    model_opinion_jager(n_agent = 5, agent_opinion = op, sim_times = 0),
    "number of rows"
  )
})

test_that("tprob must be within [0, 1]", {
  expect_error(
    model_opinion_jager(n_agent = 5, tprob = 1.2, net_type = "random", sim_times = 0),
    "tprob"
  )
})

test_that("agent_accept/reject/learn/self_influence length must be 1 or n_agent", {
  expect_error(
    model_opinion_jager(n_agent = 5, agent_accept = c(0.1, 0.2), sim_times = 0),
    "agent_accept"
  )
  expect_error(
    model_opinion_jager(n_agent = 5, agent_reject = c(0.1, 0.2), sim_times = 0),
    "agent_reject"
  )
  expect_error(
    model_opinion_jager(n_agent = 5, agent_learn = c(0.1, 0.2), sim_times = 0),
    "agent_learn"
  )
  expect_error(
    model_opinion_jager(n_agent = 5, agent_self_influence = c(0.1, 0.2), sim_times = 0),
    "agent_self_influence"
  )
})

test_that("agent_accept must not exceed agent_reject", {
  expect_error(
    model_opinion_jager(n_agent = 5, agent_accept = 0.7, agent_reject = 0.3, sim_times = 0),
    "smaller or equal"
  )
})

test_that("agent_learn must be within (0, 1]", {
  expect_error(
    model_opinion_jager(n_agent = 5, agent_learn = 0, sim_times = 0),
    "agent_learn"
  )
  expect_error(
    model_opinion_jager(n_agent = 5, agent_learn = 1.5, sim_times = 0),
    "agent_learn"
  )
  expect_silent(
    model_opinion_jager(n_agent = 5, agent_learn = 1, sim_times = 0)
  )
})

test_that("agent_self_influence must be within [0, 1]", {
  expect_error(
    model_opinion_jager(n_agent = 5, agent_self_influence = -0.1, sim_times = 0),
    "agent_self_influence"
  )
  expect_error(
    model_opinion_jager(n_agent = 5, agent_self_influence = 1.1, sim_times = 0),
    "agent_self_influence"
  )
})

test_that("net_agent dimensions must match n_agent", {
  bad_net <- matrix(1, 3, 3); diag(bad_net) <- 0
  expect_error(
    model_opinion_jager(n_agent = 5, net_agent = bad_net, sim_times = 0),
    "Number of rows and columns"
  )
})

test_that("net_agent with an isolated agent is rejected", {
  net <- matrix(1, 4, 4); diag(net) <- 0
  net[2, ] <- 0; net[, 2] <- 0  # agent 2 has no neighbours
  expect_error(
    model_opinion_jager(n_agent = 4, net_agent = net, sim_times = 0),
    "isolated"
  )
})

test_that("supplying both net_agent and net_type warns", {
  net <- matrix(1, 4, 4); diag(net) <- 0
  expect_warning(
    model_opinion_jager(n_agent = 4, net_agent = net, net_type = "random", sim_times = 0),
    "net_type"
  )
})

test_that("lattice network requires a perfect square n_agent", {
  expect_error(
    model_opinion_jager(n_agent = 10, net_type = "lattice", sim_times = 0),
    "perfect square"
  )
})

# ==========================================================================
# sim_times behaviour: unrun vs. simulated return value
# ==========================================================================

test_that("sim_times = 0 returns the unrun game at time 1", {
  G <- model_opinion_jager(n_agent = 5, sim_times = 0, seed = 1)
  expect_s3_class(G, "ABM_Game")
  expect_equal(G$time, 1)
})

test_that("sim_times > 0 runs the game and advances time", {
  G <- model_opinion_jager(n_agent = 5, sim_times = 10, seed = 1)
  expect_s3_class(G, "ABM_Game")
  expect_equal(G$time, 11)
  expect_true(length(G$log) > 1)
})

test_that("results are reproducible with the same seed", {
  G1 <- model_opinion_jager(n_agent = 8, sim_times = 15, seed = 42)
  G2 <- model_opinion_jager(n_agent = 8, sim_times = 15, seed = 42)
  expect_equal(G1$agent_opinion, G2$agent_opinion)
})

test_that("default network type is a fully connected complete graph", {
  G <- model_opinion_jager(n_agent = 6, sim_times = 0, seed = 1)
  net <- G$net_agent
  expect_equal(diag(net), rep(0, 6))
  expect_true(all(net[row(net) != col(net)] == 1))
})

test_that("agent opinions remain within [-1, 1] after simulation", {
  G <- model_opinion_jager(n_agent = 10, sim_times = 30, seed = 5)
  expect_true(all(G$agent_opinion >= -1 & G$agent_opinion <= 1))
})

test_that("influence_opinion keeps x2 opinions within [-1, 1]", {
  G <- model_opinion_jager(
    n_agent   = 10,
    plan      = c("select_alter", "update_opinion", "influence_opinion"),
    sim_times = 30,
    seed      = 5
  )
  expect_true(all(G$agent_opinion[, 2] >= -1 & G$agent_opinion[, 2] <= 1))
})

# ==========================================================================
# Regression test: change_issue() no longer hits the sample() pitfall
#
# Before the fix, change_issue() called base sample(next_issue_candid,
# size = 1) where next_issue_candid is a length-1 numeric vector (e.g. 2).
# R's sample() special-cases length-1 numeric input as "sample from
# 1:x" rather than "return x", so the topic sometimes failed to switch
# even when do_change was TRUE. With issue_change_prob = 1, do_change is
# always TRUE, so the topic must alternate deterministically every step
# once the bug is fixed.
# ==========================================================================

test_that("change_issue deterministically alternates when issue_change_prob = 1", {
  G <- model_opinion_jager(
    n_agent           = 5,
    plan              = c("change_issue"),
    issue_change_prob = 1,
    sim_times         = 40,
    seed              = 1
  )
  issue_log <- unlist(value_of_log(G, "issue_discussing", log = "all"))
  expect_true(all(issue_log %in% c(1, 2)))
  expect_true(all(diff(issue_log) != 0))
})

# ==========================================================================
# Regression test: report_trajectory(n = 1) no longer indexes past the
# selected agent vector.
#
# Before the fix, the plotting loop used agent_idx[2:length(agent_idx)];
# when agent_idx has length 1 (e.g. n = 1), 2:length(agent_idx) becomes
# 2:1 == c(2, 1), which reads a nonexistent second element (NA) and then
# tries to plot a nonexistent opinion column. Additionally, the function
# never actually subset the opinion matrix to the selected agents: it drew
# raw column 1 of the *full* n_agent-column matrix unconditionally, looped
# over agent_idx[-1] as indices into that same full matrix, and then tried
# to assign length(agent_idx) colnames onto all n_agent columns -- which
# throws a dimnames-length error whenever n < n_agent. The fix subsets
# 'opinion' to the selected agents up front, so plotting and colnames line
# up regardless of how many agents are requested.
# ==========================================================================

test_that("report_trajectory(n = 1) does not error and returns one column", {
  G <- model_opinion_jager(n_agent = 6, sim_times = 5, seed = 1)

  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  out <- G$report_trajectory(n = 1, issue_idx = 1)
  expect_true(is.matrix(out))
  expect_equal(ncol(out), 1)
  expect_equal(nrow(out), length(G$log))
})

test_that("report_trajectory(n = k) selects exactly k agents with matching column labels", {
  G <- model_opinion_jager(n_agent = 6, sim_times = 5, seed = 1)

  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  set.seed(99)
  expected_idx <- sample2(G$settings$agent_idx, 3)

  set.seed(99)
  out <- G$report_trajectory(n = 3, issue_idx = 1)

  expect_equal(ncol(out), 3)
  expect_equal(colnames(out), paste0("agent_", expected_idx))
})

# ==========================================================================
# Regression test: report_n_groups() no longer miscounts opinions
# truncated to exactly -1.
#
# cut()'s default include.lowest = FALSE excludes the lower bound of the
# leftmost interval, so a value of exactly -1 (which update_opinion()'s
# truncation step can legitimately produce) used to become NA and be
# counted as its own "group", inflating the apparent number of opinion
# clusters. We bypass the stochastic simulation and inspect
# report_n_groups() directly on a crafted log so the test is deterministic:
# three agents (-1, -0.9, 0.5) form two true clusters ({-1, -0.9} and
# {0.5}); the pre-fix behaviour would report 3 (the -1 agent counted as a
# spurious NA group on its own), the fixed behaviour reports 2.
# ==========================================================================

test_that("report_n_groups does not miscount agents whose opinion is exactly -1", {
  G <- model_opinion_jager(n_agent = 3, sim_times = 0, seed = 1)

  G$log <- list(
    t1 = list(
      agent_opinion = data.frame(x1 = c(-1, -0.9, 0.5), x2 = c(0, 0, 0)),
      time = 1
    )
  )

  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  n_groups <- G$report_n_groups(issue_idx = 1, interval = 0.5)
  expect_equal(unname(n_groups), 2)
})
