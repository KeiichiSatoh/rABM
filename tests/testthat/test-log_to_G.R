test_that("log_to_G restores ABM_G correctly", {
  # Setup
  act1 <- function(){ self$x <- self$x + 1 }
  act2 <- function(){ self$y <- self$y + 1 }
  ab   <- function(){ self$x^2 }
  g1   <- function(){ NULL }

  agents <- init_agents(
    n = 3,
    attr_df = data.frame(x = 1:3, y = 4:6),
    act_FUN = list(act1 = act1, act2 = act2),
    active_binding = ab
  )

  G <- setABM(
    agents = list(agents1 = agents, agents2 = agents),
    stage = matrix(1:9, 3, 3),
    global_FUN = g1
  )

  G <- runABM(G = G, plan = c("act1", "act2", "g1"), times = 3)

  # Call log_to_G for time = 2
  G2 <- log_to_G(G, which_time = 2)

  # Check that time is correctly restored
  expect_equal(G2$time, 2)

  # Check stage is restored correctly
  expect_equal(G2$stage, G$log[[2]]$stage)

  # Check agent attributes are restored
  expect_equal(G2$agents1[[1]]$x, G$log[[2]]$agents1[[1]]$x)
  expect_equal(G2$agents1[[1]]$y, G$log[[2]]$agents1[[1]]$y)
})

test_that("log_to_G errors for nonexistent time", {
  # Dummy ABM object with empty log
  dummy_G <- setABM()

  expect_error(log_to_G(dummy_G, which_time = 999),
               "Specified 'which_time' not found in G\\$log")
})

test_that("log_to_G warns for unmatched agent IDs", {
  # Setup with mismatched IDs
  act <- function(){ self$val <- self$val + 1 }

  agents <- init_agents(
    n = 2,
    attr_df = data.frame(val = 1:2),
    act_FUN = list(act = act)
  )

  G <- setABM(agents = list(group = agents), stage = NULL)
  G <- runABM(G = G, plan = "act", times = 1)

  # Modify agent IDs so they don't match the log
  G$group[[1]]$ID <- 100
  G$group[[2]]$ID <- 200

  expect_warning(log_to_G(G, 1))
})
