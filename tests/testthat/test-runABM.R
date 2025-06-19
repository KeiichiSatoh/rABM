test_that("runABM: plan parsing and update_FUN creation", {
  agents <- init_agents(attr_df = data.frame(x = 1:3),
                        act_FUN = list(act_plus1 = function(G, E = NULL) { self$x <- self$x + 1 }))
  G <- setABM(agents = agents,
              select_FUN = list(select_all = function(G, E = NULL) {1:length(G$agents)}),
              )
  result <- runABM(G, plan = c("select_all", "act_plus1"), times = 2, save_log = FALSE)
  expect_equal(result$agents[[1]]$x, 3)
})

test_that("runABM: update_FUN_name route", {
  agents <- init_agents(attr_df = data.frame(x = 1:2))
  G <- setABM(agents = agents,
              update_FUN = list(update_sample = function(G, E = NULL) {
                lapply(1:length(G$agents), function(i){
                  G$agents[[i]]$x <- G$agents[[i]]$x + 2
                })
              }))

  result <- runABM(G, update_FUN_name = "update_sample", times = 1, save_log = FALSE)
  expect_equal(result$agents[[1]]$x, 3)
})

test_that("runABM: stop_FUN_name works", {
  agents <- init_agents(attr_df = data.frame(x = 1:2))
  stop_immediate <- function(G, E = NULL) {TRUE}
  update_dummy <- function(){NULL}
  G <- setABM(agents = agents, stop_FUN = stop_immediate, update_FUN = update_dummy)

  result <- runABM(G, update_FUN_name = "update_dummy", stop_FUN_name = "stop_immediate", save_log = FALSE)
  expect_equal(G$time, 1)
})

test_that("runABM: seed sets correctly", {
  update_random <- function(){self$stage <- self$stage + sample(1:5, 1)}
  G <- setABM(stage = 1, update_FUN = update_random)
  result <- runABM(G, update_FUN_name = "update_random", seed = 123, times = 1, save_log = FALSE)
  expect_equal(result$stage, 4)
  expect_equal(result$notes$seed, 123)
})

test_that("runABM: return_update_FUN stores body", {
  update_random <- function(){G$stage <- G$stage + sample(1:5, 1)}
  G <- setABM(stage = 1, global_FUN = update_random)
  result <- runABM(G, plan = "update_random", return_update_FUN = TRUE,
                   save_log = FALSE, add_tryCatch = FALSE)
  expect_true("    G$update_random(G = G, E = E)" == result$notes$update_FUN_used[[2]])
})

#test_that("runABM: saveRDS_inbetween writes file", {
#  update_random <- function(){G$stage <- G$stage + sample(1:5, 1)}
#  G <- setABM(stage = 1, global_FUN = update_random)
#
#  result <- runABM(G, plan = "update_random", return_update_FUN = TRUE,
#                   save_log = FALSE, add_tryCatch = FALSE,
#                   saveRDS_inbetween = TRUE)
#  unlink("G_temp.rds")
#})

test_that("runABM: tryCatch traps errors if enabled", {
  agents <- init_agents(attr_df = data.frame(x = 1))
  faulty <- function(G, E = NULL) stop("Intentional error")

  G <- setABM(agents = agents, update_FUN = faulty)

  expect_warning(
    result <- runABM(G, update_FUN_name = "faulty", add_tryCatch = TRUE, times = 1, save_log = FALSE),
    regexp = "error occured for faulty"
  )
})


test_that("runABM: plan with multiple functions and implicit select_FUN", {
  score_up <- function(G, E = NULL) {
    self$score <- self$score + 10
  }
  agents <- init_agents(attr_df = data.frame(score = 0:2), act_FUN = score_up)

  double_scores <- function(G, E = NULL) {
    for (i in seq_along(G$agents)) {
      G$agents[[i]]$score <- G$agents[[i]]$score * 2
    }
  }

  G <- setABM(agents = agents, global_FUN = double_scores)

  plan <- c("agents:score_up", "double_scores", "agents:score_up")

  result <- runABM(G, plan = plan, times = 1, save_log = FALSE)

  # Step-by-step: +10 → *2 → +10
  expect_equal(result$agents[[1]]$score, 30)
  expect_equal(result$agents[[2]]$score, 32)
  expect_equal(result$agents[[3]]$score, 34)
})


test_that("runABM: fails if neither plan nor update_FUN_name is provided", {
  agents <- init_agents(attr_df = data.frame(x = 1:2))
  G <- setABM(agents = agents)

  expect_error(runABM(G, save_log = FALSE),
               regexp = "Either 'plan' or 'update_FUN_name' must be provided")
})


test_that("runABM: fails if update_FUN_name is not categorized correctly", {
  agents <- init_agents(attr_df = data.frame(x = 1))
  G <- setABM(agents = agents,
              plot_FUN = list(bad_fun = function(G, E = NULL) {NULL}))

  expect_error(runABM(G, update_FUN_name = "bad_fun", save_log = FALSE))
})


test_that("runABM: fails when plan refers to undefined function", {
  agents <- init_agents(attr_df = data.frame(x = 1))
  G <- setABM(agents = agents)

  expect_error(runABM(G, plan = "agents:nonexistent", save_log = FALSE))
})

