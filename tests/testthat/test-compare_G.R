test_that("compare_G detects differences in stage and agent attributes", {
  # --- Agents A: with x ---
  agents_A <- init_agents(
    3,
    attr_df = data.frame(x = 1:3),
    act_FUN = list(plus1_to_x = function() { self$x <- self$x + 1 })
  )

  # --- Agents B: with y ---
  agents_B <- init_agents(
    3,
    attr_df = data.frame(y = rep(0, 3))
  )

  # --- Set initial state ---
  G <- setABM(
    agents = list(agents_A = agents_A, agents_B = agents_B),
    stage = list(z1 = 1, mat1 = matrix(1:9, 3, 3))
  )

  # --- Run one step ---
  G <- runABM(G = G, plan = "plus1_to_x")

  # --- Compare time1 vs time2 (should detect changes in agents_A$x) ---
  out <- compare_G(G = G, time1 = 1, time2 = 2)

  expect_type(out, "list")
  expect_named(out, c("stage", "agents"))
  expect_s3_class(out$stage, "data.frame")
  expect_s3_class(out$agents, "data.frame")

  # At least one change in agents_A$x should be detected
  expect_true(any(out$agents$field_name == "x"))

  # No false detection on ID
  expect_false(any(out$agents$field_name == "ID"))

  # If same snapshot is compared, result should be empty
  same <- compare_G(G = G, time1 = 1, time2 = 1)
  expect_equal(nrow(same$agents), 0)
  expect_equal(nrow(same$stage), 0)
})

test_that("compare_G works with G1 and G2 directly", {
  agents1 <- init_agents(3, attr_df = data.frame(a = 1:3, sex = c("m", "f", "f")))
  agents2 <- init_agents(3, attr_df = data.frame(a = 1:3, sex = c("f", "f", "f")))

  G1 <- setABM(agents = c(agents = agents1))
  G2 <- setABM(agents = c(agents = agents2))

  out <- compare_G(G = G1, G2 = G2)

  expect_s3_class(out$agents, "data.frame")
  expect_true(any(out$agents$field_name == "sex"))
})

test_that("compare_G errors when both G2 and time2 are supplied", {
  agents <- init_agents(2, attr_df = data.frame(a = 1:2))
  G <- setABM(agents = agents)

  expect_error(compare_G(G = G, G2 = G, time2 = 2), "Specify either 'G2' or 'time2'")
})

test_that("compare_G returns empty diff when no changes", {
  agents <- init_agents(2, attr_df = data.frame(a = 1:2))
  G <- setABM(agents = agents, global_FUN = function(){})
  G <- runABM(G, plan = "global_FUN", times = 1)

  out <- compare_G(G = G, time1 = 1, time2 = 2)
  expect_equal(nrow(out$stage), 0)
  expect_equal(nrow(out$agents), 0)
})
