test_that("get_agents_by_idx works with agents argument (deep and shallow)", {
  agents <- init_agents(n = 3, attr_df = data.frame(x = 1:3))

  # deep copy
  result_deep <- get_agents_by_idx(agents = agents, idx = c(1, 3), deep = TRUE)
  expect_length(result_deep, 2)
  expect_equal(result_deep[[1]]$x, 1)
  expect_equal(result_deep[[2]]$x, 3)
  expect_false(identical(result_deep[[1]], agents[[1]]))  # deep copy

  # shallow copy
  result_shallow <- get_agents_by_idx(agents = agents, idx = c(1, 3), deep = FALSE)
  expect_true(identical(result_shallow[[1]], agents[[1]]))
})

test_that("get_agents_by_idx works with G object", {
  agents <- init_agents(n = 3, attr_df = data.frame(x = 1:3))
  G <- setABM(agents = agents)

  result <- get_agents_by_idx(G = G, G_agents_name = "agents", idx = c(1, 2))
  expect_length(result, 2)
  expect_equal(result[[1]]$x, 1)
  expect_equal(result[[2]]$x, 2)
})

test_that("get_agents_by_idx preserves names if available", {
  agents <- init_agents(n = 3, attr_df = data.frame(x = 1:3))
  names(agents) <- c("a1", "a2", "a3")

  result <- get_agents_by_idx(agents = agents, idx = c(2, 3))
  expect_named(result, c("a2", "a3"))
})

test_that("get_agents_by_idx throws errors when arguments are incorrect", {
  agents <- init_agents(n = 3, attr_df = data.frame(x = 1:3))
  G <- setABM(agents = agents)

  # G_agents_name missing
  expect_error(get_agents_by_idx(G = G, idx = 1), "G_agents_name must be supplied")

  # agents missing and G missing
  expect_error(get_agents_by_idx(idx = 1), "Either 'G' or 'agents' must be supplied")

  # G_agents_name not character
  expect_error(get_agents_by_idx(G = G, G_agents_name = 1, idx = 1), "G_agents_name must be a character of length 1")
})

