test_that("copy_agents works with ABM_G object", {
  G <- setABM(agents = 3)
  copied_agents <- copy_agents(G = G, G_agents_name = "agents")

  expect_type(copied_agents, "list")
  expect_length(copied_agents, 3)

  # Deep copy: Changing the original G does not affect the copy
  G$agents[[1]]$ID <- "modified"
  expect_false(identical(copied_agents[[1]]$ID, G$agents[[1]]$ID))
})

test_that("copy_agents works with a direct agent list", {
  agents <- init_agents(n = 2)
  copied_agents <- copy_agents(agents = agents)

  expect_type(copied_agents, "list")
  expect_length(copied_agents, 2)
  expect_equal(names(copied_agents), names(agents))
  expect_true(all(vapply(copied_agents, inherits, logical(1), "ABM_Agent")))
})

test_that("copy_agents warns and returns NULL for non-clonable objects", {
  agents <- list(init_agents(n = 1)[[1]], "not an agent")

  expect_warning(
    copied <- copy_agents(agents = agents),
    "2 th object in 'agents' is not the class of 'ABM_Agent'"
  )

  expect_length(copied, 2)
  expect_null(copied[[2]])
})
