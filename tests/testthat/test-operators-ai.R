
test_that("%ai% extracts agents correctly", {
  agents <- init_agents(n = 3, attr_df = data.frame(x = 1:3))
  out <- agents %ai% c(1, 3)
  expect_length(out, 2)
  expect_equal(names(out), names(agents)[c(1, 3)])
  expect_identical(out[[1]], agents[[1]])
})

test_that("%ai%<- replaces agents at specified indices", {
  agents <- init_agents(n = 3, attr_df = data.frame(x = 1:3))
  a2 <- agents[[2]]$clone(deep = TRUE)
  a2$x <- 100
  agents %ai% 2 <- list(a2)
  expect_equal(agents[[2]]$x, 100)
})

test_that("%ai%<- throws error if value length does not match idx", {
  agents <- init_agents(n = 3, attr_df = data.frame(x = 1:3))
  expect_error({
    agents %ai% c(1, 2) <- list(agents[[1]])
  }, "length")
})
