
test_that("%aa% extracts attribute values correctly", {
  agents <- init_agents(n = 3, attr_df = data.frame(x = c(10, 20, 30)))
  out <- agents %aa% "x"
  out <- unlist(out)
  expect_equal(out, c(10, 20, 30))
})

test_that("%aa%<- sets attribute values correctly", {
  agents <- init_agents(n = 3, attr_df = data.frame(x = c(0, 0, 0)))
  agents %aa% "x" <- list(100, 200, 300)
  expect_equal(agents[[1]]$x, 100)
  expect_equal(agents[[2]]$x, 200)
  expect_equal(agents[[3]]$x, 300)
})

test_that("%aa%<- throws error if value length does not match agents", {
  agents <- init_agents(n = 3, attr_df = data.frame(x = c(0, 0, 0)))
  expect_error({
    agents %aa% "x" <- list(100, 200)
  }, "length")
})
