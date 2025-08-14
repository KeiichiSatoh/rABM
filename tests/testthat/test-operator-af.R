test_that("%af% applies agent functions correctly (no args)", {
  get_older <- function() { self$age <- self$age + 1 }

  agents <- init_agents(
    n = 3,
    attr_df = data.frame(age = 1:3),
    act_FUN = get_older
  )

  # apply without arguments
  agents %aa% "get_older" %af% list()

  ages_after <- agents %aa% "age"
  expect_equal(ages_after, as.list(2:4))
})

test_that("%af% applies functions with arguments (G passed)", {
  get_older <- function() { self$age <- self$age + 10 }

  agents <- init_agents(
    n = 3,
    attr_df = data.frame(age = c(5, 10, 15)),
    act_FUN = get_older
  )

  G <- setABM(agents = agents)

  # apply to subset with G
  G$agents %ai% c(1, 3) %aa% "get_older" %af% list(G = G)

  expect_equal(G$agents[[1]]$age, 15)
  expect_equal(G$agents[[2]]$age, 10)  # untouched
  expect_equal(G$agents[[3]]$age, 25)
})

test_that("%af% returns NULL for failing functions but continues", {
  get_older <- function() { self$age <- self$age + 10 }

  agents <- init_agents(
    n = 3,
    attr_df = data.frame(age = c(5, 10, 15)),
    act_FUN = get_older
  )

  expect_error(agents %aa% "get_older" %af% list(a = 1))
  expect_true(  agents[[1]]$age == 5)
  expect_true(  agents[[2]]$age == 10)
  expect_true(  agents[[3]]$age == 15)
})
