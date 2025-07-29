test_that("Operator %aid% extracts agents by ID (shallow copy)", {
  agents <- init_agents(n = 3, attr_df = data.frame(x = 1:3), ID_start = 10)  # IDs: 10,11,12
  selected <- agents %aid% c(10, 12)

  expect_length(selected, 2)
  expect_true(all(vapply(selected, function(a) a$ID, numeric(1)) %in% c(10, 12)))
  expect_identical(selected[[1]], agents[[1]])  # same object (shallow)
})

test_that("Operator %aid_dp% extracts agents by ID (deep copy)", {
  agents <- init_agents(n = 2, attr_df = data.frame(x = 1:2), ID_start = 5)
  cloned <- agents %aid_dp% c(5)

  expect_length(cloned, 1)
  expect_equal(cloned[[1]]$ID, 5)
  expect_false(identical(cloned[[1]], agents[[1]]))  # deep copy: different object
})

test_that("Operator %aid%<- replaces agents by ID", {
  agents <- init_agents(n = 3, attr_df = data.frame(x = 1:3), ID_start = 1)

  # make deep copy of agent with ID=3
  replacement <- agents %aid_dp% 3

  # replace agent with ID=1 by agent with ID=3
  agents <- `%aid%<-`(agents, 1, replacement)

  expect_equal(agents[[1]]$ID, 3)
  expect_equal(agents[[1]]$x, 3)
})

test_that("Invalid ID raises warning", {
  agents <- init_agents(n = 2, attr_df = data.frame(x = 1:2), ID_start = 1)

  expect_warning(agents %aid% c(99))
  expect_warning(agents %aid_dp% c(999))

  expect_error(`%aid%<-`(agents, 1, list()))
})
