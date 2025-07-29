
test_that("init_agents creates correct number of agents", {
  agents <- init_agents(n = 3, attr_df = data.frame(age = c(10, 20, 30)))
  expect_length(agents, 3)
  expect_equal(agents[[1]]$age, 10)
})

test_that("init_agents raises error when n and attr_df do not match", {
  expect_error(init_agents(n = 2, attr_df = data.frame(age = c(10, 20, 30))))
})

test_that("act_FUN is correctly assigned to agents", {
  act <- function() { self$age <- self$age + 1 }
  agents <- init_agents(
    n = 2,
    attr_df = data.frame(age = c(1, 2)),
    act_FUN = act
  )
  agents[[1]]$act()
  expect_equal(agents[[1]]$age, 2)
})

test_that("active_binding is correctly computed", {
  active_binding <- list(
    status = function() {
      if (self$age >= 18) "adult" else "child"
    }
  )
  agents <- init_agents(
    n = 2,
    attr_df = data.frame(age = c(10, 20)),
    active_binding = active_binding
  )
  expect_equal(agents[[1]]$status, "child")
  expect_equal(agents[[2]]$status, "adult")
})

test_that("custom_ID is respected", {
  agents <- init_agents(
    attr_df = data.frame(age = 1:3),
    custom_ID = c(100, 200, 300)
  )
  IDs <- unlist(lapply(agents, function(a) a$ID))
  expect_equal(IDs, c(100, 200, 300))
})

test_that("other_attrs are correctly assigned", {
  mat_list <- list(matrix(1:4, 2, 2), matrix(5:8, 2, 2))
  agents <- init_agents(
    n = 2,
    attr_df = data.frame(age = c(1, 2)),
    other_attrs = list(scores = mat_list)
  )
  expect_equal(agents[[1]]$scores, matrix(1:4, 2, 2))
  expect_equal(agents[[2]]$scores, matrix(5:8, 2, 2))
})

test_that("init_agents throws error for unnamed and length-mismatched other_attrs", {
  expect_error(
    init_agents(
      n = 2,
      attr_df = data.frame(age = c(1, 2)),
      other_attrs = list(list(1, 2, 3))  # length mismatch
    )
  )
})

test_that("init_agents throws error if both n and attr_df are NULL", {
  expect_error(init_agents())
})
