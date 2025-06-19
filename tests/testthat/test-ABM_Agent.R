
test_that("ABM_Agent initializes with fields and methods correctly", {
  agent <- ABM_Agent$new(
    fields = list(ID = 1, age = 25),
    methods = list(say_hi = function() paste("Hi, I'm", self$ID))
  )
  expect_equal(agent$ID, 1)
  expect_equal(agent$age, 25)
  expect_equal(agent$say_hi(), "Hi, I'm 1")
})

test_that(".add_field adds a field", {
  agent <- ABM_Agent$new()
  agent$.add_field("x", 100)
  expect_equal(agent$x, 100)
})

test_that(".add_method adds a method", {
  agent <- ABM_Agent$new()
  agent$.add_method("get_x", function() 123)
  expect_equal(agent$get_x(), 123)
})

test_that(".add_active_binding adds active binding", {
  agent <- ABM_Agent$new(fields = list(base = 10))
  agent$.add_active_binding("doubled", function() self$base * 2)
  expect_equal(agent$doubled, 20)
})

test_that(".remove_field removes field or binding", {
  agent <- ABM_Agent$new()
  agent$.add_field("to_remove", 5)
  agent$.remove_field("to_remove")
  expect_false("to_remove" %in% ls(agent))
})

test_that(".replace_field works for a value correctly", {
  agent <- ABM_Agent$new(fields = list(val = 10))
  agent$.replace_field("val", 20)
  expect_equal(agent$val, 20)
})

test_that(".replace_field works for a method correctly", {
  agent <- ABM_Agent$new(fields = list(val = 10), methods = list(act = function(){print(1)}))
  act2 <- function(){print(2)}
  agent$.replace_field(name = "act", value = act2)
  expect_true(body(agent$act) == body(act2))
})

test_that(".rename_field works for fields and active bindings", {
  agent <- ABM_Agent$new(fields = list(old = 1))
  agent$.rename_field("old", "new")
  expect_false("old" %in% ls(agent))
  expect_equal(agent$new, 1)

  agent <- ABM_Agent$new(fields = list(base = 3))
  agent$.add_active_binding("double", function() self$base * 2)
  agent$.rename_field("double", "doubled")
  expect_equal(agent$doubled, 6)
})

test_that(".save returns expected list", {
  agent <- ABM_Agent$new(fields = list(ID = 10, age = 25))
  result <- agent$.save()
  expect_equal(result$ID, 10)
  expect_equal(result$age, 25)
  expect_null(result$say_hi)
})
