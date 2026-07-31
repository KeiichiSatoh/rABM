test_that("copy_obj.ABM_Game() returns an independent deep clone", {
  pop <- 100
  reproduce <- function() { self$pop <- self$pop * 1.1 }

  G  <- Game(State(pop), Act(reproduce))
  G2 <- copy_obj(G)

  expect_s3_class(G2, "ABM_Game")
  expect_false(identical(G, G2))

  # mutating the copy must not affect the original
  G2$pop <- 999
  expect_equal(G$pop, 100)
  expect_equal(G2$pop, 999)
})

test_that("copy_obj.ABM_Game() re-binds act_FUN to the cloned object, not the original", {
  pop <- 1
  grow <- function() { self$pop <- self$pop + 1 }

  G  <- Game(State(pop), Act(grow))
  G2 <- copy_obj(G)

  # If .rebind_dynamic_fields() were not applied, grow()'s closure would
  # still point at G's environment, and this call would mutate G$pop
  # instead of G2$pop.
  G2$grow()

  expect_equal(G2$pop, 2)
  expect_equal(G$pop, 1)
})

test_that("copy_obj.ABM_Game() re-binds active_state to the cloned object", {
  base <- 10
  doubled <- function() { self$base * 2 }

  G  <- Game(State(base), Active(doubled))
  G2 <- copy_obj(G)

  G2$base <- 50

  expect_equal(G2$doubled, 100)
  expect_equal(G$doubled, 20)
})

test_that("copy_obj.ABM_Game() preserves time, log, and notes", {
  x <- 1
  G  <- Game(State(x), time = 5, log = list(t1 = list(x = 1)), notes = list("note1"))
  G2 <- copy_obj(G)

  expect_equal(G2$time, 5)
  expect_equal(G2$log, list(t1 = list(x = 1)))
  expect_equal(G2$notes, list("note1"))
})

test_that("copy_obj.ABM_Game() preserves the field list (names and categories)", {
  x <- 1
  grow <- function() { self$x <- self$x + 1 }
  G  <- Game(State(x), Act(grow))
  G2 <- copy_obj(G)

  expect_equal(G2$.get_flist(), G$.get_flist())
})

test_that("copy_obj.R6() deep-clones a plain (non-ABM_Game) R6 object", {
  Simple <- R6::R6Class("Simple", public = list(val = 1))
  obj  <- Simple$new()
  obj2 <- copy_obj(obj)

  expect_false(identical(obj, obj2))

  obj2$val <- 99
  expect_equal(obj$val, 1)
  expect_equal(obj2$val, 99)
})

test_that("copy_obj() dispatches to the ABM_Game method rather than the generic R6 method", {
  x <- 1
  G <- Game(State(x))

  # ABM_Game objects also inherit from R6; the more specific method must
  # take precedence so that .rebind_dynamic_fields() is applied.
  expect_true(inherits(G, "R6"))

  G2 <- copy_obj(G)
  expect_true(inherits(G2, "ABM_Game"))
})

test_that("copy_obj() errors for unsupported (non-R6) classes", {
  expect_error(copy_obj(1))
  expect_error(copy_obj(list(a = 1)))
  expect_error(copy_obj("a"))
})
