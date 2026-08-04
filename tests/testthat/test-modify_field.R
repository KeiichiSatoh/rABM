#-------------------------------------------------------------------------------
# Tests for add_field() / remove_field() / replace_field()
#-------------------------------------------------------------------------------

# Helper to build a fresh Game object for each test, so tests don't leak
# state into one another via R6 reference semantics.
.make_test_game <- function() {
  pop <- 100
  reproduce <- function() { self$pop <- self$pop * 1.1 }
  Game(State(pop), Act(reproduce))
}

#===============================================================================
# add_field()
#===============================================================================

test_that("add_field() errors if 'G' is not an ABM_Game object", {
  expect_error(
    add_field(list(), State(1, name = "x")),
    "'G' must be a 'ABM_Game' class object."
  )
})

test_that("add_field() errors if '...' is empty", {
  G <- .make_test_game()
  expect_error(
    add_field(G),
    "'\\.\\.\\.' must not be empty"
  )
})

test_that("add_field() adds a new state field", {
  G <- .make_test_game()
  z <- 42
  add_field(G, State(z))

  expect_true("z" %in% names(G$.get_category()))
  expect_identical(unname(G$.get_category()["z"]), "state")
  expect_equal(G$z, 42)
})

test_that("add_field() adds a new act_FUN field", {
  G <- .make_test_game()
  greet <- function() { message("hi") }
  add_field(G, Act(greet))

  expect_true("greet" %in% names(G$.get_category()))
  expect_identical(unname(G$.get_category()["greet"]), "act_FUN")
  expect_true(is.function(G$greet))
})

test_that("add_field() accepts multiple fields and Zip() bundles in one call", {
  G <- .make_test_game()
  a <- 1
  b <- 2
  add_field(G, Zip(State(a), State(b)))

  expect_true(all(c("a", "b") %in% names(G$.get_category())))
})

test_that("add_field() propagates duplicate-name errors from G$.add()", {
  G <- .make_test_game()
  pop <- 999
  expect_error(
    add_field(G, State(pop)),
    "Duplicated field names"
  )
})

test_that("add_field() returns G invisibly", {
  G <- .make_test_game()
  z <- 1
  expect_invisible_G <- withVisible(add_field(G, State(z)))
  expect_false(expect_invisible_G$visible)
  expect_identical(expect_invisible_G$value, G)
})

#===============================================================================
# remove_field()
#===============================================================================

test_that("remove_field() errors if 'G' is not an ABM_Game object", {
  expect_error(
    remove_field(list(), "pop"),
    "'G' must be a 'ABM_Game' class object."
  )
})

test_that("remove_field() errors if '...' is empty", {
  G <- .make_test_game()
  expect_error(
    remove_field(G),
    "'\\.\\.\\.' must not be empty"
  )
})

test_that("remove_field() removes an existing field", {
  G <- .make_test_game()
  remove_field(G, "pop")

  expect_false("pop" %in% names(G$.get_category()))
  expect_false(exists("pop", envir = G, inherits = FALSE))
})

test_that("remove_field() removes multiple fields in one call", {
  G <- .make_test_game()
  remove_field(G, "pop", "reproduce")

  expect_false(any(c("pop", "reproduce") %in% names(G$.get_category())))
})

test_that("remove_field() propagates not-found errors from G$.remove()", {
  G <- .make_test_game()
  expect_error(
    remove_field(G, "does_not_exist"),
    "does not exist in the fields"
  )
})

test_that("remove_field() returns G invisibly", {
  G <- .make_test_game()
  result <- withVisible(remove_field(G, "pop"))
  expect_false(result$visible)
  expect_identical(result$value, G)
})

#===============================================================================
# replace_field()
#===============================================================================

test_that("replace_field() errors if 'G' is not an ABM_Game object", {
  expect_error(
    replace_field(list(), State(1, name = "x")),
    "'G' must be a 'ABM_Game' class object."
  )
})

test_that("replace_field() errors if '...' is empty", {
  G <- .make_test_game()
  expect_error(
    replace_field(G),
    "'\\.\\.\\.' must not be empty"
  )
})

test_that("replace_field() replaces a state field's value", {
  G <- .make_test_game()
  pop <- 555
  replace_field(G, State(pop))

  expect_equal(G$pop, 555)
  expect_identical(unname(G$.get_category()["pop"]), "state")
})

test_that("replace_field() can change a field's category (state -> active_state)", {
  G <- .make_test_game()
  pop <- function() { 12345 }
  replace_field(G, Active(pop))

  expect_identical(unname(G$.get_category()["pop"]), "active_state")
  expect_equal(G$pop, 12345)
})

test_that("replace_field() propagates not-found errors from G$.replace()", {
  G <- .make_test_game()
  ghost <- 1
  expect_error(
    replace_field(G, State(ghost)),
    "do not exist"
  )
})

test_that("replace_field() returns G invisibly", {
  G <- .make_test_game()
  pop <- 777
  result <- withVisible(replace_field(G, State(pop)))
  expect_false(result$visible)
  expect_identical(result$value, G)
})
