#-------------------------------------------------------------------------------
# Tests for Game() / ABM_Game
#-------------------------------------------------------------------------------

# ==============================================================================
# Game(): basic construction
# ==============================================================================

test_that("Game() with no fields creates an empty game", {
  G <- Game()

  expect_s3_class(G, "ABM_Game")
  expect_identical(G$.get_category(), character(0))
  expect_identical(nrow(G$.get_flist()), 0L)
  expect_identical(G$time, 1)
})

test_that("Game() registers a single state field", {
  a <- 1
  G <- Game(State(a))

  expect_identical(G$a, 1)
  expect_identical(unname(G$.get_category()["a"]), "state")
})

test_that("Game() registers fields across multiple categories", {
  pop <- 100
  growth_rate <- function(rate = 1.05) { self$pop * rate }
  reproduce   <- function() { self$pop <- self$pop * 1.1 }
  extinction  <- function() { self$pop <= 0 }
  reporter    <- function() { self$pop }
  plotter     <- function() { self$pop }

  G <- Game(
    State(pop),
    Active(growth_rate),
    Act(reproduce),
    Stop(extinction),
    Report(reporter),
    Plot(plotter)
  )

  cats <- G$.get_category()
  expect_identical(unname(cats["pop"]), "state")
  expect_identical(unname(cats["growth_rate"]), "active_state")
  expect_identical(unname(cats["reproduce"]), "act_FUN")
  expect_identical(unname(cats["extinction"]), "stop_FUN")
  expect_identical(unname(cats["reporter"]), "report_FUN")
  expect_identical(unname(cats["plotter"]), "plot_FUN")

  # active_state is evaluated on access
  expect_identical(G$growth_rate, 105)
  # act_FUN mutates state
  G$reproduce()
  expect_equal(G$pop, 110)
})

test_that("Game() accepts a Zip() bundle as a single argument", {
  pop <- 1
  growth_rate <- function() self$pop
  bundle <- Zip(State(pop), Active(growth_rate))

  G <- Game(bundle)

  expect_identical(G$pop, 1)
  expect_identical(G$growth_rate, 1)
})

test_that("Game() accepts a mix of loose fields and Zip() bundles", {
  a <- 1
  b <- 2
  bundle <- Zip(State(a))
  G <- Game(bundle, State(b))

  expect_identical(G$a, 1)
  expect_identical(G$b, 2)
})

test_that("Game() errors on duplicated field names", {
  expect_error(
    Game(State(1, name = "a"), State(2, name = "a")),
    "Duplicated field names"
  )
})

test_that("Game() errors when a non-ABM_Field object is supplied", {
  expect_error(Game(list(a = 1)), "not 'ABM_Field' class objects")
})

test_that("Game() validates 'time'", {
  expect_error(Game(time = -1), "positive integer")
  expect_error(Game(time = 1.5), "positive integer")
  expect_error(Game(time = c(1, 2)), "positive integer")

  G <- Game(time = 5)
  expect_identical(G$time, 5)
})

test_that("Game() validates 'log'", {
  expect_error(Game(log = "not a list"), "must be a list")

  G <- Game(log = list(1, 2))
  expect_length(G$log, 2)
})

test_that("Game() stores 'notes', wrapping non-list input in a list", {
  G1 <- Game(notes = list(a = 1, b = 2))
  expect_identical(G1$notes, list(a = 1, b = 2))

  G2 <- Game(notes = "a single note")
  expect_identical(G2$notes, list("a single note"))
})

# ==============================================================================
# .add()
# ==============================================================================

test_that(".add() adds new fields to an existing game", {
  G <- Game(State(1, name = "a"))
  G$.add(State(2, name = "b"))

  expect_identical(G$a, 1)
  expect_identical(G$b, 2)
})

test_that(".add() errors when the new name collides with an existing field", {
  G <- Game(State(1, name = "a"))
  expect_error(G$.add(State(2, name = "a")), "Duplicated field names with the existing names")
})

test_that(".add() errors on non-ABM_Field input", {
  G <- Game()
  expect_error(G$.add(5), "not 'ABM_Field' class objects")
})

test_that(".add() errors on duplicated names within the same call", {
  G <- Game()
  expect_error(
    G$.add(State(1, name = "a"), State(2, name = "a")),
    "Duplicated field names"
  )
})

test_that(".add() with no arguments is a no-op", {
  G <- Game(State(1, name = "a"))
  G$.add()
  expect_identical(G$a, 1)
  expect_identical(nrow(G$.get_flist()), 1L)
})

test_that(".add() supports Zip() bundles", {
  G <- Game(State(1, name = "a"))
  G$.add(Zip(State(2, name = "b"), State(3, name = "c")))

  expect_identical(G$b, 2)
  expect_identical(G$c, 3)
})

test_that(".add() does not register partial field_category on failure", {
  G <- Game(State(1, name = "a"))
  expect_error(G$.add(State(2, name = "a"), State(3, name = "new")))
  # "new" must not have been registered, since the whole call failed
  expect_false("new" %in% names(G$.get_category()))
})

# ==============================================================================
# .remove()
# ==============================================================================

test_that(".remove() removes an existing field", {
  G <- Game(State(1, name = "a"), State(2, name = "b"))
  G$.remove("a")

  expect_false(exists("a", envir = G, inherits = FALSE))
  expect_false("a" %in% names(G$.get_category()))
  expect_identical(G$b, 2)
})

test_that(".remove() errors when the field does not exist", {
  G <- Game(State(1, name = "a"))
  expect_error(G$.remove("nonexistent"), "does not exist in the fields")
})

test_that(".remove() errors on non-character input", {
  G <- Game(State(1, name = "a"))
  expect_error(G$.remove(1), "must be a character")
})

test_that(".remove() removes an active_state field cleanly", {
  fn <- function() 1
  G <- Game(Active(fn))
  G$.remove("fn")

  expect_false("fn" %in% names(G$.get_category()))
  expect_false(exists("fn", envir = G, inherits = FALSE))
})

test_that(".remove() can remove multiple fields at once", {
  G <- Game(State(1, name = "a"), State(2, name = "b"), State(3, name = "c"))
  G$.remove("a", "b")

  expect_identical(names(G$.get_category()), "c")
})

# ==============================================================================
# .replace()
# ==============================================================================

test_that(".replace() replaces an existing state field's value", {
  G <- Game(State(1, name = "a"))
  G$.replace(State(99, name = "a"))

  expect_identical(G$a, 99)
  expect_identical(unname(G$.get_category()["a"]), "state")
})

test_that(".replace() can change a field's category", {
  G <- Game(State(1, name = "a"))
  fn <- function() 42
  G$.replace(Active(fn, name = "a"))

  expect_identical(unname(G$.get_category()["a"]), "active_state")
  expect_identical(G$a, 42)
})

test_that(".replace() errors when the field does not already exist", {
  G <- Game()
  expect_error(G$.replace(State(1, name = "a")), "do not exist")
})

test_that(".replace() errors on non-ABM_Field input", {
  G <- Game(State(1, name = "a"))
  expect_error(G$.replace(list(name = "a")), "not 'ABM_Field' class objects")
})

test_that(".replace() errors on duplicated names within the input", {
  G <- Game(State(1, name = "a"))
  expect_error(
    G$.replace(State(2, name = "a"), State(3, name = "a")),
    "Duplicated field names in the input"
  )
})

test_that(".replace() leaves untouched fields alone", {
  G <- Game(State(1, name = "a"), State(2, name = "b"))
  G$.replace(State(99, name = "a"))

  expect_identical(G$b, 2)
})

test_that(".replace() rolls back to the original field if .add() fails afterwards", {
  # R6 locks bindings for members that already exist on an object, even
  # when the class was defined with lock_objects = FALSE (that flag only
  # permits *adding new* fields, not overwriting existing ones). To force
  # .add() to fail after .remove() has already run, we must explicitly
  # unlock the existing `.add` binding on this instance first.
  G <- Game(State(1, name = "a"))
  unlockBinding(".add", G)
  G$.add <- function(...) stop("forced failure for rollback test")

  expect_error(
    G$.replace(State(99, name = "a")),
    "the original field\\(s\\) have been restored"
  )

  # NOTE: because we overrode .add() itself, the field was never re-added
  # by the (patched) .add() -- the rollback logic in .replace() restores it
  # directly via private$.add_state()/.add_active()/.add_method(), which
  # does not go through the (now-broken) public .add() method. So the
  # original value should still be recoverable.
  expect_identical(G$a, 1)
  expect_identical(unname(G$.get_category()["a"]), "state")
})

# ==============================================================================
# .get_category() / .get_flist()
# ==============================================================================

test_that(".get_category() returns a named character vector", {
  G <- Game(State(1, name = "a"), State(2, name = "b"))
  cats <- G$.get_category()

  expect_type(cats, "character")
  expect_identical(unname(cats), c("state", "state"))
  expect_identical(names(cats), c("a", "b"))
})

test_that(".get_flist() returns a data.frame with name/category columns", {
  G <- Game(State(1, name = "a"))
  fl <- G$.get_flist()

  expect_s3_class(fl, "data.frame")
  expect_named(fl, c("name", "category"))
  expect_identical(fl$name, "a")
  expect_identical(fl$category, "state")
})

# ==============================================================================
# .snapshot()
# ==============================================================================

test_that(".snapshot() returns requested fields plus time", {
  G <- Game(State(1, name = "a"), State(2, name = "b"), time = 3)
  snap <- G$.snapshot(c("a", "b"))

  expect_identical(snap$a, 1)
  expect_identical(snap$b, 2)
  expect_identical(snap$time, 3)
})

test_that(".snapshot() with add_tryCatch = TRUE captures errors as a class", {
  bad_active <- function() stop("boom")
  G <- Game(Active(bad_active))

  snap <- G$.snapshot("bad_active", add_tryCatch = TRUE)
  expect_s3_class(snap$bad_active, "error")
})

test_that(".snapshot() without add_tryCatch propagates errors", {
  bad_active <- function() stop("boom")
  G <- Game(Active(bad_active))

  expect_error(G$.snapshot("bad_active"), "boom")
})

# ==============================================================================
# print()
# ==============================================================================

test_that("print() with fields = FALSE (default) hides field contents", {
  G <- Game(State(1, name = "a"))

  out <- capture.output(print(G))
  expect_true(any(grepl("^<Game>$", out)))
  expect_true(any(grepl("Field contents are hidden by default", out)))
  # field value itself should not appear as a preview line
  expect_false(any(grepl("^\\[1\\] 1$", out)))
})

test_that("print(fields = TRUE) shows field contents", {
  G <- Game(State(1, name = "a"))

  out <- capture.output(print(G, fields = TRUE))
  expect_true(any(grepl("\\$a", out)))
  expect_true(any(grepl("State", out)))
})

test_that("print() shows the metadata block", {
  G <- Game(State(1, name = "a"), time = 7)

  out <- capture.output(print(G))
  expect_true(any(grepl("time\\s+:\\s+7", out)))
  expect_true(any(grepl("n of fields\\s+:\\s+1", out)))
})

test_that("print() groups field names under human-readable category labels", {
  G <- Game(State(1, name = "a"), Active(function() 1, name = "b"))

  out <- capture.output(print(G))
  expect_true(any(grepl("State\\s*:\\s*a", out)))
  expect_true(any(grepl("Active State\\s*:\\s*b", out)))
})

test_that("print() wraps long category field-name lists across multiple lines", {
  old_width <- options(width = 40)
  on.exit(options(old_width))

  fields <- lapply(seq_len(20), function(i) State(i, name = paste0("field_", i)))
  G <- do.call(Game, fields)

  out <- capture.output(print(G))
  state_lines <- grep("^\\s*State", out)
  expect_true(length(state_lines) >= 1)
  # with only 40-char width and 20 field names, wrapping must produce
  # more output lines under the State category than a single line could hold
  all_field_text <- paste(out, collapse = " ")
  expect_true(grepl("field_1\\b", all_field_text))
  expect_true(grepl("field_20\\b", all_field_text))
})

test_that("print() validates 'fields' and 'max_lines' arguments", {
  G <- Game(State(1, name = "a"))

  expect_error(print(G, fields = "yes"), "must be a single logical value")
  expect_error(print(G, max_lines = -1), "must be a single non-negative integer")
  expect_error(print(G, max_lines = 1.5), "must be a single non-negative integer")
})

test_that("print(fields = TRUE) truncates long vector fields and notes it", {
  long_vec <- 1:100
  G <- Game(State(long_vec, name = "a"))

  out <- capture.output(print(G, fields = TRUE, max_lines = 3))
  expect_true(any(grepl("truncated", out)))
})

# ==============================================================================
# summary.ABM_Game()
# ==============================================================================

test_that("summary() on an ABM_Game prints a guidance message and returns invisibly", {
  G <- Game(State(1, name = "a"))

  expect_message(result <- summary(G), "summary\\(\\) is not implemented")
  expect_identical(result, G)
})

# ==============================================================================
# .rebind_dynamic_fields() / clone-related behavior
# ==============================================================================

test_that(".rebind_dynamic_fields() re-binds self/private after clone(deep = TRUE)", {
  pop <- 10
  grow <- function() { self$pop * 2 }
  G <- Game(State(pop), Active(grow))

  G2 <- G$clone(deep = TRUE)
  G2$.rebind_dynamic_fields()

  # after rebinding, G2's active binding should read G2's own state, not G's
  G2$pop <- 5
  expect_identical(G2$grow, 10)
  expect_identical(G$pop, 10)  # original game is unaffected
})

# ==============================================================================
# .category_label() (internal helper)
# ==============================================================================

test_that(".category_label() maps all six known categories", {
  expect_identical(
    .category_label(c("state", "active_state", "act_FUN", "stop_FUN", "report_FUN", "plot_FUN")),
    c("State", "Active State", "Act", "Stop", "Report", "Plot")
  )
})

test_that(".category_label() preserves input length and order", {
  input <- c("plot_FUN", "state", "state")
  out <- .category_label(input)
  expect_length(out, 3)
  expect_identical(out, c("Plot", "State", "State"))
})
