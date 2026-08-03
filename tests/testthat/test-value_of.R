## test-value_of.R
##
## Tests for value_of() and its methods:
##   value_of.default
##   value_of.ABM_Game
##   value_of.list
##
## value_of.list() gained a `simplify` argument (default FALSE): whether a
## single, explicitly-selected entry (`which` a length-1 character/numeric
## value other than "all") is unwrapped to its raw value (simplify = TRUE,
## the old/pre-refactor behavior) or still returned as a named list of
## length 1 (simplify = FALSE, the new default -- a uniform list-based
## return type regardless of how many entries were requested). `which =
## "all"` and multi-entry `which` vectors are unaffected by `simplify` and
## always return a named list, as before.
##
## These tests assume they run inside the package's testthat suite, so that:
##   - the internal R6 generator `ABM_Game`, the constructor `Game()`, and
##     the Field constructors `State()`/`Active()` are visible (used to
##     build real fixtures instead of a mock), and
##   - unexported helpers such as .validate_field_name(), .validate_return_FUN(),
##     .resolve_collection_idx(), and .extract_from_collection() are visible.
## If running standalone (outside the package namespace), prefix internal
## calls/objects with `pkgname:::`.

# ---------------------------------------------------------------------------
# Helpers / fixtures
# ---------------------------------------------------------------------------

# ABM_Game$new() (and the user-facing Game()) expect each element of `...`
# to be an `ABM_Field` object, produced by the real user-facing constructors
# State(), Active(), Act(), Stop(), Report(), Plot() (or the internal
# ABM_Field()/.make_field() they wrap). We now use these directly rather
# than hand-rolling ABM_Field objects, so both ABM_Game's field-registration
# path *and* the Field constructors' own validation/name-inference are
# exercised for real.

# A game with only "state" fields. Field names are inferred from the
# argument expressions via State()'s substitute()-based inference, matching
# ordinary usage like `State(agent_wealth)`.
make_state_game <- function(agent_wealth = c(10, 20, 30),
                             agent_id     = c(1, 2, 3)) {
  ABM_Game$new(
    State(agent_wealth),
    State(agent_id)
  )
}

# A game that also has an "active_state" field, to confirm value_of.ABM_Game
# works for computed/active fields, not just plain state. The active
# function is evaluated in an environment where `self` is bound, per
# ABM_Game's private$.add_active()/.rebind_dynamic_fields() machinery.
make_game_with_active <- function(pop = 100, rate = 1.05) {
  growth_rate <- function() self$pop * rate
  ABM_Game$new(
    State(pop),
    Active(growth_rate)
  )
}

# Sample nested list fixture mirroring the documentation example
sample_list <- list(
  t1 = list(a = 1, b = "x"),
  t2 = list(a = 2, b = "y"),
  t3 = list(a = 3, b = "z")
)

# ---------------------------------------------------------------------------
# value_of() generic / value_of.default
# ---------------------------------------------------------------------------

test_that("value_of() dispatches via S3 and errors for unsupported classes", {
  obj <- structure(list(a = 1), class = "some_unsupported_class")

  expect_error(
    value_of(obj, "a"),
    "not implemented for objects of class"
  )
})

test_that("value_of.default() reports the offending class(es) in the error", {
  obj <- structure(list(), class = c("foo", "bar"))
  expect_error(value_of(obj, "a"), "foo/bar")
})

# ---------------------------------------------------------------------------
# value_of.ABM_Game
# ---------------------------------------------------------------------------

test_that("fixtures: State()/Active() correctly infer field names via substitute()", {
  # Sanity check on the test fixtures themselves: make_state_game() relies
  # on State(agent_wealth) inferring the name "agent_wealth" (not, say,
  # some fixed/placeholder name), and make_game_with_active() relies on
  # Active(growth_rate) inferring "growth_rate".
  G1 <- make_state_game()
  expect_setequal(names(G1$.get_category()), c("agent_wealth", "agent_id"))

  G2 <- make_game_with_active()
  expect_setequal(names(G2$.get_category()), c("pop", "growth_rate"))
  expect_equal(unname(G2$.get_category()["growth_rate"]), "active_state")
})

test_that("value_of.ABM_Game() returns the current value of a state field", {
  G <- make_state_game()
  expect_equal(value_of(G, "agent_wealth"), c(10, 20, 30))
  expect_equal(value_of(G, "agent_id"), c(1, 2, 3))
})

test_that("value_of.ABM_Game() applies return_FUN when supplied", {
  G <- make_state_game()
  expect_equal(value_of(G, "agent_wealth", return_FUN = mean), 20)
  expect_equal(value_of(G, "agent_wealth", return_FUN = sum), 60)
})

test_that("value_of.ABM_Game() passes ... through to return_FUN", {
  G <- make_state_game(agent_wealth = c(10, NA, 30))
  expect_equal(
    value_of(G, "agent_wealth", return_FUN = mean, na.rm = TRUE),
    20
  )
})

test_that("value_of.ABM_Game() works for an active_state field", {
  # active_state fields are computed via an active binding bound to `self`;
  # value_of() should read the *current* computed value, just like any
  # other field.
  G <- make_game_with_active(pop = 100, rate = 1.05)
  expect_equal(value_of(G, "growth_rate"), 105)

  # And it should reflect changes to the underlying state it depends on.
  G$pop <- 200
  expect_equal(value_of(G, "growth_rate"), 210)
})

test_that("value_of.ABM_Game() reflects the current state, not a stale copy", {
  G <- make_state_game(agent_wealth = c(10, 20, 30))
  G$agent_wealth <- c(1, 2, 3)
  expect_equal(value_of(G, "agent_wealth"), c(1, 2, 3))
})

test_that("value_of.ABM_Game() warns and returns NULL for a field that does not exist on x", {
  # .validate_field_name() only checks that field_name is a well-formed
  # string; existence is checked separately via
  # exists(field_name, envir = x, inherits = FALSE). A genuinely unknown
  # field name should warn (so typos aren't silently swallowed) and still
  # return NULL.
  G <- make_state_game()
  expect_warning(
    result <- value_of(G, "not_a_real_field"),
    "does not exist"
  )
  expect_null(result)
})

test_that("value_of.ABM_Game() errors when field_name is not a single string", {
  G <- make_state_game()
  expect_error(value_of(G, c("agent_wealth", "agent_id")))
  expect_error(value_of(G, 123))
  expect_error(value_of(G, NULL))
})

test_that("value_of.ABM_Game() errors when return_FUN is not a function", {
  G <- make_state_game()
  expect_error(value_of(G, "agent_wealth", return_FUN = "mean"))
})

test_that("value_of.ABM_Game() can retrieve $log directly, with no warning, like any other field", {
  # Confirmed intended behavior: value_of() does not special-case $log (or
  # any other public field such as $notes); it simply returns x[[field_name]]
  # for whatever field_name is requested, current-state fields or not. Since
  # $log genuinely exists on x, no "does not exist" warning should fire.
  G <- make_state_game()
  G$log <- list(t1 = list(agent_wealth = c(1, 2)))
  expect_equal(value_of(G, "agent_wealth"), c(10, 20, 30))
  expect_no_warning(result <- value_of(G, "log"))
  expect_equal(result, list(t1 = list(agent_wealth = c(1, 2))))
})

test_that("value_of.ABM_Game() works when built via the Game() constructor", {
  # Smoke test using the documented user-facing constructor rather than
  # ABM_Game$new() directly, exercising Unzip()/field registration end-to-end.
  pop <- 50
  G <- Game(State(pop))
  expect_equal(value_of(G, "pop"), 50)
  expect_equal(value_of(G, "pop", return_FUN = function(v) v * 2), 100)
})

# ---------------------------------------------------------------------------
# value_of.list -- which = "all" (default)
# ---------------------------------------------------------------------------

test_that("value_of.list() with which='all' returns a named list of all entries", {
  result <- value_of(sample_list, "a")
  expect_type(result, "list")
  expect_named(result, c("t1", "t2", "t3"))
  expect_equal(result, list(t1 = 1, t2 = 2, t3 = 3))
})

test_that("value_of.list() with which='all' still returns a list for a single-entry x", {
  one_entry <- list(t1 = list(a = 42))
  result <- value_of(one_entry, "a")
  expect_type(result, "list")
  expect_named(result, "t1")
  expect_equal(result, list(t1 = 42))
})

test_that("value_of.list() applies return_FUN elementwise when which='all'", {
  result <- value_of(sample_list, "a", return_FUN = function(v) v * 10)
  expect_equal(result, list(t1 = 10, t2 = 20, t3 = 30))
})

# ---------------------------------------------------------------------------
# value_of.list -- 'simplify' argument default
# ---------------------------------------------------------------------------

test_that("value_of.list()'s 'simplify' argument defaults to FALSE", {
  expect_identical(formals(value_of.list)$simplify, FALSE)
})

# ---------------------------------------------------------------------------
# value_of.list -- which = single entry (name or position), simplify = FALSE
# (the default): always a named list of length 1
# ---------------------------------------------------------------------------

test_that("value_of.list() with a single character 'which' returns a named list of length 1 by default (simplify = FALSE)", {
  result <- value_of(sample_list, "a", which = "t2")
  expect_type(result, "list")
  expect_named(result, "t2")
  expect_equal(result, list(t2 = 2))
})

test_that("value_of.list() with a single numeric 'which' returns a named list of length 1 by default (simplify = FALSE)", {
  result <- value_of(sample_list, "a", which = 2)
  expect_type(result, "list")
  expect_named(result, "t2")
  expect_equal(result, list(t2 = 2))
})

test_that("value_of.list() single-entry selection (simplify = FALSE) applies return_FUN once, result still wrapped in a list", {
  result <- value_of(sample_list, "a", which = "t2", return_FUN = function(v) v * 100)
  expect_equal(result, list(t2 = 200))
})

test_that("value_of.list() single-entry selection (simplify = FALSE) passes ... to return_FUN", {
  entry_with_na <- list(t1 = list(a = c(1, NA, 3)))
  result <- value_of(entry_with_na, "a", which = "t1", return_FUN = mean, na.rm = TRUE)
  expect_equal(result, list(t1 = 2))
})

# ---------------------------------------------------------------------------
# value_of.list -- which = single entry (name or position), simplify = TRUE
# (opt-in unwrapping, matching the pre-refactor behavior)
# ---------------------------------------------------------------------------

test_that("value_of.list() with simplify = TRUE unwraps a single character 'which' selection to the raw value", {
  result <- value_of(sample_list, "a", which = "t2", simplify = TRUE)
  expect_equal(result, 2)
  expect_false(is.list(result))
})

test_that("value_of.list() with simplify = TRUE unwraps a single numeric 'which' selection to the raw value", {
  result <- value_of(sample_list, "a", which = 2, simplify = TRUE)
  expect_equal(result, 2)
  expect_false(is.list(result))
})

test_that("value_of.list() single-entry selection with simplify = TRUE applies return_FUN once, unwrapped", {
  result <- value_of(sample_list, "a", which = "t2", simplify = TRUE, return_FUN = function(v) v * 100)
  expect_equal(result, 200)
})

test_that("value_of.list() single-entry selection with simplify = TRUE passes ... to return_FUN", {
  entry_with_na <- list(t1 = list(a = c(1, NA, 3)))
  result <- value_of(entry_with_na, "a", which = "t1", simplify = TRUE, return_FUN = mean, na.rm = TRUE)
  expect_equal(result, 2)
})

# ---------------------------------------------------------------------------
# value_of.list -- 'simplify' has no effect on which = "all" or on a
# multi-entry 'which' vector: both always return a named list
# ---------------------------------------------------------------------------

test_that("simplify = TRUE has no effect on which = 'all'", {
  result_default   <- value_of(sample_list, "a")
  result_simplify  <- value_of(sample_list, "a", simplify = TRUE)
  expect_equal(result_default, result_simplify)
  expect_type(result_simplify, "list")
  expect_named(result_simplify, c("t1", "t2", "t3"))
})

test_that("simplify = TRUE has no effect on which = 'all' even for a single-entry x", {
  one_entry <- list(t1 = list(a = 42))
  result <- value_of(one_entry, "a", simplify = TRUE)
  expect_type(result, "list")
  expect_named(result, "t1")
  expect_equal(result, list(t1 = 42))
})

test_that("simplify = TRUE has no effect on a multi-entry 'which' vector", {
  result_default  <- value_of(sample_list, "a", which = c("t1", "t3"))
  result_simplify <- value_of(sample_list, "a", which = c("t1", "t3"), simplify = TRUE)
  expect_equal(result_default, result_simplify)
  expect_type(result_simplify, "list")
  expect_named(result_simplify, c("t1", "t3"))
})

# ---------------------------------------------------------------------------
# value_of.list -- regression: single-entry wrapping must not be decided by
# inspecting the *type* of the extracted value, since list-like values (such
# as a data.frame, for which is.list() is TRUE) would otherwise be
# indistinguishable from "already a named list of entries". The decision
# must be based on 'which' itself (its length / whether it is "all").
# ---------------------------------------------------------------------------

test_that("value_of.list() default (simplify = FALSE) correctly wraps a single entry even when the field value is itself list-like (e.g. a data.frame)", {
  df_list <- list(
    t1 = list(house = data.frame(ID = 1:3, block = c(1, 1, 2))),
    t2 = list(house = data.frame(ID = 4:6, block = c(2, 3, 3)))
  )
  result <- value_of(df_list, "house", which = "t1")
  expect_type(result, "list")
  expect_named(result, "t1")
  expect_true(is.data.frame(result[["t1"]]))
  expect_equal(result[["t1"]], data.frame(ID = 1:3, block = c(1, 1, 2)))
})

test_that("value_of.list() with simplify = TRUE unwraps a data.frame-valued single entry to the raw data.frame", {
  df_list <- list(
    t1 = list(house = data.frame(ID = 1:3, block = c(1, 1, 2)))
  )
  result <- value_of(df_list, "house", which = "t1", simplify = TRUE)
  expect_true(is.data.frame(result))
  expect_equal(result, data.frame(ID = 1:3, block = c(1, 1, 2)))
})

test_that("value_of.list() default (simplify = FALSE) supports positional list-indexing across a multi-entry selection, including data.frame-valued fields", {
  # Mirrors how report_segregation()/report_landlord_stat() in set_segGame()
  # consume value_of_log()'s output: entries pulled out via house_list[[t]].
  df_list <- list(
    t1 = list(house = data.frame(ID = 1:2, block = c(1, 2))),
    t2 = list(house = data.frame(ID = 3:4, block = c(1, 1)))
  )
  result <- value_of(df_list, "house", which = c("t1", "t2"))
  expect_type(result, "list")
  expect_length(result, 2)
  for (t in seq_along(result)) {
    expect_true(is.data.frame(result[[t]]))
  }
  expect_equal(result[[1]], data.frame(ID = 1:2, block = c(1, 2)))
  expect_equal(result[[2]], data.frame(ID = 3:4, block = c(1, 1)))
})

# ---------------------------------------------------------------------------
# value_of.list -- error conditions
# ---------------------------------------------------------------------------

test_that("value_of.list() errors when field_name is missing from an entry", {
  broken_list <- list(t1 = list(a = 1), t2 = list(b = 2))
  expect_error(
    value_of(broken_list, "a"),
    "Field 'a' was not found in entry 't2'\\."
  )
})

test_that("value_of.list() errors for an out-of-range numeric 'which'", {
  expect_error(
    value_of(sample_list, "a", which = 99),
    "The following entries were not found in 'x': 99"
  )
})

test_that("value_of.list() errors for a nonexistent named 'which'", {
  expect_error(
    value_of(sample_list, "a", which = "does_not_exist"),
    "The following entries were not found in 'x': does_not_exist"
  )
})

test_that("value_of.list() errors on a non-integer numeric 'which'", {
  # .resolve_collection_idx() validates that a numeric selector contains
  # only whole numbers, rather than silently truncating (e.g. 1.5 -> 1).
  # This validation is shared with value_of_log.ABM_Game()'s 'log' argument.
  expect_error(
    value_of(sample_list, "a", which = 1.5),
    "'selector' must contain whole numbers\\."
  )
})

test_that("value_of.list() errors when 'which' is an unsupported type", {
  expect_error(
    value_of(sample_list, "a", which = list(1, 2)),
    "Selector must be \"all\" or a character/numeric vector\\."
  )
  expect_error(
    value_of(sample_list, "a", which = TRUE),
    "Selector must be \"all\" or a character/numeric vector\\."
  )
})

test_that("value_of.list() errors when x is an empty list", {
  expect_error(
    value_of(list(), "a"),
    "'x' is empty: no entries are available\\."
  )
})

test_that("value_of.list() errors when x has no names", {
  unnamed_list <- list(list(a = 1), list(a = 2))
  expect_error(
    value_of(unnamed_list, "a"),
    "'x' has no names: cannot label the requested entries\\."
  )
})

test_that("value_of.list() errors when return_FUN is not a function", {
  expect_error(value_of(sample_list, "a", return_FUN = "not_a_function"))
})

test_that("value_of.list() treats entry names purely as labels (no implied order)", {
  # Reordering the list should not change which value maps to which label.
  reordered <- sample_list[c("t3", "t1", "t2")]
  result <- value_of(reordered, "a")
  expect_equal(result, list(t3 = 3, t1 = 1, t2 = 2))
})
