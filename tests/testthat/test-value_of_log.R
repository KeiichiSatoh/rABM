## test-value_of_log.R
##
## Tests for value_of_log() and its methods:
##   value_of_log.default
##   value_of_log.ABM_Game
##
## Mirrors the structure of test-value_of.R. Fixtures use the real ABM_Game
## R6 class and the real State() field constructor; G$log is populated
## directly via ABM_Game$new(..., log = list(...)) rather than via
## run_Game(), since run_Game()'s source isn't needed to exercise
## value_of_log() itself.

# ---------------------------------------------------------------------------
# Helpers / fixtures
# ---------------------------------------------------------------------------

# A game with a "state" field plus a populated, named $log of 3 entries,
# each holding a value for "agent_wealth" (and, for one entry, an extra
# field "agent_id" that is deliberately *not* present in the others, for
# testing missing-field errors).
make_game_with_log <- function(log = list(
                                  t1 = list(agent_wealth = c(1, 2, 3)),
                                  t2 = list(agent_wealth = c(2, 3, 4)),
                                  t3 = list(agent_wealth = c(3, 4, 5))
                                )) {
  agent_wealth <- c(10, 20, 30)
  ABM_Game$new(
    State(agent_wealth),
    log = log
  )
}

# A game with no log at all (the default).
make_game_without_log <- function() {
  agent_wealth <- c(10, 20, 30)
  ABM_Game$new(State(agent_wealth))
}

# ---------------------------------------------------------------------------
# value_of_log() generic / value_of_log.default
# ---------------------------------------------------------------------------

test_that("value_of_log() dispatches via S3 and errors for unsupported classes", {
  obj <- structure(list(a = 1), class = "some_unsupported_class")

  expect_error(
    value_of_log(obj, "a"),
    "not implemented for objects of class"
  )
})

test_that("value_of_log.default() reports the offending class(es) in the error", {
  obj <- structure(list(), class = c("foo", "bar"))
  expect_error(value_of_log(obj, "a"), "foo/bar")
})

# ---------------------------------------------------------------------------
# value_of_log.ABM_Game -- log = "all" (default)
# ---------------------------------------------------------------------------

test_that("value_of_log.ABM_Game() with log='all' returns a named list of all entries", {
  G <- make_game_with_log()
  result <- value_of_log(G, "agent_wealth")
  expect_type(result, "list")
  expect_named(result, c("t1", "t2", "t3"))
  expect_equal(
    result,
    list(t1 = c(1, 2, 3), t2 = c(2, 3, 4), t3 = c(3, 4, 5))
  )
})

test_that("value_of_log.ABM_Game() with log='all' still returns a list for a single-entry log", {
  G <- make_game_with_log(log = list(t1 = list(agent_wealth = c(9, 9, 9))))
  result <- value_of_log(G, "agent_wealth")
  expect_type(result, "list")
  expect_named(result, "t1")
  expect_equal(result, list(t1 = c(9, 9, 9)))
})

test_that("value_of_log.ABM_Game() applies return_FUN elementwise when log='all'", {
  G <- make_game_with_log()
  result <- value_of_log(G, "agent_wealth", return_FUN = mean)
  expect_equal(result, list(t1 = 2, t2 = 3, t3 = 4))
})

test_that("value_of_log.ABM_Game() default 'log' argument behaves like log='all'", {
  G <- make_game_with_log()
  expect_equal(value_of_log(G, "agent_wealth"), value_of_log(G, "agent_wealth", log = "all"))
})

# ---------------------------------------------------------------------------
# value_of_log.ABM_Game -- log = single entry (name or position)
# ---------------------------------------------------------------------------

test_that("value_of_log.ABM_Game() with a single character 'log' returns the raw value", {
  G <- make_game_with_log()
  result <- value_of_log(G, "agent_wealth", log = "t2")
  expect_equal(result, c(2, 3, 4))
  expect_false(is.list(result))
})

test_that("value_of_log.ABM_Game() with a single numeric 'log' returns the raw value", {
  G <- make_game_with_log()
  result <- value_of_log(G, "agent_wealth", log = 2)
  expect_equal(result, c(2, 3, 4))
  expect_false(is.list(result))
})

test_that("value_of_log.ABM_Game() single-entry selection applies return_FUN once", {
  G <- make_game_with_log()
  result <- value_of_log(G, "agent_wealth", log = "t2", return_FUN = sum)
  expect_equal(result, 9)
})

test_that("value_of_log.ABM_Game() single-entry selection passes ... to return_FUN", {
  G <- make_game_with_log(log = list(t1 = list(agent_wealth = c(1, NA, 3))))
  result <- value_of_log(G, "agent_wealth", log = "t1", return_FUN = mean, na.rm = TRUE)
  expect_equal(result, 2)
})

test_that("value_of_log.ABM_Game() with log='all' passes ... to return_FUN for every entry", {
  G <- make_game_with_log(log = list(
    t1 = list(agent_wealth = c(1, NA, 3)),
    t2 = list(agent_wealth = c(NA, 4, 6))
  ))
  result <- value_of_log(G, "agent_wealth", return_FUN = mean, na.rm = TRUE)
  expect_equal(result, list(t1 = 2, t2 = 5))
})

# ---------------------------------------------------------------------------
# value_of_log.ABM_Game -- log = vector of length >= 2
# ---------------------------------------------------------------------------

test_that("value_of_log.ABM_Game() with a character vector 'log' returns a named list", {
  G <- make_game_with_log()
  result <- value_of_log(G, "agent_wealth", log = c("t1", "t3"))
  expect_type(result, "list")
  expect_named(result, c("t1", "t3"))
  expect_equal(result, list(t1 = c(1, 2, 3), t3 = c(3, 4, 5)))
})

test_that("value_of_log.ABM_Game() with a numeric vector 'log' returns a named list", {
  G <- make_game_with_log()
  result <- value_of_log(G, "agent_wealth", log = c(1, 3))
  expect_type(result, "list")
  expect_named(result, c("t1", "t3"))
  expect_equal(result, list(t1 = c(1, 2, 3), t3 = c(3, 4, 5)))
})

test_that("value_of_log.ABM_Game() applies return_FUN elementwise for multi-entry 'log'", {
  G <- make_game_with_log()
  result <- value_of_log(G, "agent_wealth", log = c("t1", "t2"), return_FUN = sum)
  expect_equal(result, list(t1 = 6, t2 = 9))
})

test_that("value_of_log.ABM_Game() with a multi-entry vector 'log' passes ... to return_FUN for every entry", {
  G <- make_game_with_log(log = list(
    t1 = list(agent_wealth = c(1, NA, 3)),
    t2 = list(agent_wealth = c(NA, 4, 6)),
    t3 = list(agent_wealth = c(7, 8, 9))
  ))
  result <- value_of_log(G, "agent_wealth", log = c("t1", "t2"), return_FUN = mean, na.rm = TRUE)
  expect_equal(result, list(t1 = 2, t2 = 5))
})

# ---------------------------------------------------------------------------
# value_of_log.ABM_Game -- error conditions
# ---------------------------------------------------------------------------

test_that("value_of_log.ABM_Game() errors when G$log does not exist (NULL)", {
  # This is checked explicitly by value_of_log.ABM_Game() itself, before
  # .resolve_collection_idx() is even called.
  G <- make_game_without_log()
  expect_error(
    value_of_log(G, "agent_wealth"),
    "'G\\$log' does not exist: no log entries are available\\."
  )
})

test_that("value_of_log.ABM_Game() errors when G$log is an empty list", {
  # Distinct from the NULL case above: an empty (but non-NULL) log is
  # caught by .resolve_collection_idx()'s length(x) == 0 check instead,
  # with a different message.
  G <- make_game_with_log(log = list())
  expect_error(
    value_of_log(G, "agent_wealth"),
    "'G\\$log' is empty: no entries are available\\."
  )
})

test_that("value_of_log.ABM_Game() errors when G$log has no names", {
  G <- make_game_with_log(log = list(
    list(agent_wealth = c(1, 2, 3)),
    list(agent_wealth = c(2, 3, 4))
  ))
  expect_error(
    value_of_log(G, "agent_wealth"),
    "'G\\$log' has no names: cannot label the requested entries\\."
  )
})

test_that("value_of_log.ABM_Game() errors when field_name is missing from a log entry", {
  G <- make_game_with_log(log = list(
    t1 = list(agent_wealth = c(1, 2, 3)),
    t2 = list(agent_id = c(1, 2, 3))  # no 'agent_wealth' here
  ))
  expect_error(
    value_of_log(G, "agent_wealth"),
    "Field 'agent_wealth' was not found in log entry 't2'\\."
  )
})

test_that("value_of_log.ABM_Game() errors with the entry name when a single-entry selection is missing field_name", {
  # Same underlying .extract_from_collection() error path as the 'all'
  # case above, but reached via a single explicit entry selector rather
  # than the 'all'/multi-entry loop.
  G <- make_game_with_log(log = list(
    t1 = list(agent_id = c(1, 2, 3))  # no 'agent_wealth' here
  ))
  expect_error(
    value_of_log(G, "agent_wealth", log = "t1"),
    "Field 'agent_wealth' was not found in log entry 't1'\\."
  )
})

test_that("value_of_log.ABM_Game() errors for an out-of-range numeric 'log'", {
  G <- make_game_with_log()
  expect_error(
    value_of_log(G, "agent_wealth", log = 99),
    "The following entries were not found in 'G\\$log': 99"
  )
})

test_that("value_of_log.ABM_Game() errors for a nonexistent named 'log' entry", {
  G <- make_game_with_log()
  expect_error(
    value_of_log(G, "agent_wealth", log = "does_not_exist"),
    "The following entries were not found in 'G\\$log': does_not_exist"
  )
})

test_that("value_of_log.ABM_Game() lists all unmatched names/positions when several are bad", {
  G <- make_game_with_log()
  expect_error(
    value_of_log(G, "agent_wealth", log = c("t1", "nope1", "nope2")),
    "nope1, nope2"
  )
  expect_error(
    value_of_log(G, "agent_wealth", log = c(1, 50, 51)),
    "50, 51"
  )
})

test_that("value_of_log.ABM_Game() errors when 'log' is NULL or another unsupported type", {
  # log has no default-argument validation of its own; a NULL or
  # non-character/non-numeric selector falls through to
  # .resolve_collection_idx()'s final `else` branch.
  G <- make_game_with_log()
  expect_error(
    value_of_log(G, "agent_wealth", log = NULL),
    "Selector must be \"all\" or a character/numeric vector\\."
  )
  expect_error(
    value_of_log(G, "agent_wealth", log = list(1, 2)),
    "Selector must be \"all\" or a character/numeric vector\\."
  )
  expect_error(
    value_of_log(G, "agent_wealth", log = TRUE),
    "Selector must be \"all\" or a character/numeric vector\\."
  )
})

test_that("value_of_log.ABM_Game() errors on a non-integer numeric 'log'", {
  # .resolve_collection_idx() now validates that a numeric selector
  # contains only whole numbers, rather than silently truncating (e.g.
  # 1.5 -> 1).
  G <- make_game_with_log()
  expect_error(
    value_of_log(G, "agent_wealth", log = 1.5),
    "'selector' must contain whole numbers\\."
  )
  expect_error(
    value_of_log(G, "agent_wealth", log = c(1, 2.5)),
    "'selector' must contain whole numbers\\."
  )
})

test_that("value_of_log.ABM_Game() errors when field_name is not a single string", {
  G <- make_game_with_log()
  expect_error(value_of_log(G, c("agent_wealth", "agent_id")))
  expect_error(value_of_log(G, 123))
  expect_error(value_of_log(G, NULL))
  expect_error(value_of_log(G, ""))
})

test_that("value_of_log.ABM_Game() errors when return_FUN is not a function", {
  G <- make_game_with_log()
  expect_error(value_of_log(G, "agent_wealth", return_FUN = "mean"))
})

# ---------------------------------------------------------------------------
# value_of_log.ABM_Game -- field_name need not be a currently-registered state
# ---------------------------------------------------------------------------

test_that("value_of_log.ABM_Game() can retrieve a field that only exists in the log, not in current state", {
  # field_name for value_of_log() is validated only for well-formedness
  # (.validate_field_name()), not checked against G$.get_category() -- a
  # field that was logged in the past but has since been removed from
  # current state should still be retrievable from the log.
  G <- make_game_with_log(log = list(
    t1 = list(retired_field = 100),
    t2 = list(retired_field = 200)
  ))
  expect_equal(
    value_of_log(G, "retired_field"),
    list(t1 = 100, t2 = 200)
  )
})

test_that("value_of_log.ABM_Game() does not consult current state, only G$log", {
  G <- make_game_with_log()
  # Mutate current state; the log itself is untouched, so value_of_log()
  # results should be unaffected.
  G$agent_wealth <- c(999, 999, 999)
  expect_equal(
    value_of_log(G, "agent_wealth"),
    list(t1 = c(1, 2, 3), t2 = c(2, 3, 4), t3 = c(3, 4, 5))
  )
})
