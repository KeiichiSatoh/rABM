#-------------------------------------------------------------------------------
# Tests for Field constructors: State(), Active(), Act(), Stop(), Report(), Plot()
#-------------------------------------------------------------------------------

# ==============================================================================
# State()
# ==============================================================================

test_that("State() infers name from object symbol", {
  y <- 1
  f <- State(y)

  expect_s3_class(f, "ABM_Field")
  expect_identical(f$name, "y")
  expect_identical(f$value, 1)
  expect_identical(f$category, "state")
})

test_that("State() accepts an explicit name, including for literals", {
  f <- State(1, name = "y")

  expect_identical(f$name, "y")
  expect_identical(f$value, 1)
  expect_identical(f$category, "state")
})

test_that("State() explicit name overrides symbol-based inference", {
  y <- 1
  f <- State(y, name = "custom_name")

  expect_identical(f$name, "custom_name")
})

test_that("State() rejects function input", {
  fn <- function() 1
  expect_error(State(fn), "must not be a function")
})

test_that("State() rejects a call expression when name is NULL", {
  fun <- function(a = 1) a
  expect_error(State(fun(a = 3)), "must not be a call for State\\(\\)")
})

test_that("State() allows a call-like value when name is explicitly provided", {
  # NOTE: this does not "coerce" a function -- x is evaluated normally.
  # A numeric expression is fine; the point is that call *inference* is
  # bypassed once name is supplied.
  f <- State(1 + 1, name = "sum_result")
  expect_identical(f$value, 2)
  expect_identical(f$name, "sum_result")
})

test_that("State() errors when x is a bare literal and name is NULL", {
  expect_error(State(1), "must be an object name")
})

# ==============================================================================
# Active() / Act() / Stop() / Report() / Plot(): shared behavior
# ==============================================================================

# Run the same battery of checks across all five function-category
# constructors, since they share .make_FUN_field() internally.
fn_constructors <- list(
  Active = Active,
  Act    = Act,
  Stop   = Stop,
  Report = Report,
  Plot   = Plot
)

expected_categories <- c(
  Active = "active_state",
  Act    = "act_FUN",
  Stop   = "stop_FUN",
  Report = "report_FUN",
  Plot   = "plot_FUN"
)

for (ctor_name in names(fn_constructors)) {
  ctor <- fn_constructors[[ctor_name]]
  expected_category <- expected_categories[[ctor_name]]

  test_that(sprintf("%s() infers name from function symbol", ctor_name), {
    fun <- function(a = 1, b = 2) c(a, b)
    f <- ctor(fun)

    expect_s3_class(f, "ABM_Field")
    expect_identical(f$name, "fun")
    expect_identical(f$category, expected_category)
    expect_true(is.function(f$value))
    expect_identical(f$value(), c(1, 2))
  })

  test_that(sprintf("%s() accepts an explicit name", ctor_name), {
    fun <- function() 1
    f <- ctor(fun, name = "my_field")

    expect_identical(f$name, "my_field")
  })

  test_that(sprintf("%s() rejects non-function input", ctor_name), {
    # name is supplied explicitly so that name-resolution doesn't fail
    # first; this isolates the must_be_function check specifically.
    expect_error(ctor(5, name = "foo"), "must be a function")
  })

  # A function literal (e.g. `function() 1`) parses as a call whose head is
  # the symbol `function`, distinct from an invocation call like `fun(a=3)`.
  # .make_FUN_field() must treat it as a plain function value rather than
  # routing it through .coerce_call_to_FUN(); see fix applied to
  # class_ABM_Field.R (is_fn_literal check).

  test_that(sprintf("%s() rejects a bare anonymous function when name is NULL", ctor_name), {
    expect_error(ctor(function() 1), "must be an object name")
  })

  test_that(sprintf("%s() accepts an anonymous function with explicit name", ctor_name), {
    f <- ctor(function() 1, name = "anon_field")

    expect_identical(f$name, "anon_field")
    expect_identical(f$value(), 1)
  })

  test_that(sprintf("%s() call-input updates defaults without evaluating the call", ctor_name), {
    fun <- function(a = 1, b = 2) c(a, b)
    f <- ctor(fun(a = 3))

    expect_identical(f$name, "fun")
    expect_identical(f$category, expected_category)
    expect_identical(formals(f$value)$a, 3)
    expect_identical(formals(f$value)$b, 2)
    # defaults were updated, but calling still requires an explicit call
    expect_identical(f$value(), c(3, 2))
  })

  test_that(sprintf("%s() call-input infers name from the call head when name is NULL", ctor_name), {
    fun <- function(a = 1) a
    f <- ctor(fun(a = 9))

    expect_identical(f$name, "fun")
  })

  test_that(sprintf("%s() call-input respects an explicit name over the call head", ctor_name), {
    fun <- function(a = 1) a
    f <- ctor(fun(a = 9), name = "custom")

    expect_identical(f$name, "custom")
  })

  test_that(sprintf("%s() call-input errors if an unknown argument is supplied", ctor_name), {
    fun <- function(a = 1) a
    expect_error(ctor(fun(z = 9)), "unused argument")
  })

  test_that(sprintf("%s() preserves the function's original environment", ctor_name), {
    make_fun <- function() {
      local_val <- 42
      function() local_val
    }
    fun <- make_fun()
    f <- ctor(fun, name = "closure_field")

    expect_identical(f$value(), 42)
  })
}

# ==============================================================================
# Active(): read/write (missing()) semantics with call-input
# ==============================================================================

test_that("Active() call-input default affects a non-read/write argument", {
  multiply <- function(b = 2) b * 2
  f <- Active(multiply(b = 10))

  expect_identical(formals(f$value)$b, 10)
  expect_identical(f$value(), 20)
})

test_that("Active() call-input default on a missing()-based read/write argument has no effect", {
  # Mimics the read/write pattern used inside ABM_Game active bindings.
  mixed_fn <- function(v, mult = 3) {
    if (missing(v)) {
      "read-branch"
    } else {
      "write-branch"
    }
  }

  f <- Active(mixed_fn(v = 999))

  # The call-input still updates the formal default...
  expect_identical(formals(f$value)$v, 999)

  # ...but missing() only reflects whether the *caller* supplied v,
  # not whether a default exists, so a zero-arg call still hits the
  # read branch rather than being short-circuited by the injected default.
  expect_identical(f$value(), "read-branch")
  expect_identical(f$value(v = 1), "write-branch")
})

# ==============================================================================
# .validate_name1() (exercised indirectly through the constructors)
# ==============================================================================

test_that("empty name is rejected", {
  expect_error(State(1, name = ""), "must not be empty")
  expect_error(State(1, name = "   "), "must not be empty")
})

test_that("non-character name is rejected", {
  expect_error(State(1, name = 1), "must be a single character string")
  expect_error(State(1, name = NA_character_), "must be a single character string")
  expect_error(State(1, name = c("a", "b")), "must be a single character string")
})

test_that("name is trimmed of surrounding whitespace", {
  f <- State(1, name = "  y  ")
  expect_identical(f$name, "y")
})

# ==============================================================================
# ABM_Field() (direct construction, exercised via Field-producing constructors)
# ==============================================================================

test_that("ABM_Field objects carry exactly value/name/category", {
  f <- State(1, name = "y")
  expect_named(f, c("value", "name", "category"))
})

test_that("category is restricted to the six known labels", {
  # Field() itself has been removed; this checks the constraint still holds
  # for the internal constructor used by all public helpers.
  expect_error(
    ABM_Field(x = 1, name = "y", category = "not_a_real_category"),
    "should be one of"
  )
})
