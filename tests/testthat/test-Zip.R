#-------------------------------------------------------------------------------
# Tests for the ABM_Zip class: ABM_Zip() (internal), Zip(), Unzip()
#-------------------------------------------------------------------------------

# ==============================================================================
# ABM_Zip() -- internal constructor
# ==============================================================================

test_that("ABM_Zip() stores its arguments in a list with class 'ABM_Zip'", {
  z <- ABM_Zip(1, 2, 3)

  expect_s3_class(z, "ABM_Zip")
  expect_type(unclass(z), "list")
  expect_length(z, 3)
  expect_identical(unclass(z), list(1, 2, 3))
})

test_that("ABM_Zip() with no arguments returns an empty, but still classed, object", {
  z <- ABM_Zip()

  expect_s3_class(z, "ABM_Zip")
  expect_length(z, 0)
  expect_identical(unclass(z), list())
})

test_that("ABM_Zip() accepts any object type without validation", {
  z <- ABM_Zip(1, "a", TRUE, list(x = 1), function() 1, NULL)

  expect_s3_class(z, "ABM_Zip")
  expect_length(z, 6)
})

test_that("ABM_Zip() preserves names of named arguments at the container level", {
  z <- ABM_Zip(a = 1, b = 2)

  expect_identical(names(unclass(z)), c("a", "b"))
})

test_that("ABM_Zip() only sets the class attribute, and does not mutate values", {
  x <- list(1, 2)
  z <- ABM_Zip(x)

  expect_identical(z[[1]], x)
})

# ==============================================================================
# Zip() -- user-facing wrapper
# ==============================================================================

test_that("Zip() is a thin wrapper that behaves identically to ABM_Zip()", {
  z1 <- Zip(1, 2, 3)
  z2 <- ABM_Zip(1, 2, 3)

  expect_identical(z1, z2)
})

test_that("Zip() bundles ABM_Field objects into an ABM_Zip container", {
  a <- 1
  b <- 2
  z <- Zip(State(a), State(b))

  expect_s3_class(z, "ABM_Zip")
  expect_length(z, 2)
  expect_s3_class(z[[1]], "ABM_Field")
  expect_s3_class(z[[2]], "ABM_Field")
  expect_identical(z[[1]]$name, "a")
  expect_identical(z[[2]]$name, "b")
})

test_that("Zip() with no arguments returns an empty ABM_Zip", {
  z <- Zip()

  expect_s3_class(z, "ABM_Zip")
  expect_length(z, 0)
})

test_that("Zip() accepts arbitrary, non-ABM_Field objects", {
  z <- Zip(1, "a", list(x = 1))

  expect_s3_class(z, "ABM_Zip")
  expect_length(z, 3)
  expect_identical(z[[1]], 1)
  expect_identical(z[[2]], "a")
  expect_identical(z[[3]], list(x = 1))
})

test_that("Zip() can nest other ABM_Zip objects without flattening them", {
  inner <- Zip(1, 2)
  outer <- Zip(inner, 3)

  expect_s3_class(outer, "ABM_Zip")
  expect_length(outer, 2)
  expect_s3_class(outer[[1]], "ABM_Zip")   # not auto-flattened at Zip() time
  expect_identical(outer[[2]], 3)
})

# ==============================================================================
# Unzip() -- basic flattening
# ==============================================================================

test_that("Unzip() flattens plain (non-Zip) arguments as a simple list", {
  a <- 1
  b <- 2
  out <- Unzip(State(a), State(b))

  expect_type(out, "list")
  expect_false(inherits(out, "ABM_Zip"))
  expect_length(out, 2)
  expect_identical(out[[1]]$name, "a")
  expect_identical(out[[2]]$name, "b")
})

test_that("Unzip() with a single non-Zip argument returns it wrapped in a one-element list", {
  out <- Unzip(5)

  expect_length(out, 1)
  expect_identical(out[[1]], 5)
})

test_that("Unzip() with no arguments returns an empty list", {
  out <- Unzip()

  expect_identical(out, list())
})

test_that("Unzip() expands a single ABM_Zip into its constituent elements", {
  a <- 1
  b <- 2
  z <- Zip(State(a), State(b))
  out <- Unzip(z)

  expect_length(out, 2)
  expect_identical(out[[1]]$name, "a")
  expect_identical(out[[2]]$name, "b")
})

test_that("Unzip() mixes flat args and Zip args, preserving call order", {
  a <- 1
  b <- 2
  z <- Zip(State(a), State(b))
  out <- Unzip(State(a), State(b), z)

  expect_length(out, 4)
  expect_identical(vapply(out, function(f) f$name, character(1)), c("a", "b", "a", "b"))
})

test_that("Unzip() preserves the relative order across multiple Zip and non-Zip args", {
  a <- 1
  b <- 2
  z1 <- Zip(State(a))
  z2 <- Zip(State(b))
  out <- Unzip(z1, State(a), z2)

  expect_length(out, 3)
  expect_identical(vapply(out, function(f) f$name, character(1)), c("a", "a", "b"))
})

# ==============================================================================
# Unzip() -- nested ABM_Zip objects
# ==============================================================================

test_that("Unzip() recursively flattens one level of nested ABM_Zip", {
  a <- 1
  b <- 2
  p <- Zip(State(a), State(b))
  q <- Zip(p, State(a))
  out <- Unzip(q)

  expect_length(out, 3)
  expect_identical(vapply(out, function(f) f$name, character(1)), c("a", "b", "a"))
})

test_that("Unzip() recursively flattens multiple levels of nesting", {
  a <- 1
  inner  <- Zip(State(a))          # 1 element
  middle <- Zip(inner, State(a))   # inner(1) + a(1) = 2 elements
  outer  <- Zip(middle, inner)     # middle(2) + inner(1) = 3 elements
  out <- Unzip(outer)

  expect_length(out, 3)
})

test_that("Unzip() correctly flattens a Zip containing only other Zips (no leaves at top level)", {
  a <- 1
  b <- 2
  z <- Zip(Zip(State(a)), Zip(State(b)))
  out <- Unzip(z)

  expect_length(out, 2)
  expect_identical(vapply(out, function(f) f$name, character(1)), c("a", "b"))
})

test_that("Unzip() handles deep nesting without error", {
  x <- 1
  z <- Zip(State(x))
  for (i in 1:20) z <- Zip(z)  # wrap 20 levels deep

  out <- Unzip(z)

  expect_length(out, 1)
  expect_s3_class(out[[1]], "ABM_Field")
})

# ==============================================================================
# Unzip() -- edge cases
# ==============================================================================

test_that("Unzip() preserves NULL as a genuine element rather than dropping it", {
  out <- Unzip(NULL)

  expect_length(out, 1)
  expect_null(out[[1]])
})

test_that("Unzip() preserves NULL alongside other elements, keeping order and count", {
  a <- 1
  out <- Unzip(NULL, State(a), NULL)

  expect_length(out, 3)
  expect_null(out[[1]])
  expect_s3_class(out[[2]], "ABM_Field")
  expect_null(out[[3]])
})

test_that("Unzip() treats an empty ABM_Zip as contributing zero elements", {
  empty_zip <- Zip()
  a <- 1
  out <- Unzip(State(a), empty_zip, State(a))

  expect_length(out, 2)
})

test_that("Unzip() of only empty Zips returns an empty list", {
  out <- Unzip(Zip(), Zip(Zip()))

  expect_identical(out, list())
})

test_that("Unzip() does not expand objects that merely resemble a Zip but lack the class", {
  fake_zip <- list(1, 2)  # no "ABM_Zip" class
  out <- Unzip(fake_zip)

  expect_length(out, 1)
  expect_identical(out[[1]], fake_zip)
})

test_that("Unzip() drops names from named top-level arguments, keeping only values", {
  a <- 1
  b <- 2
  out <- Unzip(x = State(a), y = State(b))

  expect_null(names(out))
  expect_length(out, 2)
  expect_identical(out[[1]]$name, "a")
  expect_identical(out[[2]]$name, "b")
})

test_that("Unzip() drops names carried inside an ABM_Zip container", {
  a <- 1
  b <- 2
  z <- Zip(x = State(a), y = State(b))
  out <- Unzip(z)

  expect_null(names(out))
  expect_length(out, 2)
})

test_that("Unzip() works on non-ABM_Field payloads (no dependency on Field internals)", {
  out <- Unzip(1, Zip(2, 3), "a")

  expect_identical(out, list(1, 2, 3, "a"))
})

# ==============================================================================
# Round-trip consistency: Zip() then Unzip()
# ==============================================================================

test_that("Unzip(Zip(...)) round-trips to the original flat arguments", {
  a <- 1
  b <- 2
  original <- list(State(a), State(b))

  out <- Unzip(do.call(Zip, original))

  expect_identical(out, original)
})

test_that("Zip(Unzip(...)) re-wraps a flattened list back into a single ABM_Zip", {
  a <- 1
  b <- 2
  flat <- Unzip(Zip(State(a)), State(b))

  rewrapped <- do.call(Zip, flat)

  expect_s3_class(rewrapped, "ABM_Zip")
  expect_length(rewrapped, 2)
  expect_identical(Unzip(rewrapped), flat)
})
