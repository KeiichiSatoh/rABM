test_that("setABM: setting a single 'global_FUN' works.", {
  test_FUN <- function(){print(1)}
  G <- setABM(agents = 1, global_FUN = test_FUN)
  expect_true(is.function(G$test_FUN))
  expect_true(body(G$test_FUN)==body(test_FUN))
})

test_that("setABM: setting two 'global_FUN's works.", {
  test_FUN1 <- function(){print(1)}
  test_FUN2 <- function(){print(2)}
  G <- setABM(agents = 1, global_FUN = list(test_FUN1 = test_FUN1, test_FUN2 = test_FUN2))
  expect_true(all(c("test_FUN1", "test_FUN2") %in% ls(G)))
})

test_that("setABM: setting a unmaked 'global_FUN's works.", {
  G <- setABM(agents = 1, global_FUN = function(){print(1)})
  expect_true("global_FUN" %in% ls(G))
})


test_that("setABM: collectly setting each FUN categories.", {
  G <- setABM(global_FUN = function(){},
              select_FUN = function(){},
              stop_FUN = function(){},
              update_FUN = function(){},
              summary_FUN = function(){},
              plot_FUN = function(){})
  expect_true(
    all(c("global_FUN","select_FUN","stop_FUN","update_FUN","summary_FUN","plot_FUN") %in%
          G$.__enclos_env__$private$field_category)
  )
})
