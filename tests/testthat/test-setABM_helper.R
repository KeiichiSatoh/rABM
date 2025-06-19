test_that(".shape_G_FUN works for a function", {
  test_FUN <- function(){print(1)}
  test_FUN_sbs <- substitute(test_FUN)
  out <- .shape_G_FUN(FUN = test_FUN, FUN_sbs = test_FUN_sbs, FUN_category = "global_FUN")
  expect_true(out$category=="global_FUN")
})
