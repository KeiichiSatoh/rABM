test_that("copy_G creates a deep copy of an ABM_G object", {
  G1 <- setABM(stage = 1)
  G2 <- copy_G(G1)
  # check if G1 and G2 are identical
  expect_equal(G2$stage, G1$stage)
  # Yet they have different reference（deep copy）
  G2$stage <- 2
  expect_false(identical(G1$stage, G2$stage))

  # check class
  expect_s3_class(G2, "ABM_G")
})
