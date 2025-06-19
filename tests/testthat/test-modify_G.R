# test/testthat/test-modify_G.R

test_that("modify_G: deletes stage correctly", {
  G <- setABM(agents = 2, stage = data.frame(age = c(1,2)))
  G2 <- modify_G(G, field_name = "stage", method = "delete", deep_clone = TRUE)
  expect_false("stage" %in% ls(G2))
})

test_that("modify_G: field is deleted (without deep clone)", {
  G <- setABM(agents = 2, stage = data.frame(age = c(1,2)))
  G2 <- modify_G(G, field_name = "stage", method = "delete", deep_clone = FALSE)
  expect_null(G2)
  expect_false("stage" %in% ls(G))
})

test_that("modify_G: error when deleting nonexistent field", {
  G <- setABM(agents = 2, stage = data.frame(age = c(1,2)))
  expect_error(modify_G(G, field_name = "nonexistent", method = "delete"))
})

test_that("modify_G: add_stage works", {
  G <- setABM(agents = 2, stage = data.frame(age = c(1,2)))
  G2 <- modify_G(G, "stage2", method = "add_stage", new_obj = matrix(0,2,2))
  expect_true("stage2" %in% ls(G2))
})

test_that("modify_G: add_global_FUN works", {
  G <- setABM(agents = 2, stage = data.frame(age = c(1,2)))
  global_FUN <- function(){print(1)}
  G2 <- modify_G(G, "global_FUN_added", method = "add_global_FUN", new_obj = global_FUN)
  expect_true("global_FUN_added" %in% ls(G2))
})

test_that("modify_G: add_select_FUN works", {
  G <- setABM(agents = 2, stage = data.frame(age = c(1,2)))
  select_FUN <- function(){1}
  G2 <- modify_G(G, "select_FUN_added", method = "add_select_FUN", new_obj = select_FUN)
  expect_true("select_FUN_added" %in% ls(G2))
})

test_that("modify_G: add_stop_FUN works", {
  G <- setABM(agents = 2, stage = data.frame(age = c(1,2)))
  stop_FUN <- function(){TRUE}
  G2 <- modify_G(G, "stop_FUN_added", method = "add_stop_FUN", new_obj = stop_FUN)
  expect_true("stop_FUN_added" %in% ls(G2))
})

test_that("modify_G: add_update_FUN works", {
  G <- setABM(agents = 2, stage = data.frame(age = c(1,2)))
  update_FUN <- function(){2}
  G2 <- modify_G(G, "update_FUN_added", method = "add_update_FUN", new_obj = update_FUN)
  expect_true("update_FUN_added" %in% ls(G2))
})

test_that("modify_G: add_active_binding works", {
  G <- setABM(agents = 2, stage = data.frame(age = c(1,2)))
  active_binding <- function(){2}
  G2 <- modify_G(G, "active_binding_added", method = "add_active_binding", new_obj = active_binding)
  expect_true("active_binding_added" %in% ls(G2))
  expect_true(G2$active_binding_added==2)
})

test_that("modify_G: renames a field", {
  G <- setABM(agents = 2, stage = data.frame(age = c(1,2)))
  G2 <- modify_G(G, "stage", method = "rename", new_field_name = "stage2")
  expect_true("stage2" %in% ls(G2))
  expect_false("stage" %in% ls(G2))
})

test_that("modify_G: replaces a field.", {
  G <- setABM(agents = 2, stage = data.frame(age = c(1,2)))
  G2 <- modify_G(G, "stage", method = "replace", new_obj = matrix(0,2,2))
  expect_false(identical(G2$stage2, G$stage))
})

test_that("modify_G: works adding G and E correctly", {
  G <- setABM(agents = 2, stage = data.frame(age = c(1,2)))
  G2 <- modify_G(G, field_name = "added_global_FUN", method = "add_global_FUN",
                 new_obj = function(){print(1)},
                 deep_clone = TRUE)
  expect_true(all(names(formals(G2$added_global_FUN)) %in% c("G", "E")))
})

test_that("modify_G: Put error correctly, when adding no function to FUN", {
  G <- setABM(agents = 2, stage = data.frame(age = c(1,2)))
  expect_error(
    modify_G(G, field_name = "added_global_FUN", method = "add_global_FUN",
             new_obj = c(1),
             deep_clone = TRUE)
  )
})

test_that("modify_G: Put error correctly, when no field name exists for 'rename' method", {
  G <- setABM(agents = 2, stage = list(a = 1))
  expect_error(modify_G(G, field_name = "b", new_field_name = "c", method = "rename"))
})

test_that("modify_G: test 'add_agents' method", {
  G <- setABM(agents = 2, stage = data.frame(age = c(1,2)))
  agents_B <- init_agents(n = 2)
  G2 <- modify_G(G, field_name = "agents_B",
                 method = "add_agents",
             new_obj = agents_B,
             deep_clone = TRUE)
  expect_true("agents_B" %in% ls(G2))
})

