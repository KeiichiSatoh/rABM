
test_that("ABM_G initialization works", {
  G <- setABM()
  expect_s3_class(G, "ABM_G")
  expect_true(all(c("log", "notes", "time") %in% ls(G)))
})

test_that(".add_agents() works", {
  G <- setABM()
  agents <- init_agents(n = 3)
  G$.add_agents("agents_added", agents)
  expect_true("agents_added" %in% ls(G))
})

test_that(".add_stage() works", {
  G <- setABM()
  G$.add_stage("stage1", matrix(1:4, 2, 2))
  expect_true(is.matrix(G$stage1))
  expect_equal(G$stage1[1, 1], 1)
})

test_that(".add_FUN() works", {
  G <- setABM()
  myfun <- function() { return(42) }
  G$.add_FUN("global", myfun, FUN_category = "global_FUN")
  expect_true(is.function(G$global))
  expect_equal(G$global(), 42)
})

test_that(".add_active_binding() works", {
  G <- setABM()
  myactive <- function(){return(1)}
  G$.add_active_binding(name = "active_field", FUN = myactive)
  expect_equal(G$active_field, 1)
})

test_that(".remove_field() works", {
  G <- setABM()
  G$.add_stage("stage1", 100)
  G$.remove_field("stage1")
  expect_false("stage1" %in% ls(G))
})

test_that(".replace_field() works", {
  G <- setABM()
  G$.add_stage("stage1", 100)
  G$.replace_field("stage1", 200)
  expect_equal(G$stage1, 200)
})


test_that(".rename_field() works", {
  G <- setABM()
  G$.add_stage("stage1", 100)
  G$.rename_field(name = "stage1", new_name = "stage2")
  expect_true("stage2" %in% ls(G))
  expect_false("stage1" %in% ls(G))
})

test_that(".field_list() works", {
  G <- setABM()
  G$.add_stage("stage1", 100)
  df <- G$.field_list()
  expect_true("stage1" %in% df$name)
})

test_that(".agent_attr() works", {
  G <- setABM()
  agents <- init_agents(n = 2)
  G$.add_agents("agents", agents)
  attr_vals <- G$.agent_attr("agents", "ID")
  expect_equal(unlist(attr_vals), c(1, 2))
})

test_that(".save() works", {
  G <- setABM()
  agents <- init_agents(n = 2)
  G$.add_agents("agents", agents)
  G$.add_stage("stage1", 100)
  G$.save()
  expect_equal(names(G$log), "t1")
  expect_equal(G$log$t1$time, 1)
})


