test_that("modify_agents: 'add_agents' works",{
  G <- setABM(agents = init_agents(n = 3, attr_df = data.frame(age = c(1,2,3))))
  new_agents <- init_agents(n = 2, attr_df = data.frame(age = c(4, 5)))
  G2 <- modify_agents(G = G, G_agents_name = "agents", new_obj = new_agents, method = "add_agent")
  expect_true(all(names(G2$agents) == paste0("ID", 1:5)))
})

test_that("modify_agents: return 'agents' properly if input was agents",{
  agents = init_agents(n = 3, attr_df = data.frame(age = c(1,2,3)))
  new_agents <- init_agents(n = 2, attr_df = data.frame(age = c(4, 5)))
  updated_agents <- modify_agents(agents = agents, new_obj = new_agents, method = "add_agent")
  expect_true(all(names(updated_agents) == paste0("ID", 1:5)))
})

test_that("modify_agents: add an object with 'add_attr_df'", {
  G <- setABM(agents = init_agents(attr_df = data.frame(x = c(1,2,3))))
  age <- c(11,12,13)
  G2 <- modify_agents(G = G, G_agents_name = "agents", new_obj = age, method = "add_attr_df")
  expect_true("age" %in% names(G2$agents$ID1))
  expect_true(G2$agents$ID1$age==11)
})

test_that("modify_agents: add a data.frame with two columns with 'add_attr_df'", {
  G <- setABM(agents = init_agents(attr_df = data.frame(x = c(1,2,3))))
  G2 <- modify_agents(G = G, G_agents_name = "agents",
                      new_obj = data.frame(y = 4:6, z = 7:9), method = "add_attr_df")
  expect_true("y" %in% names(G2$agents$ID1))
  expect_true("z" %in% names(G2$agents$ID1))
})


test_that("modify_agents: add an act_FUN with 'add_act_FUN'", {
  G <- setABM(agents = init_agents(attr_df = data.frame(x = c(1,2,3))))
  new_act1 <- function(){print(1)}
  G2 <- modify_agents(G = G, G_agents_name = "agents", new_obj = new_act1, method = "add_act_FUN")
  expect_true("new_act1" %in% ls(G2$agents$ID1))
})


test_that("modify_agents: add two act_FUNs with 'add_act_FUN'", {
  G <- setABM(agents = init_agents(attr_df = data.frame(x = c(1,2,3))))
  new_act1 <- function(){print(1)}
  new_act2 <- function(){print(2)}
  G2 <- modify_agents(G = G, G_agents_name = "agents", new_obj = list(new_act1 = new_act1,
                                                                     new_act2 = new_act2),
                      method = "add_act_FUN")
  expect_true(all(c("new_act1", "new_act2") %in% ls(G2$agents$ID1)))
})


test_that("modify_agents: add an active binding with 'add_active_binding'", {
  G <- setABM(agents = init_agents(attr_df = data.frame(x = c(1,2,3))))
  random <- function(){rnorm(1)}
  G2 <- modify_agents(G = G, G_agents_name = "agents", new_obj = random, method = "add_active_binding")
  expect_true("random" %in% ls(G2$agents$ID1))
})


test_that("modify_agents: add two active bindings with 'add_active_binding'", {
  G <- setABM(agents = init_agents(attr_df = data.frame(x = c(1,2,3))))
  random <- function(){rnorm(1)}
  G2 <- modify_agents(G = G, G_agents_name = "agents", new_obj = list(random1 = random,
                                                                     random2 = random), method = "add_active_binding")
  expect_true("random1" %in% ls(G2$agents$ID1))
  expect_true("random2" %in% ls(G2$agents$ID1))
})


test_that("modify_agents: add two other_attrs with 'add_other_attrs'", {
  G <- setABM(agents = init_agents(attr_df = data.frame(x = c(1,2,3))))
  G2 <- modify_agents(G = G, G_agents_name = "agents", new_obj = list(y = c(4,5,6),
                                                                     z = list(matrix(1,2,2),
                                                                              matrix(2,2,2),
                                                                              matrix(3,2,2))), method = "add_other_attrs")
  expect_true(G2$agents$ID1$y == 4)
  expect_true(all(G2$agents$ID1$z == matrix(1,2,2)))
})


test_that("modify_agents: 'delete_field' works", {
  G <- setABM(agents = init_agents(attr_df = data.frame(x = c(1,2,3))))
  G2 <- modify_agents(G = G, G_agents_name = "agents", field_name = "x", method = "delete_field")
  expect_false("x" %in% ls(G2$agents$ID1))
})


test_that("modify_agents: 'rename' for scolar works", {
  G <- setABM(agents = init_agents(attr_df = data.frame(x = c(1,2,3))))
  G2 <- modify_agents(G = G, G_agents_name = "agents", field_name = "x", new_field_name = "x2", method = "rename")
  expect_false("x" %in% ls(G2$agents$ID1)) # false
  expect_true("x2" %in% ls(G2$agents$ID1)) # true
})

test_that("modify_agents: 'rename' for active binding works", {
  random <- function(){rnorm(1)}
  G <- setABM(agents = init_agents(attr_df = data.frame(x = c(1,2,3)), active_binding_field = random))
  G2 <- modify_agents(G = G, G_agents_name = "agents", field_name = "random", new_field_name = "random2", method = "rename")
  expect_false("random" %in% ls(G2$agents$ID1))
  expect_true("random2" %in% ls(G2$agents$ID1))
})


test_that("modify_agents: 'rename' for act_FUN works", {
  act_FUN <- function(){print(1)}
  G <- setABM(agents = init_agents(attr_df = data.frame(x = c(1,2,3)), act_FUN = act_FUN))
  G2 <- modify_agents(G = G, G_agents_name = "agents", field_name = "act_FUN", new_field_name = "act_FUN2", method = "rename")
  expect_false("act_FUN" %in% ls(G2$agents$ID1))
  expect_true("act_FUN2" %in% ls(G2$agents$ID1))
})


test_that("modify_agents: 'copy' for scolar works", {
  G <- setABM(agents = init_agents(attr_df = data.frame(x = c(1,2,3))))
  G2 <- modify_agents(G = G, G_agents_name = "agents", field_name = "x", method = "copy", new_field_name = "x2")
  expect_true(G2$agents$ID1$x == G2$agents$ID1$x2)
  expect_true(G2$agents$ID2$x == G2$agents$ID2$x2)
})

test_that("modify_agents: 'copy' for act_FUN works", {
  G <- setABM(agents = init_agents(attr_df = data.frame(x = c(1,2,3)), act_FUN = function(){print(1)}))
  G2 <- modify_agents(G = G, G_agents_name = "agents", field_name = "act", method = "copy", new_field_name = "act2")
  expect_true(body(G2$agents$ID1$act)==body(G2$agents$ID1$act2))
})

test_that("modify_agents: 'copy' for active binding works", {
  random <- function(){rnorm(1)}
  G <- setABM(agents = init_agents(attr_df = data.frame(x = c(1,2,3)), active_binding_field = random))
  G2 <- modify_agents(G = G, G_agents_name = "agents", field_name = "random", method = "copy", new_field_name = "random2")
  expect_true(all(c("random", "random2") %in% ls(G2$agents$ID1)))
})

test_that("modify_agents: 'replace' works for a vector input", {
G <- setABM(agents = init_agents(n = 3, attr_df = data.frame(x = 1:3)))
G2 <- modify_agents(G = G, G_agents_name = "agents", new_obj = 4:6, method = "replace", field_name = "x")
expect_true(G2$agents$ID1$x==4)
})

test_that("modify_agents: 'replace' works for a data.frame input", {
G <- setABM(agents = init_agents(n = 3, attr_df = data.frame(x = 1:3)))
G2 <- modify_agents(G = G, G_agents_name = "agents", new_obj = data.frame(4:6), method = "replace", field_name = "x")
expect_true(G2$agents$ID1$x==4)
})

test_that("modify_agents: 'replace' works for a data.frame even when the latter has more than 1 column", {
G <- setABM(agents = init_agents(n = 3, attr_df = data.frame(x = 1:3)))
expect_warning(modify_agents(G = G, G_agents_name = "agents", new_obj = data.frame(4:6, 7:9), method = "replace", field_name = "x"))
G2 <- suppressWarnings(modify_agents(G = G, G_agents_name = "agents", new_obj = data.frame(4:6, 7:9), method = "replace", field_name = "x"))
expect_true(G2$agents$ID1$x==4)
})

test_that("modify_agents: 'replace' works when for list object.", {
  mat <- matrix(0, 2, 2)
  mat2 <- matrix(1, 2, 2)
  G <- setABM(agents = init_agents(n = 3, other_attrs = list(mat1 = list(mat, mat, mat))))
  G2 <- modify_agents(G = G, G_agents_name = "agents", new_obj = list(mat2, mat2, mat2), method = "replace", field_name = "mat1")
  expect_true(all(G2$agents$ID1$mat1==mat2))
})

test_that("modify_agents: 'replace' works for a single act_FUN object.", {
  afun <- function(){print(1)}
  afun2 <- function(){print(2)}
  G <- setABM(agents = init_agents(n = 3, act_FUN = afun))
  G2 <- modify_agents(G = G, G_agents_name = "agents", new_obj = afun2, method = "replace",
                      field_name = "afun")
  expect_true(body(G2$agents$ID1$afun)==body(afun2))
})

test_that("modify_agents: 'replace' works for an act_FUN object whose element differs for each agent.", {
  afun  <- function(){print(0)}
  afun1 <- function(a = 1){print(1)}
  afun2 <- function(a = 2){print(2)}
  afun3 <- function(a = 3){print(3)}
  new_afun <- list(afun1, afun2, afun3)
  G <- setABM(agents = init_agents(n = 3, act_FUN = afun))
  G2 <- modify_agents(G = G, G_agents_name = "agents", new_obj = new_afun,
                      method = "replace",
                      field_name = "afun")
  expect_true(body(G2$agents$ID1$afun)==body(afun1))
  expect_true(body(G2$agents$ID2$afun)==body(afun2))
  expect_true(body(G2$agents$ID3$afun)==body(afun3))
})

test_that("modify_agents: 'delete_agent' works properly.", {
  G <- setABM(agents = init_agents(n = 5))
  G2 <- modify_agents(G = G, G_agents_name = "agents", method = "delete_agent", agent_posit = c(2,3))
  expect_true(all(names(G2$agents) %in% c("ID1", "ID4", "ID5")))
})


