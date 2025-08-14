# test input
# agents <- init_agents(attr_df = data.frame(age = 1:3))
# G <- setABM(agents = agents, stage = list(y = 1))

# test a select_FUN
# run_once <- function(){self$time >= 2}
# test_FUN(G = G, new_FUN_name = "run_once",
#          new_FUN = run_once, FUN_type = "stop_FUN", print_changes = T)

# test a new act_FUN
# get_older = function(){self$age <- self$age + 1} # agents' new act_FUN
# test_FUN(G = G, new_FUN_name = "get_older",
#          new_FUN = get_older, FUN_type = "act_FUN",
#          agents_name = "agents", agents_idx = 1, print_changes = T)

# test a new global_FUN
# increase_y <- function(a = 1){self$y <- self$y + a} # a new global_FUN
# test_FUN(G = G, new_FUN_name = "increase_y", new_FUN = increase_y, FUN_type = "global_FUN")

