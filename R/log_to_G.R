#' Restore ABM_G object from log
#'
#' Reconstructs an \code{ABM_G} object at a specific simulation time
#' by retrieving the state stored in the \code{G$log} field.
#' This is useful for debugging, inspection, or replaying past simulation states.
#'
#' @param G An \code{ABM_G} object that contains a \code{log} field.
#' @param which_time Integer. The time point (as stored in \code{G$log[[t]]$time}) to restore.
#'
#' @return A reconstructed \code{ABM_G} object representing the state at the specified simulation time.
#'   The returned object includes reconstructed agents, attributes, methods, and global functions,
#'   as well as stage variables.
#'
#' @details
#' This function assumes that only agent attributes and stage fields are stored in the log,
#' while act_FUNs and active bindings are retained from the current environment.
#'
#' If some agent IDs present in the log are no longer found in the current \code{G} object,
#' a warning is issued and the act_FUNs of the first available agent are used as a fallback.
#'
#' @examples
#' act1 <- function(){ self$x <- self$x + 1 }
#' act2 <- function(){ self$y <- self$y + 1 }
#' ab   <- function(){ self$x^2 }
#' g1   <- function(){ print(1) }
#'
#' agents <- init_agents(
#'   n = 3,
#'   attr_df = data.frame(x = 1:3, y = 4:6),
#'   act_FUN = list(act1 = act1, act2 = act2),
#'   active_binding = ab
#' )
#'
#' G <- setABM(
#'   agents = list(agents1 = agents, agents2 = agents),
#'   stage = matrix(1:9, 3, 3),
#'   global_FUN = g1
#' )
#'
#' G <- runABM(G = G, plan = c("act1", "act2", "g1"), times = 3)
#'
#' # Restore G at time = 2
#' G2 <- log_to_G(G = G, which_time = 2)
#'
#' @export

log_to_G <- function(G, which_time){
  # retrieve the time points
  time_points <- vapply(G$log, function(l) l$time, numeric(1))
  time_idx <- which(time_points == which_time)
  if (length(time_idx) == 0) stop("Specified 'which_time' not found in G$log.")

  # retrieve the values from the log
  G_log <- G$log[[time_idx]]

  # get field list
  field_list <- G$.field_list()

  # reconstruct agents
  agents_names <- unique(na.exclude(field_list$agent_name))
  if(length(agents_names)==0){
    # when there are no agents
    agents <- NULL
  }else{
    # if there are agents
    agents <- vector("list", length(agents_names))
    names(agents) <- agents_names
    for(agents_name in agents_names){
      # safe guard the error
      if (!agents_name %in% names(G_log)) {
        warning(paste("Agent group", agents_name, "not found in log. Skipped."))
        next
      }
      # retrieve agent IDs
      log_agents_IDs <- vapply(G_log[[agents_name]], function(X){X$ID}, numeric(1))
      G_agents_IDs <- vapply(G[[agents_name]], function(X){X$ID}, numeric(1))
      # retrieve the attribute names
      attribute_names <- field_list$name[field_list$agent_name==agents_name & field_list$category=="agent_attribute" & field_list$active_binding==FALSE]
      attribute_names <- setdiff(attribute_names, "ID")
      act_FUN_names  <- field_list$name[field_list$agent_name==agents_name & field_list$category=="act_FUN"]
      AB_names       <- field_list$name[field_list$agent_name==agents_name & field_list$category=="agent_attribute" & field_list$active_binding==TRUE]
      agent_len <- length(log_agents_IDs)
      # retrieve the attributes
      if(length(attribute_names)==0){
        agent_attributes <- NULL
      }else{
        agent_attributes <- lapply(attribute_names, function(x){
          lapply(1:agent_len, function(i){G_log[[agents_name]][[i]][[x]]})
        })
        names(agent_attributes) <- attribute_names
      }

      # act_FUNs
      if(length(act_FUN_names)==0){
        agents_act_FUN <- NULL
      }else{
        ## indexing the matched agents' position
        G_agents_idx <- match(log_agents_IDs, G_agents_IDs)

        ## warning if some of the G_agents_IDs is NA
        if (any(is.na(G_agents_idx))) {
          warning(paste0(
            "Unmatched agent IDs in '", agents_name,
            "': ", paste(log_agents_IDs[is.na(G_agents_idx)], collapse = " "),
            "\n", "act_FUN(s) from the first agent was used as fallback."
          ))
          G_agents_idx[is.na(G_agents_idx)] <- 1
        }

        ## retrieve the functions
        agents_act_FUN <- lapply(act_FUN_names, function(FUN_name){
          lapply(G_agents_idx, function(i){
            G[[agents_name]][[i]][[FUN_name]]
          })
        })
        names(agents_act_FUN) <- act_FUN_names
      }

      # active bindings
      ## retrieve the functions
      if(length(AB_names)==0){
        agents_active_bindings <- NULL
      }else{
        agents_active_bindings <- lapply(AB_names, function(AB_name){
          lapply(G_agents_idx, function(i){
            G[[agents_name]][[i]]$.__enclos_env__$.__active__[[AB_name]]
          })
        })
        names(agents_active_bindings) <- AB_names
      }

      # reconstruct agents
      reconstructed_agents <- init_agents(
        n = agent_len, act_FUN = agents_act_FUN,
        active_binding = agents_active_bindings,
        other_attrs = agent_attributes, custom_ID = log_agents_IDs)
      # attach the reconstructed agents
      agents[[agents_name]] <- reconstructed_agents
    }
  }

  # reconstruct global fields-----------------------------
  stage_names <- field_list$name[field_list$category=="stage" & field_list$active_binding==FALSE]
  stage_active_binding_names <- field_list$name[field_list$category=="stage" & field_list$active_binding==TRUE]
  global_FUN_names <- field_list$name[field_list$category=="global_FUN"]
  select_FUN_names <- field_list$name[field_list$category=="select_FUN"]
  stop_FUN_names <- field_list$name[field_list$category=="stop_FUN"]
  update_FUN_names <- field_list$name[field_list$category=="update_FUN"]
  summary_FUN_names <- field_list$name[field_list$category=="summary_FUN"]
  plot_FUN_names <- field_list$name[field_list$category=="plot_FUN"]

  # retrieve each fields
  if(length(stage_names) > 0){
    stages <- lapply(stage_names, function(stage_name){G_log[[stage_name]]})
    names(stages) <- stage_names
  }else{
    stages <- NULL
  }

  if(length(stage_active_binding_names) > 0){
    stage_active_bindings <- lapply(stage_active_binding_names, function(name){G$.__enclos_env__$.__active__[[name]]})
    names(stage_active_bindings) <- stage_active_binding_names
  }else{
    stage_active_bindings <- NULL
  }

  if(length(global_FUN_names) > 0){
    global_FUNs <- lapply(global_FUN_names, function(name){G[[name]]})
    names(global_FUNs) <- global_FUN_names
  }else{
    global_FUNs <- NULL
  }

  if(length(select_FUN_names) > 0){
    select_FUNs <- lapply(select_FUN_names, function(name){G[[name]]})
    names(select_FUNs) <- select_FUN_names
  }else{
    select_FUNs <- NULL
  }

  if(length(stop_FUN_names) > 0){
    stop_FUNs <- lapply(stop_FUN_names, function(name){G[[name]]})
    names(stop_FUNs) <- stop_FUN_names
  }else{
    stop_FUNs <- NULL
  }

  if(length(update_FUN_names) > 0){
    update_FUNs <- lapply(update_FUN_names, function(name){G[[name]]})
    names(update_FUNs) <- update_FUN_names
  }else{
    update_FUNs <- NULL
  }

  if(length(summary_FUN_names) > 0){
    summary_FUNs <- lapply(summary_FUN_names, function(name){G[[name]]})
    names(summary_FUNs) <- summary_FUN_names
  }else{
    summary_FUNs <- NULL
  }

  if(length(plot_FUN_names) > 0){
    plot_FUNs <- lapply(plot_FUN_names, function(name){G[[name]]})
    names(plot_FUNs) <- plot_FUN_names
  }else{
    plot_FUNs <- NULL
  }

  # reconstruct G-----------------------------
  reconstructed_G <- setABM(agents = agents, stage = stages,
                            active_binding = stage_active_bindings,
         global_FUN = global_FUNs, select_FUN = select_FUNs, stop_FUN = stop_FUNs,
         update_FUN = update_FUNs, summary_FUN = summary_FUNs, plot_FUN = plot_FUNs,
         time = which_time)
  # return
  reconstructed_G
  }#-------end of function

