################################################################################
# .shape_agent
################################################################################

.shape_agent <- function(agents, agents_sbs){
  # Check if agents_sbs is NULL
  if(is.null(agents_sbs)){
    return(NULL)
  }

  # get object label
  if(is.symbol(agents_sbs)){
    agents_label <- deparse(agents_sbs)
  }else{
    agents_label <- "agents"
  }

  # check the class. If the input 'agents' is a numeric, create agents
  agents <- if(!all(lapply(unlist(agents), function(x){class(x)[[1]]})=="ABM_Agent")){
    ## If "ABM_Agent" class is not "agents"
    lapply(unlist(agents), function(y){
      if(is.numeric(y)){
        init_agents(n = y)
      }else{
        stop("agents must be either ABM_Agent class object or a positive integer.")
      }
    })
  }else{
    agents
  }

  # Check if the 'agents' have multiple agent types
  if(is.list(agents[[1]])==FALSE){
    agent_formatted <- list(agents)
  }else{
    agent_formatted <- agents
  }

  # check if names are available
  if(is.null(names(agent_formatted))){
    ## if there is no name
    if(length(agent_formatted)==1){
      names(agent_formatted) <- agents_label
    }else{
      names(agent_formatted) <- paste0(agents_label, 1:length(agent_formatted))
    }
  }

  # check if only a part of them has a name
  if(any(names(agent_formatted)=="")){
    stop("Put names to each agent object.")
  }

  # set field_category as "agent"
  field_category <- rep("agent", length(agent_formatted))
  names(field_category) <- names(agent_formatted)

  # output
  agent_formatted <- list(value = agent_formatted, category = field_category)
  agent_formatted
}


#---------------------------------------------------------
# .shape_stage
#---------------------------------------------------------

#' @import rlang
.shape_stage <- function(stage = NULL, stage_sbs = NULL){
  # Return if the input is NULL
  if(is.null(stage_sbs)){
    return(NULL)
  }

  # get object label
  if(is.list(stage) & !is.data.frame(stage)){
    ## If stage is more than one object as list
    stage_label <- as.character(stage_sbs)[-1]
    stage_label <- sapply(stage_label, function(X){
      if(exists(X)){
        X
      }else{
        ""
      }
    })
  }else{
    ## Other
    if(is.symbol(stage_sbs)){
      stage_label <- deparse(stage_sbs)
    }else{
      stage_label <- "stage"
    }
  }

  # Convert the input into list
  if(!is.list(stage)|is.data.frame(stage)){
    stage_list <- list(stage)
  }else{
    stage_list <- stage
  }

  # Check each element
  stage_formatted <- lapply(stage_list, function(X){
    if(is.function(X)){
      stop("Do not assign a function directly to a stage. If you intend to create an active binding field, use the 'active_binding' argument instead.")
    }
    X
  })

  # Format the names
  # if there is no name
  if(is.null(names(stage_formatted))){
    if(length(stage_formatted)==1|length(stage_formatted)==length(stage_label)){
      names(stage_formatted) <- stage_label
    }else{
      names(stage_formatted) <- paste0(stage_label, 1:length(stage_formatted))
    }
  }

  # Check if only a part of the elements has a name
  if(any(names(stage_formatted)=="")){
    if(length(names(stage_formatted)[names(stage_formatted)==""])==1){
      names(stage_formatted)[names(stage_formatted)==""] <- "Y"
    }else{
      names(stage_formatted)[names(stage_formatted)==""] <- paste0("stage", 1:length(names(stage_formatted)==""))
    }
  }

  # define field_category
  field_category <- rep("stage", length(stage_formatted))
  names(field_category) <- names(stage_formatted)

  # return
  stage_formatted <- list(value = stage_formatted, category = field_category)
  stage_formatted
}


#---------------------------------------------------------
# .shape_active_binding
#---------------------------------------------------------

#' @import rlang
.shape_active_binding <- function(
    active_binding = NULL,
    active_binding_sbs = NULL){

  # Return if NULL
  if(is.null(active_binding)){
    return(NULL)
  }

  # object label
  if(is.symbol(active_binding_sbs)){
    active_binding_label <- deparse(active_binding_sbs)
  }else{
    active_binding_label <- "ABF"
  }

  # convert into list
  if(!is.list(active_binding)){
    active_binding <- list(active_binding)
  }
  active_binding_formatted <- active_binding

  ## makes all the inputs into functions
  for(i in 1:length(active_binding_formatted)){
    if(is.character(active_binding_formatted[[i]])){
      parsed_FUN <- parse(text = active_binding_formatted[[i]])[[1]]
      if(is.name(parsed_FUN)){
        retrieved_FUN <- get(parsed_FUN)
        stopifnot("The 'active_binding' retrieved from the specified object must be a function." = is.function(retrieved_FUN))
        active_binding_formatted[[i]] <- retrieved_FUN
      }else if(is.call(parsed_FUN)){
        retrieved_FUN <- get(call_name(parsed_FUN))
        stopifnot("The 'active_binding' retrieved from the specified object must be a function." = is.function(retrieved_FUN))
        original_args <- formals(retrieved_FUN)
        parsed_args <- call_args(parsed_FUN)
        # create an empty function and then attach formals and a body
        FUN <- function(){}
        original_args[names(parsed_args)] <- parsed_args
        body(FUN) <- body(retrieved_FUN)
        formals(FUN) <- original_args
        active_binding_formatted[[i]] <- FUN
      }
    }
  }

  # check if all elements are functions
  stopifnot("active_binding must be a function." = all(unlist(lapply(active_binding_formatted, is.function))))

  # set names
  # if there are no names
  if(is.null(names(active_binding_formatted))){
    if(length(active_binding_formatted)==1){
      names(active_binding_formatted) <- active_binding_label
    }else{
      names(active_binding_formatted) <- paste0(active_binding_label, 1:length(active_binding_formatted))
    }
  }

  # check if only a part of elements has names
  if(any(names(active_binding_formatted)=="")){
    if(length(names(active_binding_formatted)[names(active_binding_formatted)==""])==1){
      names(active_binding_formatted)[names(active_binding_formatted)==""] <- "ABF"
    }else{
      names(active_binding_formatted)[names(active_binding_formatted)==""] <- paste0("ABF", 1:length(names(active_binding_formatted)==""))
    }
  }

  # define field_category
  field_category <- rep("stage", length(active_binding_formatted))
  names(field_category) <- names(active_binding_formatted)

  # return
  active_binding_formatted <- list(value = active_binding_formatted,
                                         category = field_category)
  active_binding_formatted
}

#------------------------------------------------
# assign_func_envs: active_binding用
#------------------------------------------------

assign_func_envs <- function(objs, target_env) {
  if (is.null(target_env)) return(objs)

  lapply(objs, function(x) {
    if (is.function(x)) environment(x) <- target_env
    x
  })
}

#-------------------------------------------------------------------------------
# .get_FUN
#-------------------------------------------------------------------------------

#' @import rlang

.get_FUN <- function(FUN){
  # return FUN if the input is closure
  if(is.function(FUN)|rlang::is_closure(FUN)){
    FUN
  }else if(is.character(FUN)){
    # if FUN is a character, get the function from environment
    if(exists(FUN)){
      ## if FUN exists in the environment, get it
      FUN <- get(FUN)
      stopifnot("The retrieved object in global_FUN is not a function." = is.function(FUN))
      FUN
    }else{
      ## if FUN does not exists in the environment, try to get it as a call
      parsed_FUN <- parse(text = FUN)[[1]]
      func_name <- rlang::call_name(parsed_FUN)
      if(exists(func_name)){
        ### if func_name, convert it accordingly
        func_args <- rlang::call_args(parsed_FUN)
        assign("temp_func", get(func_name))
        #### update the formals according to the intended arguments
        if(length(func_args) > 0){
          for(k in 1:length(func_args)){
            formals(temp_func)[names(func_args[k])] <- func_args[k]
          }
        }
        ### return temp_func
        temp_func
      }else{
        ### if no func_name found
        stop("FUN does not exists in the environment.")
      }
    }
    # if FUN is a character---------//
  }else{
    stop("The retrieved object is not a function.")
  }
}


#-------------------------------------------------------------------------------
# .shape_G_FUN
#-------------------------------------------------------------------------------

.shape_G_FUN <- function(FUN, FUN_sbs,
                       FUN_category = c("global_FUN", "select_FUN", "stop_FUN",
                                        "update_FUN", "summary_FUN", "plot_FUN")){
  # Return NULL if input is NULL
  if(is.null(FUN)){
    return(NULL)
  }

  # match.arg: FUN_category
  FUN_category <- match.arg(FUN_category)

  # set object label
  if(is.symbol(FUN_sbs)){
    FUN_label <- deparse(FUN_sbs)
  }else{
    FUN_label <- FUN_category
  }

  # Check if it is list object
  if(!is.list(FUN)){
    FUN_list <- list(FUN)
  }else{
    FUN_list <- FUN
  }

  # Get functions to each element of FUN
  FUN_formatted <- lapply(FUN_list, function(X){
    .get_FUN(FUN = X)
  })

  # Set G and E to formals
  FUN_formatted <- lapply(FUN_formatted, function(FUN){
    # Delete G or E if the user already set
    current_formals <- formals(FUN)
    current_formals[which(names(current_formals)=="G")|which(names(current_formals)=="E")] <- NULL
    formals(FUN) <- c(alist(G = G, E = E), current_formals) # add G = G, E = E
    FUN
  })

  # add names to list if it is not set yet
  if(is.null(names(FUN_formatted))){
    if(length(FUN_formatted)==1){
      names(FUN_formatted) <- FUN_label
    }else{
      names(FUN_formatted) <- paste0(FUN_label, 1:length(FUN_formatted))
    }
  }

  # Check if only a part of the elements has a name
  if(any(names(FUN_formatted)=="")){
    stop("Put names to each FUN object.")
  }

  # define FUN_category
  FUN_category_formatted <- rep(FUN_category, length(FUN_formatted))
  names(FUN_category_formatted) <- names(FUN_formatted)

  # output
  FUN_formatted <- list(value = FUN_formatted,
                        category = FUN_category_formatted)
  FUN_formatted
}
