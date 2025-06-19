#' Run the ABM simulation
#'
#' @description
#' Runs a simulation using an \code{ABM_G} object, either by executing a predefined update
#' function or by constructing an update function from a user-defined execution plan.
#' The simulation proceeds step by step and can include logging, custom stop conditions,
#' and RDS file output.
#'
#' @param G An \code{ABM_G} object representing the simulation state and structure.
#' @param plan A character vector specifying the sequence of functions to execute
#' in each simulation step. Can include agent-level functions using the syntax \code{agent:fun(arg=val)}.
#' Defaults to \code{NULL}.
#' @param update_FUN_name A character scalar specifying the name of an \code{update_FUN}
#' already stored in \code{G}. If \code{NULL}, an update function is generated from \code{plan}.
#' @param stop_FUN_name A character scalar indicating the name of a stop function stored in \code{G}.
#' If \code{NULL}, the simulation stops after \code{times} steps.
#' @param times Integer specifying the number of steps to run the simulation
#' (used only if \code{stop_FUN_name} is not provided). Defaults to \code{1}.
#' @param save_log Logical; if \code{TRUE}, logs the state at each time step using \code{G$.save()}.
#' Defaults to \code{TRUE}.
#' @param seed Optional integer; if \code{NULL}, the seed is derived from \code{Sys.time()}.
#' @param add_tryCatch Logical; if \code{TRUE}, each function call is wrapped in \code{tryCatch()}
#' to allow simulations to continue in case of errors. Defaults to \code{TRUE}.
#' @param return_update_FUN Logical; if \code{TRUE}, the body of the final \code{update_FUN} is saved in
#' \code{G$notes$update_FUN_used}. Defaults to \code{FALSE}.
#' @param temp_E Logical; if \code{TRUE}, creates a temporary environment \code{E} passed
#' to each function during simulation. Defaults to \code{TRUE}.
#' @param saveRDS_inbetween Logical; if \code{TRUE}, saves the entire \code{G} object as an RDS file
#' at every simulation step. Defaults to \code{FALSE}.
#' @param RDS_file_name A character scalar specifying the file name for saving RDS
#' objects if \code{saveRDS_inbetween = TRUE}. Defaults to \code{"G_temp.rds"}.
#'
#' @return The updated \code{ABM_G} object after running the simulation.
#' If \code{return_update_FUN = TRUE}, the update function used is stored as a character
#' vector in \code{G$notes$update_FUN_used}.
#'
#' @details
#' The simulation proceeds by either calling a registered \code{update_FUN} stored in \code{G},
#' or generating a new one based on a user-defined \code{plan}.
#' Each element of \code{plan} can be a function name (e.g., \code{"fun_name"})
#' or a namespaced string specifying the target agent group (e.g., \code{"agents:fun_name"}).
#'
#' If multiple elements are provided in \code{plan}, they are executed sequentially
#' in the specified order at each time step. For example,
#' \code{c("children:get_older", "parents:get_older")} applies the \code{get_older}
#' function to both children and parents agents in order.
#'
#' If no \code{select_FUN} is provided prior to an \code{act_FUN}, a default
#' \code{[select_all]} is automatically inserted for convenience.
#' When using \code{plan}, each entry can reference a global or agent-level function,
#' and arguments can be passed via standard R function syntax. Agent-level functions must be
#' preceded by a valid \code{select_FUN}, which selects the agents to which the function applies.
#' If no \code{select_FUN} is present, a default \code{[select_all]} is added automatically.
#'
#' If \code{add_tryCatch = TRUE}, runtime errors in user-defined functions are caught
#' and logged as warnings, and simulation continues.
#'
#' To examine the simulation runtime, the total elapsed time is printed and also saved in
#' \code{G$notes$simulation_took}.
#'
#' @examples
#' # Setup agents
#' children_attr <- data.frame(age = c(0, 1, 2))
#' get_older <- function() { self$age <- self$age + 1 }
#' children <- init_agents(attr_df = children_attr, act_FUN = get_older)
#'
#' # Initialize the ABM environment
#' G1 <- setABM(agents = children)
#'
#' # Run simulation for 5 steps using a plan
#' G1_out <- runABM(G = G1, plan = "get_older", times = 5)
#' G1_out$children
#'
#' # Run simulation using a named update function
#' add_1_to_all_children <- function(){lapply(1:length(G$children),
#' function(i){G$children[[i]]$age <- G$children[[i]]$age + 1})}
#' G2 <- modify_G(G = G1, field_name = "add_1_to_all_children",
#' method = "add_update_FUN",
#'                new_obj = add_1_to_all_children)
#' G2_out <- runABM(G = G2, update_FUN_name = "add_1_to_all_children",
#'                  times = 5)
#' G2_out$children
#'
#' # A plan with two act_FUN for different agents
#' parent_attr <- data.frame(age = c(30, 31, 32))
#' parents <- init_agents(attr_df = parent_attr, act_FUN = get_older)
#' G3 <- modify_G(G = G1, field_name = "parents",
#' method = "add_agents", new_obj = parents)
#' G3_out <- runABM(G = G3, plan = c("children:get_older", "parents:get_older"),
#' times = 3)
#' G3_out$children
#' G3_out$parents
#'
#' @export

runABM <- function(G,
                   plan = NULL,
                   update_FUN_name = NULL,
                   stop_FUN_name = NULL,
                   times = 1,
                   save_log = TRUE,
                   seed = NULL,
                   add_tryCatch = TRUE,
                   return_update_FUN = FALSE,
                   saveRDS_inbetween = FALSE,
                   temp_E = TRUE,
                   RDS_file_name = "G_temp.rds"){
  # deep clone the G
  G <- G$clone(deep = TRUE)

  # validate the input
  stopifnot(inherits(G, "ABM_G"))
  stopifnot(is.null(plan) || (is.vector(plan) && all(is.character(plan))))
  stopifnot(is.null(update_FUN_name) || (is.character(update_FUN_name) && length(update_FUN_name) == 1))


  # create E(tempral_environment)
  if (temp_E) {
    E <- rlang::new_environment()
    on.exit(rm(E, envir = environment()), add = TRUE)
  } else {
    E <- NULL
  }

  # Retrieve the field_list
  field_list <- G$.field_list()

  # Check whether update_FUN_name or plan is available
  stopifnot("Either 'plan' or 'update_FUN_name' must be provided." = !is.null(plan)||!is.null(update_FUN_name))

  # If update_FUN_name is available:
  if(!is.null(update_FUN_name)){
    #------------------------------
    # [Based on update_FUN]
    #------------------------------
    stopifnot("update_FUN_name must be a character with the length of one." = (is.character(update_FUN_name) && length(update_FUN_name)==1))
    stopifnot("Inputted 'update_FUN_name' does not exist in 'G'." = update_FUN_name %in% ls(G))
    stopifnot("Inputted 'update_FUN_name' is not an update_FUN type." = field_list$category[field_list$name == update_FUN_name]=="update_FUN")

    #### create update_FUN from selected update_FUN
    update_FUN <- function(G = G, E = E){}
    ## add_tryCatch?
    if(add_tryCatch){
      update_FUN_text <- parse(
        text = paste0("tryCatch(G$", update_FUN_name, "(G = G, E = E)",
                      ", error = function(e){warning(paste0(",
                      "'error occured for ",
                      update_FUN_name,"'))
                          return(NULL)})"
        ))[[1]]
    }else{
      update_FUN_text <- parse(text = paste0("G$", update_FUN_name, "(G = G, E = E)"))[[1]]
    }
    #### attach to body
    body(update_FUN) <- as.call(append(as.list(body(update_FUN)),
                                       update_FUN_text))
    ## announce update_FUN
    cat("update_FUN:", update_FUN_name, "\n")
    # -----based on update_FUN_name----------//
  }else{
    ##--------------------------------
    ## [Based on plan]
    ##--------------------------------
    ## Check the input of the 'plan'
    stopifnot("plan must be a vector." = (is.vector(plan) & !is.list(plan)))
    stopifnot("plan must be character string(s)." = all(is.character(plan)))

    ## Retrieve and create the relevant functions
    # clean plan input
    plan <- stringr::str_remove_all(plan, pattern = " ") # remove spaces

    # n plan
    n_plan <- length(plan)

    # parse agents
    agent_loc <- stringr::str_locate(plan, pattern = ":")
    agent_name <- unlist(lapply(1:n_plan, function(i){
      stringr::str_sub(plan[i], end = agent_loc[i,1] - 1)
    }))

    # reshape plan so that only consists of FUNs
    plan2 <- unlist(lapply(1:n_plan, function(i){
      if(is.na(agent_loc[i,1])){
        plan[i]
      }else{
        stringr::str_sub(plan[i], start = agent_loc[i,1] + 1)
      }
    }))

    # parse FUN arguments
    FUN_args <- vector("list", n_plan)
    for(i in 1:n_plan){
      parsed_obs <- parse(text = plan2[i])[[1]]
      if(is.symbol(parsed_obs)){
        names(FUN_args)[[i]] <- as.character(parsed_obs)
      }else{
        names(FUN_args)[[i]] <- as.character(parsed_obs[[1]])
        FUN_args[[i]] <- as.list(parsed_obs[-1])
      }
    }

    # create a new field_list that only consists of the FUNs to be used
    idx <- rep(NA_integer_, n_plan)
    for (i in 1:n_plan) {
      FUN_name <- names(FUN_args)[i]

      if (is.na(agent_name[i])) {
        w <- which(field_list$name == FUN_name)
      } else {
        w <- which(field_list$agent_name == agent_name[i] & field_list$name == FUN_name)
      }
      idx[i] <- if (length(w) > 0) {w[1]}else{NA_integer_}
    }

    # stop if there is NA
    if(any(is.na(idx))){
      stop(paste("The following FUN stated in the 'plan' was not found:", names(FUN_args)[is.na(idx)]))
    }

    # create a plan_list
    plan_list <- field_list[idx, ]

    # replace FUN if it is required
    for(i in 1:n_plan){
      if(!is.null(FUN_args[[i]])){
        if(is.na(plan_list[i,"agent_name"])){
          ## global FUN
          FUN <- G[[plan_list[i, "name"]]]
          formals(FUN)[names(FUN_args[[i]])] <- FUN_args[[i]]
          G <- modify_G(G = G, field_name = plan_list[i, "name"],
                        method = "replace", new_obj = FUN)
        }else{
          ## act_FUN(required to replace act_FUN for each agents)
          FUN <- vector("list", length(G[[plan_list[i, "agent_name"]]]))
          agent_name <- plan_list[i, "agent_name"]
          FUN_name <- plan_list[i, "name"]
          for(p in 1:length(G[[agent_name]])){
            FUN[[p]] <- G[[agent_name]][[p]][[FUN_name]]
            formals(FUN[[p]])[names(FUN_args[[i]])] <- FUN_args[[i]]
          }
          G <- modify_agents(G = G, G_agents_name = agent_name,
                        new_obj = FUN, method = "replace",
                        field_name = FUN_name)
        }
      }
    }

    ## Check if FUNs in the plan are either "global_FUN", "act_FUN", or "select_FUN".
    stopifnot("'plan' must be either global_FUN, act_FUN, select_FUN." = plan_list$category %in% c("global_FUN","act_FUN","select_FUN"))

    ## check if select_FUN is placed before act_FUN
    ## If no select_FUN, add [select_all]
    if(plan_list$category[1]=="act_FUN"){
      plan_list <- rbind(
        data.frame(agent_name = plan_list$agent_name[1],
                   name = "[select_all]",
                   category = "select_FUN",
                   type = "function",
                   active_binding = FALSE),
        plan_list)
    }

    temp <- vector("list", length = NROW(plan_list))
    temp[[1]] <- plan_list[1, ]
    if(nrow(plan_list)>=2){
      for(i in 2:nrow(plan_list)){
        if(plan_list$category[i]=="act_FUN" & plan_list$category[i-1]!="select_FUN"){
          temp[[i]] <- rbind(
            data.frame(agent_name = plan_list$agent_name[i],
                       name = "[select_all]",
                       category = "select_FUN",
                       type = "function",
                       active_binding = FALSE),
            plan_list[i, ])
        }else{
          temp[[i]] <- plan_list[i, ]
        }
      }
    }
    plan_list <- do.call(rbind, temp)

    ## check the position of select_FUN
    select_FUN_posit <- which(plan_list$category == "select_FUN")
    posit_check <- unlist(lapply(select_FUN_posit, function(posit){
      tryCatch(plan_list[posit + 1, "category"]=="act_FUN",
               error = function(e){FALSE})}))
    stopifnot("All select_FUN must be followed by act_FUN." = all(posit_check)==TRUE)

    ## create parts of update_FUN based on plan
    update_FUN_parts <- vector(mode = "list", NROW(plan_list))
    for(j in 1:length(update_FUN_parts)){
      update_FUN_parts[[j]] <- switch(
        plan_list$category[j],
        "select_FUN" = {NULL},
        "act_FUN" = {
          # preparation of select_FUN
          if(plan_list$name[j-1]=="[select_all]"){
            agent <- plan_list$agent_name[j-1]
            text_select_FUN <- paste0("1:length(G$",agent,")")
          }else{
            FUN <- plan_list$name[j-1]
            ## add_tryCatch?
            if(add_tryCatch){
              # store a warning message
              warning_message <- paste0("error occured for ", shQuote(plan_list$name[j - 1]), ".")
              # construct
              text_select_FUN <- paste(
                "tryCatch(",
                paste0("G$", plan_list$name[j - 1], "(G = G, E = E)"),
                ", error = function(e){",
                "e", "\n",
                "return(NULL)}",
                ")"
              )
            }else{
              text_select_FUN <- paste0("G$", FUN, "(G = G, E = E)")
            }
          } #--preparation of select_FUN
          # act_FUN
          agent <- plan_list$agent_name[j]
          FUN <- plan_list$name[j]
          # add_tryCatch?
          tryCatch(G$children[[i]]$get_older(G = G, E = E), error = function(e) {
            message(paste(sprintf("[%d th agent] \n", i), e))
            return(NULL)
          })

          if(add_tryCatch){
            act_FUN_text <- paste(
              "lapply(sample(", text_select_FUN, "), function(i) {",
              "tryCatch(", "G$", agent, "[[i]]$", FUN, "(G = G, E = E)",",",
              "error = function(e){",
              "message(paste(",
              "sprintf('[%d th agent] \n', i), e))",
              "\n",
              "return(NULL)",
              "}",
              ")",
              "})"
            )
          }else{
            act_FUN_text <- paste0(
              "lapply(sample(", text_select_FUN, "), function(i){G$", agent, "[[i]]$", FUN,"(G = G, E = E)})")
          }  #----add_tryCatch?----//
          parse(text = act_FUN_text)[[1]]
        },
        "global_FUN" = {
          FUN <- plan_list$name[j]
          if(add_tryCatch){
            FUN_text <- paste0("tryCatch(G$", FUN, "(G = G, E = E)",
                               ", error = function(e){message(' ', e)",
                               "\n",
                               "return(NULL)})")
          }else{
            FUN_text <- paste0("G$", FUN,"(G = G, E = E)")
          }#---add_tryCatch?----//
          parse(text = FUN_text)[[1]]
        })
    } #-----Create FUN parts of update_FUN

    ## exclude NULL from parts of udpate_FUN
    update_FUN_parts <- Filter(Negate(is.null), update_FUN_parts)

    ## create an structure of update_FUN, and then combine parts
    update_FUN <- function(G = G, E = E){}
    body(update_FUN) <- as.call(append(as.list(body(update_FUN)),
                                       update_FUN_parts))
    ## announce the plan
    plan_print <- cbind(plan_list, colon = "")
    plan_print[!is.na(plan_print$agent_name),"colon"] <- ":"
    plan_print[is.na(plan_print$agent_name),"agent_name"] <- ""

    cat(c("update_FUN created from plan: ","\n",
          "  ", plan_print$agent_name[1], plan_print$name[1], plan_print$colon[1], sep = ""))
    if(NROW(plan_print)>1){
      for(i in 2:NROW(plan_print)){
        cat(" -> ", plan_print$agent_name[i],
            plan_print$colon[i], plan_print$name[i], sep = "")}
    }
    cat("\n", "\n") # change the line
  } #----end of |Based on plan|--------//

  # stop_FUN_name
  if(!is.null(stop_FUN_name)){
    stop_FUN <- G[[stop_FUN_name]]
    cat(paste0("stop_FUN:", stop_FUN_name))
  }else{
    ## based on time
    stopifnot("times must be a positive integer." = (is.numeric(times) && times >= 1))
    sim_time <- G$time + times
    stop_FUN <- function(G, E = NULL){G$time >= sim_time}
    cat(paste0("stop_FUN: ", "[stop times at ", sim_time, "]"))
    cat("\n")
  }

  # RDS_file_name
  if(saveRDS_inbetween == TRUE){
    stopifnot("RDS_file_name must be character scaler." = (is.character(RDS_file_name) && length(RDS_file_name)==1))
  }

  # seed
  if(is.null(seed)){
    seed <- as.integer(Sys.time())
  }
  set.seed(seed = seed)
  G$notes$seed <- seed

  # Ready to run
  cat(paste0("\n"))
  cat(paste0("Ready to run...", "\n"))

  # current time
  cat(paste0("  start time  : ", G$time, "\n"))

  # start_time
  start_time <- Sys.time()

  # implement the update (CORE PART!!)
  repeat {
    if (stop_FUN(G = G, E = E)){break}
    G$time <- G$time + 1
    cat(paste0("  current time: ", G$time, "\n"))
    update_FUN(G = G, E = E)
    if (save_log){G$.save()}
    if (saveRDS_inbetween){saveRDS(G, file = RDS_file_name)}
  }

  # end_time
  end_time <- Sys.time()

  # Finished
  cat("Finished.", "\n", "\n")

  # delete temporary log place holders
  if (length(G$log) > G$time) {
    for (i in seq(G$time + 1, length(G$log))) {
      G$log[[i]] <- NULL
    }
  }


  # implementation time（in seconds）
  time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
  # change the format of the time
  hours <- floor(time_taken / 3600)
  minutes <- floor((time_taken %% 3600) / 60)
  seconds <- floor(time_taken %% 60)
  milliseconds <- round((time_taken %% 1) * 1000)
  # 「hour:min:sec.msec」
  time_hms <- sprintf("%02d:%02d:%02d.%03d", hours, minutes, seconds, milliseconds)
  # show the implementation time
  cat(paste("Simulation took", time_hms, "(hh:mm:ss.mmm)", "\n"))

  # simulation timeをnoteに
  G$notes$simulation_took <- time_hms

  # return
  if(return_update_FUN==TRUE){
    G$notes$update_FUN_used <- deparse(body(update_FUN))
  }
  G
}

