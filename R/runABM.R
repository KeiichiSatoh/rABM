#' @title Run the ABM simulation
#' @description
#' \code{runABM} runs a simulation using the \code{ABM_G} object.
#' The simulation is driven by agent behaviors and various user-defined
#' configurations.
#'
#' @param G An object of class \code{ABM_G}, representing the simulation environment.
#' @param schedule A character vector specifying the sequence of functions to
#' execute during each simulation step. Defaults to \code{NULL}.
#' @param remove_field A character vector of field names to remove from
#' the \code{ABM_G} object. Defaults to \code{NULL}.
#' @param rename_field A named character vector where names are
#' new field names and values are the current field names.
#' Defaults to \code{NULL}.
#' @param keep_field A character vector specifying fields to retain.
#' Defaults to \code{NULL}.
#' @param update_FUN_name The name of the update function to use,
#' as a character scalar. If \code{NULL}, an update function
#'  is generated from the specification provided in \code{schedule}.
#'  Defaults to \code{NULL}.
#' @param stop_FUN_name The name of the stopping function to use,
#' as a character scalar. If \code{NULL}, the simulation stops after
#' a specified number of steps defined in \code{times}. Defaults to \code{NULL}.
#' @param times An integer specifying the number of simulation steps to run.
#' Used if \code{stop_FUN_name} is not provided. Defaults to \code{1}.
#' @param save_log Logical; if \code{TRUE}, logs each simulation step.
#' Defaults to \code{FALSE}.
#' @param add_tryCatch Logical; if \code{TRUE}, wraps function calls
#' with \code{tryCatch} to handle errors gracefully. Defaults to \code{TRUE}.
#' @param return_update_FUN Logical; if \code{TRUE},
#' returns the update function along with the \code{ABM_G} object.
#' Defaults to \code{FALSE}.
#' @param saveRDS_inbetween Logical; if \code{TRUE},
#' saves the \code{ABM_G} object as an RDS file at each step.
#' Defaults to \code{FALSE}.
#' @param temp_E Logical; whether to create a temporary environment \code{E}
#'  so that the \code{E} object can be referred to during the simulation.
#'  Defaults to \code{TRUE}.
#' @param RDS_file_name A character scalar specifying the file name
#' for saving the RDS file, used when \code{saveRDS_inbetween} is \code{TRUE}.
#' Defaults to \code{"G_temp.rds"}.
#' @param log_place_holder Logical; if both \code{save_log} and \code{log_place_holder}
#' is \code{TRUE}, it creates a temporary copy of the log so that it will
#' be overwritten during the simulation.
#' @param n_log_place_holder An positive integer specifying the number of
#' log_place_holder to create. Default is \code{1000}. This specification is used
#' only when \code{times} is not defined.
#' @return A modified \code{ABM_G} object reflecting the simulation results.
#' @importFrom rlang parse_expr new_environment
#' @import R6
#' @export
#' @details
#' \code{runABM} runs a simulation using the \code{ABM_G} object.
#' Users can customize the simulation by defining the update functions,
#' stopping criteria, and scheduling behavior.
#'
#' This function is designed with flexibility in mind,
#' enabling users to perform exploratory analyses efficiently.
#'
#' Specifically, it supports the following major features:
#'
#' - Editing field names
#' - Configuring updates for each simulation step
#' - Managing the storage of updated objects
#'
#' Detailed explanations for each feature are provided below.
#'
#' ### Setting Field Modification Arguments
#' These options are intended to allow users to predefine fields for
#' various scenarios in \code{G}, making it easier to select fields according
#' to the model. This eliminates the need to change the field names
#' referenced by update functions for each model,
#' enabling smoother simulations. The key points are as follows:
#'
#' Field modifications are applied in the following order:
#' \code{rename_field}, \code{keep_field}, \code{remove_field}.
#' A deep copy of \code{G} is created before these operations,
#' ensuring that the original \code{G} object remains unchanged.
#'
#' - \code{rename_field}: Provide a named character vector in the format
#'  \code{c("new_field_name" = "old_field_name")}.
#'  For example, to rename \code{field_setting2} to \code{field},
#'  use \code{rename_field = c("field" = "field_setting2")}.
#'  You can rename multiple fields, such as
#'  \code{rename_field = c("field1" = "field_setting1_2", "field2" = "field_setting2_2")}.
#'
#' - \code{keep_field}: Specify the fields to retain in the \code{G} object
#' during simulation. This reduces the size of the output \code{G} object.
#' For example, to retain only \code{field1} and \code{field2},
#' use \code{keep_field = c("field1", "field2")}.
#'
#' - \code{remove_field}: Specify the fields to remove in the \code{G} object
#' during simulation. For example, to remove \code{field3} and \code{field4},
#' use \code{remove_field = c("field3", "field4")}.
#'
#' Users can specify multiple options simultaneously,
#' but doing so may lead to errors. It is generally recommended to use only
#' one option at a time.
#'
#' ### \code{update_FUN} and \code{schedule}
#' Update functions must be preconfigured in \code{G},
#' such as \code{update_FUN}, \code{select_FUN}, \code{global_FUN},
#' \code{agent_FUN}, or \code{partial_update_FUN_body}.
#' You can check what is available in \code{G} by printing it.
#'
#' Users can specify one \code{update_FUN} field name categorized
#' as \code{update_FUN} by providing its name as a character string
#' to \code{update_FUN_name}.
#'
#' If \code{update_FUN_name} is not used, users can specify a
#' sequence of function names as a character vector in \code{schedule}.
#' This creates an \code{update_FUN} internally, executed in the specified order.
#'  Note that \code{select_FUN} must precede \code{agent_FUN};
#'  if not specified, a default \code{select_FUN} selecting all agents is
#'  automatically added.
#'
#' If neither \code{update_FUN_name} nor \code{schedule} is provided,
#' \code{runABM} automatically selects the first suitable \code{update_FUN}
#'  candidate from \code{G$.field_list()}.
#'
#' To handle unexpected errors during simulations,
#' \code{runABM} by default wraps \code{update_FUN} with \code{tryCatch},
#' which returns \code{NULL} on errors, allowing the simulation to continue.
#' While convenient, this may make it harder to detect fundamental issues
#' in the update function.
#'
#' To inspect the exact \code{update_FUN} used,
#' set \code{return_update_FUN = TRUE}.
#' The update_FUN used in the simulation is stored into `G$notes$update_FUN_used`.
#'
#' ### Log Storage
#' If intermediate states are of interest,
#' set \code{save_log = TRUE} to log each update step.
#' Note that logging may increase the simulation runtime.
#'
#' To mitigate memory allocation issues caused by creating new objects
#' during each update, \code{runABM} pre-allocates memory for logs
#' by initializing placeholders in \code{G$log}.
#' By default, 1000 placeholders are created
#'  (modifiable via \code{n_log_place_holder}). To disable this behavior, set \code{log_place_holder = FALSE}.
#'
#' ### Notes on Saving RDS Files
#' For long-running simulations, consider to use \code{saveRDS_inbetween = TRUE}
#' to save the \code{G} object as an RDS file after each update step.
#' Specify the file name using \code{RDS_file_name}.
#' Note that saving files increases simulation runtime.
#'
#' ### Simulation Runtime
#' The runtime of \code{runABM} is displayed in the console upon completion.
#' This excludes preprocessing time for field modifications
#' and \code{update_FUN} construction.
#' The runtime is stored into `G$note$simulation_took`.
#'
#' @examples
#' # setup agents
#' agent_attr <- data.frame(age = c(0, 1, 2))
#' get_older <- function() { self$age <- self$age + 1 }
#' agents <- init_agent(attr_df = agent_attr, act_FUN = get_older)
#' # Initialize the ABM environment
#' G <- setABM(agents = agents)
#'
#' # Run simulation for 5 steps
#' result <- runABM(G = G, schedule = "get_older", times = 5)
#' # Check each agent's age
#' result$agents

runABM <- function(G,
                   schedule = NULL,
                   remove_field = NULL,
                   rename_field = NULL,
                   keep_field = NULL,
                   update_FUN_name = NULL,
                   stop_FUN_name = NULL,
                   times = 1,
                   save_log = FALSE,
                   add_tryCatch = TRUE,
                   return_update_FUN = FALSE,
                   saveRDS_inbetween = FALSE,
                   temp_E = TRUE,
                   RDS_file_name = "G_temp.rds",
                   log_place_holder = TRUE,
                   n_log_place_holder = 1000){
  # Gをdeep clone
  G <- G$clone(deep = TRUE)
  # GがABM_Gかをチェック
  stopifnot("G must be the class of ABM_G" = class(G)[1]=="ABM_G")

  # rename_fieldに対処
  if(!is.null(rename_field)){
    stopifnot("rename_field must be a character vector." = (is.vector(rename_field) & is.character(rename_field)))
    stopifnot("rename_field must be inputted as c('new_name' = 'old.name'." = !is.null(names(rename_field)))
    stopifnot("some elements in rename_field lacks name." = any(names(rename_field)=="")==FALSE)
    stopifnot("Meta data cannot be renamed." = any(rename_field %in% c("time", "log","notes"))==FALSE)
    rename_field_df <- data.frame(old_name = rename_field, new_name = names(rename_field))
    field_list <- G$.field_list()
    for(i in 1:NROW(rename_field_df)){
      if(field_list$category[rename_field_df[i,"old_name"]] %in% c("global_FUN", "select_FUN","stop_FUN","update_FUN")){
        G$.add_FUN(name = rename_field_df[i, "new_name"],
                   FUN = G[[rename_field_df[i,"old_name"]]],
                   FUN_category = field_list$category[rename_field_df[i,"old_name"]]) # copy
        G$.remove_field(name = rename_field_df[i, "old_name"])
      }else if(field_list$category[rename_field_df[i, "old_name"]]=="agent"){
        G$.add_agents(name = rename_field_df[i, "new_name"],
                      agents = G[[rename_field_df[i, "old_name"]]])
        G$.remove_field(name = rename_field_df[i, "old_name"])
      }else{
        G$.add_stage(name = rename_field_df[i, "new_name"],
                     stage = G[[rename_field_df[i, "old_name"]]])
        G$.remove_field(field_name = rename_field_df[i, "old_name"])
      }
    }
    # 処理をプリント
    cat("Renamed field: ")
    for(i in 1:NROW(rename_field_df)){
      cat(rename_field_df$old_name[i], " -> ", rename_field_df$new_name[i], "; ", sep = "")
    }
    cat("\n")
  } #----rename_field対処ここまで

  # keep_fieldに対処
  if(!is.null(keep_field)){
    stopifnot("keep_field must be a character vector." = (is.vector(keep_field) & is.character(keep_field)))
    stopifnot("keep_field and remove_field cannot be set at the same time." = !is.null(remove_field))
    field_list <- G$.field_list()
    remove_field <- setdiff(field_list$name, keep_field)
    for(X in remove_field){
      G$.remove_field(field_name = X)
    }
    cat("Kept field:", keep_field, sep = " ")
    cat("Removed field:", remove_field, sep = " ")
    cat("\n")
  } #-----keep_field対処ここまで

  # remove_fieldに対処
  if(!is.null(remove_field)){
    stopifnot("remove_field must be a character vector." = (is.vector(remove_field) & is.character(remove_field)))
    stopifnot("Meta data cannot be removed." = any(remove_field %in% c("time", "log","notes"))==FALSE)
    for(X in remove_field){
      G$.remove_field(field_name = X)
    }
    cat("Removed field:", remove_field, sep = " ")
    cat("\n")
  } #-----remove_field対処ここまで

  # E(tempral_environment)を新たに作る
  if(temp_E){
    E <- rlang::new_environment()
  }else{
    E <- NULL
  }
  on.exit(rm(E, envir = environment()), add = TRUE)

  # この名前の処理まですべて行われた段階で、改めて現状のfield_listを取得する
  field_list <- G$.field_list()

  # update_FUN_nameとscheduleの状況に合わせて対処
  if(is.null(update_FUN_name) && is.null(schedule)){
    ## (1) update_FUN_nameとscheduleが両方投入されていない場合
    update_FUN_candid <- field_list[field_list$category %in% c("act_FUN","global_FUN", "update_FUN"), ]
    if(NROW(update_FUN_candid)==0){
      ### (1)-1: 一つもupdate_FUNの候補がない場合
      stop("No update_FUN candidate found in G.")
    }else{
      ### (1)-2: update_FUNの候補がある場合、一番上のものを使用する
      update_FUN_decided <- update_FUN_candid[1, ]
      ### 決めたupdate_FUNをアナウンス
      cat("update_FUN:", update_FUN_decided$name,"\n")
      ###
      if(update_FUN_decided$category=="act_FUN"){
        #### 決めたupdate_FUNがact_FUNの場合
        update_FUN <- function(G = G, E = E){self <- self}
        #### tryCatchによる場合分け
        if(add_tryCatch){
          update_FUN_text <- parse(text = paste0(
            "lapply(sample(1:length(G$", update_FUN_decided$agent_name, ")), function(i){",
            "tryCatch(","G$", update_FUN_decided$agent_name, "[[i]]$", update_FUN_decided$name, "(G = G, E = E)",
            ", error = function(e){
            message(paste0('error occured for agent ', i))
            return(NULL)})", "})"
          ))[[1]]
        }else{
          update_FUN_text <- parse(text = paste0(
            "lapply(sample(1:length(G$", update_FUN_decided$agent_name, ")), function(i){G$",
            update_FUN_decided$agent_name, "[[i]]$", update_FUN_decided$name, "(G = G, E = E)})"
          ))[[1]]
        }
        #### bodyに貼り付ける
        body(update_FUN) <- as.call(append(as.list(body(update_FUN)),
                                           update_FUN_text))
      }else{
        #### 決めたupdate_FUNがglobal_FUNもしくはupdate_FUNの場合
        update_FUN <- function(G = G, E = E){}
        #### tryCatchで分岐
        if(add_tryCatch){
          update_FUN_text <- parse(
            text = paste0("tryCatch(G$", update_FUN_decided$name, "(G = G, E = E)",
                          ", error = function(e){message(paste0(",
                          "'error occured for ",
                          update_FUN_decided$name,"'))
                          return(NULL)})"
            ))[[1]]
        }else{
          update_FUN_text <- parse(text = paste0("G$", update_FUN_decided$name, "(G = G, E = E)"))[[1]]
        }
        #### bodyに貼り付ける
        body(update_FUN) <- as.call(append(as.list(body(update_FUN)),
                                           update_FUN_text))
      }
    }
    #-----update_FUN_nameもscheduleのいずれも投入されていない場合
  }else if(!is.null(update_FUN_name)){
    ## (2) update_FUN_nameが投入されている場合
    stopifnot("update_FUN_name must be a character with the length of one." = (is.character(update_FUN_name) && length(update_FUN_name)==1))
    stopifnot("Inputted 'update_FUN_name' is not update_FUN type." = field_list$category[field_list$name == update_FUN_name]=="update_FUN")
    #### 決めたupdate_FUNにselfを組み合わせる
    update_FUN <- function(G = G, E = E){}
    ## tryCatchをするか
    if(add_tryCatch){
      update_FUN_text <- parse(
        text = paste0("tryCatch(G$", update_FUN_name, "(G = G, E = E)",
                      ", error = function(e){message(paste0(",
                      "'error occured for ",
                      update_FUN_name,"'))
                          return(NULL)})"
        ))[[1]]
    }else{
      update_FUN_text <- parse(text = paste0("G$", update_FUN_name, "(G = G, E = E)"))[[1]]
    }
    #### bodyに貼り付ける
    body(update_FUN) <- as.call(append(as.list(body(update_FUN)),
                                       update_FUN_text))
    ## update_FUNをアナウンス
    cat("update_FUN:", update_FUN_name, "\n")
    # -----update_FUN_nameが投入されている場合
  }else{
    # (3) scheduleに基づく場合---------------------------------
    ## scheduleのフォーマットを確認
    stopifnot("schedule must be a vector." = (is.vector(schedule) & !is.list(schedule)))
    stopifnot("schedule must be character string(s)." = all(is.character(schedule)))

    ## 実際に存在する名前か確認
    check_FUN_name <- setdiff(schedule, field_list$name)
    if(length(check_FUN_name)>0){
      stop(paste("The following name in schedule does not exist:", check_FUN_name))
    }

    # scheduleで指定されたFUNのみからなるリストを作成
    FUN_posit <- unlist(lapply(schedule, function(X){which(X==field_list$name)}))
    schedule_list <- field_list[FUN_posit, ]

    ## scheduleにglobal_FUN, act_FUN, select_FUN, partial_update_FUN_body以外が入っている場合にはストップ
    stopifnot("'schedule' must be either global_FUN, act_FUN, select_FUN, or partial_update_FUN_body." = schedule_list$category %in% c("global_FUN","act_FUN","select_FUN","partial_update_FUN_body"))

    ## agentのアクションの前にselect_FUNがあるか確認し、ない場合には当該agent全員を選ぶようにscheduleを書き直す
    if(schedule_list$category[1]=="act_FUN"){
      schedule_list <- rbind(
        data.frame(agent_name = schedule_list$agent_name[1],
                   name = "[select_all]",
                   category = "select_FUN",
                   type = "function",
                   active_binding = FALSE),
        schedule_list)
    }
    temp <- vector("list", length = NROW(schedule_list))
    temp[[1]] <- schedule_list[1, ]
    if(nrow(schedule_list)>=2){
      for(i in 2:nrow(schedule_list)){
        if(schedule_list$category[i]=="act_FUN" & schedule_list$category[i-1]!="select_FUN"){
          temp[[i]] <- rbind(
            data.frame(agent_name = schedule_list$agent_name[i],
                       name = "[select_all]",
                       category = "select_FUN",
                       type = "function",
                       active_binding = FALSE),
            schedule_list[i, ])
        }else{
          temp[[i]] <- schedule_list[i, ]
        }
      }
    }
    schedule_list <- do.call(rbind, temp)

    ## select_FUNの位置をチェック
    select_FUN_posit <- which(schedule_list$category == "select_FUN")
    posit_check <- unlist(lapply(select_FUN_posit, function(posit){
      tryCatch(schedule_list[posit + 1, "category"]=="act_FUN",
               error = function(e){FALSE})}))
    stopifnot("All select_FUN must be followed by act_FUN." = all(posit_check)==TRUE)

    ## scheduleに基づきupdate_FUNの要素を作成する
    update_FUN_parts <- vector(mode = "list", NROW(schedule_list))
    for(j in 1:length(update_FUN_parts)){
      update_FUN_parts[[j]] <- switch(
        schedule_list$category[j],
        "select_FUN" = {NULL},
        "partial_update_FUN_body" = {
          G[[schedule_list$name[j]]]
        },
        "act_FUN" = {
          # select_FUN部の前処理
          if(schedule_list$name[j-1]=="[select_all]"){
            agent <- schedule_list$agent_name[j-1]
            text_select <- paste0("1:length(G$",agent,")")
          }else{
            FUN <- schedule_list$name[j-1]
            ## tryCatchによる分岐
            if(add_tryCatch){
              text_select <- paste0(
                "tryCatch(",
                "G$", FUN, "(G = G, E = E)",
                ", error = function(e){
                         message(paste0('error occured for ", FUN, ".'))
                return(NULL)})")
            }else{
              text_select <- paste0("G$", FUN, "(G = G, E = E)")
            }
          } #--select_FUN部の前処理
          # act_FUNの処理
          agent <- schedule_list$agent_name[j]
          FUN <- schedule_list$name[j]
          # tryCatchによる分岐
          if(add_tryCatch){
            act_FUN_text <- paste0(
              "lapply(sample(", text_select, "), function(i){",
              "tryCatch(","G$", agent, "[[i]]$", FUN, "(G = G, E = E)",
              ", error = function(e){
            message(paste0('error occured for agent i for ", FUN,"'))
            return(NULL)})", "})"
            )
          }else{
            act_FUN_text <- paste0(
              "lapply(sample(", text_select, "), function(i){G$", agent, "[[i]]$", FUN,"(G = G, E = E)})")
          }  #----add_tryCatchによる処理
          parse(text = act_FUN_text)[[1]]
        },
        "global_FUN" = {
          FUN <- schedule_list$name[j]
          if(add_tryCatch){
            FUN_text <- paste0("tryCatch(G$", FUN, "(G = G, E = E)",
                               ", error = function(e){message(paste0(",
                               "'error occured for ", FUN,"'))
                          return(NULL)})")
          }else{
            FUN_text <- paste0("G$", FUN,"(G = G, E = E)")
          }#---add_tryCatch処理
          parse(text = FUN_text)[[1]]
        })
    } #-----FUN要素を構成する

    ## 得られたパーツのうち、NULLの部分を外す
    update_FUN_parts <- Filter(Negate(is.null), update_FUN_parts)

    ## 基となるupdate_FUNを作成し、パーツを組み合わせる
    update_FUN <- function(G = G, E = E){}
    body(update_FUN) <- as.call(append(as.list(body(update_FUN)),
                                       update_FUN_parts))
    ## スケジュールについて知らせる
    schedule_print <- cbind(schedule_list, colon = "")
    schedule_print[!is.na(schedule_print$agent_name),"colon"] <- ":"
    schedule_print[is.na(schedule_print$agent_name),"agent_name"] <- ""

    cat(c("update_FUN created from schedule: ","\n",
          "  ", schedule_print$agent_name[1], schedule_print$name[1], schedule_print$colon[1], sep = ""))
    if(NROW(schedule_print)>1){
      for(i in 2:NROW(schedule_print)){
        cat(" -> ", schedule_print$agent_name[i],
            schedule_print$colon[i], schedule_print$name[i], sep = "")}
    }
    cat("\n", "\n") # 次の表示と重ならないように行替え
  } #----scheduleに基づく場合

  # stop_FUN_name
  if(!is.null(stop_FUN_name)){
    stop_FUN <- G[[stop_FUN_name]]
    cat(paste0("stop_FUN:", stop_FUN_name))
  }else{
    ## timesに基づく
    stopifnot("times must be a positive integer." = (is.numeric(times) && times >= 1))
    sim_time <- G$time + times
    stop_FUN <- function(G, E = NULL){G$time >= sim_time}
    cat(paste0("stop_FUN: ", "[stop times at ", sim_time, "]"))
    cat("\n")
  }

  #
  # RDS_file_nameの処理
  if(saveRDS_inbetween == TRUE){
    stopifnot("RDS_file_name must be character scaler." = (is.character(RDS_file_name) && length(RDS_file_name)==1))
  }

  # log_place_holderの処理
  if(save_log){
    if(log_place_holder){
      temp <- list(G$log$t1)
      if(!is.null(times)){
        n_copy <- times
      }else{
        stopifnot("'n_log_place_holder' must be a positive integer." = n_log_place_holder > 0 & length(n_log_place_holder)==1)
        n_copy <- n_log_place_holder
      }
      copy <- rep(temp, n_copy)
      names(copy) <- paste0("t", (length(G$log)+1):(length(G$log) + n_copy))
      G$log <- c(G$log, copy)
    }
  }

  # Ready to run
  cat(paste0("\n"))
  cat(paste0("Ready to run...", "\n"))

  # current time
  cat(paste0("  start time  : ", G$time, "\n"))

  # start_timeを記録
  start_time <- Sys.time()

  # save_log = FALSEの場合
  if(save_log == FALSE & saveRDS_inbetween==FALSE){
    repeat{
      # 時間を更新
      G$time <- G$time + 1
      cat(paste0("  current time: ", G$time, "\n"), sep = "")
      # update
      update_FUN(G = G, E = E)
      # 終了条件を確認
      if(stop_FUN(G = G, E = E)){break}
    }
    #---save_log = Fここまで--------------
  }else if(save_log == TRUE & saveRDS_inbetween==FALSE){
    repeat{
      # 時間を更新
      G$time <- G$time + 1
      cat(paste0("  current time: ", G$time, "\n"), sep = "")
      # update
      update_FUN(G = G, E = E)
      # 現状を記録
      G$.save()
      # 終了条件を確認
      if(stop_FUN(G = G, E = E)){break}
    }
    #----save_log = Tここまで--------------
  }else if(save_log == FALSE & saveRDS_inbetween==TRUE){
    repeat{
      # 時間を更新
      G$time <- G$time + 1
      cat(paste0("  current time: ", G$time, "\n"), sep = "")
      # update
      update_FUN(G = G, E = E)
      # saveRDS
      saveRDS(G, file = RDS_file_name)
      # 終了条件を確認
      if(stop_FUN(G = G, E = E)){break}
    }
    #---save_log = Fここまで--------------
  }else if(save_log == TRUE & saveRDS_inbetween==TRUE){
    repeat{
      # 時間を更新
      G$time <- G$time + 1
      cat(paste0("  current time: ", G$time, "\n"), sep = "")
      # update
      update_FUN(G = G, E = E)
      # 現状を記録
      G$.save()
      # saveRDS
      saveRDS(G, file = RDS_file_name)
      # 終了条件を確認
      if(stop_FUN(G = G, E = E)){break}
    }
    #----save_log = Tここまで--------------
  }else{
    stop("The setting about 'save_log' and 'saveRDS_inbetween' seems to be wrong.")
  }

  # end_timeを記録
  end_time <- Sys.time()

  # Finished
  cat("Finished.", "\n", "\n")

  # 不要な、もしくは仮置き版のログを消去する
  if(length(G$log) > G$time){
    times <- 1:length(G$log)
    times <- times[(G$time+1):length(G$log)]
    for(i in times){
      G$log[[i]] <- NULL
    }
  }

  # 実行時間を計算（秒単位で取得）
  time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
  # 時、分、秒、ミリ秒に変換
  hours <- floor(time_taken / 3600)
  minutes <- floor((time_taken %% 3600) / 60)
  seconds <- floor(time_taken %% 60)
  milliseconds <- round((time_taken %% 1) * 1000)
  # ミリ秒までの時間を「時:分:秒.ミリ秒」の形式にフォーマット
  time_hms <- sprintf("%02d:%02d:%02d.%03d", hours, minutes, seconds, milliseconds)
  # 実行時間を表示
  cat(paste("Simulation took", time_hms, "(hh:mm:ss.mmm)", "\n"))

  # simulation timeをnoteに
  G$notes$simulation_took <- time_hms

  # リターン
  if(return_update_FUN==TRUE){
    G$notes$update_FUN_used <- update_FUN
  }
  G
}# 関数ここまで

