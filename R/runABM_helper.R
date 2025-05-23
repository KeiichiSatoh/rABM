#-------------------------------------------------------------------------------
# .shape_selectFUN
#-------------------------------------------------------------------------------

.shape_selectFUN <- function(selectFUN_sbs = selectFUN_sbs){
  # selectFUN_sbs = NULLの場合：NULLを戻す
  if(is.null(selectFUN_sbs)){
    out <- list(selectFUN = NULL,
                selectFUN_label = NULL)
    return(out)
  }

  # selectFUN_sbs = "all"の場合：関数を付与
  if(selectFUN_sbs=="all"){
    ## "all"の場合
    selectFUN_shaped <- function(G = G){
      agent_n <- length(G$agent)
      sample(1:agent_n)}
    selectFUN_label <- "all"
  }else if(selectFUN_sbs == "one"){
    ## "one"の場合
    selectFUN_shaped <- function(G = G){
      agent_n <- length(G$agent)
      sample(1:agent_n, size = 1)}
    selectFUN_label <- "one"
  }else if(is.call(selectFUN_sbs)){
    ## callの場合
    selectFUN_name <- call_name(selectFUN_sbs)
    if(selectFUN_name=="function"){
      ### Callのうち無名関数の場合
      selectFUN_shaped <- eval(selectFUN_sbs)
      current_args <- formals(selectFUN_shaped)
      current_args[which(names(current_args)=="G")] <- NULL
      formals(selectFUN_shaped) <- c(alist(G = G), current_args)
      selectFUN_label <- "anonym_selectFUN"
    }else{
      ### callのうちすでに存在する場合
      selectFUN_shaped <- get(selectFUN_name)
      current_args <- formals(selectFUN_shaped)
      selectFUN_args_override <- call_args(selectFUN_sbs)
      for(i in 1:length(selectFUN_args_override)){
        current_args[which(names(current_args)==names(selectFUN_args_override)[i])] <- selectFUN_args_override[i]
      }
      current_args[which(names(current_args)=="G")] <- NULL
      formals(selectFUN_shaped) <- c(alist(G = G), current_args)
      selectFUN_label <- deparse(selectFUN_sbs)
    }
  }else if(is.name(selectFUN_sbs)){
    ## nameの場合
    stopifnot("selectFUN does not exists in the environment" = exists(as.character(selectFUN_sbs)))
    stopifnot("selectFUN is not the class of function" = is.function(get(as.character(selectFUN_sbs))))
    selectFUN_shaped <- get(selectFUN_sbs)
    current_args <- formals(selectFUN_shaped)
    current_args[which(names(current_args)=="G")] <- NULL
    formals(selectFUN_shaped) <- c(alist(G = G), current_args)
    selectFUN_label <- as.character(selectFUN_sbs)
  }else{
    stop("Incorrect input to selectFUN")
  }
  out <- list(selectFUN = selectFUN_shaped,
              selectFUN_label = selectFUN_label)
  out
}

#-------------------------------------------------------------------------------
# .shape_stopFUN
#-------------------------------------------------------------------------------

.shape_stopFUN <- function(stopFUN_sbs = stopFUN_sbs, init_time){
  # stopFUN_sbs = NULLの場合：1に変更
  if(is.null(stopFUN_sbs)){
    stopFUN_sbs <- 1
    warning("stopFUN is set to be 1")
  }
  # 場合わけして処理
  if(is.numeric(stopFUN_sbs)){
    # numericの場合：
    times <- stopFUN_sbs
    names(times) <- "times"
    stopifnot("If being numeric, stopFUN must be greater than 0" = times > 0)
    stopFUN_shaped <- function(){(G$time - init_time) >= times}
    formals(stopFUN_shaped) <- c(alist(G = G), init_time = init_time, times)
    stopFUN_label <- paste0("run_", times, "times")
  }else if(is.call(stopFUN_sbs)){
    ## callの場合
    stopFUN_name <- call_name(stopFUN_sbs)
    if(stopFUN_name=="function"){
      ### Callのうち無名関数の場合
      stopFUN_shaped <- eval(stopFUN_sbs)
      current_args <- formals(stopFUN_shaped)
      current_args[which(names(current_args)=="G")] <- NULL
      formals(stopFUN_shaped) <- c(alist(G = G), current_args)
      stopFUN_label <- "anonym_stopFUN"
    }else{
      ### callのうちすでに存在する場合
      stopFUN_shaped <- get(stopFUN_name)
      current_args <- formals(stopFUN_shaped)
      stopFUN_args_override <- call_args(stopFUN_sbs)
      for(i in 1:length(stopFUN_args_override)){
        current_args[which(names(current_args)==names(stopFUN_args_override)[i])] <- stopFUN_args_override[i]
      }
      current_args[which(names(current_args)=="G")] <- NULL
      formals(stopFUN_shaped) <- c(alist(G = G), current_args)
      stopFUN_label <- deparse(stopFUN_sbs)
    }
  }else if(is.name(stopFUN_sbs)){
    ## nameの場合
    stopifnot("stopFUN does not exists in the environment" = exists(as.character(stopFUN_sbs)))
    if(is.numeric(get(as.character(stopFUN_sbs)))){
      # numericの場合：
      times <- get(as.character(stopFUN_sbs))
      names(times) <- "times"
      stopifnot("If being numeric, stopFUN must be greater than 0" = times > 0)
      stopFUN_shaped <- function(){(G$time - init_time) >= times}
      formals(stopFUN_shaped) <- c(alist(G = G), init_time = init_time, times)
      stopFUN_label <- paste0("run_", times, "times")
    }else if(is.function(get(as.character(stopFUN_sbs)))){
      stopFUN_shaped <- get(stopFUN_sbs)
      current_args <- formals(stopFUN_shaped)
      current_args[which(names(current_args)=="G")] <- NULL
      formals(stopFUN_shaped) <- c(alist(G = G), current_args)
      stopFUN_label <- as.character(stopFUN_sbs)
    }
  }else{
    stop("Incorrect input to stopFUN")
  }
  out <- list(stopFUN = stopFUN_shaped,
              stopFUN_label = stopFUN_label)
  out
}


#-------------------------------------------------------------------------------
# .shape_updateFUN(revised)
#--------------------------------------------------------------------------------

.shape_updateFUN <- function(updateFUN_sbs = updateFUN_sbs){
  # updateFUN_sbs = NULLの場合：通常のRUNに変更
  if(is.null(updateFUN_sbs)){
    updateFUN_shaped <- function(G = G, selected_agent = NULL){
      lapply(X = selected_agent, function(X){
        tryCatch({G$agent[[X]]$.f(G = G)},
                 error = function(e){
                   message("ERROR occured in time ", G$time, " with agent ", X, ".", "\n")
                   return(NULL)
                 })
    })
    }
    updateFUN_label <- "default"
  }else if(is.call(updateFUN_sbs)){
    # その他のユーザー指定の場合
    ## callの場合
    updateFUN_name <- call_name(updateFUN_sbs)
    if(updateFUN_name=="function"){
      ### Callのうち無名関数の場合
      updateFUN_shaped <- eval(updateFUN_sbs)
      update_args <- formals(updateFUN_shaped)
      current_args[which(names(current_args)=="G")] <- NULL
      formals(updateFUN_shaped) <- c(alist(G = G), current_args)
      updateFUN_label <- "anonym_updateFUN"
    }else{
      ### callのうちすでに存在する場合
      updateFUN_shaped <- get(updateFUN_name)
      current_args <- formals(updateFUN_shaped)
      updateFUN_args_override <- call_args(updateFUN_sbs)
      for(i in 1:length(updateFUN_args_override)){
        current_args[which(names(current_args)==names(updateFUN_args_override)[i])] <- updateFUN_args_override[i]
      }
      current_args[which(names(current_args)=="G")] <- NULL
      formals(updateFUN_shaped) <- c(alist(G = G), current_args)
      updateFUN_label <- deparse(updateFUN_sbs)
    }
  }else if(is.name(updateFUN_sbs)){
    ## nameの場合
    stopifnot("updateFUN does not exists in the environment" = exists(as.character(updateFUN_sbs)))
    stopifnot("updateFUN is not function" = is.function(get(as.character(updateFUN_sbs))))
    updateFUN_shaped <- get(updateFUN_sbs)
    current_args <- formals(updateFUN_shaped)
    current_args[which(names(current_args)=="G")] <- NULL
    formals(updateFUN_shaped) <- c(alist(G = G), current_args)
    updateFUN_label <- as.character(updateFUN_sbs)
  }else{
    stop("Incorrect input to updateFUN")
  }
  out <- list(updateFUN = updateFUN_shaped,
              updateFUN_label = updateFUN_label)
  out
}


#-------------------------------------------------------------------------------
# .body_insert
#-------------------------------------------------------------------------------

.body_insert <- function(FUN, expr = expression(), after = 1, block){
  body(FUN) <- as.call(append(as.list(body(FUN))[[block]], expr, after = after))
  FUN
}


#-------------------------------------------------------------------------------
# .construct_do_repeat
#--------------------------------------------------------------------------------

.construct_do_repeat <- function(save_log = save_log){
  # ひな形を作る
  do_repeat <- function(G, stopFUN, stopFUN_args,
                        selectFUN, selectFUN_args, updateFUN, updateFUN_args){
    repeat{
      # 時間を更新
      G$time <- G$time + 1
      cat(paste0(G$time, " "))
      # update
      do.call(updateFUN, args = updateFUN_args)
      if(do.call(stopFUN, stopFUN_args)){
        break
      }
    }
  }

  # アウトプット
  do_repeat
}
