################################################################################
# .shape_agent
################################################################################

.shape_agent <- function(agents, agents_sbs){
  # NULLかどうかを判定
  if(is.null(agents_sbs)){
    return(NULL)
  }

  # オブジェクトラベル
  if(is.symbol(agents_sbs)){
    agents_label <- deparse(agents_sbs)
  }else{
    agents_label <- "agents"
  }

  # classを確認し、番号の場合にはagentを作成する
  agents <- if(!all(lapply(unlist(agents), function(x){class(x)[[1]]})=="ABM_Agent")){
    ## "ABM_Agent" classではない場合
    lapply(unlist(agents), function(y){
      if(is.numeric(y)){
        init_agent(n = y)
      }else{
        stop("agents must be either ABM_Agent class object or a positive integer.")
      }
    })
  }else{
    agents
  }

  # インプットが複数のエージェントタイプを含むものかで場合分け
  if(is.list(agents[[1]])==FALSE){
    agent_formatted <- list(agents)
  }else{
    agent_formatted <- agents
  }

  # 名前がついているかを確認
  if(is.null(names(agent_formatted))){
    ## 名前がついていない場合
    if(length(agent_formatted)==1){
      names(agent_formatted) <- agents_label
    }else{
      names(agent_formatted) <- paste0(agents_label, 1:length(agent_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(agent_formatted)=="")){
    stop("Put names to each agent object.")
  }

  # field_categoryを定義する
  field_category <- rep("agent", length(agent_formatted))
  names(field_category) <- names(agent_formatted)

  # アウトプット
  agent_formatted <- list(value = agent_formatted, category = field_category)
  agent_formatted
}


#---------------------------------------------------------
# .shape_stage
#---------------------------------------------------------

#' @import rlang
.shape_stage <- function(stage = NULL, stage_sbs = NULL){
  # NULLかどうかを判定
  if(is.null(stage_sbs)){
    return(NULL)
  }

  # オブジェクトラベル
  if(is.list(stage) & !is.data.frame(stage)){
    ## stageに投入されたのが二つ以上のリストだった場合
    stage_label <- as.character(stage_sbs)[-1]
    stage_label <- sapply(stage_label, function(X){
      if(exists(X)){
        X
      }else{
        ""
      }
    })
  }else{
    ## それ以外
    if(is.symbol(stage_sbs)){
      stage_label <- deparse(stage_sbs)
    }else{
      stage_label <- "stage"
    }
  }

  # リスト形式にすべて統一する
  if(!is.list(stage)|is.data.frame(stage)){
    stage_list <- list(stage)
  }else{
    stage_list <- stage
  }

  # 要素ごとにチェックをする
  stage_formatted <- lapply(stage_list, function(X){
    if(is.function(X)){
      stop("Do not assign a function directly to a stage. If you intend to create an active binding field, use the 'active_binding_field' argument instead.")
    }
    X
  })

  # 名前を処理する
  # listに名前がついていない場合
  if(is.null(names(stage_formatted))){
    if(length(stage_formatted)==1|length(stage_formatted)==length(stage_label)){
      names(stage_formatted) <- stage_label
    }else{
      names(stage_formatted) <- paste0(stage_label, 1:length(stage_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(stage_formatted)=="")){
    if(length(names(stage_formatted)[names(stage_formatted)==""])==1){
      names(stage_formatted)[names(stage_formatted)==""] <- "Y"
    }else{
      names(stage_formatted)[names(stage_formatted)==""] <- paste0("stage", 1:length(names(stage_formatted)==""))
    }
  }

  # field_categoryを定義する
  field_category <- rep("stage", length(stage_formatted))
  names(field_category) <- names(stage_formatted)

  # アウトプット
  stage_formatted <- list(value = stage_formatted, category = field_category)
  stage_formatted
}


#---------------------------------------------------------
# .shape_active_binding_field
#---------------------------------------------------------

#' @import rlang
.shape_active_binding_field <- function(
    active_binding_field = NULL,
    active_binding_field_sbs = NULL){

  # NULLかどうかを判定
  if(is.null(active_binding_field)){
    return(NULL)
  }

  # オブジェクトラベル
  if(is.symbol(active_binding_field_sbs)){
    active_binding_field_label <- deparse(active_binding_field_sbs)
  }else{
    active_binding_field_label <- "ABF"
  }

  # リスト形式にすべて統一する
  if(!is.list(active_binding_field)){
    active_binding_field <- list(active_binding_field)
  }
  active_binding_field_formatted <- active_binding_field

  ## すべての形をfunction型に揃える
  for(i in 1:length(active_binding_field_formatted)){
    if(is.character(active_binding_field_formatted[[i]])){
      parsed_FUN <- parse(text = active_binding_field_formatted[[i]])[[1]]
      if(is.name(parsed_FUN)){
        retrieved_FUN <- get(parsed_FUN)
        stopifnot("The 'active_binding' retrieved from the specified object must be a function." = is.function(retrieved_FUN))
        active_binding_field_formatted[[i]] <- retrieved_FUN
      }else if(is.call(parsed_FUN)){
        retrieved_FUN <- get(call_name(parsed_FUN))
        stopifnot("The 'active_binding' retrieved from the specified object must be a function." = is.function(retrieved_FUN))
        original_args <- formals(retrieved_FUN)
        parsed_args <- call_args(parsed_FUN)
        # からのFUNを作成し、formalsとbodyを付加する
        FUN <- function(){}
        original_args[names(parsed_args)] <- parsed_args
        body(FUN) <- body(retrieved_FUN)
        formals(FUN) <- original_args
        active_binding_field_formatted[[i]] <- FUN
      }
    }
  }

  # すべて関数型になっているか確認
  stopifnot("active_binding_field must be a function." = all(unlist(lapply(active_binding_field_formatted, is.function))))

  # 名前を処理する
  # listに名前がついていない場合
  if(is.null(names(active_binding_field_formatted))){
    if(length(active_binding_field_formatted)==1){
      names(active_binding_field_formatted) <- active_binding_field_label
    }else{
      names(active_binding_field_formatted) <- paste0(active_binding_field_label, 1:length(active_binding_field_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(active_binding_field_formatted)=="")){
    if(length(names(active_binding_field_formatted)[names(active_binding_field_formatted)==""])==1){
      names(active_binding_field_formatted)[names(active_binding_field_formatted)==""] <- "ABF"
    }else{
      names(active_binding_field_formatted)[names(active_binding_field_formatted)==""] <- paste0("ABF", 1:length(names(active_binding_field_formatted)==""))
    }
  }

  # field_categoryを定義する
  field_category <- rep("stage", length(active_binding_field_formatted))
  names(field_category) <- names(active_binding_field_formatted)

  # アウトプット
  active_binding_field_formatted <- list(value = active_binding_field_formatted,
                                         category = field_category)
  active_binding_field_formatted
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
  # FUNが関数かclosureの場合にはそのままFUNを返す
  if(is.function(FUN)|rlang::is_closure(FUN)){
    FUN
  }else if(is.character(FUN)){
    # FUNがcharacter型の場合には当該関数を環境から取得する
    if(exists(FUN)){
      ## FUNが環境に存在するならば、そちらから取得
      FUN <- get(FUN)
      stopifnot("The retrieved object in global_FUN is not a function." = is.function(FUN))
      FUN
    }else{
      ## FUNが存在しないならば、callとして取得できないか試す
      parsed_FUN <- parse(text = FUN)[[1]]
      func_name <- rlang::call_name(parsed_FUN)
      if(exists(func_name)){
        ### func_nameが存在している場合、処理を行う
        func_args <- rlang::call_args(parsed_FUN)
        assign("temp_func", get(func_name))
        #### 元の関数のデフォルト値を新しい値に置き換える
        if(length(func_args) > 0){
          for(k in 1:length(func_args)){
            formals(temp_func)[names(func_args[k])] <- func_args[k]
          }
        }
        ### temp_funcを戻す
        temp_func
      }else{
        ### func_nameが存在しない場合
        stop("FUN does not exists in the environment.")
      }
    }
    # FUNがcharacter型の場合：ここまで----------------------
  }else{
    stop("The retrieved object is not a function.")
  }
}


#-------------------------------------------------------------------------------
# .shape_global_FUN
#-------------------------------------------------------------------------------
#' @import rlang

.shape_global_FUN <- function(global_FUN, global_FUN_sbs){
  if(is.null(global_FUN)){
    return(NULL)
  }

  # オブジェクトラベル
  if(is.symbol(global_FUN_sbs)){
    global_FUN_label <- deparse(global_FUN_sbs)
  }else{
    global_FUN_label <- "global_FUN"
  }

  # リストかどうか
  if(!is.list(global_FUN)){
    global_FUN_list <- list(global_FUN)
  }else{
    global_FUN_list <- global_FUN
  }

  # それぞれのFUNについて、functionを取得する
  global_FUN_formatted <- lapply(global_FUN_list, function(X){
    .get_FUN(FUN = X)
  })

  # act_FUN_listの中身において、引数にGを追加する
  global_FUN_formatted <- lapply(global_FUN_formatted, function(FUN){
    # すでにユーザーが誤ってGを引数に書いていたらひとまず消す
    current_formals <- formals(FUN)
    current_formals[which(names(current_formals)=="G")|which(names(current_formals)=="E")] <- NULL
    formals(FUN) <- c(alist(G = G, E = E), current_formals) # G=G,E=Eを足す
    FUN
  })

  # listに名前がついていない場合
  if(is.null(names(global_FUN_formatted))){
    if(length(global_FUN_formatted)==1){
      names(global_FUN_formatted) <- global_FUN_label
    }else{
      names(global_FUN_formatted) <- paste0(global_FUN_label, 1:length(global_FUN_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(global_FUN_formatted)=="")){
    stop("Put names to each global_FUN object.")
  }

  # field_categoryを定義する
  field_category <- rep("global_FUN", length(global_FUN_formatted))
  names(field_category) <- names(global_FUN_formatted)

  # アウトプット
  global_FUN_formatted <- list(value = global_FUN_formatted,
                               category = field_category)
  global_FUN_formatted
}


#-------------------------------------------------------------------------------
# .shape_select_FUN
#-------------------------------------------------------------------------------
#' @import rlang

.shape_select_FUN <- function(select_FUN, select_FUN_sbs){
  if(is.null(select_FUN)){
    return(NULL)
  }

  # オブジェクトラベル
  if(is.symbol(select_FUN_sbs)){
    select_FUN_label <- deparse(select_FUN_sbs)
  }else{
    select_FUN_label <- "select_FUN"
  }

  # リストかどうか
  if(!is.list(select_FUN)){
    select_FUN_list <- list(select_FUN)
  }else{
    select_FUN_list <- select_FUN
  }

  # それぞれのFUNについて、functionを取得する
  select_FUN_formatted <- lapply(select_FUN_list, function(X){
    .get_FUN(FUN = X)
  })

  # act_FUN_listの中身において、引数にGを追加する
  select_FUN_formatted <- lapply(select_FUN_formatted, function(FUN){
    # すでにユーザーが誤ってGを引数に書いていたらひとまず消す
    current_formals <- formals(FUN)
    current_formals[which(names(current_formals)=="G")|which(names(current_formals)=="E")] <- NULL
    formals(FUN) <- c(alist(G = G, E = E), current_formals) # G=G,E=Eを足す
    FUN
  })

  # listに名前がついていない場合
  if(is.null(names(select_FUN_formatted))){
    if(length(select_FUN_formatted)==1){
      names(select_FUN_formatted) <- select_FUN_label
    }else{
      names(select_FUN_formatted) <- paste0(select_FUN_label, 1:length(select_FUN_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(select_FUN_formatted)=="")){
    stop("Put names to each select_FUN object.")
  }

  # field_categoryを定義する
  field_category <- rep("select_FUN", length(select_FUN_formatted))
  names(field_category) <- names(select_FUN_formatted)

  # アウトプット
  select_FUN_formatted <- list(value = select_FUN_formatted,
                               category = field_category)
  select_FUN_formatted
}

#-------------------------------------------------------------------------------
# .shape_stop_FUN
#-------------------------------------------------------------------------------

.shape_stop_FUN <- function(stop_FUN, stop_FUN_sbs){
  if(is.null(stop_FUN)){
    return(NULL)
  }

  # オブジェクトラベル
  if(is.symbol(stop_FUN_sbs)){
    stop_FUN_label <- deparse(stop_FUN_sbs)
  }else{
    stop_FUN_label <- "stop_FUN"
  }

  # リストかどうか
  if(!is.list(stop_FUN)){
    stop_FUN_list <- list(stop_FUN)
  }else{
    stop_FUN_list <- stop_FUN
  }

  # それぞれのFUNについて、functionを取得する
  stop_FUN_formatted <- lapply(stop_FUN_list, function(X){
    .get_FUN(FUN = X)
  })

  # act_FUN_listの中身において、引数にGを追加する
  stop_FUN_formatted <- lapply(stop_FUN_formatted, function(FUN){
    # すでにユーザーが誤ってGを引数に書いていたらひとまず消す
    current_formals <- formals(FUN)
    current_formals[which(names(current_formals)=="G")|which(names(current_formals)=="E")] <- NULL
    formals(FUN) <- c(alist(G = G, E = E), current_formals) # G=G,E=Eを足す
    FUN
  })

  # listに名前がついていない場合
  if(is.null(names(stop_FUN_formatted))){
    if(length(stop_FUN_formatted)==1){
      names(stop_FUN_formatted) <- stop_FUN_label
    }else{
      names(stop_FUN_formatted) <- paste0(stop_FUN_label, 1:length(stop_FUN_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(stop_FUN_formatted)=="")){
    stop("Put names to each stop_FUN object.")
  }

  # field_categoryを定義する
  field_category <- rep("stop_FUN", length(stop_FUN_formatted))
  names(field_category) <- names(stop_FUN_formatted)

  # アウトプット
  stop_FUN_formatted <- list(value = stop_FUN_formatted,
                             category = field_category)
  stop_FUN_formatted
}

#-------------------------------------------------------------------------------
# .shape_update_FUN
#-------------------------------------------------------------------------------


.shape_update_FUN <- function(update_FUN, update_FUN_sbs){
  if(is.null(update_FUN)){
    return(NULL)
  }

  # オブジェクトラベル
  if(is.symbol(update_FUN_sbs)){
    update_FUN_label <- deparse(update_FUN_sbs)
  }else{
    update_FUN_label <- "update_FUN"
  }

  # リストかどうか
  if(!is.list(update_FUN)){
    update_FUN_list <- list(update_FUN)
  }else{
    update_FUN_list <- update_FUN
  }

  # それぞれのFUNについて、functionを取得する
  update_FUN_formatted <- lapply(update_FUN_list, function(X){
    .get_FUN(FUN = X)
  })

  # act_FUN_listの中身において、引数にGを追加する
  update_FUN_formatted <- lapply(update_FUN_formatted, function(FUN){
    # すでにユーザーが誤ってGを引数に書いていたらひとまず消す
    current_formals <- formals(FUN)
    current_formals[which(names(current_formals)=="G")|which(names(current_formals)=="E")] <- NULL
    formals(FUN) <- c(alist(G = G, E = E), current_formals) # G=G,E=Eを足す
    FUN
  })

  # listに名前がついていない場合
  if(is.null(names(update_FUN_formatted))){
    if(length(update_FUN_formatted)==1){
      names(update_FUN_formatted) <- update_FUN_label
    }else{
      names(update_FUN_formatted) <- paste0(update_FUN_label, 1:length(update_FUN_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(update_FUN_formatted)=="")){
    stop("Put names to each update_FUN object.")
  }

  # field_categoryを定義する
  field_category <- rep("update_FUN", length(update_FUN_formatted))
  names(field_category) <- names(update_FUN_formatted)

  # アウトプット
  update_FUN_formatted <- list(value = update_FUN_formatted,
                             category = field_category)
  update_FUN_formatted
}

#-------------------------------------------------------------------------------
# .shape_partial_update_FUN_body
#-------------------------------------------------------------------------------

.shape_partial_update_FUN_body <- function(
    partial_update_FUN_body,
    partial_update_FUN_body_sbs){
  if(is.null(partial_update_FUN_body)){
    return(NULL)
  }

  # オブジェクトラベル
  if(is.symbol(partial_update_FUN_body_sbs)){
    partial_update_FUN_body_label <- deparse(partial_update_FUN_body_sbs)
  }else{
    partial_update_FUN_body_label <- "partial_update_FUN_body"
  }

  # リストかどうか
  if(!is.list(partial_update_FUN_body)){
    partial_update_FUN_body_list <- list(partial_update_FUN_body)
  }else{
    partial_update_FUN_body_list <- partial_update_FUN_body
  }

  # それぞれのリスト要素について整形し、call型に変換する
  partial_update_FUN_body_formatted <- lapply(partial_update_FUN_body_list, function(X){
    if(is.expression(X)){
      X[[1]]
    }else if(is.character(X)){
      parse(text = X)[[1]]
    }else{
      stop("partial_update_FUN_body must be either an expression, a character string, or a list of these types.")
    }
  })

  # listに名前がついていない場合
  if(is.null(names(partial_update_FUN_body_formatted))){
    if(length(partial_update_FUN_body_formatted)==1){
      names(partial_update_FUN_body_formatted) <-partial_update_FUN_body_label
    }else{
      names(partial_update_FUN_body_formatted) <- paste0(partial_update_FUN_body_label, 1:length(partial_update_FUN_body_formatted))
    }
  }

  # 一部にしか名前がつけられていないということはないか確認
  if(any(names(partial_update_FUN_body_formatted)=="")){
    stop("Put names to each partial_update_FUN_body object.")
  }

  # field_categoryを定義する
  field_category <- rep("partial_update_FUN_body", length(partial_update_FUN_body_formatted))
  names(field_category) <- names(partial_update_FUN_body_formatted)

  # アウトプット
  partial_update_FUN_body_formatted <- list(value = partial_update_FUN_body_formatted,
                                            category = field_category)
  partial_update_FUN_body_formatted
}

