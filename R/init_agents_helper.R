
#-------------------------------------------------------------------------------
# .shape_agent_attr
#-------------------------------------------------------------------------------
#' Format agent attribute into data.frame
#'
#' This function standardizes agent attributes passed in as either vector or data.frame
#' and returns a data.frame with a single column named appropriately.
#'
#' @param attr A vector or data.frame representing an attribute.
#' @param attr_sbs Symbol of the attribute (from substitute()).
#' @return A data.frame or NULL.
#' @keywords internal

.shape_agent_attr <- function(attr, attr_sbs){
  # 場合分け
  if(is.null(attr)){
    return(NULL)
  }else if(is.vector(attr) & !is.list(attr)){
    df <- as.data.frame(attr)
    if(is.symbol(attr_sbs)){
      colnames(df) <- deparse(attr_sbs)
    }else{
      colnames(df) <- "X"
    }
  }else if(is.data.frame(attr)){
    df <- attr
  }else{
    stop("attr must be either vector or data.frame.")
  }

  # リターン
  df
}

#-------------------------------------------------------------------------------
# .shape_act_FUN: 投入された関数基づいて、第一階層にアクション名、第二階層に、
# 各エージェントのアクションがネストされたリストを返す。
#
# 投入値がベクトルの場合には、各エージェントに関するものとみなす。もしも一つしかなければ、
# エージェント分コピーする。
# リストは、異なるアクションとみなす。
#
# 投入は、クロージャ―、関数、関数名の文字型、関数名の文字型に修正値が付いたものの
# 四つを受け入れる。
#
# リストの名前は、アクション名として扱う。名前がない場合には、act1, act2と順につける。
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------


.shape_act_FUN <- function(act_FUN, act_FUN_sbs, n){
  if(is.null(act_FUN)){
    return(NULL)
  }

  # 行動ルールの長さを測る
  n_act <- length(act_FUN)

  # 各行動のアウトプット用リストを作成する
  out <- vector("list", n_act)

  # リストかどうかを判定する
  if(!is.list(act_FUN)){
  ## リストではない場合--------
    # 単一行動ルール、アクターによる差異なしと判定できるため、そのように処理
    out[[1]] <- lapply(1:n, function(x){.get_act_FUN(FUN = act_FUN)})
  }else{
  ## リストの場合---------------
    ## 各リストの第一階層ごとに処理を行う
    for(p in 1:n_act){
      if(!is.list(act_FUN[[p]])){
        ### リストではない場合：アクターによる差異なし
        out[[p]] <- lapply(1:n, function(x){.get_act_FUN(FUN = act_FUN[[p]])})
      }else{
        ### リストである場合：アクターによる差異あり
        if(length(act_FUN[[p]])==1){
          #### リストの中身は1個のみ：人数分コピーする
          out[[p]] <- lapply(1:n, function(x){.get_act_FUN(FUN = act_FUN[[p]])})
        }else if(length(act_FUN[[p]])==n){
          #### リストの中身は人数分ある：各中身を張り付ける
          out[[p]] <- act_FUN[[p]]
        }else{
          stop("Length of the element within list of act_FUN must match n.")
        }
      }
    }
  }

  # 名前をoutの第一階層に付加する
  if(is.null(names(act_FUN))){
    ## namesがact_FUNについていない場合
    ## act_labelをsymbol名から取得する
    if(is.symbol(act_FUN_sbs)){
      act_label <- as.character(act_FUN_sbs)
    }else{
      act_label <- "act"
    }
    ## act_labelを付与する
    if(length(out)==1){
      ### outの長さが１の場合、actをつける
      names(out) <- act_label
    }else{
      ### outの長さが2以上の場合、act1から順に番号をつける
      names(out) <- paste0(act_label, 1:length(out))
    }
    ## namesがact_FUNについていない場合：ここまで--
  }else{
    ## namesがact_FUNについている場合
    stopifnot("Names must be provided for each list element." = length(names(act_FUN))==length(out))
    names(out) <- names(act_FUN)
  }

  # outに付与されているact_FUNが関数かどうかを確認する
  for(p in 1:length(out)){
    check <- all(unlist(lapply(1:n, function(i){is.function(out[[p]][[i]])})))
    if(check==FALSE){
      stop(paste0("Some of the objects nested under the ", p, "-th element of act_FUN are not functions."))
    }
  }

  # アウトプット
  out
}


#-------------------------------------------------------------------------------
# .get_act_FUN: FUNとして入ってきたものをすべて関数型で返す
#-------------------------------------------------------------------------------

.get_act_FUN <- function(FUN){
  # FUNが関数かclosureの場合にはそのままFUNを返す
  if(is.function(FUN)|rlang::is_closure(FUN)){
    FUN
  }else if(is.character(FUN)){
    # FUNがcharacter型の場合には当該関数を環境から取得する
    if(exists(FUN)){
      ## FUNが環境に存在するならば、そちらから取得
      FUN <- get(FUN)
      stopifnot("The retrieved object in act_FUN is not a function." = is.function(FUN))
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
    stop("The retrieved object in act_FUN is not a function.")
  }
}

