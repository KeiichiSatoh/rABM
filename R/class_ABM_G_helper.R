
#===============================================================================
# summarise_value:
#===============================================================================

#' @importFrom stats sd
.summarise_value <- function(X, name){
  # field_typeがない場合
  if(is.null(attr(X, "field_type"))){
    ## リストの要素ごとに処理
    desc <- if(is.character(X)|is.factor(X)){
      prop <- prop.table(table(X))
      data.frame(name = paste0(name, "_", 1:length(prop), "*"), n = length(X), mean = as.vector(prop), sd = NA, min = 0, max = 1, dim = 1)
    }else if(is.matrix(X)|is.array(X)){
      data.frame(name = name, n = NA, mean = mean(X), sd = sd(X), min = min(X), max = max(X), dim = paste(dim(X), collapse = " "))
    }else{
      data.frame(name = name, n = length(X), mean = mean(X), sd = sd(X), min = min(X), max = max(X), dim = 1)
    }
  # field_typeがない場合---------
  }else{
  # field_typeがある場合
    desc <- switch(attr(X, "field_type"),
                   "net" = {data.frame(name = name, n = NA, mean = mean(X), sd = sd(X),
                                       min = min(X), max = max(X), dim = paste(dim(X), collapse = " "))},
                   "mat" = {data.frame(name = name, n = NA, mean = mean(X), sd = sd(X),
                                       min = min(X), max = max(X), dim = paste(dim(X), collapse = " "))},
                   "euc" = {
                     col_label <- colnames(X)
                     n <- NROW(X)
                     desc <- lapply(1:NCOL(X), function(i){
                       data.frame(name = paste0(name,"_", col_label[i]), n = n,
                                  mean = mean(X[[i]]), sd = sd(X[[i]]), min = min(X[[i]]), max = max(X[[i]]),
                                  dim = 1)})
                     do.call(rbind, desc)
                   },
                   "df"  = {
                     col_label <- colnames(X)
                     n <- NROW(X)
                     desc <- lapply(1:NCOL(X), function(i){
                       if(is.character(X[[i]])|is.factor(X[[i]])){
                         prop <- prop.table(table(X[[i]]))
                         data.frame(name = paste0(name, "_", 1:length(prop),"*"), n = length(X[[i]]), mean = as.vector(prop), sd = NA, min = 0, max = 1, dim = 1)
                       }else{
                         data.frame(name = paste0(name,"_", col_label[i]), n = n,
                                    mean = mean(X[[i]]), sd = sd(X[[i]]), min = min(X[[i]]), max = max(X[[i]]),
                                    dim = 1)
                       }})
                     do.call(rbind, desc)
                   },
                   "other" = {
                     if(!is.list(X)){
                       X <- list(X)
                       names(X) <- name
                     }
                     ## リストの要素ごとに処理
                     desc <- lapply(1:length(X), function(i){
                       if(is.character(X[[i]])|is.factor(X[[i]])){
                         prop <- prop.table(table(X[[i]]))
                         data.frame(name = paste0(names(X)[i], "_", 1:length(prop),"*"), n = length(X[[i]]), mean = as.vector(prop), sd = NA, min = 0, max = 1, dim = 1)
                       }else if(is.matrix(X[[i]])|is.array(X[[i]])){
                         data.frame(name = names(X)[i], n = NA, mean = mean(X[[i]]), sd = sd(X[[i]]), min = min(X[[i]]), max = max(X[[i]]), dim = paste(dim(X), collapse = " "))
                       }else{
                         data.frame(name = names(X)[i], n = length(X[[i]]), mean = mean(X[[i]]), sd = sd(X[[i]]), min = min(X[[i]]), max = max(X[[i]]), dim = 1)
                       }
                     })
                     do.call(rbind, desc)
                   }
    )
  } # field_typeがある場合
  # output
    desc
}
