#' @title Modify a Set of ABM_Agent Objects
#' @description
#' #' This function modifies internal fields or structure of agent objects
#' of class \code{ABM_Agent}. It allows adding or deleting agents,
#' renaming, copying, replacing, or deleting specific fields,
#' and adding various types of components (attributes, methods, active bindings).
#'
#' For modifying the top-level structure of an ABM model (e.g., adding or renaming the agent set itself),
#' use [modify_G()] instead.
#'
#' @details
#' This function is intended to modify the internal structure of agents (objects of class `ABM_Agent`). It can be used either directly on a list of agents or via a field (typically `"agents"`) stored inside an `ABM_G` object.
#'
#' **Important distinction from `modify_G()`:**
#' While `modify_G()` operates on the structure of the `ABM_G` object itself—including adding or removing fields like `stage`, `global_FUN`, or even the entire `agents` field—`modify_agents()` assumes that the `agents` field already exists and focuses on modifying its contents.
#'
#' Use `modify_agents()` when you want to:
#' \itemize{
#'   \item Add or remove attributes, methods, or active bindings to/from each agent
#'   \item Rename or copy existing fields inside each agent
#'   \item Replace values in a specific field across agents
#' }
#'
#' Use `modify_G()` when you want to:
#' \itemize{
#'   \item Add, replace, or delete the `agents` field itself
#'   \item Modify top-level components of the ABM system (e.g., `stage`, `global_FUN`, etc.)
#' }
#'
#' For a detailed explanation of operations available via the `method` argument, see below:
#'
#' \describe{
#'   \item{\code{"add_agent"}}{Adds one or more new `ABM_Agent` objects to the list. Duplicate IDs will be reassigned.}
#'   \item{\code{"delete_agent"}}{Remove agents by position in `agents` list.}
#'   \item{\code{"add_attr_df"}}{Adds new attributes from a vector or data.frame. Each row must correspond to one agent.}
#'   \item{\code{"add_act_FUN"}}{Adds action functions. Each will receive `G` and `E` as arguments and be prefixed internally with `self <- self`.}
#'   \item{\code{"add_active_binding"}}{Adds active bindings. The input must be a named list of functions.}
#'   \item{\code{"add_other_attrs"}}{Adds arbitrary named attributes as standard fields (not active bindings or functions).}
#'   \item{\code{"rename"}}{Renames a field inside each agent. The field is deleted and re-added under a new name using the correct method.}
#'   \item{\code{"replace"}}{Replaces a field with new values. Accepts vectors, data.frames, or lists depending on the field type.}
#'   \item{\code{"copy"}}{Duplicates a field under a new name. Field type is preserved (attribute, binding, or method).}
#'   \item{\code{"delete"}}{Removes a field from all agents.}
#' }
#'
#' @param G An object of class `ABM_G`. If supplied, the agents will be extracted from its field (specified by `G_agent_name`).
#' @param G_agent_name A string giving the name of the field in `G` that stores the agents (e.g., `"agents"`). Required if `G` is provided.
#' @param new_obj The object(s) to add or use for replacement. The structure depends on the `method`.
#' @param method A string specifying the modification method. One of:
#' \code{"add_agent"}, \code{"add_attr_df"}, \code{"add_act_FUN"}, \code{"add_active_binding"},
#' \code{"add_other_attrs"}, \code{"rename"}, \code{"replace"}, \code{"copy"}, \code{"delete"}.
#' @param field_name The name of the field to rename, replace, copy, or delete.
#' @param new_field_name The new name to assign when renaming or copying a field.
#' @param delete_agent_posit Integer vector specifying the positions of agents to delete when `method = "delete_agent"`.
#' @param agents A list of `ABM_Agent` objects. Used if `G` is not supplied.
#' @param deep_clone Logical. If `TRUE` (default), the function operates on a deep clone of the object(s).
#'
#' @return If `G` is supplied, returns a modified `ABM_G` object. Otherwise, returns a modified list of `ABM_Agent` objects.
#'
#' @seealso [modify_G()] for modifying the `ABM_G` object itself.
#' @export
#' @examples
#' # Create agents and ABM system
#' agents <- init_agents(n = 3, attr_df = data.frame(age = c(11, 12, 13)))
#' G <- setABM(agents = agents)
#'
#' # Add new agents
#' new_agents <- init_agents(n = 2, attr_df = data.frame(age = c(14, 15)))
#' G <- modify_agents(G = G, G_agent_name = "agents", new_obj = new_agents,
#'                    method = "add_agent")
#'
#' # Add new attribute
#' G <- modify_agents(G = G, G_agent_name = "agents",
#'                    new_obj = data.frame(height = c(100, 110, 120, 130, 140)),
#'                    method = "add_attr_df")
#'
#' # Rename a field
#' G <- modify_agents(G = G, G_agent_name = "agents", field_name = "age",
#'                    new_field_name = "age_years", method = "rename")
#'
#' # Replace a field
#' G <- modify_agents(G = G, G_agent_name = "agents", field_name = "age_years",
#'                    new_obj = c(101, 111, 121, 131, 141), method = "replace")
#'
#' # Delete a field
#' G <- modify_agents(G = G, G_agent_name = "agents", field_name = "age_years",
#'                    method = "delete")
#'
#' # Delete the second and third agent
#' G <- modify_agents(G = G, G_agent_name = "agents", method = "delete_agent",
#'                    delete_agent_posit = c(2, 3))
#'
#' # Modify agents directly (without ABM_G)
#' agents <- modify_agents(agents = agents,
#'                         new_obj = list(flag = c(TRUE, FALSE, TRUE)),
#'                         method = "add_other_attrs")

modify_agents <- function(
    G = NULL,
    G_agent_name = NULL,
    new_obj = NULL,
    method = c("add_agent",
               "delete_agent",
               "add_attr_df",
               "add_act_FUN",
               "add_active_binding",
               "add_other_attrs",
               "rename",
               "replace",
               "copy",
               "delete"),
    field_name = NULL,
    new_field_name = NULL,
    delete_agent_posit = NULL,
    agents = NULL,
    deep_clone = TRUE){
  # substitute(sbs)
  new_obj_sbs <- substitute(new_obj)
  # match method argument
  method <- match.arg(method)

  # check the G input
  if(!is.null(G)){
    stopifnot("G must be class of 'ABM_G'" = class(G)[1] == "ABM_G")
    stopifnot("The 'G_agent_name' must be provided" = !is.null(G_agent_name))
    stopifnot("The 'G_agent_name' must be a character vector of lenth 1." = length(G_agent_name)==1 && is.character(G_agent_name))
    stopifnot("No matched agent new_obj to 'G_agent_name' found in 'G'." = G_agent_name %in% ls(G))
    if(!is.null(agents)){
      message("Note: 'agents' was provided but will be ignored because 'G' is also supplied.")
    }
    if(deep_clone){
      G <- G$clone(deep = TRUE)
    }
    agents <- G[[G_agent_name]]
  }else{
  # check the 'agents' input
    stopifnot("Either 'agents' or 'G' must be supplied." = !is.null(agents))
    stopifnot("All class of 'agents' must be class of 'ABM_Agent'" =
                all(unlist(lapply(agents, function(p){class(p)[1] == "ABM_Agent"}))))
    if(deep_clone){
      agents <- lapply(agents, function(p){p$clone(deep = TRUE)})
    }
  }

  # agent_n
  agent_n <- length(agents)

  # check the field_name
  if(method %in% c("rename", "replace", "delete", "copy")){
    stopifnot("'field_name' must be supplied for the chosen 'method'." = !is.null(field_name))
    stopifnot("'field_name' must be a character vector of length 1." = length(field_name)==1 && is.character(field_name))
  }

  # check the "new_field_name"
  if(method %in% c("rename", "copy")){
    stopifnot("'new_field_name' must be supplied for the chosen 'method'." = !is.null(new_field_name))
  }

  # print the message if 'new_field_name' is supplied to "replace"
  if(method=="replace" && !is.null(new_field_name)){
    message("Note: 'new_field_name' will be used as the name for the replaced field.")
  }

  # check the adequacy of "new_field_name"
  if(method %in% c("rename", "replace", "copy") && !is.null(new_field_name)){
    stopifnot("'new_field_name' must be supplied for the chosen 'method'." = !is.null(new_field_name))
    stopifnot("'new_field_name' must be a character vector of length 1." = length(new_field_name)==1 && is.character(new_field_name))
  }

  # preparation for rename, replace or copy
  if(method %in% c("rename", "replace", "copy")){
    # pre-define the function to judge the method
    judge_adequate_method <- function(agents, field_name){
      field_type <- agents[[1]]$.__enclos_env__$private$field_type()[field_name]
      is_active_binding <- field_name %in% names(agents[[1]]$.__enclos_env__$.__active__)
      if(field_type == "function"){
        "add_act_FUN"
      }else{
        if(is_active_binding){
          "add_active_binding"
        }else{
          "add_other_attrs"
        }
      }
    }
    # predefine the function to get the adequate field
    get_adequate_field <- function(agents, field_name, next_method){
      new_obj <- switch(next_method,
                        "add_other_attrs" = {
                          list(lapply(1:length(agents), function(i){
                            agents[[i]][[field_name]]
                          }))
                        },
                        "add_active_binding" = {
                          list(agents[[1]]$.__enclos_env__$.__active__[[field_name]])
                        },
                        "add_act_FUN" = {
                          list(lapply(1:length(agents), function(i){
                            agents[[i]][[field_name]]
                          }))
                        })
      # output
      new_obj
    }
    # get the next_method
    next_method <- judge_adequate_method(agents = agents, field_name = field_name)
    # switch by the current method
    # Note: After switch(), the method is updated to 'next_method' for actual execution
    switch(method,
      "rename" = {
        new_obj <- get_adequate_field(agents = agents, field_name = field_name, next_method = next_method)
        names(new_obj) <- new_field_name
        # delete the current field
        lapply(1:length(agents), function(i){
          agents[[i]]$.remove_field(name = field_name)
        })
        # new_obj_sbs
        new_obj_sbs <- new_field_name
      },
      "replace" = {
        # retrieve the current field
        current_field <- agents[[1]][[field_name]]
        # check the object for replace
        if(!is.data.frame(new_obj) && is.list(new_obj)){
          stopifnot("The 'new_obj' must match the number of agents when the 'new_obj' is 'list'. Be sure to set the first element of the list must be the same as the numebr of agents." = length(new_obj)==agent_n)
        }
        if(is.data.frame(new_obj)){
          if(NCOL(new_obj) > 1){
            message("Note: Only the first column of 'new_obj' (data.frame) will be used for replacement.")
          }
          new_obj <- list(new_obj[,1])
        }else{
          new_obj <- list(new_obj)
        }
        names(new_obj) <- field_name
        # check the adequacy
        if(length(new_obj[[1]][[1]]) != length(current_field)){
          warning("The element of 'new_obj' does not match the current field.")
        }
        stopifnot("The length of the 'new_obj' must be the number of agents." = length(new_obj[[1]])==agent_n)

        # delete the current field
        lapply(1:length(agents), function(i){
          agents[[i]]$.remove_field(name = field_name)
        })
        # new_obj_sbs
        new_obj_sbs <- field_name
      },
      "copy" = {
        new_obj <- get_adequate_field(agents = agents, field_name = field_name, next_method = next_method)
        names(new_obj) <- new_field_name
        # new_obj_sbs
        new_obj_sbs <- new_field_name
      })
    # replace the method with next_method
    method <- next_method
  } #------preparation for rename, replace or copy------//

  # switch
  switch(method,
         "add_agent" = {
           # check if 'new_obj' are supplied
           stopifnot("The 'new_obj' must be supplied for this method." = !is.null(new_obj))
           # check if all elements in new_obj is class "ABM_Agent"
           stopifnot("All objects in 'new_obj' must be class 'ABM_Agent'" =
                       all(unlist(lapply(new_obj, function(p){class(p)[1]=="ABM_Agent"}))))
           # check if each agent in 'new_obj' has the same field names
           stopifnot("Each agent must have the same set of field names" =
                       all(unlist(lapply(new_obj, function(p){all(ls(new_obj[[1]])==ls(p))}))))
           # check if 'agents' and 'new_obj' has the same field name.
           stopifnot("'agents' and the agents in 'new_obj' must have the same set of field names" =
                       all(ls(agents[[1]])==ls(new_obj[[1]])))
           # modify ID if they are duplicate
           if(any(unlist(lapply(agents, function(p){p$ID})) %in% unlist(lapply(new_obj, function(q){q$ID})))){
             agent_max_ID <- max(unlist(lapply(agents, function(p){p$ID})))
             for(i in 1:length(new_obj)){
               new_obj[[i]]$ID <- agent_max_ID + i
               names(new_obj)[i] <- paste0("ID", agent_max_ID + i)
             }
             message("Note: Some IDs in 'new_obj' were duplicated with existing agents. New IDs have been automatically assigned.")
           }
           # add
           agents <- c(agents, new_obj)
         },#---"add_agent"---//
         "delete_agent" = {
           stopifnot("Provide 'delete_agent_posit'." = !is.null(delete_agent_posit))
           stopifnot("some of the agents that matchs 'delete_agent_posit' do not exist." = any(delete_agent_posit %in% 1:agent_n))
           agent_label <- names(agents)
           remain_agent_posit <- setdiff(1:agent_n, delete_agent_posit)
           new_agent_list <- vector("list", length(remain_agent_posit))
           names(new_agent_list) <- agent_label[remain_agent_posit]
           for(p in 1:length(remain_agent_posit)){
             new_agent_list[[p]] <- agents[[remain_agent_posit[p]]]
           }
           # overwrite agents
           agents <- new_agent_list
         }, #---"delete_agent"--//
         "add_attr_df" = {
           # shape new_obj
           new_obj <- .shape_agent_attr(attr = new_obj, attr_sbs = new_obj_sbs)
           # check if the nrow match agents
           stopifnot("'NROW(new_obj)' must match the number of agents." = length(agents) == nrow(new_obj))
           # add new objects
           lapply(1:agent_n, function(i){
             lapply(1:length(new_obj[i,]), function(j){
               agents[[i]]$.add_field(name = colnames(new_obj)[j],
                                      value = new_obj[i,j])
             })
           })
         }, #-----"add_attr_df"----//
         "add_act_FUN" = {
           # shape the input
           act_FUN_list <- .shape_act_FUN(act_FUN = new_obj, act_FUN_sbs = new_obj_sbs, n = agent_n)
           # shape the formals
           act_FUN_list <- lapply(act_FUN_list, function(FUN_vec){
             lapply(FUN_vec, function(FUN){
               # すでにユーザーが誤ってGを引数に書いていたらひとまず消す
               current_formals <- formals(FUN)
               current_formals[which(names(current_formals)=="G")|which(names(current_formals)=="E")] <- NULL
               formals(FUN) <- c(alist(G = G, E = E), current_formals) # G=G, E = Eを足す
               # self <- selfを1行目に足す
               if(body(FUN)[[2]]!="self <- self"){
                 body(FUN) <- as.call(append(as.list(body(FUN)), expression(self <- self), after=1))
               }
               FUN
             })
           })
           # add
           act_names <- names(act_FUN_list)
           lapply(1:agent_n, function(i){
             lapply(1:length(act_FUN_list), function(p){
               agents[[i]]$.add_method(name = act_names[p], method = act_FUN_list[[p]][[i]])
             })
           })
         }, #------"add_act_FUN"-----//
         "add_active_binding" = {
           new_obj_list <- .shape_active_binding_field_agent(active_binding_field = new_obj,
                                                             active_binding_field_sbs = new_obj_sbs)
           obj_name <- names(new_obj_list)
           lapply(1:agent_n, function(i){
             lapply(1:length(obj_name), function(p){
               agents[[i]]$.add_active_binding(name = obj_name[p], FUN = new_obj_list[[p]])
             })
           })
         },  #------"add_active_binding"----//
         "add_other_attrs" = {
           ## check the input
           stopifnot("'new_obj' for this method must be a list." = is.list(new_obj) & !is.data.frame(new_obj))
           ## 中身の値の確認
           lapply(new_obj, function(X){
             stopifnot("The length of each element within 'new_obj' must be the number of agents." = agent_n == length(X))
             if(is.function(X)){
               stop("Elements passed through 'new_obj' cannot be functions.")
             }
           })
           ## shape the labels
           new_obj_character <- as.character(new_obj_sbs)[-1]
           new_obj_name <- sapply(new_obj_character, function(X){
             parsed_X <- parse(text = X)[[1]]
             if(is.symbol(parsed_X)){
               as.character(X)
             }else{
               return(NA)
             }
           })
           new_obj_label <- names(new_obj)
           if(is.null(new_obj_label)){
             new_obj_label <- new_obj_name
           }else{
             new_obj_label[new_obj_label==""] <- new_obj_name[new_obj_label==""]
           }
           new_obj_label[is.na(new_obj_label)] <- paste0("Z", 1:length(new_obj_label[is.na(new_obj_label)]))
           names(new_obj) <- new_obj_label
           # add
           new_obj_name <- names(new_obj)
           n_new_obj <- length(new_obj_name)
           lapply(1:agent_n, function(i){
             lapply(1:n_new_obj, function(p){
               agents[[i]]$.add_field(name = new_obj_name[p], value = new_obj[[p]][[i]])
             })
           })
         }, #-------"add_other_attrs"---------//
         "delete" = {
           lapply(1:agent_n, function(i){
             agents[[i]]$.remove_field(name = field_name)
           })
         } #-------"delete"-------------------//
  ) #---switch-------------//

  # Change the output in accordance to the input
  if(!is.null(G)){
    G <- modify_G(G, field_name = G_agent_name, method = "replace", new_obj = agents, deep_clone = deep_clone)
    return(G)
  }else{
    return(agents)
  }
}


