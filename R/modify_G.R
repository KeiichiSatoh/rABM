#' @title Modify fields in an ABM_G object
#' @description
#' This function allows users to modify, rename, delete, or add fields
#'  (i.e., stage, global_FUN/select_FUN/update_FUN/stop_FUN, and active bindings)
#'  to an \code{ABM_G} object.
#'
#' @param G An object of class \code{ABM_G}, created by \code{setABM()}.
#' @param field_name A string indicating the name of the field to modify, delete, or rename.
#' @param method A string specifying the operation to perform.
#' One of \code{"add_agents"}, \code{"add_stage"}, \code{"add_global_FUN"}, \code{"add_select_FUN"},
#' \code{"add_stop_FUN"}, \code{"add_update_FUN"}, \code{"add_active_binding"},
#' \code{"rename"}, \code{"replace"}, or \code{"delete"}.
#' @param new_obj The object to assign to the field (e.g., a set of 'ABM_agents',
#' vector, matrix, data.frame, list or a function). Required for \code{"add_*"} and
#' \code{"replace"} methods.
#' @param new_field_name Optional. A string specifying the new name of the field,
#' used when \code{method = "rename"} or when renaming during \code{"replace"}.
#' @param deep_clone Logical. If \code{TRUE} (default),
#' the function will operate on a deep clone of \code{G} and return the modified
#' object. If \code{FALSE}, \code{G} will be modified in place and \code{NULL}
#' will be returned.
#' @return If \code{deep_clone = TRUE}, returns a modified \code{ABM_G} object.
#' If \code{FALSE}, returns \code{NULL} and modifies the object in place.
#'
#' @details
#' - For \code{method = "delete"}, the specified field will be removed.
#' - For \code{method = "rename"}, the field will be renamed.
#' - For \code{method = "replace"}, the specified field will be replaced,
#' optionally with a new name.
#' - For \code{method = "add_*"}, a new field will be added to the object.
#'
#' **Note**: If the user wants to modify the contents of \code{agents}, use
#' \code{modify_agents()} instead.
#'
#' @seealso [modify_agents()]
#'
#' @examples
#' G <- setABM(agents = 2, stage = data.frame(age = c(1, 2)),
#'             global_FUN = function(){NULL},
#'             select_FUN = function(){NULL},
#'             update_FUN = function(){NULL},
#'             stop_FUN = function(){TRUE},
#'             active_binding_field = function(){1})
#'
#' # Add a new stage
#' G2 <- modify_G(G, field_name = "stage2", method = "add_stage", new_obj = matrix(0, 2, 2))
#'
#' # Rename an existing field
#' G2 <- modify_G(G, field_name = "stage", method = "rename", new_field_name = "stage_renamed")
#'
#' # Replace a function
#' new_FUN <- function() print("new")
#' G2 <- modify_G(G, field_name = "global_FUN", method = "replace", new_obj = new_FUN)
#'
#' @export

modify_G <- function(G, field_name,
                     method = c(
                       "add_agents",
                       "add_stage",
                       "add_global_FUN", "add_select_FUN",
                       "add_stop_FUN","add_update_FUN","add_active_binding",
                       "rename",
                       "replace",
                       "delete"),
                     new_obj = NULL,
                     new_field_name = NULL,
                     deep_clone = TRUE){
  # check the input
  stopifnot("G must be class of 'ABM_G'" = class(G)[1] == "ABM_G")
  method <- match.arg(method)

  # check the conflict of the arguments
  if(method=="delete"){
    if(!is.null(new_obj)){
      warning("The 'new_obj' argument is ignored when method is 'delete'.")
    }
    if(!is.null(new_field_name)){
      warning("The 'new_field_name' argument is ignored when method is 'delete'.")
    }
  }else if(method=="rename"){
    stopifnot("The 'new_field_name' must be supplied when 'method' is 'rename'." = !is.null(new_field_name))
    stopifnot("The 'new_field_name' must be a character vector." = is.character(new_field_name))
    stopifnot("The 'new_field_name' must be oflength 1." = length(new_field_name) == 1)
    if(!is.null(new_obj)){
      warning("The input in 'new_obj' is ignored when 'method' is 'rename'.")
    }
  }else if(method=="replace"){
    stopifnot("The 'new_field_name' argument must be supplied when method is 'rename'." = !is.null(new_obj))
    if(!is.null(new_field_name)){
      message("Since 'new_field_name' is supplied, it will be used as the new field name.")
      stopifnot("'new_field_name' must be a character." = is.character(new_field_name))
      stopifnot("'new_field_name' must be a length of 1." = length(new_field_name) == 1)
    }
  }else{
    stopifnot("'new_obj' must be supplied to the selected 'method'." = !is.null(new_obj))
    if(!is.null(new_field_name)){
      warning("The 'new_field_name' argument is ignored.")
    }
  }

  # deep clone G (optional)
  if(deep_clone){
    G <- G$clone(deep = TRUE)
  }

  # When method = rename:
  if(method=="rename"){
    # check if such field exists
    stopifnot("No field exists that matches 'field_name'." = field_name %in% ls(G))
    # get field_category
    field_category <- G$.__enclos_env__$private$field_category[field_name]
    # Whether the new_obj is active_binding
    active_binding_names <- names(G$.__enclos_env__$.__active__)
    is_active_binding <- field_name %in% active_binding_names
    # retrieve and store new_obj
    new_obj <- G[[field_name]]
    # delete the field
    G$.remove_field(name = field_name)
    # overwrite the method
    method <- switch(field_category,
           "agent" = {"add_agents"},
           "stage" = {ifelse(is_active_binding, "add_active_binding", "add_stage")},
           "global_FUN" = {"add_global_FUN"},
           "select_FUN" = {"add_select_FUN"},
           "stop_FUN" = {"add_stop_FUN"},
           "update_FUN" = {"add_update_FUN"}
           )
    # overwrite field_name
    field_name <- new_field_name
  }else if(method=="replace"){
    # check if such field exists
    stopifnot("No field exists that matches 'field_name'." = field_name %in% ls(G))
    # get field_category
    field_category <- G$.__enclos_env__$private$field_category[field_name]
    # Whether the field is active_binding
    active_binding_names <- names(G$.__enclos_env__$.__active__)
    is_active_binding <- field_name %in% active_binding_names
    # delete the field
    G$.remove_field(name = field_name)
    # overwrite the method
    method <- switch(field_category,
                     "agent" = {"add_agents"},
                     "stage" = {ifelse(is_active_binding, "add_active_binding", "add_stage")},
                     "global_FUN" = {"add_global_FUN"},
                     "select_FUN" = {"add_select_FUN"},
                     "stop_FUN" = {"add_stop_FUN"},
                     "update_FUN" = {"add_update_FUN"}
    )
    if(!is.null(new_field_name)){
      field_name <- new_field_name
    }
  }

  # check if new_obj is provided
  if(method %in% c("add_agents", "add_global_FUN", "add_select_FUN", "add_stop_FUN", "add_update_FUN")){
    stopifnot("The 'new_obj' argument must be supplied for the chosen method" = !is.null(new_obj))
  }

  # check if new_obj is function and add formals of G and E for the relevant methods.
  if(method %in% c("add_global_FUN", "add_select_FUN", "add_stop_FUN", "add_update_FUN")){
    stopifnot("The 'new_obj' argument must be a function for the chosen method." = is.function(new_obj))
    # add G and E to formals
    current_formals <- formals(new_obj)
    current_formals[which(names(current_formals)=="G")|which(names(current_formals)=="E")] <- NULL
    formals(new_obj) <- c(alist(G = G, E = E), current_formals) # add G=G and E=E
  }

  # switch
  switch(method,
         "delete" = {
           # get field_category
           field_category <- G$.__enclos_env__$private$field_category[field_name]
           # stop if field_category is agent
           if(field_category=="agent"){
             stop("'modify_G' cannot be used to modify fields of category 'agent'. Please use 'modify_agents()' instead.")
           }
           # delete
           G$.remove_field(name = field_name)
         },
         "add_agents" = {
           G$.add_agents(name = field_name, agents = new_obj)
         },
         "add_stage" = {
           G$.add_stage(name = field_name, stage = new_obj)
         },
         "add_global_FUN" = {
           G$.add_FUN(name = field_name,
                      FUN = new_obj,
                      FUN_category = "global_FUN")
         },
         "add_select_FUN" = {
           G$.add_FUN(name = field_name,
                      FUN = new_obj,
                      FUN_category = "select_FUN")
         },
         "add_stop_FUN" = {
           G$.add_FUN(name = field_name,
                      FUN = new_obj,
                      FUN_category = "stop_FUN")
         },
         "add_update_FUN" = {
           G$.add_FUN(name = field_name,
                      FUN = new_obj,
                      FUN_category = "update_FUN")
         },
         "add_active_binding" = {
           G$.add_active_binding(name = field_name,
                                 FUN = new_obj)
         }
         )
  # return
  if(deep_clone){
    return(G)
  }else{
    return(NULL)
  }
}



