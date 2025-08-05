#' @title ABM_Agent Class
#' @description
#' The \code{ABM_Agent} class defines an agent used in agent-based modeling (ABM),
#' offering mechanisms for managing attributes, methods, and active bindings.
#'
#' @details
#' This R6-based class is intended for internal infrastructure and supports:
#' - Adding/removing agent fields, methods, and active bindings
#' - Dynamic modification and inspection of agent structure
#' - Customized field replacement and renaming
#'
#' While agents can be created via `ABM_Agent$new()`, users are advised to use
#' \code{\link{init_agents}} for batch initialization and validation.
#' Similarly, \code{\link{modify_agents}} provides safer field-level manipulation.
#'
#' Field and method names must be unique within the agent. Duplicates will raise an error.
#'
#' @import R6
#' @export

ABM_Agent <- R6::R6Class(
  "ABM_Agent", lock_objects = FALSE, cloneable = TRUE,
  public = list(
    #' @description
    #' Initializes a new agent with specified fields and methods.
    #' @param fields A named list of fields to initialize.
    #' @param methods A named list of methods (functions) to assign.
    #' @return A new \code{ABM_Agent} instance.
    #' @keywords internal
    initialize = function(fields = list(), methods = list()) {
      # set objects to fields
      for (name in names(fields)) {
        stopifnot("Do not use the same field name." = name %in% names(self)==FALSE)
        self[[name]] <- fields[[name]]
      }
      # set objects to methods
      for (name in names(methods)) {
        stopifnot("Do not use the same field name." = name %in% names(self)==FALSE)
        self[[name]] <- methods[[name]]
        environment(self[[name]]) <- self$.__enclos_env__
      }
    },

    #' @description
    #' Adds a new field to the agent.
    #' @param name A string indicating the name of the field.
    #' @param value The value to assign.
    #' @return None.
    #' @keywords internal
    .add_field = function(name, value){
      self[[name]] <- value
    },

    #' @description
    #' Adds a new method (function) to the agent.
    #' @param name Name to assign the method.
    #' @param method A function object to assign.
    #' @return None.
    #' @keywords internal
    .add_method = function(name, method) {
      # Set the method's environment to the agent's environment
      environment(method) <- self$.__enclos_env__
      self[[name]] <- method
    },

    #' @description
    #' Adds an active binding to the agent.
    #' @param name The name of the active binding.
    #' @param FUN A function defining the active binding behavior.
    #' @return None.
    #' @keywords internal
    .add_active_binding = function(name, FUN){
      self_env <- self$.__enclos_env__$self

      # If the name already exists, then delete it
      if (exists(name, envir = self_env, inherits = FALSE)) {
        if (bindingIsActive(name, self_env)) {
          remove(list = name, envir = self_env)
        } else {
          self$.remove_field(name)
        }
      }

      # attach environment
      environment(FUN) <- self$.__enclos_env__
      # set active_binding
      makeActiveBinding(name, FUN, self_env)
      # set into the environment of self
      FUN_list <- list(FUN)
      names(FUN_list) <- name
      self$.__enclos_env__$.__active__ <- c(self$.__enclos_env__$.__active__,
                                            FUN_list)
    },

    #' @description
    #' Removes a field (including active bindings or methods) from the agent.
    #' @param name The name of the field to remove.
    #' @return None.
    #' @keywords internal
    .remove_field = function(name){
      rm(list = name, envir = self)
      active_binding_names <- names(self$.__enclos_env__$.__active__)
      if(name %in% active_binding_names){
        self$.__enclos_env__$.__active__[[name]] <- NULL
      }
    },

    #' @description
    #' Replaces an existing field or method with a new value or function.
    #' @param name Name of the field to replace.
    #' @param value The new value or function to assign.
    #' @return None.
    #' @keywords internal
    .replace_field = function(name, value){
      # retrieve the type
      field_list <- self$.field_list()
      field_info <- field_list[which(field_list$name==name), ]
      # delete first
      self$.remove_field(name)
      # then add a new value
      if(field_info$category=="act_FUN"){
        self$.add_method(name = name, value)
      }else if(field_info$active_binding){
        self$.add_active_binding(name = name, FUN = value)
      }else{
        self[[name]] <- value
      }
    },

    #' @description
    #' Renames an existing field, method, or active binding.
    #' @param name Current name of the field.
    #' @param new_name New name to assign.
    #' @return None.
    #' @keywords internal
    .rename_field = function(name, new_name){
      field_list <- self$.field_list()
      field_info <- field_list[which(field_list$name == name), ]

      if (field_info$category == "act_FUN") {
        FUN <- self[[name]]
        self$.add_method(name = new_name, method = FUN)
      } else if (field_info$active_binding) {
        FUN <- self$.__enclos_env__$.__active__[[name]]
        self$.add_active_binding(name = new_name, FUN = FUN)
      } else {
        self[[new_name]] <- self[[name]]
      }
      self$.remove_field(name = name)
    },

    #' @description
    #' Returns a data frame describing current fields, their types, and binding status.
    #' @return A data.frame with field names, categories, types, and whether they are active bindings.
    #' @keywords internal
    .field_list = function(){
      fields <- ls(self)[!ls(self) %in% c("ID", "print","clone","initialize")]
      field_type <- as.vector(private$field_type())
      field_category <- ifelse(field_type=="function", "act_FUN", "agent_attribute")
      active_binding <- fields %in% names(self$.__enclos_env__$.__active__)
      data.frame(
        name = c("ID", fields),
        category = c("agent_attribute", field_category),
        type = c("scalar", field_type),
        active_binding = c(FALSE, active_binding)
      )
    },

    #' @description
    #' Prints a structured summary of the agent's attributes.
    #' @return None. Output is printed to the console.
    #' @details
    #' This method provides a detailed view of the agent's attributes,
    #' categorizing them based on their type.
    #' Attributes are grouped into the following categories:
    #' - ID: Agent's ID.
    #' - Scalar fields: Single values.
    #' - Vector fields: Multiple values in a vector.
    #' - Matrix fields: 2D data structures.
    #' - Array fields: Multidimensional data structures.
    #' - Data frame fields: Tabular data.
    #' - Other fields: Fields that do not fit the above types.
    #' - act_FUN: Actions of the agents.
    #' Fields that are active bindings are marked with an asterisk (`*`) next to their names.
    #' Active bindings are fields whose values are computed dynamically upon access.
    print = function(){
      # analyze the field_type
      field_type <- private$field_type()
      active_binding_names <- names(self$.__enclos_env__$.__active__)
      # differentiate the fields
      scalar_field <- names(field_type[field_type=="scalar"])
      vector_field <- names(field_type[field_type=="vector"])
      matrix_field <- names(field_type[field_type=="matrix"])
      array_field <- names(field_type[field_type=="array"])
      data.frame_field <- names(field_type[field_type=="data.frame"])
      other_field <- names(field_type[field_type=="other"])
      act_FUN_field <- names(field_type[field_type=="function"])
      # print
      cat("<ABM_Agent>", "\n")
      cat("[scalar & vector values]", "\n")
      ## ID
      cat(" ", "ID: ", self$ID, "\n", sep = "")
      ## scalar
      lapply(scalar_field, function(X){
        if(X %in% active_binding_names){
          cat(" ", X, "*", ": ", self[[X]], "\n", sep = "")
        }else{
          cat(" ", X, ": ", self[[X]], "\n", sep = "")
        }
      })
      ## vector
      lapply(vector_field, function(X){
        if(X %in% active_binding_names){
          cat("", X, "*", ": ", self[[X]], "\n", sep = "")
        }else{
          cat("", X, ": ", self[[X]], "\n", sep = "")
        }
      })
      ## other field types
      if(length(matrix_field)>0|length(array_field)>0|length(data.frame_field)>0|length(other_field)>0){
        cat("[other field names]", "\n")
      }
      ## matrix
      if(!length(matrix_field)==0){
        cat("", "matrix: ", sep = " ")
        lapply(matrix_field, function(X){
          if(X %in% active_binding_names){
            cat(X, "*", " ", sep = "")
          }else{
            cat(X, " ", sep = "")
          }
        })
        cat("\n")
      }
      ## array
      if(!length(array_field)==0){
        cat("", "array: ", sep = " ")
        lapply(array_field, function(X){
          if(X %in% active_binding_names){
            cat(X, "*", " ", sep = "")
          }else{
            cat(X, " ", sep = "")
          }
        })
        cat("\n")
      }
      ## data.frame
      if(!length(data.frame_field)==0){
        cat("", "data.frame: ", sep = " ")
        lapply(data.frame_field, function(X){
          if(X %in% active_binding_names){
            cat(X, "*", " ", sep = "")
          }else{
            cat(X, " ", sep = "")
          }
        })
        cat("\n")
      }
      ## other
      if(!length(other_field)==0){
        cat("", "other: ", sep = " ")
        lapply(other_field, function(X){
          if(X %in% active_binding_names){
            cat(X, "*", " ", sep = "")
          }else{
            cat(X, " ", sep = "")
          }
        })
        cat("\n")
      }
      ## act_FUN
      if(!length(act_FUN_field)==0){
        cat("", "act_FUN:", act_FUN_field, "\n", sep = " ")
      }
      ## Note of the active_binding
      if(length(active_binding_names)>0){
        cat("\n", "Note: Field names marked with '*' are active bindings.", "\n")
      }
    },

    #' @description
    #' Retrieve the current attributes of the agent as a list.
    #' @return
    #' A named list containing the values of the agent's fields, including the \code{ID} field.
    #' Methods including \code{act_FUN} are excluded from the output.
    #' @details
    #' This method returns the fields of the \code{ABM_Agent}
    #' object in a standard list format.
    #' It is primarily used for saving the state of an
    #' agent at each round during a simulation.
    #'
    #' Note: This method is intended for internal use during
    #' simulations. Users are not expected to call this method directly.
    .save = function(){
      field_type <- private$field_type()
      field_names <- names(field_type[field_type != "function"])
      values <- lapply(X = c("ID", field_names), function(X){self[[X]]})
      names(values) <- c("ID", field_names)
      values}
  ),

  private = list(
    # @description
    # Determines and categorizes field types in the agent.
    # @return A named character vector of types for each field.
    # @keywords internal
    # @details
    # Possible field types include:
    # - `"act_FUN"`: If the field is a function.
    # - `"scalar"`: If the field is a scalar (a vector of length 1).
    # - `"vector"`: If the field is a vector with length greater than 1.
    # - `"matrix"`: If the field is a matrix.
    # - `"array"`: If the field is an array (other than a matrix).
    # - `"data.frame"`: If the field is a data frame.
    # - `"other"`: For all other field types.
    field_type = function(){
      fields <- ls(self)[!ls(self) %in% c("initialize","clone", "print", "ID")]
      if(length(fields)==0){
        return(NULL)
      }
      sapply(fields, function(X){
        X_got <- self[[X]]
        if(is.function(X_got)){
          "function"
        }else if(is.atomic(X_got) & length(X_got)==1){
          "scalar"
        }else if(is.atomic(X_got) & is.vector(X_got)){
          "vector"
        }else if(is.matrix(X_got)){
          "matrix"
        }else if(is.array(X_got)){
          "array"
        }else if(is.data.frame(X_got)){
          "data.frame"
        }else{
          "other"
        }
      })
    }
  ) #----private-----
)

