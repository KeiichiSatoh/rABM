#' @title ABM_Agent Class
#' @description
#' The \code{ABM_Agent} class provides methods and fields for creating and
#' managing agent's action and attributes in an agent-based model (ABM).
#' It allows agents to have custom fields, methods, and active bindings,
#' making this class highly flexible for various simulation scenarios.
#'
#' @details
#' `ABM_Agent` is the class for agents that includes:
#' - A initialize method (i.e., `new()`)
#' - Methods for adding/removing fields, methods, and active bindings
#' (i.e., `.add_field()`, `.add_method()`, `.add_active_binding()`,
#' `remove_field()`).
#' - Functions to retrieve agent attributes (i.e., `.save()`).
#' - A print method to display agent information (i.e., `print()`.
#' - clone method to copy the instanced agent object (i.e., `clone()`).
#'
#' This class is implemented using the `R6` class. While agents can
#' be instantiated directly using `ABM_Agent$new()`,
#' it is highly recommended to use \link{init_agent} for batch creation of
#' agents for the actual usage. The `init_agent` function provides
#' the navigation for setting various field types and includes validation checks
#' that is not present in the `ABM_Agent` class method.
#'
#' Similarly, to modify fields of agents,
#' consider using the \link{modify_agents} function for the safer approach.
#'
#' Name of the fields and methods:
#' Each field name and method name must be unique within the agent's namespace.
#' Attempting to provide a duplicate name will result in an error,
#' as duplicate names can interfere with the agent's proper functioning.
#' Field names are set as attributes, while methods are assigned as callable
#' functions.
#' The environment of each method is automatically set to the agent instance.
#'
#' @import R6
#' @export

ABM_Agent <- R6::R6Class("ABM_Agent", lock_objects = FALSE, cloneable = TRUE,
                         public = list(
                           #' @description
                           #' Initialize a new agent with custom fields and methods.
                           #' @param fields A named list of fields to initialize the agent with.
                           #' Each field is assigned a value in the new agent instance.
                           #' @param methods A named list of functions (methods) to initialize the agent with.
                           #' Each method is added to the agent's instance.
                           #' @return
                           #' The instance of the agent with the provided fields and methods.
                           initialize = function(fields = list(), methods = list()) {
                             # フィールドをオブジェクトにセット
                             for (name in names(fields)) {
                               stopifnot("Do not use the same field name." = name %in% names(self)==FALSE)
                               self[[name]] <- fields[[name]]
                             }
                             # メソッドをオブジェクトにセット
                             for (name in names(methods)) {
                               stopifnot("Do not use the same field name." = name %in% names(self)==FALSE)
                               self[[name]] <- methods[[name]]
                               environment(self[[name]]) <- self$.__enclos_env__
                             }
                           },

                           #' @description
                           #' Add a new field to the agent.
                           #' @param name A string representing the name of the field to add. The name must be unique within the agent.
                           #' @param value The value to assign to the new field.
                           #' @return
                           #' This method does not return a value. It adds the specified field to the agent.
                           .add_field = function(name, value){
                             stopifnot("Do not use the same field name." = name %in% names(self)==FALSE)
                             self[[name]] <- value
                           },

                           #' @description
                           #' Add a new method to the agent.
                           #' @param name A string representing the name of the method to add. The name must be unique within the agent.
                           #' @param method A function to add as a method. The function will be associated with the specified name.
                           #' @return
                           #' This method does not return a value. It adds the specified method to the agent.
                           .add_method = function(name, method) {
                             stopifnot("Do not use the same field name." = name %in% names(self) == FALSE)
                             # Check if the provided method is a function
                             stopifnot("method must be a function." = is.function(method))
                             self[[name]] <- method
                             # Set the method's environment to the agent's environment
                             environment(self[[name]]) <- self$.__enclos_env__
                           },

                           #' @description
                           #' Add a new active binding to the agent.
                           #' @param name A string representing the name of the active binding.
                           #' The name must be unique within the agent.
                           #' @param FUN A function that defines the behavior of the active binding.
                           #' This function is dynamically called whenever the active binding is accessed.
                           #' @return
                           #' This method does not return a value. It modifies the agent by adding the specified active binding.
                           .add_active_binding = function(name, FUN){
                             stopifnot("Do not use the same field name." = (name %in% names(self)==FALSE))
                             # 関数かどうかを確認
                             stopifnot("FUN must be a function." = is.function(FUN))
                             # environmentを付加する
                             environment(FUN) <- self$.__enclos_env__
                             # active_bindingをする
                             makeActiveBinding(name, FUN, self$.__enclos_env__$self)
                             # 名前とセットにして環境に保存する
                             FUN_list <- list(FUN)
                             names(FUN_list) <- name
                             self$.__enclos_env__$.__active__ <- c(self$.__enclos_env__$.__active__,
                                                                   FUN_list)
                           },

                           #' @description
                           #' Remove a field or method from the agent.
                           #' @param name A string representing the name of the field to remove.
                           #' The field must exist in the agent instance.
                           #' @return
                           #' This method does not return a value. It modifies the agent instance by removing the specified field.
                           .remove_field = function(name){
                             rm(list = name, envir = self)
                             if(name %in% self$.active_binding_names()){
                               self$.__enclos_env__$.__active__[[name]] <- NULL
                             }
                           },

                           #'@description
                           #' Show the property of the fields in a structured format.
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
                           #' Print the agent's attributes in a structured format.
                           #' @return
                           #' This method does not return a value. It outputs the agent's attributes to the console.
                           #' #' @details
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
                             # タイプを分析する
                             field_type <- private$field_type()
                             active_binding_names <- names(self$.__enclos_env__$.__active__)
                             # フィールド分け
                             scalar_field <- names(field_type[field_type=="scalar"])
                             vector_field <- names(field_type[field_type=="vector"])
                             matrix_field <- names(field_type[field_type=="matrix"])
                             array_field <- names(field_type[field_type=="array"])
                             data.frame_field <- names(field_type[field_type=="data.frame"])
                             other_field <- names(field_type[field_type=="other"])
                             act_FUN_field <- names(field_type[field_type=="function"])
                             # 表示
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
                             ## 追記の必要性
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
                           # Determine the types of fields in the agent. The method excludes basic predefined fields (`initialize`, `clone`, `print`, `ID`) from the analysis, as these are not informative.
                           # return
                           # A named character vector where the names represent field names, and the values indicate the type of each field.
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

