#' ABM class
#' @title ABM_G Class
#' @docType class
#' @description \code{ABM_G} is the core class of \code{netABM} that bundle
#' datasets for running ABM.
#' @field log list of the past dataset.
#' @field notes list of the notes.
#'
#' @details
#' `G` stands for "Game". This class is implemented using the `R6` class system.
#' Although `G` object can be instantiated directly using `ABM_G$new()`,
#' it is highly recommended to use the \link{setABM} and \link{init_agents}
#' function for practical usage.
#' These functions function provides navigation for setting agents and various
#' field types and includes validation checks that are not present in
#' the `ABM_G` class method.
#'
#' Similarly, to modify fields of agents or entire `G` object, use the
#' \link{modify_agents} and \link{modify_G} function for the safer approach, respectively.
#'
#' Name of the fields and methods: Each field name and method name must
#' be unique.
#'
#' ### Important Note on Copying an `ABM_G` Object
#'
#' Copying an `R6` object differs from copying other R objects. For example,
#' if a user copies `G` to `G2` using `G2 <- G`, modifying the fields of `G2`
#' will also modify the fields of `G`. This happens because both objects share
#' the same encapsulated environment. To avoid unintended behavior,
#' use the \link{copy_G} and \link{copy_agents} function.
#'
#' ***Technical note***:
#'   - The function's environment is set to the object's encapsulated environment
#'    (`self$.__enclos_env__`) to ensure access to the object's internal state.
#'   - The created active binding is registered in the object's private field
#'   category as a `"stage"`.
#'
#' @import R6
#' @seealso [modify_G()], [modify_agents()], [copy_G()], [copy_agents()]
#' @export

ABM_G <- R6::R6Class(
  "ABM_G", lock_objects = FALSE, cloneable = TRUE,
  public = list(
    log = NULL,
    notes = NULL,
    #' @description
    #' Initialize a new `ABM_G` object with custom fields and methods.
    #' @param fields A named list of fields to initialize the `ABM_G` object.
    #' Each field is assigned a value in the new instance.
    #' @param methods A named list of functions (methods) to initialize the `ABM_G` object.
    #' Each method is added to the instance.
    #' @param log A list of the previous `ABM_G` object values (default is `NULL`).
    #' @param field_category A named list specifying categories of fields.
    #' @param notes A list of the notes (deefault is `NULL`).
    #' @return A new instance of the `ABM_G` object with the provided fields and methods.
    #' @details The field_types are either 'agent', 'stage',
    #' 'global_FUN', 'select_FUN', 'stop_FUN', 'update_FUN', 'summary_FUN', 'plot_FUN'.
    initialize = function(fields = list(),
                          methods = list(),
                          log = NULL,
                          field_category = NA,
                          notes = NULL) {
      # set field to the object
      for (field_name in names(fields)) {
        self[[field_name]] <- fields[[field_name]]
      }
      # set method to the object
      for (method_name in names(methods)) {
        self[[method_name]] <- methods[[method_name]]
        environment(self[[method_name]]) <- self$.__enclos_env__
      }

      # notes
      self$notes <- notes

      # field_category
      private$field_category <- field_category
    },

    #' @description
    #' Add a set of agents to the `ABM_G` object.
    #' @param name A character string specifying the field name for agents.
    #' @param agents A list of agents (must be objects of class `ABM_Agent`).
    .add_agents = function(name, agents){
      stopifnot("Do not set a name that already exists in the fields." = any(name == ls(self))==FALSE)
      stopifnot("All agent must be ABM_Agent class objects" = all(sapply(agents, function(X){class(X)[[1]]})=="ABM_Agent"))
      self[[name]] <- agents
      # register field category
      new_agents <- "agent"
      names(new_agents) <- name
      private$field_category <- c(private$field_category, new_agents)
    },

    #' @description
    #' Add a stage to the `ABM_G` object.
    #' @param name A character string specifying the field name for the stage.
    #' @param stage The stage object to be added (e.g., a matrix or data frame).
    .add_stage = function(name, stage){
      stopifnot("Do not set a name that already exists in the fields." = any(name == ls(self))==FALSE)
      stopifnot("Do not assign a function directly to a stage. If you intend to create an active binding field, use the 'active_binding_field' argument instead." = !is.function(stage))
      self[[name]] <- stage
      # register field category
      new_stage <- "stage"
      names(new_stage) <- name
      private$field_category <- c(private$field_category, new_stage)
    },

    #' @description
    #' Add a function (FUN) to the `ABM_G` object.
    #' @param name A character string specifying the field name for the function.
    #' @param FUN The function to be added.
    #' @param FUN_category The type of the function, one of `global_FUN`, `select_FUN`, `stop_FUN`, or `update_FUN`.
    .add_FUN = function(name, FUN, FUN_category = c("global_FUN","select_FUN","stop_FUN","update_FUN","summary_FUN","plot_FUN")){
      stopifnot("Do not set a name that already exists in the fields." = any(name == ls(self))==FALSE)
      retrieved_FUN <- .get_FUN(FUN = FUN)
      self[[name]] <- retrieved_FUN
      environment(self[[name]]) <- self$.__enclos_env__
      # register field_category
      FUN_category <- match.arg(FUN_category)
      new_FUN <- FUN_category
      names(new_FUN) <- name
      private$field_category <- c(private$field_category, new_FUN)
    },

    #' @description
    #' Add an active binding field to the `ABM_G` object.
    #' @param name A character string specifying the name of the active binding field.
    #' The name must be unique and not already exist in the current fields.
    #' @param FUN A function that defines the behavior of the active binding.
    #' The function is executed whenever the active binding field is accessed or modified.
    #' @details
    #' Active bindings are a feature of R that links a field's value to a function.
    #' Active bindings allow the field's value to be dynamically computed or
    #' updated whenever it is accessed or modified.
    #' @seealso [activeBindingFunction]
    .add_active_binding = function(name, FUN){
      stopifnot("Do not set a name that already exists in the fields." = any(name == ls(self))==FALSE)
      # check if the input is function
      stopifnot("FUN must be a function." = is.function(FUN))
      # attach environment
      environment(FUN) <- self$.__enclos_env__
      # active_bindingをする
      makeActiveBinding(name, FUN, self$.__enclos_env__$self)
      # save the name to the environment
      FUN_list <- list(FUN)
      names(FUN_list) <- name
      # If active binding does not exists, create it
      if(!".__active__" %in% names(self$.__enclos_env__)){
        self$.__enclos_env__$.__active__ <- FUN_list
      }else{
        self$.__enclos_env__$.__active__ <- c(self$.__enclos_env__$.__active__,
                                              FUN_list)
      }
      # register field_category
      new_stage <- "stage"
      names(new_stage) <- name
      private$field_category <- c(private$field_category, new_stage)
    },

    #' @description
    #' Removes a field from the `ABM_G` object, including any
    #'  associated active bindings and field category entries.
    #' @param name A character string specifying the name of the field to be removed. The field must exist in the object.
    #' @details
    #' The `.remove_field` method performs the following actions:
    #' - Removes the specified field from the object environment.
    #' - If the field is an active binding, it also removes the corresponding entry
    #'  from the active bindings list in the object's encapsulated environment.
    .remove_field = function(name){
      # check if such field exists
      if(!(name %in% ls(self))){
        stop(paste0("'", name, "'", " was not found."))
      }
      # if exists, proceed to remove...
      rm(list = name, envir = self)
      active_binding_names <- names(self$.__enclos_env__$.__active__)
      if(name %in% active_binding_names){
        self$.__enclos_env__$.__active__[[name]] <- NULL
      }
      # delete field_category
      index <- which(names(private$field_category)==name)
      private$field_category <- private$field_category[-index]
    },

    #' @description
    #' Replaces an existing field or method with a new value or function.
    #' @param name Name of the field to replace.
    #' @param value The new value or function to assign.
    #' @return None.
    #' @keywords internal
    .replace_field = function(name, value){
      # retrieve the type
      is_agent <- private$field_category[name]=="agent"
      field_list <- self$.field_list()
      field_info <- field_list[which(field_list$name==name), ]

      # delete first
      self$.remove_field(name)

      # then add a new value
      if(is_agent){
        self$.add_agents(name = name, agents = value)
      }else{
        if(field_info$category %in% c("global_FUN", "select_FUN", "stop_FUN", "update_FUN", "summary_FUN",
                                      "plot_FUN")){
          self$.add_FUN(name = name, FUN = value, FUN_category = field_info$category)
        }else if(field_info$active_binding){
          self$.add_active_binding(name = name, FUN = value)
        }else{
          self$.add_stage(name = name, stage = value)
        }
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
      # add a new value
      if(field_info$category %in% c("global_FUN", "select_FUN", "stop_FUN",
                                    "update_FUN", "summary_FUN", "plot_FUN")){
        FUN <- self[[name]]
        self$.add_FUN(name = new_name, FUN = FUN, FUN_category = field_info$category)
      }else if(field_info$active_binding){
        FUN <- self$.__enclos_env__$.__active__[[name]]
        self$.add_active_binding(name = new_name, FUN = FUN)
      }else if(private$field_category[name]=="agent"){
        agents <- self[[name]]
        self$.add_agents(name = new_name, agents = agents)
      }else{
        value <- self[[name]]
        self$.add_stage(name = new_name, stage = value)
      }
      # then delete a field of the old name
      self$.remove_field(name = name)
    },

    #' @description
    #' This method provides a detailed, structured output of the current state
    #'  of an `ABM_G` object, including agents, stages, functions, and metadata.
    #' @details
    #' The `print` method organizes and displays the fields of the `ABM_G` object
    #' in a categorized manner:
    #' - **Agents**: Lists the agents in the object along with their attributes, categorized by scalar, vector, matrix, array, and data.frame types. Active bindings are marked with an asterisk (`*`).
    #' - **Stages**: Lists stage fields and their types, including active bindings (marked with an asterisk).
    #' - **Functions**: Displays the names of global, select, stop, update, summary, and plot functions.
    #' - **Metadata**: Outputs metadata such as the current simulation time, log entries, and notes.
    #' Active bindings in the object are explicitly highlighted,
    #' and a note is included to explain the significance of fields marked with `*`.
    print = function(){
      # obtain the field_category
      field_category <- private$field_category
      field_category <- na.exclude(field_category)
      field_type <- private$field_type()
      # print
      cat("<ABM_G>", "\n")
      # agent
      agent_category <- names(field_category)[field_category=="agent"]
      if(length(agent_category)>0){
        cat("[agent]", "\n")
        for(X in agent_category){
          agent_field_list <- self[[X]][[1]]$.field_list()
          agent_field_list <- agent_field_list[-1, ]  # delete the row of ID
          agent_field_name <- agent_field_list$name
          agent_field_type <- agent_field_list$type
          agent_active_binding <- agent_field_list$name[agent_field_list$active_binding]
          cat(X, " (n = ", length(self[[X]]), ")", "\n", sep = "")
          ## agent:scalar
          cat("  scalar: ID ", sep = "")
          if(length(agent_field_type[agent_field_type=="scalar"])>0){
            lapply(agent_field_name[agent_field_type=="scalar"], function(X){
              if(X %in% agent_active_binding){
                cat(X, "*", " ", sep = "")
              }else{
                cat(X, " ", sep = "")
              }
            })
          }
          cat("\n")
          ## agent: vector
          if(length(agent_field_type[agent_field_type=="vector"])>0){
            cat("  vector: ")
            lapply(agent_field_name[agent_field_type=="vector"], function(X){
              if(X %in% agent_active_binding){
                cat(X, "*", " ", sep = "")
              }else{
                cat(X, " ", sep = "")
              }
            })
            cat("\n")
          }
          ## agent: matrix
          if(length(agent_field_type[agent_field_type=="matrix"])>0){
            cat("  matrix: ")
            lapply(agent_field_name[agent_field_type=="matrix"], function(X){
              if(X %in% agent_active_binding){
                cat(X, "*", " ", sep = "")
              }else{
                cat(X, " ", sep = "")
              }
            })
            cat("\n")
          }
          ## agent: array
          if(length(agent_field_type[agent_field_type=="array"])>0){
            cat("  array : ")
            lapply(agent_field_name[agent_field_type=="array"], function(X){
              if(X %in% agent_active_binding){
                cat(X, "*", " ", sep = "")
              }else{
                cat(X, " ", sep = "")
              }
            })
            cat("\n")
          }
          ## agent: data.frame
          if(length(agent_field_type[agent_field_type=="data.frame"])>0){
            cat("  data.frame: ")
            lapply(agent_field_name[agent_field_type=="data.frame"], function(X){
              if(X %in% agent_active_binding){
                cat(X, "*", " ", sep = "")
              }else{
                cat(X, " ", sep = "")
              }
            })
            cat("\n")
          }
          ## agent: other
          if(length(agent_field_type[agent_field_type=="other"])>0){
            cat("  other : ")
            lapply(agent_field_name[agent_field_type=="other"], function(X){
              if(X %in% agent_active_binding){
                cat(X, "*", " ", sep = "")
              }else{
                cat(X, " ", sep = "")
              }
            })
            cat("\n")
          }
          ## agent: act_FUN
          if(length(agent_field_type[agent_field_type=="function"])>0){
            cat("  act_FUN:", agent_field_name[agent_field_type=="function"], sep = " ")
            cat("\n")
          }
          ##
          cat("\n")
        } #---agent for文ここまで
      } #-----agent
      # stage
      stage_type <- field_type[names(field_category[field_category=="stage"])]
      stage_active_binding <- names(stage_type)[names(stage_type) %in% names(self$.__enclos_env__$.__active__)]
      if(length(stage_type)>0){
        cat("[stage]", "\n")
        if(length(stage_type[stage_type=="scalar"])>0){
          cat("  scalar: ", sep = "")
          lapply(names(stage_type[stage_type=="scalar"]), function(X){
            if(X %in% stage_active_binding){
              cat(X, "*", " ", sep = "")
            }else{
              cat(X, " ", sep = "")
            }
          })
          cat("\n")
        }
        ## stage: vector
        if(length(stage_type[stage_type=="vector"])>0){
          cat("  vector: ")
          lapply(names(stage_type[stage_type=="vector"]), function(X){
            if(X %in% stage_active_binding){
              cat(X, "*", " ", sep = "")
            }else{
              cat(X, " ", sep = "")
            }
          })
          cat("\n")
        }
        ## stage: matrix
        if(length(stage_type[stage_type=="matrix"])>0){
          cat("  matrix: ")
          lapply(names(stage_type[stage_type=="matrix"]), function(X){
            if(X %in% stage_active_binding){
              cat(X, "*", " ", sep = "")
            }else{
              cat(X, " ", sep = "")
            }
          })
          cat("\n")
        }
        ## stage: array
        if(length(stage_type[stage_type=="array"])>0){
          cat("  array : ")
          lapply(names(stage_type[stage_type=="array"]), function(X){
            if(X %in% stage_active_binding){
              cat(X, "*", " ", sep = "")
            }else{
              cat(X, " ", sep = "")
            }
          })
          cat("\n")
        }
        ## stage: data.frame
        if(length(stage_type[stage_type=="data.frame"])>0){
          cat("  data.frame: ")
          lapply(names(stage_type[stage_type=="data.frame"]), function(X){
            if(X %in% stage_active_binding){
              cat(X, "*", " ", sep = "")
            }else{
              cat(X, " ", sep = "")
            }
          })
          cat("\n")
        }
        ## stage: other
        if(length(stage_type[stage_type=="other"])>0){
          cat("  other : ")
          lapply(names(stage_type[stage_type=="other"]), function(X){
            if(X %in% stage_active_binding){
              cat(X, "*", " ", sep = "")
            }else{
              cat(X, " ", sep = "")
            }
          })
          cat("\n")
        }
        ##
        cat("\n")
      } #-----stage

      # Functions
      if(length(field_category[field_category %in% c("global_FUN", "select_FUN", "stop_FUN", "update_FUN","summary_FUN", "plot_FUN")])>0){
        cat("[FUN]", "\n")
        # global_FUN
        global_FUN_category <- names(field_category)[field_category=="global_FUN"]
        if(length(global_FUN_category)>0){
          cat("global_FUN :", global_FUN_category, "\n", sep = " ")
        }
        # select_FUN
        select_FUN_category <- names(field_category)[field_category=="select_FUN"]
        if(length(select_FUN_category)>0){
          cat("select_FUN :", select_FUN_category, "\n", sep = " ")
        }
        # stop_FUN
        stop_FUN_category <- names(field_category)[field_category=="stop_FUN"]
        if(length(stop_FUN_category)>0){
          cat("stop_FUN   :", stop_FUN_category, "\n", sep = " ")
        }
        # update_FUN
        update_FUN_category <- names(field_category)[field_category=="update_FUN"]
        if(length(update_FUN_category)>0){
          cat("update_FUN :", update_FUN_category, "\n", sep = " ")
        }
        # summary_FUN
        summary_FUN_category <- names(field_category)[field_category=="summary_FUN"]
        if(length(update_FUN_category)>0){
          cat("summary_FUN:", summary_FUN_category, "\n", sep = " ")
        }
        # plot_FUN
        plot_FUN_category <- names(field_category)[field_category=="plot_FUN"]
        if(length(update_FUN_category)>0){
          cat("plot_FUN   :", plot_FUN_category, "\n", sep = " ")
        }
        cat("\n")
      }
      # meta data
      cat("[meta data]", "\n", sep = "")
      cat("time :", self$time, "\n")
      cat("log  :", if(all(is.na(self$log))){"NULL"}else{length(self$log)}, "\n")
      cat("notes:", names(self$notes), "\n")

      ## Note of the active_binding
      if(length(stage_active_binding)>0){
        cat("\n", "Note: Field names marked with '*' are active bindings.", "\n")
      }

    },

    #' @description
    #' This method saves the current state of the `ABM_G` object into its log.
    #' The saved state includes field values for agents, stages,
    #' and metadata at the current simulation time.
    #' @details
    #' The `.save` method performs the following operations:
    #' - **Field Retrieval**:
    #'   - Extracts all fields categorized as `"agent"` or `"stage"` from the object.
    #'   - Metadata fields such as `time` are also included in the saved state.
    #' - **Agent Data Extraction**:
    #'   - For each agent field, calls the agent's internal `.save` method to
    #'   capture its current state.
    #'   - The saved values are organized by agent names for easy retrieval.
    #' - **Log Entry Creation**:
    #'   - Combines the extracted data into a single log entry.
    #'   - The log entry is stored in the `log` field,
    #'   with the current simulation time (`time`) used as the entry name
    #'   (e.g., `"t1"`, `"t2"`).
    #'
    #' - **Log Update**:
    #'   - Appends the new log entry if the current time exceeds the number of
    #'   existing log entries.
    #'   - Overwrites the corresponding log entry if it already exists for the
    #'   current time.
    .save = function(){
      # get the field name
      field <- private$field_category
      field_agent <- names(field[field=="agent"])
      field_other <- c(names(field[field=="stage"]), "time")
      G_values <- lapply(field_other, function(X){
        X_retrieved <- self[[X]]
        if(any(class(X_retrieved) == "data.table")){
          data.table::as.data.table(as.data.frame(X_retrieved))
        }else{
          X_retrieved
        }
      })
      names(G_values) <- field_other
      # get the agents' values
      G_agent <- lapply(field_agent, function(field_agent_p){
        agents <- self[[field_agent_p]]
        values <- lapply(1:length(agents), function(i){
          agents[[i]]$.save()
        })
        names(values) <- names(agents)
        values
      })
      names(G_agent) <- field_agent
      # combine the obtained values and attach them to new_log
      G_values <- c(G_values, G_agent)
      new_log <- list(G_values)
      names(new_log) <- paste0("t", self$time)
      if(length(self$log) < self$time){
        self$log <- c(self$log, new_log)
      }else{
        self$log[[self$time]] <- new_log[[1]]
      }
    },

    #' @description
    #' Generates a list of all fields in the `ABM_G` object,
    #' categorizing them based on their type, category, and active binding status.
    #' @details
    #' The `.field_list` method collects and organizes
    #' information about the fields in the `ABM_G` object.
    #'
    #' The resulting data frame contains the following columns:
    #' - `agent_name`: The name of the set of agents (or `NA` for non-agent fields).
    #' - `name`: The field name.
    #' - `category`: The field category (e.g., `"agent"`, `"stage"`).
    #' - `type`: The field type (e.g., `"scalar"`, `"vector"`, `"matrix"`, `"function"`).
    #' - `active_binding`: A logical value indicating if the field is an active binding.
    #' @return
    #' A data frame containing information about all fields in the `ABM_G` object.
    .field_list = function(){
      # field_type
      field_category <- private$field_category
      # agent
      agent_names <- names(field_category[field_category=="agent"])
      if(length(agent_names) > 0){
        agent_list <- lapply(agent_names, function(agents){
          data.frame(
            agent_name = agents,
            self[[agents]][[1]]$.field_list()
          )
        })
        agent_list <- do.call(rbind, agent_list)
      }else{
        agent_list <- NULL
      }
      # other
      other_field <- field_category[field_category!="agent"]
      active_binding_names <- names(self$.__enclos_env__$.__active__)
      active_binding <- names(other_field) %in% active_binding_names
      field_type <- private$field_type()
      field_type <- field_type[names(other_field)]
      if(length(other_field)>0){
        other_list <- data.frame(
          agent_name = NA,
          name = names(other_field),
          category = as.vector(other_field),
          type = as.vector(field_type),
          active_binding = active_binding)
      }else{
        other_list <- NULL
      }
      # summarize
      field_list <- do.call(rbind, list(agent_list, other_list))
      field_list
    },

    ##' @description
    #' This internal method retrieves the specified attributes of agents in the `ABM_G` object, either from the current state or from a specified log entry. It is designed for internal use, with the expectation that a more user-friendly interface will be provided for general usage.
    #' @param agents A character string specifying the field name of the agents whose attributes are to be retrieved.
    #' @param attr A character string specifying the name of the attribute to retrieve for each agent.
    #' @param log An optional integer specifying the log entry to retrieve the attribute from. If `NULL`, the attribute is retrieved from the current state of the agents (default is `NULL`).
    #' @details
    #' This method provides flexibility for accessing agent attributes:
    #' - If `log = NULL`, the method retrieves the attribute values from the current state of the agents.
    #' - If `log` is specified, the method retrieves the attribute values from
    #' the specified log entry. The log entry must exist, and the agents must be present in that log entry.
    #' The method is optimized for internal use and assumes that the `agents` field
    #'  and `log` structure are properly managed.
    #' @return
    #' A list containing the values of the specified attribute for each agent.
    .agent_attr = function(agents, attr, log = NULL){
      # log = NULL: get the data from the current field
      if(is.null(log)){
        lapply(1:length(self[[agents]]), function(i){
          self[[agents]][[i]][[attr]]
        })
      }else{
        lapply(1:length(self$log[[log]][[agents]]), function(i){
          self$log[[log]][[agents]][[attr]]
        })
      }
    }
  ),  #---public----#

  private = list(
    deep_clone = function(name, value){
      # deep_clone for agents.
      field_type <- private$field_category
      agent_type <- names(field_type)[field_type=="agent"]
      if(name %in% agent_type){
        agent_cloned <- lapply(self[[name]], function(agent_i){
          agent_i$clone(deep = TRUE)
        })
        names(agent_cloned) <- names(self[[name]])
        attr(agent_cloned, "field_type") <- "agent"
        agent_cloned
      }else{
        value
      }
    },
    field_category = NULL,

    # description
    # Determine the types of fields in `G`. The method excludes basic predefined fields (`initialize`, `clone`, `print`, `ID`) from the analysis, as these are not informative.
    # return
    # A named character vector where the names represent field names, and the values indicate the type of each field.
    # Possible field types include:
    # - `"function"`: If the field is a function.
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
  )
)
