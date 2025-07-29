#' @title Create a set of ABM_Agent class agents
#' @description Creates and initializes agents with specified attributes, actions, and other parameters.
#'
#' @param n An integer specifying the number of agents to create. If \code{NULL}, \code{n} is derived from \code{attr_df} if provided.
#' @param attr_df A data frame or a vector containing the initial attributes for each agent.
#' Each row represents an agent's attributes.
#' @param act_FUN A function or list of functions defining the actions or
#' methods for each agent.
#' @param active_binding A function or list of functions defining the output of the active bindings.
#' @param ID_start An integer specifying the starting value for agent IDs. Default is \code{1}.
#' @param custom_ID Optional vector of agent IDs. If provided, its length must match \code{n}.
#' @param other_attrs A named list of additional attributes for each agent. Each list element must have the same length as \code{n}.
#'
#' @return A list of initialized agent objects, each with an assigned ID and specified attributes and actions.
#'
#' @details
#' The \code{init_agents} function generates agent instances with specified attributes
#' and functions.
#'
#' **Number of agents**:
#' Either \code{n} or \code{attr_df} must be provided to specify the number of agents.
#' If both \code{n} and \code{attr_df} are given, the function checks that
#' the number of agents (\code{n}) matches the number of rows in \code{attr_df}.
#'
#' **act_FUN**:
#' When a single function is provided, it is replicated across all agents,
#' meaning that each agent follows the same behavior rule.
#' If you want each agent to have a different behavior, pass a vector or list of
#' functions, each corresponding to an agent. For example, to assign three different
#' functions \code{act_x1}, \code{act_x2}, and \code{act_x3} to three agents respectively,
#' set:
#'
#' \code{act_FUN = list(act1 = list(act_x1, act_x2, act_x3))}
#'
#' To assign multiple actions to each agent, note that the first level of
#' the list will be shared across all agents.
#' For example, to assign two actions, \code{act_x} and \code{act_y}, to all agents,
#' set:
#'
#' \code{act_FUN = list(act_x = act_x, act_y = act_y)}.
#'
#' To assign unique actions for each agent individually,
#' use a structure like:
#'
#' \code{act_FUN = list(act_x = list(act_x2, act_x2, act_x3),
#'                      act_y = list(act_y1, act_y2, act_y3))}.
#'
#' You can specify each agent's behavior rule in four ways:
#' - Function object: The basic approach, such as \code{act_FUN = act_x1}.
#' - Closure: Pass the function directly, as in \code{act_FUN = function(){...}}.
#' - Function name as a string: When the function object already exists in the environment,
#'  it can be called by name, e.g., \code{act_FUN = "act_x1"}.
#' - Function name as a string with arguments: Include specific arguments within
#'  the function call if needed. This is useful for built-in package functions,
#'  e.g., \code{act_FUN = "act_x1(a = 1)"}.
#'
#' Automatically, \code{G = G} and \code{E = E} are added to the function arguments
#' within \code{act_FUN} and \code{active_binding} to facilitate simulation in the \code{runABM} function.
#'
#' **active_binding**:
#' An active binding field recalculates its value based on other fields of
#' the agent according to a predefined function.
#' The specification follows the same format as for \code{act_FUN}.
#' When creating a custom function, ensure that it returns
#' the desired field value. For more information,
#' see the R6 package tutorial: <https://r6.r-lib.org/articles/Introduction.html>.
#'
#' Example of active binding:
#' Suppose you want each agent to have a `status` field that automatically returns
#'  `"adult"` if the agent's `age >= 18`, and `"child"` otherwise.
#'  You can define this behavior as follows:
#'
#' \code{
#' active_binding <- list(
#'   status = function() {
#'     if (self$age >= 18) {
#'       "adult"
#'     } else {
#'       "child"
#'     }
#'   }
#' )
#' }
#'
#' Note: In this example, `status` is not stored as a regular field;
#' it is dynamically computed from the `age` field each time it is accessed.
#'
#' Important considerations for active binding:
#' - Active bindings depend on other fields (e.g., `self$age` in the above example).
#' These dependent fields must be defined before the active binding is accessed.
#' Otherwise, an error such as "object 'age' not found" will occur.
#' - You can reference other fields using `self$` within the function body.
#' - Be cautious not to create circular dependencies (e.g., `status` depends on
#' `score`, and `score` depends on `status`).
#'
#' **ID_start**:
#' Each agent in a simulation should have a unique ID. If using multiple types of
#' agents within a simulation,
#' set this value to avoid ID overlap between agent groups.
#'
#' **other_attrs**:
#' The primary way to specify agent attributes is through \code{attr_df}, a data frame.
#' However, if an agent's attribute needs to be a vector, matrix, data frame, or list
#'  (e.g., for individual agent-level data), use \code{other_attrs}
#' as a list. Ensure each attribute within \code{other_attrs} has \code{n} elements
#' to match the number of agents.
#' Similar to \code{act_FUN}, the first list level represents fields.
#' Below are examples of specifying attributes as matrices
#' for three agents:
#' - For a single field: \code{other_attrs = list(mat1 = list(mat1_1, mat1_2, mat1_3))}
#' - For multiple fields: \code{other_attrs = list(mat1 = list(mat1_1, mat1_2, mat1_3), mat2 = list(mat2_1, mat2_2, mat2_3))}
#' @seealso \code{\link{ABM_Agent}}
#' @import R6
#' @export
#'
#' @examples
#' # Example usage
#' init_agents(n = 5, attr_df = data.frame(age = c(25, 30, 35, 40, 45)))
#'

init_agents <- function(n = NULL, attr_df = NULL, act_FUN = NULL,
                       active_binding = NULL, ID_start = 1,
                       custom_ID = NULL,
                       other_attrs = NULL){
  # Process attr_df
  attr_sbs <- substitute(attr_df)
  attr_df <- .shape_agent_attr(attr = attr_df, attr_sbs = attr_sbs)

  # Check if n is an integer
  if(!is.null(n)){
    stopifnot("n must be numeric." = is.numeric(n))
  }

  # check the integration between n and attr_df
  if(!is.null(n) & !is.null(attr_df)){
    ## When both n and attr_df are provided: check if they are the same
    stopifnot("'n' must match the number of rows in 'attr_df'." = n == NROW(attr_df))
  }else if(is.null(n) & !is.null(attr_df)){
    ## n is NULL and attr_df presents: get n from attr_df
    n <- NROW(attr_df)
  }else if(!is.null(n) & is.null(attr_df)){
    ## n is present and attr_df is NULL: remain n as it is
    n <- n
  }else{
    ## Neither n and attr_df exists: Express ERROR
    stop("Either 'n' or 'attr_df' must be provided.")
  }

  # Process act_FUN
  act_FUN_sbs <- substitute(act_FUN)
  act_FUN_list <- .shape_act_FUN(act_FUN = act_FUN,
                                 act_FUN_sbs = act_FUN_sbs,
                                 n = n)

  # Process active_binding_field
  active_binding_sbs <- substitute(active_binding)
  active_binding_list <- .shape_act_FUN(act_FUN = active_binding,
                                        act_FUN_sbs = active_binding_sbs,
                                        n = n)

  # Add G = G, E = E to formals
  act_FUN_list <- lapply(act_FUN_list, function(FUN_vec){
    lapply(FUN_vec, function(FUN){
      # Delete G if the user already wrote it.
      current_formals <- formals(FUN)
      current_formals[which(names(current_formals)=="G")|which(names(current_formals)=="E")] <- NULL
      formals(FUN) <- c(alist(G = G, E = E), current_formals) # add G=G, E = E
      FUN
    })
  })

  active_binding_list <- lapply(active_binding_list, function(FUN_vec){
    lapply(FUN_vec, function(FUN){
      # Delete G if the user already wrote it.
      current_formals <- formals(FUN)
      current_formals[which(names(current_formals)=="G")|which(names(current_formals)=="E")] <- NULL
      formals(FUN) <- c(alist(G = G, E = E), current_formals) # add G=G, E = E
      FUN
    })
  })

  # agentID
  if(!is.null(custom_ID)){
    stopifnot("The length of 'custom_ID' must match the number of agents." = length(custom_ID) == n)
    agentID <- custom_ID
  }else{
    agentID <- seq(ID_start, length.out = n)
  }

  # other_attrs-------
  if(!is.null(other_attrs)){
    other_attrs_sbs <- substitute(other_attrs)

    ## check the input(data.frame is not allowed)
    stopifnot("other_attrs must be a list." = is.list(other_attrs) & !is.data.frame(other_attrs))

    ## check the contents
    lapply(other_attrs, function(X){
      stopifnot("Each element in 'other_attrs' must have length 'n'." = length(X) == n)
      if(is.function(X)){
        stop("Elements in 'other_attrs' must not be functions.")
      }
    })

    ## get unnamed labels from substitutes
    other_attrs_expr <- as.list(other_attrs_sbs)[-1] # drop the function name
    other_attrs_obs_name <- sapply(other_attrs_expr, function(expr){
      if(is.symbol(expr)){
        deparse(expr)
      }else{
        NA_character_
      }
    })

    ## Integrate unnamed and named elements
    other_attrs_label <- names(other_attrs)
    if(is.null(other_attrs_label)){
      other_attrs_label <- other_attrs_obs_name
    }else{
      other_attrs_label[other_attrs_label==""] <- other_attrs_obs_name[other_attrs_label==""]
    }

    # append the labels to unnamed objects
    other_attrs_label[is.na(other_attrs_label)] <- paste0("Z", seq_len(sum(is.na(other_attrs_label))))

    # append the labels
    names(other_attrs) <- other_attrs_label
  }

  # append attr_df and other_attrs
  attr_list <- c(as.list(attr_df), other_attrs)

  # check if attr_listとact_FUN_listの名前が重複していないかをチェック
  name_table <- table(c(names(attr_list), names(act_FUN_list), names(active_binding_list)))
  duplicated_field_name <- names(name_table)[name_table>1]
  if(length(duplicated_field_name) > 0){
    stop(paste0("The field name '", duplicated_field_name, "' is duplicated. Please provide a unique name for each field."))
  }

  # create an R6 class for agents
  act_FUN_len <- length(act_FUN_list)
  act_FUN_i <- vector("list", act_FUN_len)
  names(act_FUN_i) <- names(act_FUN_list)

  out <- lapply(1:n, function(i){
    ID_i <- c(ID = agentID[i])
    if(is.null(attr_list)){
      attr_i <- NULL
    }else{
      attr_i <- lapply(attr_list, function(X){X[[i]]})
      names(attr_i) <- names(attr_list)
    }
    if(act_FUN_len==0){
      act_FUN_i <- NULL
    }else{
      for(p in 1:act_FUN_len){
        act_FUN_i[[p]] <- act_FUN_list[[p]][[i]]
      }
    }
    # add to Agents
    Agent <- ABM_Agent$new(fields = c(ID_i, attr_i), methods = act_FUN_i)

    # process active_bindings
    if(!is.null(active_binding_list)){
      active_binding_i <- lapply(names(active_binding_list), function(X){active_binding_list[[X]][[i]]})
      names(active_binding_i) <- names(active_binding_list)
      active <- assign_func_envs(active_binding_i, Agent$.__enclos_env__)
      for(name in names(active)){
        makeActiveBinding(name, active[[name]], Agent$.__enclos_env__$self)
      }
      Agent$.__enclos_env__$.__active__ <- active
    }
    # return
    Agent
  })

  # return
  out
}

