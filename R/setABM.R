#' @title Construct an `ABM_G` Class Object
#' @description
#' The `setABM` function constructs a `G` object (short for 'game') for
#' agent-based model (ABM) simulations.
#' The created `G` object serves as the foundation for running simulations
#' using the `runABM` function.
#'
#' @param agents A list of `ABM_Agent` objects representing agents in the model,
#' typically created using the \link{init_agent} function. Alternatively,
#' a positive integer can be provided to create that number of agents.
#' Default is `NULL`, meaning no agents will be created.
#' @param stage A data structure (e.g., matrix, list, or other formats)
#' representing the 'stage' where agents act. Default is `NULL`.
#' @param active_binding_field A named list specifying active binding fields
#' that allow dynamic behavior for specific stages. Default is `NULL`.
#' @param global_FUN A function for global operations affecting the `G` object. Default is `NULL`.
#' @param select_FUN A function to select agents during simulation based on specific criteria. Default is `NULL`.
#' @param stop_FUN A function defining the stopping conditions for the simulation. Default is `NULL`.
#' @param update_FUN A function that encapsulates one complete iteration of agent updates during the simulation. Default is `NULL`.
#' @param partial_update_FUN_body A body of expressions to construct parts of `update_FUN`.
#' Provide either character or `expression` objects. Default is `NULL`.
#' @param log A list containing snapshots of the `G` object at each simulation step. Default is `NULL`.
#' @param time An integer representing the current time step of the `G` object.
#' The initial time is 1. Default is `NULL`, which sets `time = 1`.
#' @param notes A list or data frame for user-defined notes or metadata related to the simulation. Default is `NULL`.
#' @param init A named list of initial values for any of the arguments, including `agents`, `stage`, `active_binding_field`,
#' `global_FUN`, `select_FUN`, `stop_FUN`, `update_FUN`, `partial_update_FUN_body`, `log`, `time`, `notes`.
#' If both `init` and individual arguments are specified, the values in `init` take precedence. Default is an empty list.
#' @return An object of class `ABM_G` representing the initialized ABM.
#' This object contains fields and methods required to manage the simulation.
#'
#' @details
#' The `setABM` function offers a flexible interface to define agents, stages,
#' and functions for ABM simulations.
#' Key rules for parameter configuration include:
#' - For a single object, pass it directly.
#' - For multiple objects, use a `list`.
#' - Named fields should use the format `list(field_name = x)`.
#' If no name is provided (e.g., `list(x)`), the object name will be used as the field name.
#'
#' ### Parameter-Specific Notes
#' - **`agents`**:
#'   - Specify a number to create a corresponding number of agents.
#'   This approach is useful for initializing the skeleton of a `G` object,
#'     with further details to be added later using functions like \link{modify_agent}.
#'   - Alternatively, provide a list of `ABM_Agent` objects created via \link{init_agent}.
#'     To include multiple agent groups, use a list, e.g., `agents = list(teacher = 2, student = 3)`.
#' - **`stage`**: Accepts various formats, including scalar, vector, matrix, array, data.frame, or list.
#'   Multiple stages can be specified as `stage = list(stage1 = mat1, stage2 = mat2)`.
#' - **Functions (`active_binding_field`, `global_FUN`, `select_FUN`, `stop_FUN`, `update_FUN`)**:
#'   Functions can be specified in one of four ways:
#'   1. Function object: e.g., `act_FUN = act_x1`.
#'   2. Anonymous function: e.g., `act_FUN = function() {...}`.
#'   3. Function name as a string: e.g., `act_FUN = "act_x1"`.
#'   4. Function name with arguments as a string: e.g., `act_FUN = "act_x1(a = 1)"`.
#' - **`active_binding_field`**: This parameter specifies dynamic fields
#' that calculate their values based on other fields in the `G` object.
#' - **`global_FUN`**: Operates on fields of the `G` object, similar to `act_FUN` at the agent level.
#' - **`select_FUN`**: Returns agent's index in the list of agents (NOT: `IDs`) based on a selection condition.
#' - **`stop_FUN`**: Ends the simulation if specific conditions are met (returns `TRUE`).
#' - **`update_FUN`**: Encapsulates one complete iteration of updates.
#' - **`partial_update_FUN_body`**: A modular way to predefine parts of `update_FUN`.
#'
#' ### Additional Notes
#' All functions (except `partial_update_FUN_body` and `active_binding_field`)
#' automatically receive two arguments internally: `G` (the `G` object) and `E`
#'  (a temporary environment for intermediate objects).
#'
#' ### Logging and Metadata
#' - **`log`**: Stores snapshots of the `G` object after each simulation step.
#' - **`time`**: Tracks the current simulation time step, starting at 1 by default.
#' - **`notes`**: Allows users to store custom metadata or notes as a list.
#'
#' @seealso [runABM], [init_agent], [ABM_G]
#' @author Keiichi Satoh
#' @import R6
#' @export
#' @examples
#' G <- setABM(agents = 3, stage = matrix(0, 3, 3))

setABM <- function(
    agents = NULL,
    stage = NULL,
    active_binding_field = NULL,
    global_FUN = NULL,
    select_FUN = NULL,
    stop_FUN = NULL,
    update_FUN = NULL,
    partial_update_FUN_body = NULL,
    log = NULL,
    time = NULL,
    notes = NULL,
    init = list(agents = NULL,
                stage = NULL,
                active_binding_field = NULL,
                global_FUN = NULL, select_FUN = NULL, stop_FUN = NULL, update_FUN = NULL,
                partial_update_FUN_body = NULL,
                log = NULL, time = NULL, notes = NULL)){
  # agent----------
  if(!is.null(init$agents)){
    agents <- init$agents
    agents_sbs <- substitute(agents)
  }else{
    agents_sbs <- substitute(agents)
  }
  agents_formatted <- .shape_agent(agents = agents,
                                   agents_sbs = agents_sbs)

  # stage------------
  if(!is.null(init$stage)){
    stage <- init$stage
    stage_sbs <- substitute(stage)
  }else{
    stage_sbs <- substitute(stage)
  }
  stage_formatted <- .shape_stage(stage = stage,
                                  stage_sbs = stage_sbs)


  # active_binding_field---
  if(!is.null(init$active_binding_field)){
    active_binding_field <- init$active_binding_field
    active_binding_field_sbs <- substitute(active_binding_field)
  }else{
    active_binding_field_sbs <- substitute(active_binding_field)
  }
  active_binding_field_formatted <- .shape_active_binding_field(
    active_binding_field = active_binding_field,
    active_binding_field_sbs = active_binding_field_sbs)

  # global_FUN----
  if(!is.null(init$global_FUN)){
    global_FUN <- init$global_FUN
    global_FUN_sbs <- substitute(global_FUN)
  }else{
    global_FUN_sbs <- substitute(global_FUN)
  }
  global_FUN_formatted <- .shape_global_FUN(global_FUN = global_FUN,
                                            global_FUN_sbs = global_FUN_sbs)

  # select_FUN----
  if(!is.null(init$select_FUN)){
    select_FUN <- init$select_FUN
    select_FUN_sbs <- substitute(select_FUN)
  }else{
    select_FUN_sbs <- substitute(select_FUN)
  }
  select_FUN_formatted <- .shape_select_FUN(select_FUN = select_FUN,
                                            select_FUN_sbs = select_FUN_sbs)

  # stop_FUN----
  if(!is.null(init$stop_FUN)){
    stop_FUN <- init$stop_FUN
    stop_FUN_sbs <- substitute(stop_FUN)
  }else{
    stop_FUN_sbs <- substitute(stop_FUN)
  }
  stop_FUN_formatted <- .shape_stop_FUN(stop_FUN = stop_FUN,
                                        stop_FUN_sbs = stop_FUN_sbs)

  # update_FUN----
  if(!is.null(init$update_FUN)){
    update_FUN <- init$update_FUN
    update_FUN_sbs <- substitute(update_FUN)
  }else{
    update_FUN_sbs <- substitute(update_FUN)
  }
  update_FUN_formatted <- .shape_update_FUN(update_FUN = update_FUN,
                                            update_FUN_sbs = update_FUN_sbs)

  # partial_FUN_expr---
  if(!is.null(init$partial_update_FUN_body)){
    partial_update_FUN_body <- init$partial_update_FUN_body
    partial_update_FUN_body_sbs <- substitute(partial_update_FUN_body)
  }else{
    partial_update_FUN_body_sbs <- substitute(partial_update_FUN_body)
  }
  partial_update_FUN_body_formatted <- .shape_partial_update_FUN_body(
    partial_update_FUN_body = partial_update_FUN_body,
    partial_update_FUN_body_sbs = partial_update_FUN_body_sbs)

  ## log---------
  if(!is.null(init$log)){log <- init$log}
  if(is.null(log)){
    log <- list(log = NULL)
  }else if(is.list(log)){
    log <- log
  }else{
    stop("log must be NULL or list.")
  }

  ## time--------
  if(!is.null(init$time)){time <- init$time}
  if(is.null(time)){
    time <- list(time = 1)
  }else if(rlang::is_scalar_atomic(time) && time >= 1){
    time <- list(time = time)
  }else{
    stop("time must be integer greater than or equal to 1.")
  }

  ## notes------
  if(!is.null(init$notes)){notes <- init$notes}
  if(is.null(notes)){
    notes <- NULL
  }else{
    if(is.data.frame(notes)){
      notes <- as.list(notes)
    }else if(!is.list(notes)){
      notes <- as.list(notes)
    }
  }

  ## field_category
  field_category <- c(agents_formatted$category,
                      stage_formatted$category,
                      active_binding_field_formatted$category,
                      global_FUN_formatted$category,
                      select_FUN_formatted$category,
                      stop_FUN_formatted$category,
                      update_FUN_formatted$category,
                      partial_update_FUN_body_formatted$category)

  ## Gを生成する
  G <- ABM_G$new(fields = c(agents_formatted$value,
                            stage_formatted$value,
                            partial_update_FUN_body_formatted$value,
                            time),
                 methods = c(global_FUN_formatted$value,
                             select_FUN_formatted$value,
                             stop_FUN_formatted$value,
                             update_FUN_formatted$value),
                 field_category = field_category,
                 log = log, notes = notes)

  # active_bindingを処理する(NULLではない場合)
  if(!is.null(active_binding_field_formatted$value)){
    active <- assign_func_envs(active_binding_field_formatted$value, G$.__enclos_env__)
    for(name in names(active)){
      makeActiveBinding(name, active[[name]], G$.__enclos_env__$self)
    }
    G$.__enclos_env__$.__active__ <- active
  }

  # logがNULLの場合には初期値を貼り付ける
  if(is.null(G$log)){
    G$.save()}

  # 同じ名前のfieldがないかを確認し、あればストップをかける
  check_name_tb <- table(ls(G))>1
  if(any(check_name_tb)){
    stop(paste0("The following field has a duplicated name. Please give each field a unique name: ", names(check_name_tb[check_name_tb])))
  }

  # Gを返却する
  G
}
