#' Agent's act_FUN Execution Operator
#'
#' @description
#' This operator is part of the **`rABM`** framework and provides a concise and robust way
#' to apply a list of 'act_FUN' using a shared set of arguments.
#' It is particularly useful when working with action functions stored in
#' `ABM_Agent` objects and accessed via operators like \code{\link{%aa%}} or subsets using \code{\link{%ai%}}.
#'
#' The `%af%` operator applies each function in the list using `do.call()` with the provided
#' argument list. If a function raises an error, the error is caught and reported as a warning,
#' and the corresponding result is set to `NULL`. Execution continues for the remaining functions.
#'
#' This allows for robust batch execution of agent behaviors within a simulation step.
#'
#' @param act_FUNs A list of functions.
#' @param args A named list of arguments to apply to each function via `do.call()`.
#'
#' @return
#' A list of return values from each function. If a function fails, its result will be `NULL`.
#'
#' @examples
#' # Define an action function to increase age
#' get_older <- function() { self$age <- self$age + 1 }
#'
#' # Initialize agents with the action
#' agents <- init_agents(
#'   n = 3,
#'   attr_df = data.frame(age = 1:3),
#'   act_FUN = get_older
#' )
#'
#' # Apply to all agents (no arguments required)
#' agents %aa% "get_older" %af% list()
#'
#' # Create ABM_G object
#' G <- setABM(
#'   agents = agents
#' )
#'
#' # Apply to a subset of agents with argument G
#' G$agents %ai% c(1, 3) %aa% "get_older" %af% list(G = G)
#'
#' @seealso \code{\link{%aa%}} to extract agent functions,
#' \code{\link{%ai%}} to subset agents by index.
#'
#' @rdname operators_agent_function
#' @export
`%af%` <- function(act_FUNs, args){
  lapply(seq_along(act_FUNs), function(i){
    tryCatch(do.call(act_FUNs[[i]], args = args),
             error = function(e){warning(sprintf("[ERROR] %d-th act_FUN error: %s", i, e$message))
               return(NULL)})
  })
}


