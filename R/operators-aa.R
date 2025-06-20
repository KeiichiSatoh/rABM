#' Agent attribute operations
#'
#' These operators provide access to and assignment of agent attributes
#' within a list of ABM_Agent objects.
#'
#' The `%aa%` operator extracts a specified attribute from each agent.
#' The `%aa%<-` operator assigns values to a specified attribute in each agent.
#'
#' @param agents A list of ABM_Agent objects.
#' @param val_name A character string. The name of the attribute to access or assign.
#' @param value A list of values to assign (for `%aa%<-`). Must match the length of `agents`.
#'
#' @return
#' - `%aa%` returns a named list of attribute values.
#' - `%aa%<-` returns the modified list of agents.
#'
#' @examples
#' # Create agents with age attribute
#' agents <- init_agents(n = 3, attr_df = data.frame(age = 1:3))
#'
#' # Extract and set attributes using %aa%
#' agents %aa% "age"
#' agents %aa% "age" <- list(10, 20, 30)
#'
#' # Use within an ABM_G object (G)
#' G <- setABM(agents = init_agents(n = 3, attr_df = data.frame(age = 1:3)))
#' G$agents %aa% "age"
#' G$agents %aa% "age" <- list(10, 20, 30)
#'
#' # Combine with %ai% to access subsets
#' G <- setABM(agents = init_agents(n = 3, attr_df = data.frame(age = 1:3)))
#' G$agents %ai% c(1, 3) %aa% "age"
#' G$agents %ai% c(1, 3) %aa% "age" <- list(10, 30)
#'
#' @rdname agent_attr_ops
#' @export
`%aa%` <- function(agents, val_name) {
  X <- lapply(seq_along(agents), function(i) agents[[i]][[val_name]])
  if (!is.null(nm <- names(agents))) names(X) <- nm
  return(X)
}

#' @rdname agent_attr_ops
#' @export
`%aa%<-` <- function(agents, val_name, value) {
  stopifnot(length(agents) == length(value))

  for (i in seq_along(agents)) {
    agents[[i]][[val_name]] <- value[[i]]
  }

  return(agents)
}
