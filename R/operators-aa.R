#' Agent Attribute Operations
#'
#' @description
#' These operators, integral to the **`rABM`** package framework, provide a direct
#' and intuitive way to access and modify individual attributes of **`ABM_Agent`** objects.
#' They are designed to work seamlessly within an **`ABM_G`** object's agent list or
#' with a standalone list of `ABM_Agent` objects, simplifying data manipulation in
#' Agent-Based Modeling (ABM) contexts.
#'
#' The `%aa%` operator extracts a specified attribute from each agent in the list,
#' returning a named list of these attribute values.
#'
#' The `%aa%<-` replacement operator assigns new values to a specified attribute
#' for each agent. The `value` list provided for assignment must match the length
#' of the `agents` list.
#'
#' @param agents A list of ABM_Agent objects.
#' @param val_name A character string. The name of the attribute to access or assign.
#' @param value A list of values to assign (for `%aa%<-`).
#'
#' @return
#' `%aa%` returns a named list of attribute values extracted from the agents.
#'
#' `%aa%<-` returns the modified list of agents with the updated attribute values.
#'
#' @examples
#' # Initialize agents with an 'age' attribute
#' agents <- init_agents(n = 3, attr_df = data.frame(age = 1:3, name = c("A", "B", "C")))
#'
#' # Extract agent attributes using %aa%
#' agents %aa% "age"
#' agents %aa% "name"
#'
#' # Set agent attributes using %aa%<-
#' agents %aa% "age" <- list(10, 20, 30)
#' agents %aa% "name" <- list("Alpha", "Beta", "Gamma")
#' print(agents %aa% "age")
#' print(agents %aa% "name")
#'
#' # Use within an ABM_G object
#' G <- setABM(agents = init_agents(n = 3, attr_df = data.frame(age = 1:3)))
#' G$agents %aa% "age"
#' G$agents %aa% "age" <- list(1, 2, 3)
#' print(G$agents %aa% "age")
#'
#' # Combine with %ai% to access subsets and attributes
#' # First, extract a subset of agents by index, then access their 'age' attribute
#' G$agents %ai% c(1, 3) %aa% "age"
#' # Then, update the 'age' attribute for that subset
#' G$agents %ai% c(1, 3) %aa% "age" <- list(50, 70)
#' print(G$agents %aa% "age") # Verify changes
#'
#' @seealso \code{\link{ABM_Agent}} for the agent class,
#' \code{\link{%ai%}} for accessing agents by index.
#'
#' @rdname operators_agent_attribute
#' @export
`%aa%` <- function(agents, val_name) {
  X <- lapply(seq_along(agents), function(i) agents[[i]][[val_name]])
  if (!is.null(nm <- names(agents))) names(X) <- nm
  return(X)
}

#' @rdname operators_agent_attribute
#' @export
`%aa%<-` <- function(agents, val_name, value) {
  stopifnot(length(agents) == length(value))

  for (i in seq_along(agents)) {
    agents[[i]][[val_name]] <- value[[i]]
  }

  return(agents)
}
