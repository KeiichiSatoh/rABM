#' Agent Selection by Index Operations
#'
#' @description
#' These operators  enable efficient and intuitive
#' extraction and replacement of agents within an **`ABM_G`** object's agent
#' list or a standalone list of **`ABM_Agent`** objects.
#' They are specifically designed for **Agent-Based Modeling (ABM)**
#' contexts, allowing you to access and modify agents by their numerical index.
#'
#' The `%ai%` operator extracts agents by index and returns shallow references
#' to the agents (i.e., original objects). In contrast, the `%ai%` operator
#' returns deep clones of the agents are returned instead.
#'
#' The `%ai%<-` replacement operator updates the agents at the given indices.
#' The length of `value` must match the length of `idx`.
#'
#' @param agents A list of ABM_Agent objects.
#' @param idx An integer vector of indices to extract or replace.
#' @param value A list of replacement agents. Must match the length of `idx`.
#'
#' @return
#' - `%ai%` returns a list of agents which are shallow copied.
#' - `%aidp%` returns a list of agents which are deep copied.
#' - `%ai%<-` returns the modified agent list.
#'
#' @examples
#' # Create and extract agents
#' agents <- init_agents(n = 3, attr_df = data.frame(x = 1:3))
#' agents %ai% c(1, 3)
#'
#' # Use deep clone if needed
#' agents %ai% c(1, 3)                  # This will return shallow copies
#' agents %aidp% c(1, 3)                # This will return deep copies
#'
#' # replace the 2nd agent to the 1st agent
#' agents %ai% 2 <- list(agents[[1]])
#'
#' # Use within ABM_G object
#' G <- setABM(agents = agents)
#' G$agents %ai% c(2, 3)
#' G$agents %ai% c(2, 3) <- list(G$agents[[1]], G$agents[[1]])
#'
#' @seealso \code{\link{%aa%}} for accessing and
#' modifying agent attributes.
#'
#' @rdname operators_agent_index
#' @export
`%ai%` <- function(agents, idx) {
  p <- lapply(idx, function(i) agents[[i]])
  if (!is.null(nm <- names(agents))) names(p) <- nm[idx]
  p
}

#' @rdname operators_agent_index
#' @export
`%aidp%` <- function(agents, idx) {
  p <- lapply(idx, function(i) agents[[i]]$clone(deep = TRUE))
  if (!is.null(nm <- names(agents))) names(p) <- nm[idx]
  p
}


#' @rdname operators_agent_index
#' @export
`%ai%<-` <- function(agents, idx, value) {
  stopifnot(length(idx) == length(value))

  for (i in seq_along(idx)) {
    agents[[idx[i]]] <- value[[i]]
  }

  agents
}
