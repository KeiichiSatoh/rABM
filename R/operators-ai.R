#' Agent selection operations
#'
#' These operators allow you to extract or replace a subset of agents in a list,
#' typically used within an agent-based modeling (ABM) framework.
#'
#' The `%ai%` operator extracts agents by index. By default, it returns shallow references
#' to the agents (i.e., original objects). When `deep = TRUE` is specified, deep clones
#' of the agents are returned instead.
#'
#' The `%ai%<-` replacement operator updates the agents at the given indices.
#'
#' @usage agents \%ai\% idx
#' @usage \%ai\%(agents, idx, deep = FALSE)
#' @usage agents \%ai\% idx <- value
#'
#' @param agents A list of ABM_Agent objects.
#' @param idx An integer vector of indices to extract or replace.
#' @param deep Logical. If TRUE, clones of the agents are returned. Only used with `%ai%`. Default is FALSE.
#' @param value A list of replacement agents. Must match the length of `idx`.
#'
#' @return
#' - `%ai%` returns a list of agents (shallow or deep copies).
#' - `%ai%<-` returns the modified agent list.
#'
#' @examples
#' # Create and extract agents
#' agents <- init_agents(n = 3, attr_df = data.frame(x = 1:3))
#' agents %ai% c(1, 3)
#'
#' # Use deep clone if needed
#' `%ai%`(agents, c(1, 3), deep = TRUE)
#'
#' # Use within ABM_G object
#' G <- setABM(agents = agents)
#' G$agents %ai% c(2, 3)
#' G$agents %ai% c(2, 3) <- list(G$agents[[1]], G$agents[[1]])
#'
#' @seealso \code{\link{%aa%}} for accessing agent attributes,
#'   and \code{\link{%aa%<-}} for modifying agent attributes.
#'
#' @rdname agent_selection_ops
#' @export
`%ai%` <- function(agents, idx, deep = FALSE) {
  if (deep) {
    p <- lapply(idx, function(i) agents[[i]]$clone(deep = TRUE))
  } else {
    p <- lapply(idx, function(i) agents[[i]])
  }
  if (!is.null(nm <- names(agents))) names(p) <- nm[idx]
  p
}

#' @rdname agent_selection_ops
#' @export
`%ai%<-` <- function(agents, idx, value) {
  stopifnot(length(idx) == length(value))

  for (i in seq_along(idx)) {
    agents[[idx[i]]] <- value[[i]]
  }

  agents
}
