#' Select Agents by Index
#'
#' @description
#' This function extracts agents from an **`ABM_G`** object's agent
#' list or from a standalone list of **`ABM_Agent`** objects.
#'
#' It is functionally equivalent to the `%ai%` (shallow copy) and
#' `%aidp%` (deep copy) operators. The main difference is that this function
#' is safer due to its validation checks and uses deep copy by default, while the
#' operators are slightly faster due to their lightweight design.
#'
#' You can achieve similar behavior to the `%ai%<-` replacement operator
#' by using \code{modify_agents()} with \code{method = "replace_agent"}.
#'
#' @seealso [%ai%], [%aidp%], [modify_agents()]
#'
#' @param G An object of class `ABM_G`. If supplied, agents will be extracted
#' from its field specified by `G_agents_name`.
#' @param G_agents_name A string giving the name of the field in `G` that stores
#' the agents (e.g., "agents"). Required if `G` is provided.
#' @param idx An integer vector of indices specifying which agents to extract.
#' @param agents A list of `ABM_Agent` objects. Used if `G` is not supplied.
#' @param deep Logical. If `TRUE` (default), a deep clone of each agent is returned;
#' if `FALSE`, a shallow copy is returned.
#'
#' @return A list of agents (either shallow or deep cloned).
#'
#' @examples
#' # Create and extract agents
#' agents <- init_agents(n = 3, attr_df = data.frame(x = 1:3))
#' get_agents_by_idx(agents = agents, idx = c(1, 3))
#'
#' # Use within ABM_G object
#' G <- setABM(agents = agents)
#' get_agents_by_idx(G = G, G_agents_name = "agents", idx = c(1, 2))
#'
#' @export

get_agents_by_idx <- function(G = NULL, G_agents_name = NULL, idx, agents = NULL, deep = TRUE){
  # Extract agents from G if supplied
  if (!is.null(G)) {
    stopifnot("G_agents_name must be supplied." = !is.null(G_agents_name))
    stopifnot("G_agents_name must be a character of length 1." = is.character(G_agents_name) && length(G_agents_name) == 1)
    agents <- G[[G_agents_name]]
  }

  # Check agents input
  stopifnot("Either 'G' or 'agents' must be supplied." = !is.null(agents))

  # Clone agents (deep or shallow)
  p <- if (deep) {
    lapply(idx, function(i) agents[[i]]$clone(deep = TRUE))
  } else {
    lapply(idx, function(i) agents[[i]])
  }

  # Assign names if available
  if (!is.null(nm <- names(agents))) names(p) <- nm[idx]

  # Return extracted agents
  p
}

