#' Copy Agents from ABM_G Object or Agent List
#'
#' @description
#' Creates deep copies of agents from an \code{ABM_G} object or from a standalone list of agents.
#' This function ensures that each agent is cloned independently to avoid in-place modification
#' due to R6's reference semantics.
#'
#' @param G An \code{ABM_G} object from which agents will be extracted and copied. If supplied,
#' \code{G_agents_name} must also be provided.
#' @param G_agents_name A character string specifying the field name within \code{G}
#' that contains the agent list.
#' @param agents A list of agents (typically of class \code{ABM_Agent}) to be copied directly.
#'
#' @return A list of copied agents. If copying from \code{G}, the list corresponds to
#' \code{G[[G_agents_name]]}. If copying from \code{agents}, the result is a deep copy of each element.
#'
#' @details
#' Since agents are implemented as R6 objects, assignments like \code{agents2 <- agents} will
#' only create a new reference to the same objects, leading to unintentional in-place modification.
#' This function prevents such behavior by performing deep copies via the \code{clone(deep = TRUE)} method.
#'
#' If an object in the supplied \code{agents} list is not clonable (e.g., not an \code{ABM_Agent}),
#' a warning will be issued and that entry will be replaced with \code{NULL} in the result.
#
#' @export
#'
#' @examples
#' G <- setABM(agents = 3)
#' copied_from_G <- copy_agents(G = G, G_agents_name = "agents")
#'
#' agents <- init_agents(n = 3)
#' copied_from_list <- copy_agents(agents = agents)

copy_agents <- function(G = NULL, G_agents_name = NULL, agents = NULL) {
  # copy agents from G
  if (!is.null(G)) {
    stopifnot(!is.null(G_agents_name),
              is.character(G_agents_name),
              length(G_agents_name) == 1)

    G_copied <- G$clone(deep = TRUE)
    return(G_copied[[G_agents_name]])

  } else if (!is.null(agents)) {
    copied_agents <- lapply(seq_along(agents), function(i) {
      tryCatch({
        agents[[i]]$clone(deep = TRUE)
      }, error = function(e) {
        warning(sprintf("%d th object in 'agents' is not the class of 'ABM_Agent'.", i))
        NULL  # Return NULL for failed objects
      })
    })
    names(copied_agents) <- names(agents)
    return(copied_agents)

  } else {
    stop("Either 'G' or 'agents' must be supplied.")
  }
}

