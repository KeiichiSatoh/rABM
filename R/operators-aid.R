#' Agent Selection and Replacement by Agents IDs
#'
#' @description
#' These operators enable efficient and intuitive extraction and replacement of
#' agents from either an **`ABM_G`** object (via its `$agents` list) or
#' from a standalone list of **`ABM_Agent`** objects.
#'
#' The `%aid%` operator extracts agents by ID and returns shallow references
#' to the agents (i.e., original objects). In contrast, the `%aid_dp%` operator
#' returns deep clones of the agents instead.
#'
#' The `%aid%<-` replacement operator updates the agents at the given IDs.
#' The length of `value` must match the length of `ID`.
#'
#' If any of the provided IDs are not found among the agents, a warning is issued
#'  and those IDs are skipped. The corresponding value entries are also
#'  skipped accordingly.
#'
#' @param agents A list of ABM_Agent objects.
#' @param ID An integer vector of ID(s) to extract or replace.
#' @param value A list of replacement agents. Must match the length of `ID`.
#'
#' @return
#' - `%aid%` returns a list of agents which are shallow copied.
#' - `%aid_dp%` returns a list of agents which are deep copied.
#' - `%aid%<-` returns the modified agent list.
#'
#' @examples
#' # Create and extract agents
#' agents <- init_agents(n = 3, attr_df = data.frame(x = 1:3), ID_start = 2) # agent's ID starts from ID2
#' agents %aid% c(2, 4)
#'
#' # Use deep clone if needed
#' agents %aid% c(2, 4)                  # This will return shallow copies
#' agents %aid_dp% c(2, 4)                # This will return deep copies
#'
#' # # Replace the agent with ID 2 with the agent with ID 3
#' agents %aid% 2 <- agents %aid% 3
#'
#' # Use within ABM_G object
#' agents <- init_agents(n = 3, attr_df = data.frame(x = 1:3), ID_start = 2) # agent's ID starts from ID2
#' G <- setABM(agents = agents)
#' G$agents %aid% c(2, 3)
#'
#'
#' @seealso \code{\link{%ai%}} for accessing agents by index, and
#' \code{\link{%aa%}} for accessing and modifying agent attributes.
#'
#' @rdname operators_agent_ID
#' @export

`%aid%` <- function(agents, ID) {
  # get the whole IDs
  obs_ID <- vapply(agents, function(a) a$ID, numeric(1))
  # match the ID to the obs_ID
  idx <- match(ID, obs_ID)
  if(any(is.na(idx))){
    warning("Some of the 'ID's do not exist, which are skipped.")
    idx <- idx[!is.na(idx)]
  }
  # operate
  p <- lapply(idx, function(i) agents[[i]])
  if (!is.null(nm <- names(agents))) names(p) <- nm[idx]
  p
}


#' @rdname operators_agent_ID
#' @export
`%aid_dp%` <- function(agents, ID) {
  # get the whole IDs
  obs_ID <- vapply(agents, function(a) a$ID, numeric(1))
  # match the ID to the obs_ID
  idx <- match(ID, obs_ID)
  if(any(is.na(idx))){
    warning("Some of the 'ID's do not exist, which are skipped.")
    idx <- idx[!is.na(idx)]
  }
  # operate
  p <- lapply(idx, function(i) agents[[i]]$clone(deep = TRUE))
  if (!is.null(nm <- names(agents))) names(p) <- nm[idx]
  p
}

#' @rdname operators_agent_ID
#' @export
`%aid%<-` <- function(agents, ID, value) {
  # get the whole IDs
  obs_ID <- vapply(agents, function(a) a$ID, numeric(1))

  # match the ID to the obs_ID
  idx <- match(ID, obs_ID)

  # warn and skip non-matching IDs
  if (any(is.na(idx))) {
    warning("Some of the 'ID's do not exist, which are skipped.")
    valid <- !is.na(idx)
    value <- value[valid]
    idx <- idx[valid]
  }

  stopifnot(length(idx) == length(value))

  # perform replacement
  for (i in seq_along(idx)) {
    agents[[idx[i]]] <- value[[i]]
  }

  agents
}
