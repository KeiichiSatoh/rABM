#' @title Compare Two ABM_G States
#'
#' @description
#' This function compares two ABM_G objects, either at two different time
#' points within a single object or across two objects,
#' and returns a structured report of differences in both stage-level variables
#' and agent-level attributes.
#'
#' Stage fields and agent attributes are traversed recursively to extract their
#' deepest values. Element-wise differences are identified, and any unsupported
#'  or missing comparisons are skipped with a warning.
#'
#' @param G An object of class `ABM_G`. This can optionally contain a time slice (`G$time`).
#' @param G2 An optional `ABM_G` object to compare against `G`.
#' If `time2` is supplied, `G2` must be `NULL`.
#' @param time1 Optional. A single integer indicating which time slice in `G$time`
#' to treat as the first comparison target.
#' @param time2 Optional. A single integer indicating which time slice in `G$time`
#'  to use as the second comparison target. Cannot be used together with `G2`.
#'
#' @return A named list with two components:
#' \describe{
#'   \item{`stage`}{A data.frame summarizing differences in stage-level variables across G and G2.}
#'   \item{`agents`}{A data.frame summarizing differences in agent-level attributes across G and G2.}
#' }
#'
#' @details
#' - If `time1` is specified, `G$time[[time1]]` is used in place of `G`.
#' - If `time2` is specified, it is assumed that `G$time[[time2]]` provides
#' the second comparison target, and `G2` must be `NULL`.
#' Agent attributes named ID are automatically excluded from comparison.
#'
#' Agents are compared based on their IDs rather than their positions in the list.
#' This ensures that logically corresponding agents are matched.
#'
#' @examples
#' agents1 <- init_agents(3, attr_df = data.frame(age = 1:3, sex = c("m","m","f")))
#' agents2 <- init_agents(3, attr_df = data.frame(age = 1:3, sex = c("m","f","f")))
#' G1 <- setABM(agents = list(agents = agents1))
#' G2 <- setABM(agents = list(agents = agents2))
#' compare_G(G1, G2)
#'
#' # Use time slices within G
#' G <- setABM(stage = list(y = 1), global_FUN = list(increase_y = function(){self$y <- self$y + 1}))
#' G <- runABM(G, plan = "increase_y", times = 1)
#' compare_G(G, time1 = 1, time2 = 2)
#' @export

compare_G <- function(G, G2 = NULL, time1 = NULL, time2 = NULL) {
  # Check G is ABM_G object
  stopifnot("'G' must be of class 'ABM_G'." = inherits(G, "ABM_G"))

  # If time1 is provided
  if (!is.null(time1)) {
    # validation is made within log_to_G function
    G1 <- log_to_G(G = G, which_time = time1)
    message("G at time 1 has been set as 'G1'.\n\n")
  }else{
    G1 <- copy_G(G)
    message("'G' has been set as 'G1'.\n\n")
  }
  G1_field_list <- G1$.field_list()

  # If time2 is provided (and G2 is not)
  if (!is.null(time2)) {
    if (!is.null(G2)) {
      stop("Specify either 'G2' or 'time2', not both.")
    }
    G2 <- log_to_G(G = G, which_time = time2)
    message("G at time 2 has been set as 'G2'.\n\n")
  } else {
    stopifnot("'G2' must be of class 'ABM_G'." = inherits(G2, "ABM_G"))
    G2 <- copy_G(G2)
  }
  G2_field_list <- G2$.field_list()

  #----------------------------------------------------------
  # unpack function (retrieve the deepest value in the list.)
  #----------------------------------------------------------
  unpack <- function(x) {
    if (!(is.list(x) && ("list" %in% class(x) || "data.frame" %in% class(x)))) {
      return(list(x))
    }
    unlist(lapply(x, unpack), recursive = FALSE)
  }
  #----------------------------------------------------------

  # warning flag environment（to avoid printing warnings)
  warned_once_env <- new.env(parent = emptyenv())

  #==============================================
  # comparing stages
  G1_stage_name <- G1_field_list$name[G1_field_list$category == "stage"]
  G2_stage_name <- G2_field_list$name[G2_field_list$category == "stage"]
  stage_names <- union(G1_stage_name, G2_stage_name)

  stage_compare_df <- data.frame(field_name = character(),
                                 idx = character(),
                                 G1 = character(),
                                 G2 = character(),
                                 stringsAsFactors = FALSE)

  stage_compare_list <- list()

  if (length(stage_names) > 0) {
    G1_stage <- mget(stage_names, envir = G1, ifnotfound = list(NULL))
    G2_stage <- mget(stage_names, envir = G2, ifnotfound = list(NULL))

    G1_stage_value <- unpack(G1_stage)
    G2_stage_value <- unpack(G2_stage)

    stage_comparison_name <- union(names(G1_stage_value), names(G2_stage_value))

    for (name in stage_comparison_name) {
      G1_value <- G1_stage_value[[name]]
      G2_value <- G2_stage_value[[name]]

      if (is.null(G1_value)) {
        temp_stage <- data.frame(field_name = name, idx = "NULL", G = "NULL", G2 = as.character(G2_value))
      } else if (is.null(G2_value)) {
        temp_stage <- data.frame(field_name = name, idx = "NULL", G = as.character(G1_value), G2 = "NULL")
      } else {
        if (any(is.na(G1_value))) warning("G1 has NA in stage field: ", name)
        if (any(is.na(G2_value))) warning("G2 has NA in stage field: ", name)

        idx_diff <- tryCatch({
          which(G1_value != G2_value)
        }, error = function(e) {
          key <- paste0("stage_", name)
          if (!exists(key, envir = warned_once_env)) {
            warning("Unsupported comparison in stage field: ", name)
            assign(key, TRUE, envir = warned_once_env)
          }
          return(NULL)
        })

        if (!is.null(idx_diff) && length(idx_diff) > 0) {
          temp_stage <- data.frame(field_name = name,
                                   idx = idx_diff,
                                   G1 = as.character(G1_value[idx_diff]),
                                   G2 = as.character(G2_value[idx_diff]))
        } else {
          temp_stage <- NULL
        }
      }

      if (!is.null(temp_stage)) {
        stage_compare_list[[length(stage_compare_list) + 1]] <- temp_stage
      }
    }
  }

  stage_compare_df <- if (length(stage_compare_list) > 0) {
    do.call(rbind, stage_compare_list)
  } else {
    data.frame(field_name = character(),
               idx = character(),
               G1 = character(),
               G2 = character(),
               stringsAsFactors = FALSE)
  }

  # =============================================
  # comparing agents
  G1_agents_name <- unique(na.exclude(G1_field_list$agent_name))
  G2_agents_name <- unique(na.exclude(G2_field_list$agent_name))
  agents_names <- union(G1_agents_name, G2_agents_name)

  agents_compare_list <- list()

  for (agents_name in agents_names) {
    G1_attrs <- G1_field_list$name[G1_field_list$agent_name == agents_name & G1_field_list$category == "agent_attribute"]
    G2_attrs <- G2_field_list$name[G2_field_list$agent_name == agents_name & G2_field_list$category == "agent_attribute"]
    attribute_names <- union(setdiff(G1_attrs, "ID"), setdiff(G2_attrs, "ID"))

    G1_IDs <- if (!is.null(G1[[agents_name]])) vapply(G1[[agents_name]], function(a) a$ID, numeric(1)) else numeric(0)
    G2_IDs <- if (!is.null(G2[[agents_name]])) vapply(G2[[agents_name]], function(a) a$ID, numeric(1)) else numeric(0)
    IDs <- union(G1_IDs, G2_IDs)

    for (ID in IDs) {
      G1_agent_i <- tryCatch(G1[[agents_name]] %aid_dp% ID, error = function(e) list())
      G2_agent_i <- tryCatch(G2[[agents_name]] %aid_dp% ID, error = function(e) list())

      G1_vals <- unpack(mget(attribute_names, envir = G1_agent_i[[1]], ifnotfound = list(NULL)))
      G2_vals <- unpack(mget(attribute_names, envir = G2_agent_i[[1]], ifnotfound = list(NULL)))

      attr_names <- union(names(G1_vals), names(G2_vals))

      for (name in attr_names) {
        G1_value <- G1_vals[[name]]
        G2_value <- G2_vals[[name]]

        if (is.null(G1_value)) {
          temp <- data.frame(agents_name = agents_name, agent_ID = ID,
                             field_name = name, idx = "NULL",
                             G1 = "NULL", G2 = as.character(G2_value))
        } else if (is.null(G2_value)) {
          temp <- data.frame(agents_name = agents_name, agent_ID = ID,
                             field_name = name, idx = "NULL",
                             G1 = as.character(G1_value), G2 = "NULL")
        } else {
          if (any(is.na(G1_value))) warning("G1 has NA in: ", agents_name, " ID=", ID, " attr=", name)
          if (any(is.na(G2_value))) warning("G2 has NA in: ", agents_name, " ID=", ID, " attr=", name)

          idx_diff <- tryCatch({
            which(G1_value != G2_value)
          }, error = function(e) {
            key <- paste0(agents_name, "_", ID, "_", name)
            if (!exists(key, envir = warned_once_env)) {
              warning("Unsupported comparison in agent field: ", key)
              assign(key, TRUE, envir = warned_once_env)
            }
            return(NULL)
          })

          if (!is.null(idx_diff) && length(idx_diff) > 0) {
            temp <- data.frame(agents_name = agents_name, agent_ID = ID,
                               field_name = name,
                               idx = idx_diff,
                               G1 = as.character(G1_value[idx_diff]),
                               G2 = as.character(G2_value[idx_diff]))
          } else {
            temp <- NULL
          }
        }

        if (!is.null(temp)) {
          agents_compare_list[[length(agents_compare_list) + 1]] <- temp
        }
      }
    }
  }

  agents_compare_df <- if (length(agents_compare_list) > 0) {
    do.call(rbind, agents_compare_list)
  } else {
    data.frame(agents_name = character(), agent_ID = integer(),
               field_name = character(), idx = character(),
               G1 = character(), G2 = character(), stringsAsFactors = FALSE)
  }

  # return the results as a list
  list(
    stage = stage_compare_df,
    agents = agents_compare_df
  )
}
