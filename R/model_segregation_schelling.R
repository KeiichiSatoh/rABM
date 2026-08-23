#' Schelling segregation model
#'
#' Run a Schelling-type segregation model on a 2D grid using the rABM framework.
#'
#' This function initializes a population of agents assigned to discrete grid
#' locations, each belonging to a group. Agents evaluate the proportion of
#' neighboring agents belonging to the same group and relocate if the proportion
#' falls below a specified threshold.
#'
#' The simulation proceeds until convergence.
#'
#' @param vacant_prop A numeric scalar in (0, 1) indicating the proportion of
#'   empty cells in the grid.
#' @param group_prop A numeric vector specifying the proportion of each group.
#'   Must sum to 1 and have length >= 2.
#' @param minimum_same_prop A numeric scalar or vector indicating the minimum
#'   acceptable proportion of same-group neighbors for each agent. If scalar,
#'   it is recycled to all agents.
#' @param n_row Number of rows in the grid.
#' @param n_col Number of columns in the grid.
#' @param neib_type Neighborhood type. Either \code{"moore"} (8 neighbors) or
#'   \code{"neumann"} (4 neighbors).
#' @param max_times Reserved for the special value \code{0}: if
#'   \code{max_times == 0}, the model is initialized but never run, and the
#'   unrun \code{ABM_Game} object is returned immediately. For any other
#'   value the simulation is run to convergence (see \code{convergence_thresh},
#'   \code{convergence_eval_by}); \code{max_times} does not currently cap the
#'   number of steps in that case.
#' @param convergence_thresh Convergence tolerance for the stopping rule,
#'   based on changes in \code{unhappy_agent_prop} (see \code{convergence_eval_by}).
#' @param convergence_eval_by How convergence is evaluated (passed to
#'   \code{add_stop_convergence()}'s \code{eval_by} argument), e.g. \code{"sd"}.
#'
#' @details
#' Unhappy agents are defined to be those who are surrounded by less than
#' the desired minimum proportion of the same group of other agents at the current place.
#' Those unhappy agents will then move to another place at the next run.
#'
#' Concretely, the update procedure proceeds as follows:
#' \enumerate{
#'   \item Unhappy agents move to another randomly chosen place.
#'   \item The map of the city is updated accordingly.
#'   \item The proportion of the surrounding same group agents at the new place is
#'     calculated for each agent.
#'   \item The proportion of the agents who are still unhappy at the new place is
#'     calculated.
#' }
#'
#' @return
#' An \code{ABM_Game} object: either the freshly initialized (unrun) model
#' when \code{max_times == 0}, or the finished model after running to
#' convergence otherwise.
#'
#' @section Plot functions:
#' The model registers one plot function:
#' \itemize{
#'   \item \code{plot_city}: Visualizes the spatial distribution of agents,
#'     annotated with the current unhappy-agent proportion and average
#'     same-group neighbor proportion.
#' }
#'
#' @section Report function:
#' \code{report_stats()} (invisibly) returns:
#' \itemize{
#'   \item Current time, proportion of unhappy agents, and average same-group
#'     neighbor proportion (default, \code{log = NULL}), or
#'   \item A time series (and, if \code{show_plot = TRUE}, a plot) of the same
#'     statistics when \code{log} is supplied (e.g. \code{log = "all"}). A
#'     message is printed confirming the requested time(s) were retrieved.
#' }
#'
#' @examples
#' \dontrun{
#' res <- model_segregation_schelling(
#'   vacant_prop = 0.2,
#'   group_prop = c(0.5, 0.5),
#'   minimum_same_prop = 0.5,
#'   n_row = 30,
#'   n_col = 30
#' )
#'
#' # Plot final configuration
#' plot(res, "plot_city")
#'
#' # Report average same-group proportion
#' res$report_stats()
#'
#' # Time series across the run
#' res$report_stats(log = "all")
#' }
#'
#' @references
#' Schelling, T. C. (1971). Dynamic models of segregation.
#' \emph{Journal of Mathematical Sociology}, 1(2), 143–186.
#'
#' @export
model_segregation_schelling <- function(
    vacant_prop = 0.2,
    group_prop = c(0.5, 0.5),
    minimum_same_prop = 0.3,
    n_row = 10,
    n_col = 10,
    neib_type = c("moore", "neumann"),
    max_times = 500,
    convergence_thresh = 0.01,
    convergence_eval_by = "sd"){
  # ======= Validation =======
  stopifnot(
    "'vacant_prop' must be a single numeric value strictly between 0 and 1." =
      is.numeric(vacant_prop) && length(vacant_prop) == 1 && vacant_prop > 0 && vacant_prop < 1
  )
  stopifnot(
    "'n_row' must be a single positive integer." =
      is.numeric(n_row) && length(n_row) == 1 && n_row > 0 && n_row %% 1 == 0
  )
  stopifnot(
    "'n_col' must be a single positive integer." =
      is.numeric(n_col) && length(n_col) == 1 && n_col > 0 && n_col %% 1 == 0
  )
  stopifnot(
    "'group_prop' must be a numeric vector of length >= 2 with all positive values." =
      is.numeric(group_prop) && length(group_prop) >= 2 && all(group_prop > 0)
  )
  stopifnot(
    "'group_prop' must sum to 1." =
      isTRUE(all.equal(sum(group_prop), 1))
  )
  neib_type <- match.arg(neib_type)
  n_agent <- floor(n_row * n_col * (1 - vacant_prop))
  if (length(minimum_same_prop) == 1 && is.numeric(minimum_same_prop)) {
    minimum_same_prop <- rep(minimum_same_prop, n_agent)
  }
  stopifnot(
    "'minimum_same_prop' must be numeric, of length 'n_agent' (or a single value to be recycled), with all values in [0, 1]." =
      is.numeric(minimum_same_prop) && length(minimum_same_prop) == n_agent &&
      all(minimum_same_prop >= 0) && all(minimum_same_prop <= 1)
  )
  # ======= Initialize the Game object =======
  G <- Game()
  # ======= State =====================
  # group_prop's names (if any) are dropped so that agent$group is always a
  # 1-based integer label (1, 2, ...), matching the 0 = "vacant" convention
  # used by group_map()/plot_city() below. Named proportions would otherwise
  # produce character labels and break image() in plot_city().
  # agent
  agent_group <- make_group_labels(n_agent, prop = unname(group_prop))
  agent_place <- sample(seq_len(n_row * n_col), n_agent)
  agent <- data.frame(ID = seq_len(n_agent),
                      group = agent_group,
                      place = agent_place,
                      minimum_same_prop = minimum_same_prop)
  add_field(G, State(agent))
  # settings
  settings <- list(n_agent = n_agent,
                   n_place = n_row * n_col,
                   city_nrow = n_row,
                   city_ncol = n_col,
                   neib_type = neib_type,
                   n_groups = length(group_prop))
  add_field(G, State(settings))
  # Other cached states
  # City
  ## Helper: create_map --
  create_map <- function(agent_place, agent_group, n_row, n_col){
    city <- matrix(NA, n_row, n_col)
    city[agent_place] <- agent_group
    city
  }
  city <- create_map(agent_place = agent$place, agent_group = agent$group,
                     n_row = n_row, n_col = n_col)
  add_field(G, State(city))
  # same_group_prop
  ## helper: calc_same_prop --
  calc_same_prop <- function(agent_posit, agent_group, city, neib_type){
    neib <- grid_neighbors(posit = agent_posit, mat = city, grid_type = neib_type)
    gr <- unique(agent_group)
    agent_ID <- seq_along(agent_group)
    n_same_gr <- numeric(length(agent_group))
    for(i in seq_along(gr)){
      gr_eval <- agent_group == gr[i]
      idx <- agent_ID[gr_eval]
      n_same_gr[idx] <- matrixStats::rowCounts(neib, rows = idx, value = gr[i], na.rm = TRUE)
    }
    # number of total actors
    n_row_total <- ncol(neib) - matrixStats::rowCounts(neib, value = NA)
    # proportion
    same_prop <- as.numeric(n_same_gr / n_row_total)
    same_prop[is.nan(same_prop)] <- 0
    same_prop
  }
  same_group_prop <- calc_same_prop(agent_posit = agent$place,
                                    agent_group = agent$group,
                                    city = city,
                                    neib_type = neib_type)
  add_field(G, State(same_group_prop))
  # unhappy agent
  unhappy_agent <- agent$ID[same_group_prop < minimum_same_prop]
  unhappy_agent_prop <- length(unhappy_agent) / settings$n_agent
  add_field(G, State(unhappy_agent), State(unhappy_agent_prop))
  # ======= Plot ========
  plot_city <- function(){
    # use the configured number of groups (length(group_prop)), not the
    # number currently observed among agents -- rounding/random adjustment
    # in make_group_labels() could otherwise leave a configured group with
    # zero agents and shrink num_groups below what brewer.pal() supports.
    num_groups <- self$settings$n_groups
    if(num_groups == 2){
      cols <- c("white", "#E41A1C", "#377EB8")
    }else{
      cols <- c("white", RColorBrewer::brewer.pal(num_groups, "Set1"))
    }
    # Retrieve the relevant object
    city <- self$city
    city[is.na(city)] <- 0
    # plot
    image(t(apply(city, 2, rev)), col = cols, axes = FALSE,
          main = paste("t =", self$time),
          sub = paste(
            paste0("Unhappy agents (%): ", round(self$unhappy_agent_prop * 100, 1)), "\n",
            paste0("Avg. same group neighbors (%): ", round(mean(self$same_group_prop)*100, 1))
          ))
  }
  add_field(G, Plot(plot_city))
  # ======== Act ============
  # move
  move <- function(){
    vacant_place <- setdiff(seq_len(self$settings$n_place),
                            self$agent$place)
    unhappy_agent <- self$unhappy_agent
    if(length(vacant_place) < length(unhappy_agent)){
      unhappy_agent <- sample2(unhappy_agent, size = length(vacant_place))
    }
    selected_place <- sample2(vacant_place, size = length(unhappy_agent))
    # move
    self$agent$place[unhappy_agent] <- selected_place
  }
  # update_city
  update_city <- function(){
    self$city <- create_map(agent_place = self$agent$place,
               agent_group = self$agent$group,
               n_row = self$settings$city_nrow,
               n_col = self$settings$city_ncol)
  }
  # update_same_group_prop
  update_same_group_prop <- function(){
    self$same_group_prop <- calc_same_prop(agent_posit = self$agent$place,
                   agent_group = self$agent$group,
                   city = self$city,
                   neib_type = self$settings$neib_type)
  }
  # update_unhappy_agent
  update_unhappy_agent <- function(){
    self$unhappy_agent <- self$agent$ID[self$same_group_prop < self$agent$minimum_same_prop]
    self$unhappy_agent_prop <- length(self$unhappy_agent) / self$settings$n_agent
  }
  add_field(G,
            Act(move),
            Act(update_city),
            Act(update_same_group_prop),
            Act(update_unhappy_agent))
  # ======== stop_FUN =======
  # add_stop_convergence() registers a "converged" stop_FUN based on
  # 'unhappy_agent_prop', which run_Game() below uses directly as its stop
  # condition. Note this means 'max_times' does not cap the run -- if
  # convergence is never reached, the simulation will keep going.
  G <- suppressMessages(
    add_stop_convergence(G, watching_field = "unhappy_agent_prop",
                         eval_by = convergence_eval_by,
                         thresh = convergence_thresh)
  )
  # ======== report_FUN =======
  report_stats <- function(log = NULL, show_plot = TRUE){
    if(is.null(log)){
      # retrieve stats
      time <- self$time
      prop_unhappy <- self$unhappy_agent_prop
      avg_same_group_neib <- mean(self$same_group_prop)
      # print
      cat("Time                            :", time, "\n")
      cat("Proportion of unhappy agents (%):", round(prop_unhappy * 100, 1), "\n")
      cat("Average same group neighbors (%):", round(avg_same_group_neib * 100, 1), "\n")
      invisible(c(time = time,
               prop_unhappy = prop_unhappy,
               avg_same_group_neib = avg_same_group_neib))
    }else{
      #-- FROM Log -----
      # retrieve stats
      out <- data.frame(
        time = as.numeric(value_of_log(self, "time", log = log)),
        prop_unhappy = as.numeric(value_of_log(self, "unhappy_agent_prop", log = log)),
        avg_same_group_neib = as.numeric(value_of_log(self, "same_group_prop", log = log, return_FUN = mean))
      )
      if(isTRUE(show_plot)){
        plot(x = out$time, y = out$avg_same_group_neib, type = "l", ylim = c(0,1),
             col = "blue", xlab = "time", ylab = "stats (%)", lwd = 2,
             sub = paste("Avg. same group (blue);",
                         "Prop. unhappy agents (red)"))
        par(new = TRUE)
        plot(x = out$time, y = out$prop_unhappy, type = "l", ylim = c(0,1),
             col = "red", lwd = 2, lty = 5, xlab = "", ylab = "",
             axes = FALSE)
      }
      message("The stats of the specified time(s) were calculated and returned.")
      invisible(out)
    }
  }
  add_field(G, Report(report_stats))
  # ======== add notes about which fields to save ====
  G$notes$fields_to_save <- c("agent", "same_group_prop",
                              "unhappy_agent_prop", "settings",
                              "plot_city", "city")
  # ======== Return the initial G (if max_times = 0) =====
  if(max_times == 0){
    return(G)
  }
  # ======== run ====================
  G <- run_Game(G = G,
                plan = c("move", "update_city", "update_same_group_prop", "update_unhappy_agent"),
                nm_stop_FUN = "converged",
                fields_to_save = G$notes$fields_to_save)
  # ======== output =================
  G
}
