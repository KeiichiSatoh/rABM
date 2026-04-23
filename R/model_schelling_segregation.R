#' Schelling segregation model
#'
#' Run a Schelling-type segregation model on a 2D grid using the rABM framework.
#'
#' This function initializes a population of agents assigned to discrete grid
#' locations, each belonging to a group. Agents evaluate the proportion of
#' neighboring agents belonging to the same group and relocate if the proportion
#' falls below a specified threshold.
#'
#' The simulation proceeds until convergence or a maximum number of iterations
#' is reached.
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
#' @param max_times Maximum number of simulation steps.
#' @param tol Convergence tolerance for stopping rule based on changes in
#'   average same-group neighbor proportion.
#'
#' @details
#' The model consists of the following components:
#' \itemize{
#'   \item \strong{State variables}: agent group membership and location.
#'   \item \strong{Active states}: neighborhood composition and unhappy agents.
#'   \item \strong{Behavior}: unhappy agents relocate to vacant positions.
#'   \item \strong{Stopping rule}: convergence of average same-group neighbor proportion
#'     or reaching \code{max_times}.
#' }
#'
#' Neighborhood relationships are precomputed as a sparse matrix for efficiency.
#' Agent-group membership is also represented as a sparse matrix to accelerate
#' matrix operations.
#'
#' The simulation uses vectorized matrix operations for computing neighborhood
#' statistics, making it suitable for moderately large grids.
#'
#' @return
#' A list with the following elements:
#' \itemize{
#'   \item \code{G_init}: The initial \code{ABM_Game} object before simulation.
#'   \item \code{G_finished}: The \code{ABM_Game} object after simulation.
#' }
#'
#' @section Plot functions:
#' The model includes two plot functions:
#' \itemize{
#'   \item \code{plot_city}: Visualizes the spatial distribution of agents.
#'   \item \code{plot_unhappy_prop}: Tracks the proportion of unhappy agents over time.
#' }
#'
#' @section Report function:
#' \code{report_avg_same_prop()} returns:
#' \itemize{
#'   \item Current average same-group neighbor proportion (default), or
#'   \item Time series and plot when \code{from_log = TRUE}.
#' }
#'
#' @examples
#' \dontrun{
#' res <- model_schelling_segregation(
#'   vacant_prop = 0.2,
#'   group_prop = c(0.5, 0.5),
#'   minimum_same_prop = 0.5,
#'   n_row = 30,
#'   n_col = 30
#' )
#'
#' # Plot final configuration
#' plot(res$G_finished, "plot_city")
#'
#' # Plot unhappy proportion over time
#' plot(res$G_finished, "plot_unhappy_prop")
#'
#' # Report average same-group proportion
#' res$G_finished$report_avg_same_prop()
#' }
#'
#' @references
#' Schelling, T. C. (1971). Dynamic models of segregation.
#' \emph{Journal of Mathematical Sociology}, 1(2), 143–186.
#'
#' @export

model_schelling_segregation <- function(
    vacant_prop = 0.2,
    group_prop = c(0.5, 0.5),
    minimum_same_prop = 0.3,
    n_row = 30,
    n_col = 30,
    neib_type = c("moore", "neumann"),
    max_times = 500,
    tol = 0.1){

  # ======= Validation =======
  stopifnot(is.numeric(vacant_prop) && length(vacant_prop)==1 && vacant_prop > 0 && vacant_prop < 1)
  stopifnot(is.numeric(n_row) && length(n_row)==1 && n_row > 0)
  stopifnot(is.numeric(n_col) && length(n_col)==1 && n_col > 0)
  stopifnot(
    is.numeric(group_prop),
    length(group_prop) >= 2,
    all(group_prop > 0)
  )
  neib_type <- match.arg(neib_type)
  n_agent <- floor(n_row * n_col * (1 - vacant_prop))
  if(length(minimum_same_prop) == 1 && is.numeric(minimum_same_prop)) minimum_same_prop <- rep(minimum_same_prop, n_agent)
  stopifnot(all(is.numeric(minimum_same_prop)) && length(minimum_same_prop)==n_agent && all(minimum_same_prop >= 0) && all(minimum_same_prop <= 1))

  # ======= State ============
  agent_group <- make_group_labels(n_agent, prop = group_prop)
  agent_place <- sample(1:(n_row * n_col), n_agent)

  # cache some networks
  ## ---- agent * group -----
  group_nms <- unique(agent_group)
  net_agent_group <- matrix(0, nrow = n_agent, ncol = length(group_nms),
                            dimnames = list(seq_len(n_agent),
                                            paste0("group", group_nms)))
  for(j in seq_len(length(group_nms))){
    net_agent_group[ ,j] <- as.numeric(group_nms[j] == agent_group)
  }
  net_agent_group <- Matrix::Matrix(net_agent_group, sparse = TRUE)

  # ---- place * place neighborhood ----
  net_place_neib <- make_grid_neighborhood(n_row = n_row, n_col = n_col, type = neib_type,
                                           return_as = "network")

  settings <- list(n_agent = n_agent,
                   n_place = n_row * n_col,
                   n_row_place = n_row,
                   n_col_place = n_col,
                   net_place_neib = net_place_neib,
                   net_agent_group = net_agent_group,
                   minimum_same_prop = minimum_same_prop,
                   max_times = max_times,
                   tol = tol)

  G <- Game(State(agent_group), State(agent_place), State(settings))

  # ======= active state =======
  net_agent_place <- function(){
    Matrix::sparseMatrix(
      i = seq_len(self$settings$n_agent),
      j = self$agent_place,
      x = 1,
      dims = c(self$settings$n_agent, self$settings$n_place)
    )
  }
  add_field(G, Active(net_agent_place))

  same_group_neib_prop <- function(){
    ap <- self$net_agent_place  # 1回だけ取得
    neib_group_num <- ap %*% self$settings$net_place_neib %*%
      Matrix::t(ap) %*% self$settings$net_agent_group
    neib_group_num_prop <- neib_group_num / Matrix::rowSums(neib_group_num)
    neib_group_num_prop[is.nan(neib_group_num_prop)] <- 0
    Matrix::rowSums(self$settings$net_agent_group * neib_group_num_prop)
  }
  add_field(G, Active(same_group_neib_prop))

  unhappy_agent <- function(){
    which(self$same_group_neib_prop < self$settings$minimum_same_prop)
  }
  add_field(G, Active(unhappy_agent))

  # ======= Plot ========
  plot_city <- function(){
    num_groups <- ncol(self$settings$net_agent_group)
    if(num_groups == 2){
      cols <- c("white", "#E41A1C", "#377EB8")
    }else{
      cols <- c("white", RColorBrewer::brewer.pal(num_groups, "Set1"))
    }

    # Retrieve the relevant object
    city <- matrix(0, self$settings$n_row_place, self$settings$n_col_place)
    city[self$agent_place] <- self$agent_group

    # plot
    image(t(apply(city, 2, rev)), col = cols, axes = FALSE,
          main = paste("t =", self$time),
          sub = paste(
            paste0("Unhappy agents (%): ", round(length(self$unhappy_agent)/self$settings$n_agent * 100, 1)), "\n",
            paste0("Avg. same group neighbors (%): ", round(mean(self$same_group_neib_prop)*100, 1))
          ))
  }
  add_field(G, Plot(plot_city))

  plot_unhappy_prop <- function(){
    n_unhappy_agent <- unlist(value_of(self, field_name = "unhappy_agent",
                                       log = "all", return_FUN = length))
    unhappy_prop <- n_unhappy_agent/self$settings$n_agent
    time <- unlist(value_of(self, field_name = "time", log = "all"))

    plot(x = time, y = unhappy_prop, type = "l",
         ylim = c(0, 1), ylab = "Unhappy agents (%)", xlab = "time")
  }
  add_field(G, Plot(plot_unhappy_prop))

  # ======== Act ============
  move <- function(){
    vacant_place <- setdiff(seq_len(self$settings$n_place),
                            self$agent_place)
    unhappy_agent <- self$unhappy_agent

    if(length(vacant_place) < length(unhappy_agent)){
      unhappy_agent <- sample(unhappy_agent, size = length(vacant_place))
    }

    selected_place <- sample2(vacant_place, size = length(unhappy_agent))

    # move
    self$agent_place[unhappy_agent] <- selected_place
  }
  add_field(G, Act(move))

  # ======== stop_FUN =======
  eval_converge <- c()
  add_field(G, State(eval_converge))

  stop_by_convergence <- function(){
    if(self$time >= self$settings$max_times) return(TRUE)
    if(length(self$eval_converge) >= 10){
      if(all(abs(diff(self$eval_converge) < self$settings$tol))) return(TRUE)
      # update
      self$eval_converge <- c(self$eval_converge[-1], mean(self$same_group_neib_prop))
    }else{
      # update
      self$eval_converge <- c(self$eval_converge, mean(self$same_group_neib_prop))
    }
    return(FALSE)
  }
  add_field(G, Stop(stop_by_convergence))

  # ======== report_FUN =======
  report_avg_same_prop <- function(from_log = FALSE){
    if(isFALSE(from_log)){
      return(mean(self$same_group_neib_prop)*100)
    }else{
      same_gr_prop <- unlist(value_of(self, "same_group_neib_prop", log = "all", return_FUN = mean))
      times <- unlist(value_of(self, field_name = "time", log = "all"))
      plot(x = times, y = same_gr_prop * 100, xlab = "time", ylab = "Average same group neighbors (%)",
           ylim = c(0, 100), type = "l")

      data.frame(time = times,
                 same_group_prop = same_gr_prop)
    }
  }
  add_field(G, Report(report_avg_same_prop))

  # ======== Save the initial G =====
  G_init <- copy_obj(G)

  # ======== run ====================
  G_finished <- run_Game(G = G, plan = "move", nm_stop_FUN = "stop_by_convergence")

  # ======== output =================
  out <- list(G_init = G_init,
              G_finished = G_finished)
  return(out)
}
