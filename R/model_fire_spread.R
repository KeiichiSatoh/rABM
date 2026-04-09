#' Forest Fire Spread Simulation
#'
#' Simulates the spread of fire through a forest on a two-dimensional grid
#' using a cellular automaton model. Each cell is either vacant, intact,
#' burning, or burned. At each time step, burning trees ignite their Moore
#' neighbours and then burn out. The simulation stops when all non-vacant
#' trees have burned or the maximum number of time steps is reached.
#'
#' @param vacancy_prop A numeric scalar in \eqn{[0, 1]} giving the proportion
#'   of grid cells that contain no tree. Default: \code{0.4}.
#' @param ignition_point Optional. Specifies the cell(s) where fire starts.
#'   \itemize{
#'     \item If \code{NULL} (default), one intact tree is chosen at random.
#'     \item If a two-column integer matrix, each row is interpreted as
#'           \code{(row, col)} coordinates and converted to a linear index
#'           via \code{\link{grid_index}}.
#'     \item If a single integer (or integer vector), it is used directly as
#'           a linear cell index.
#'   }
#' @param n_row A positive integer giving the number of rows in the grid.
#'   Default: \code{50}.
#' @param n_col A positive integer giving the number of columns in the grid.
#'   Default: \code{50}.
#' @param seed An optional positive integer passed to \code{\link{set.seed}}
#'   for reproducibility. If \code{NULL} (default), no seed is set.
#' @param spark A logical value. If \code{TRUE}, one additional randomly
#'   chosen intact cell is ignited at every time step, simulating flying
#'   sparks. Default: \code{FALSE}.
#' @param burn_prob A numeric scalar in \eqn{[0, 1]} giving the probability
#'   that an intact tree adjacent to a burning tree (or struck by a spark)
#'   catches fire at a given time step. A value of \code{1} means fire always
#'   spreads; lower values introduce stochastic resistance.
#'   Default: \code{1}.
#'
#' @details
#' ## Cell states
#'
#' Each cell is encoded as an integer:
#'
#' | Code | State   |
#' |------|---------|
#' | 0    | Vacant  |
#' | 1    | Burned  |
#' | 2    | Burning |
#' | 3    | Intact  |
#'
#' ## Neighbourhood
#'
#' Fire spreads to the Moore neighbourhood (eight surrounding cells) of every
#' currently burning cell. Edge cells have fewer than eight neighbours;
#' missing neighbours are stored as \code{NA} and excluded during propagation.
#'
#' ## Time step logic
#'
#' Each time step proceeds as follows:
#'
#' \enumerate{
#'   \item Identify all currently burning cells.
#'   \item For each burning cell, collect intact neighbours. Each neighbour
#'         independently ignites with probability \code{burn_prob}.
#'   \item If \code{spark = TRUE}, one additional random intact cell ignites
#'         (also subject to \code{burn_prob}).
#'   \item All cells that were burning at the start of the step transition to
#'         burned (\code{2 -> 1}).
#' }
#'
#' ## Stopping rule
#'
#' The simulation halts when either:
#' \itemize{
#'   \item all non-vacant cells have burned, or
#'   \item the time step counter reaches \code{max_time} (hard-coded to
#'         \code{300} inside the internal \code{sim_stop} function).
#' }
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{\code{G_init}}{A \code{Game} object capturing the initial state of
#'     the grid before any fire propagation.}
#'   \item{\code{G_finished}}{A \code{Game} object capturing the final state
#'     of the grid together with the complete simulation log. Pass this object
#'     to \code{\link{plot}} or \code{\link{animate_log}} for visualisation.}
#' }
#'
#' @note
#' \itemize{
#'   \item The maximum number of time steps is hard-coded to \code{300} inside
#'         the internal \code{sim_stop} function. Modify the function body
#'         directly if a different limit is required.
#'   \item Post-simulation plotting is \strong{not} performed automatically.
#'         Use the returned \code{G_finished} object explicitly
#'         (see Examples).
#' }
#'
#' @seealso
#' \code{\link{make_grid_neighborhood}},
#' \code{\link{run_Game}},
#' \code{\link{animate_log}},
#' \code{\link{grid_index}}
#'
#' @examples
#' \dontrun{
#' # Basic run with default settings
#' result <- model_fire_spread(vacancy_prop = 0.3, seed = 1)
#'
#' # Visualise selected time steps
#' plot(result$G_finished, name = "plot_land", log = c(1, 10, 20, 30, 42))
#'
#' # Animate selected time steps
#' animate_log(result$G_finished,
#'             name = "plot_land")
#'
#' # With stochastic spread and flying sparks
#' result2 <- model_fire_spread(
#'   vacancy_prop = 0.1,
#'   burn_prob    = 0.7,
#'   spark        = TRUE,
#'   seed         = 1
#' )
#'
#' # Fixed ignition point supplied as (row, col) coordinates
#' result3 <- model_fire_spread(
#'   ignition_point = matrix(c(25, 25), nrow = 1),
#'   vacancy_prop = 0.2,
#'   seed = 1
#' )
#' }
#' @export
model_fire_spread <- function(
    vacancy_prop = 0.3,
    ignition_point = NULL,
    n_row = 50,
    n_col = 50,
    seed = NULL,
    spark = FALSE,
    burn_prob = 1
){
  # set seed
  if(!is.null(seed)){
    stopifnot("'seed' must be a positive integer." = length(seed) == 1 && is.numeric(seed))
    set.seed(seed = seed)
  }
  stopifnot("'burn_prob' must be a numeric scalar ranging 0 and 1." = length(burn_prob) && length(burn_prob)==1 && burn_prob <= 1 && burn_prob >= 0)
  # ===== State ===========
  # Create a grid
  n_cell <- n_row * n_col
  n_alive <- floor(n_cell*(1 - vacancy_prop))
  alive_idx <- sample(1:n_cell, size = n_alive)
  tree_condition <- rep(0, n_cell)
  tree_condition[alive_idx] <- 3

  # neighborhood index
  tree_neib_idx <- make_grid_neighborhood(n_row = n_row, n_col = n_col, type = "moore")

  # create a Game
  G <- Game(State(tree_condition), State(tree_neib_idx))

  # settings
  settings <- list(n_row = n_row,
                   n_col = n_col,
                   n_cell = n_cell,
                   spark = spark,
                   burn_prob = burn_prob)
  add_field(G, State(settings))

  #===== Active State ==========

  # landmap
  land_map <- function(){
    matrix(self$tree_condition, self$settings$n_row, self$settings$n_col)}
  add_field(G, Active(land_map))

  # statistics
  tree_stat <- function(){
    cond <- factor(self$tree_condition,
                   levels = c(0, 1, 2, 3),
                   labels = c("vacant", "burned", "burning", "intact"))
    round(prop.table(table(cond, exclude = "vacant"))*100,1)
  }
  add_field(G, Active(tree_stat))


  #===== plot_FUN ==============
  plot_land <- function() {
    mat <- self$land_map
    graphics::image(
      z = t(mat[nrow(mat):1, , drop = FALSE]),
      col = c("white", "brown", "red", "green"),
      breaks = c(-0.5, 0.5, 1.5, 2.5, 3.5),
      axes = FALSE,
      main = paste("time =", self$time)
    )
  }
  add_field(G, Plot(plot_land))

  #===== Act ===============
  # define burning
  burn <- function(spark = self$settings$spark, burn_prob = self$settings$burn_prob){
    # select burning tree
    burning_idx <- which(self$tree_condition == 2)
    if(length(burning_idx)==0) return(NULL)

    for(id in burning_idx){
      # burn the neighboring trees
      neib_idx <- na.exclude(self$tree_neib_idx[id, ])
      green_tree_idx <- which(self$tree_condition == 3)
      neib_idx_to_burn <- intersect(neib_idx, green_tree_idx)
      if(burn_prob < 1){
        which_really_burn <- sample(c(TRUE, FALSE),
                                    size = length(neib_idx_to_burn),
                                    replace = TRUE,
                                    prob = c(burn_prob, 1 - burn_prob))
        neib_idx_to_burn <- neib_idx_to_burn[which_really_burn]
      }
      self$tree_condition[neib_idx_to_burn] <- 2

      # if spark == TRUE
      # select one place and burn it if it is a green tree
      if(isTRUE(spark)){
        cell_idx <- sample(seq_len(self$settings$n_cell), size = 1)
        if(self$tree_condition[cell_idx]==3){
          if(burn_prob < 1){
            really_burn <- sample(c(TRUE, FALSE), 1, prob = c(burn_prob, 1 - burn_prob))
            if(isFALSE(really_burn)) next
          }
          self$tree_condition[cell_idx] <- 2
        }
      }
    }

    # burn-out
    self$tree_condition[burning_idx] <- 1
  }
  add_field(G, Act(burn))


  #==== stop_FUN =============
  sim_stop <- function(max_time = 300){
    is_completely_burned <- self$tree_stat["burned"]==100
    is_time_met <- self$time >= max_time

    # is time to stop
    is_completely_burned||is_time_met
  }
  add_field(G, Stop(sim_stop))

  #==== report_FUN ============
  report_stat <- function(){
    stat <- do.call(rbind, value_of(self, "tree_stat", log = "all"))
    time <- value_of(self, "time", log = "all")

    plot(y = stat[, "burned"], x = time, type = "l", col = "brown", ylim = c(0,100),
         ylab = "%",  lwd=2,
         sub = paste("[legend] green = intact; red = burning; brown = burned"))
    par(new=T)
    plot(y = stat[, "burning"], x = time, type = "l", col = "red", ylim = c(0,100),
         lwd = 2, ann = FALSE)
    par(new=T)
    plot(y = stat[, "intact"], x = time, type = "l", col = "green", ylim = c(0,100),
         lwd = 2, ann = FALSE)

    # return
    stat
  }
  add_field(G, Report(report_stat))



  #==== before enter into run_Game, burn the initial point ==============
  if(!is.null(ignition_point)){
    if(ncol(ignition_point)==2) ignition_point <- grid_index(ignition_point, n_row = n_row)
    # burn
    G$tree_condition[ignition_point] <- 2
  }else{
    green_tree_idx <- which(G$tree_condition==3)
    ignition_point <- sample(green_tree_idx, 1)
    G$tree_condition[ignition_point] <- 2
  }

  #===== save the initial condition ========
  G_init <- copy_obj(G)

  # run_Game
  G_finished <- run_Game(G, plan = "burn", nm_stop_FUN = "sim_stop", seed = seed)

  # oupput
  out <- list(G_init = G_init,
              G_finished = G_finished)
  out
}
