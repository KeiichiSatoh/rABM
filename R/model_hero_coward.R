
#' @title Heroes and Cowards toy model (rABM)
#'
#' @description
#' \code{model_hero_coward()} builds and runs a simplified **Heroes and Cowards**
#' agent-based model (inspired by the NetLogo model) using **rABM**.
#'
#' Agents are placed in a 2D continuous rectangular space and repeatedly move toward
#' a target point determined by their type:
#' \itemize{
#'   \item \strong{Hero} (\code{agent_type == 1}): moves toward the midpoint between its friend and enemy.
#'   \item \strong{Coward} (\code{agent_type == 0}): moves toward the \emph{shield point}, symmetric to the
#'   friend position with respect to the midpoint (i.e., moves away from the enemy while staying aligned
#'   with the friend–enemy line).
#'   \item \strong{Mixed} (\code{agent_type == 9}): randomly assigned to hero/coward at each time step.
#' }
#'
#' The model uses bounded space with the movement rule \code{movement_type = "stop"} in
#' \code{\link{act_coord_move}} (agents stop at the first boundary encountered).
#' The space is assumed to have lower bounds fixed at 0, i.e., \code{[0, x_max] × [0, y_max]}.
#'
#' @details
#' ## Returned objects
#' The function returns a list with two \code{ABM_Game} objects:
#' \itemize{
#'   \item \code{G_init}: initial game state (deep-cloned before execution)
#'   \item \code{G_finished}: finished game after running \code{sim_time} steps
#' }
#'
#' \code{G_finished} contains:
#' \itemize{
#'   \item \strong{States}: \code{agent_id}, \code{agent_type}, \code{posit}, \code{friend}, \code{enemy}, \code{settings}
#'   \item \strong{Active states}: \code{friend_posit}, \code{enemy_posit}
#'   \item \strong{Act functions}: \code{move_agent}, \code{change_mixed_type}
#'   \item \strong{Plot functions}: \code{plot_space}, \code{plot_relationship}
#'   \item \strong{Report functions}: \code{trace_posit_mean}, \code{trace_posit_sd}
#' }
#'
#' ## Friend/enemy relationship
#' Each agent has exactly one friend and one enemy, stored as integer vectors of length \code{n}
#' (names are agent IDs). If \code{relationship} is not provided, friend/enemy are sampled at random
#' excluding the agent itself.
#'
#' ## Randomness
#' If \code{seed} is \code{NULL}, the seed is set from the current time (\code{Sys.time()}) and used via
#' \code{set.seed()}.
#'
#' @param n Integer. Number of agents.
#' @param speed Numeric scalar. Movement distance per step passed to \code{\link{act_coord_move}}.
#' @param x_max,y_max Numeric scalars. Upper bounds of the space. Lower bounds are assumed to be 0.
#' @param sim_time Integer. Number of simulation steps (times) for \code{\link{run_Game}}.
#' @param agent_type Optional numeric/integer vector of length \code{n}. Each element must be one of
#'   \code{0} (coward), \code{1} (hero), \code{9} (mixed). If \code{NULL}, types are assigned randomly.
#' @param init_posit Optional numeric matrix \code{n × 2} with columns \code{c("x","y")} giving initial positions.
#'   All positions must fall within \code{[0, x_max]} and \code{[0, y_max]}.
#' @param relationship Optional integer matrix \code{n × 2}. Column 1 is friend index, column 2 is enemy index.
#'   Indices must be in \code{1:n}; friend and enemy must differ; neither may equal the ego agent.
#' @param seed Optional numeric/integer scalar. Random seed.
#'
#' @return A list with elements \code{G_init} and \code{G_finished}, both \code{ABM_Game} objects.
#'
#' @examples
#' \dontrun{
#' SIM <- model_hero_coward(n = 40, sim_time = 500)
#' SIM$G_finished$trace_posit_mean()
#' SIM$G_finished$trace_posit_sd()
#' animate_log(SIM$G_finished, name = "plot_space", delay = 0.2, check = FALSE)
#' }
#'
#' @seealso
#' \code{\link{Game}}, \code{\link{State}}, \code{\link{Active}}, \code{\link{Act}},
#' \code{\link{Plot}}, \code{\link{Report}}, \code{\link{add_field}},
#' \code{\link{run_Game}}, \code{\link{act_coord_move}}, \code{\link{animate_log}}
#'
#' @export
model_hero_coward <- function(n = 30,
                              speed = 0.1,
                              x_max = 1,
                              y_max = 1,
                              sim_time = 300,
                              agent_type = NULL,
                              init_posit = NULL,
                              relationship = NULL,
                              seed = NULL) {

  # ---- minimal but important checks (init-time only; not per-tick) ----
  stopifnot("'n' must be a single positive integer." =
              is.numeric(n) && length(n) == 1L && !is.na(n) && n >= 1 && (n %% 1 == 0))
  stopifnot("'sim_time' must be a single positive integer." =
              is.numeric(sim_time) && length(sim_time) == 1L && !is.na(sim_time) &&
              sim_time >= 1 && (sim_time %% 1 == 0))
  stopifnot("'speed' must be a single non-negative number." =
              is.numeric(speed) && length(speed) == 1L && !is.na(speed) && speed >= 0)
  stopifnot("'x_max' and 'y_max' must be single positive numbers." =
              is.numeric(x_max) && length(x_max) == 1L && !is.na(x_max) && x_max > 0 &&
              is.numeric(y_max) && length(y_max) == 1L && !is.na(y_max) && y_max > 0)

  # ---- seed ----
  if (is.null(seed)) seed <- as.numeric(Sys.time())
  set.seed(seed = seed)

  # ---- agent ID ----
  agent_id <- seq_len(n)

  # ---- agent_type ----
  if (!is.null(agent_type)) {
    stopifnot("The length of 'agent_type' must be 'n'." = length(agent_type) == n)
    stopifnot("Elements of 'agent_type' must be 0, 1, or 9." = all(agent_type %in% c(0, 1, 9)))
  } else {
    # assume make_group_labels(n, prop=c(.5,.5)) returns 1/2 labels
    agent_type <- make_group_labels(n, prop = c(0.5, 0.5))
    agent_type[agent_type == 2] <- 0
  }
  names(agent_type) <- agent_id

  # ============================================================
  # Initial state: positions
  # ============================================================
  if (!is.null(init_posit)) {
    stopifnot("'init_posit' must be a matrix of n x 2." = is.matrix(init_posit))
    stopifnot("Number of rows must be 'n' in 'init_posit'." = nrow(init_posit) == n)
    stopifnot("Number of columns must be 2 in 'init_posit'." = ncol(init_posit) == 2)

    if (!all(init_posit[, 1] >= 0 & init_posit[, 1] <= x_max) ||
        !all(init_posit[, 2] >= 0 & init_posit[, 2] <= y_max)) {
      stop("Each agent position must be within [0, x_max] x [0, y_max].")
    }

    posit <- init_posit
    dimnames(posit) <- list(as.character(agent_id), c("x", "y"))
  } else {
    posit <- matrix(0, nrow = n, ncol = 2,
                    dimnames = list(as.character(agent_id), c("x", "y")))
    posit[, 1] <- runif(n = n, min = 0, max = x_max)
    posit[, 2] <- runif(n = n, min = 0, max = y_max)
  }

  # ============================================================
  # Friend/enemy relationship
  # ============================================================
  if (!is.null(relationship)) {
    stopifnot("'relationship' must be a matrix of n x 2." =
                is.matrix(relationship) && nrow(relationship) == n && ncol(relationship) == 2)
    stopifnot("Do not put NA in 'relationship'." = !anyNA(relationship))
    stopifnot("Elements in 'relationship' must be within 1..n." =
                all(relationship >= 1 & relationship <= n))
    stopifnot("Do not set alters both as friend and enemy." =
                all(relationship[, 1] != relationship[, 2]))
    stopifnot("Do not set the agent itself as friend/enemy." =
                all(vapply(agent_id, function(i) all(relationship[i, ] != i), logical(1))))

    friend <- relationship[, 1]
    enemy  <- relationship[, 2]
  } else {
    alt_mat <- vapply(agent_id, function(i) sample(agent_id[-i], size = 2), integer(2))
    friend <- alt_mat[1, ]
    enemy  <- alt_mat[2, ]
  }
  names(friend) <- agent_id
  names(enemy)  <- agent_id

  # ============================================================
  # Settings
  # ============================================================
  settings <- list(
    n = n,
    space_max = c(x = x_max, y = y_max),
    speed = speed,
    seed = seed,
    init_agent_type = agent_type
  )

  # ============================================================
  # Game object
  # ============================================================
  G <- Game(
    State(agent_id),
    State(agent_type),
    State(posit),
    State(friend),
    State(enemy),
    State(settings)
  )

  # ============================================================
  # Plot FUNs
  # ============================================================
  plot_space <- function() {
    col <- rep("red", self$settings$n)
    col[self$agent_type == 1] <- "blue"

    plot(
      x = self$posit[, 1], y = self$posit[, 2],
      xlim = c(0, self$settings$space_max[1]),
      ylim = c(0, self$settings$space_max[2]),
      xlab = "x", ylab = "y", type = "n",
      main = paste("time =", self$time),
      sub = "hero = blue; coward = red"
    )
    text(x = self$posit[, 1], y = self$posit[, 2], labels = self$agent_id, col = col)
  }

  plot_relationship <- function(id = 1) {
    col <- rep("black", self$settings$n)
    col[self$enemy[id]]  <- "red"
    col[self$friend[id]] <- "blue"
    col[id] <- "green"

    plot(
      x = self$posit[, 1], y = self$posit[, 2],
      xlim = c(0, self$settings$space_max[1]),
      ylim = c(0, self$settings$space_max[2]),
      xlab = "x", ylab = "y", type = "n",
      main = paste("time =", self$time),
      sub = "red = enemy; blue = friend; green = focusing agent"
    )
    text(x = self$posit[, 1], y = self$posit[, 2], labels = self$agent_id, col = col)
  }

  G <- add_field(G = G, Plot(plot_space), Plot(plot_relationship))

  # ============================================================
  # Active state
  # ============================================================
  friend_posit <- function() self$posit[self$friend, , drop = FALSE]
  enemy_posit  <- function() self$posit[self$enemy,  , drop = FALSE]

  G <- add_field(G, Active(friend_posit), Active(enemy_posit))

  # ============================================================
  # Act FUNs
  # ============================================================

  move_agent <- function(speed = self$settings$speed) {
    # target positions (may be outside; boundary is handled by act_coord_move)
    target_posit <- get_target_posit(
      friend_posit = self$friend_posit,
      enemy_posit  = self$enemy_posit,
      agent_type   = self$agent_type
    )

    new_posit <- act_coord_move(
      agent_posit     = self$posit,
      target_posit    = target_posit,
      movement_speed  = speed,
      space_max       = self$settings$space_max,
      movement_type   = "stop"   # NetLogo-like: stop at boundary
    )

    self$posit <- new_posit
  }

  change_mixed_type <- function() {
    agent_type <- self$settings$init_agent_type
    idx <- agent_type == 9
    if (any(idx)) {
      agent_type[idx] <- sample(c(1, 0), size = sum(idx), replace = TRUE)
    }
    self$agent_type <- agent_type
  }

  G <- add_field(G = G, Act(move_agent), Act(change_mixed_type))

  # ============================================================
  # Report FUNs
  # ============================================================
  trace_posit_mean <- function() {
    posit_log <- value_of(G = self, field_name = "posit", log = seq_len(length(self$log)))
    mean_posit <- do.call(rbind, lapply(posit_log, colMeans))

    y_max_plot <- max(self$settings$space_max)

    plot(
      x = seq_len(nrow(mean_posit)),
      y = mean_posit[, 1], ylim = c(0, y_max_plot), type = "l",
      col = "red",
      xlab = "time", ylab = "posit Mean (red = x, blue = y)"
    )
    par(new = TRUE)
    plot(
      x = seq_len(nrow(mean_posit)),
      y = mean_posit[, 2], ylim = c(0, y_max_plot), type = "l",
      col = "blue",
      xlab = "", ylab = "", axes = FALSE
    )

    mean_posit
  }

  trace_posit_sd <- function() {
    posit_log <- value_of(G = self, field_name = "posit", log = seq_len(length(self$log)))
    posit_sd <- do.call(rbind, lapply(posit_log, function(x) apply(x, 2, sd)))

    max_sd <- max(posit_sd)

    plot(
      x = seq_len(nrow(posit_sd)),
      y = posit_sd[, 1], ylim = c(0, max_sd), type = "l",
      col = "red",
      xlab = "time", ylab = "posit SD (red = x, blue = y)"
    )
    par(new = TRUE)
    plot(
      x = seq_len(nrow(posit_sd)),
      y = posit_sd[, 2], ylim = c(0, max_sd), type = "l",
      col = "blue",
      xlab = "", ylab = "", axes = FALSE
    )

    posit_sd
  }

  G <- add_field(G, Report(trace_posit_mean), Report(trace_posit_sd))

  # ============================================================
  # Save initial state and run
  # ============================================================
  G_init <- G$clone(deep = TRUE)

  G_finished <- run_Game(
    G = G_init,
    plan = c("change_mixed_type", "move_agent"),
    times = sim_time
  )

  # ============================================================
  # Output
  # ============================================================
  list(G_init = G_init, G_finished = G_finished)
}



# ============================================================
# Helpers for Heroes & Cowards (defined once; not per-tick)
# ============================================================

get_mid_point <- function(friend_posit, enemy_posit) {
  (friend_posit + enemy_posit) / 2
}

get_shield_point <- function(friend_posit, enemy_posit) {
  # mid = (friend + enemy)/2
  # shield = friend - (mid - friend) = 2*friend - mid
  mid_point <- (enemy_posit + friend_posit) / 2
  friend_posit - (mid_point - friend_posit)
}

get_target_posit <- function(friend_posit, enemy_posit, agent_type) {
  n <- nrow(friend_posit)
  target_posit <- matrix(NA_real_, nrow = n, ncol = 2)

  cowards <- agent_type == 0
  if (any(cowards)) {
    target_posit[cowards, ] <- get_shield_point(
      friend_posit = friend_posit[cowards, , drop = FALSE],
      enemy_posit  = enemy_posit[cowards,  , drop = FALSE]
    )
  }

  heros <- agent_type == 1
  if (any(heros)) {
    target_posit[heros, ] <- get_mid_point(
      friend_posit = friend_posit[heros, , drop = FALSE],
      enemy_posit  = enemy_posit[heros,  , drop = FALSE]
    )
  }

  target_posit
}


