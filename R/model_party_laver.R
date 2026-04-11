#' @title Laver (2005) Party Competition Model
#'
#' @description
#' Simulates the dynamic party competition model described in:
#'
#' Laver, M. (2005). Policy and the dynamics of political competition.
#' \emph{American Political Science Review}, 99(2), 263--281.
#'
#' Parties and voters are placed in a two-dimensional policy space.
#' At each time step, voters support the nearest party, and parties
#' update their positions according to their assigned decision rule.
#'
#' @details
#' ## Action sequence
#' At each time step, actions proceed in the following order:
#' \enumerate{
#'   \item \strong{voter_vote}: Each voter supports the nearest party.
#'   \item \strong{party_move}: Each party updates its position according to its rule.
#' }
#'
#' ## Time indexing
#' In rABM, \code{time = 1} is treated as the initial state. Because voting
#' always precedes party movement, no vote record exists at \code{time = 1}.
#' Accordingly, vote-based statistics and plots start from \code{time = 2}.
#' This is consistent with the Laver (2005) model, in which parties always
#' move in response to a prior vote.
#'
#' ## Party decision rules
#' \describe{
#'   \item{\code{"aggregator"}}{
#'     Moves to the mean position of its current supporters.
#'     If the party has no supporters, it remains in place.
#'   }
#'   \item{\code{"sticker"}}{
#'     Does not move. Remains at its initial position throughout the simulation.
#'   }
#'   \item{\code{"predator"}}{
#'     If the party is the largest (by vote share), it remains in place.
#'     Otherwise, it moves toward the largest party by \code{speed}.
#'     If multiple parties tie for the largest, one is selected at random.
#'   }
#'   \item{\code{"hunter"}}{
#'     If the previous move increased vote share, repeats the same move.
#'     If not, turns 180 degrees and makes a unit move in a direction chosen
#'     randomly within the half-space now faced (i.e., uniformly within
#'     ±90 degrees of the new direction).
#'     On the first move, or when no prior movement exists, moves in a
#'     random direction.
#'   }
#' }
#'
#' ## Policy space
#' The policy space is unbounded (\code{"unlimit"}). Parties and voters
#' are placed in a two-dimensional Euclidean space with no boundary constraints.
#'
#' @param n_voter Integer. Number of voters. Default: \code{100}.
#' @param n_party Integer. Number of parties. Default: \code{4}.
#' @param party_rule Character vector of length \code{n_party}. Decision rule
#'   for each party. Each element must be one of \code{"aggregator"},
#'   \code{"sticker"}, \code{"predator"}, or \code{"hunter"}.
#'   Default: \code{c("aggregator", "hunter", "predator", "sticker")}.
#' @param party_posit Numeric matrix of shape \code{(n_party, 2)} giving the
#'   initial positions of parties in the policy space.
#' @param voter_posit Numeric matrix of shape \code{(n_voter, 2)} giving the
#'   positions of voters in the policy space.
#' @param speed Positive numeric scalar. Distance moved per time step for
#'   \code{"predator"} and \code{"hunter"} parties. Default: \code{0.5}.
#' @param sim_times Positive integer. Number of time steps to simulate.
#'   Default: \code{30}.
#' @param seed Integer or \code{NULL}. Random seed for reproducibility.
#'   If \code{NULL}, no seed is set. Default: \code{NULL}.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{\code{G_init}}{The initial game object before simulation.}
#'   \item{\code{G_finished}}{The game object after simulation.}
#' }
#'
#' @export
model_party_laver <- function(
    n_voter = 100,
    n_party = 4,
    party_rule = c("aggregator", "hunter", "predator", "sticker"),
    party_posit = cbind(
      x1 = rnorm(n = n_party, mean = 0, sd = 1 / 3),
      x2 = rnorm(n = n_party, mean = 0, sd = 1 / 3)
    ),
    voter_posit = cbind(
      x1 = rnorm(n = n_voter, mean = 0, sd = 1 / 3),
      x2 = rnorm(n = n_voter, mean = 0, sd = 1 / 3)
    ),
    speed = 0.5,
    sim_times = 30,
    seed = NULL
){
  #====== validation ==============
  stopifnot("The nrow of 'party_posit' must be 'n_party'." =
              nrow(party_posit) == n_party)
  stopifnot("The nrow of 'voter_posit' must be 'n_voter'." =
              nrow(voter_posit) == n_voter)
  stopifnot("The ncol of 'party_posit' and 'voter_posit' must be the same." =
              ncol(party_posit) == ncol(voter_posit))
  stopifnot("The length of 'party_rule' must be 'n_party'." =
              length(party_rule) == n_party)
  stopifnot("'speed' must be a numeric scalar." =
              is.numeric(speed) && length(speed) == 1)
  stopifnot("'sim_times' must be a positive scalar." =
              is.numeric(sim_times) && length(sim_times) == 1 && sim_times > 0)

  #====== seed ====================
  if (!is.null(seed)) {
    set.seed(seed = seed)
  }

  #====== put labels ==============
  party_ID <- paste0("P", seq_len(n_party))
  voter_ID <- paste0("V", seq_len(n_voter))
  rownames(party_posit) <- party_ID
  rownames(voter_posit) <- voter_ID

  #====== State ===================
  settings <- list(
    n_voter  = n_voter,
    n_party  = n_party,
    party_ID = party_ID,
    voter_ID = voter_ID,
    speed    = speed
  )

  G <- Game(State(settings))
  add_field(G, State(party_rule), State(party_posit), State(voter_posit))

  voter_voted_to <- matrix(NA_integer_, nrow = n_voter, ncol = 1,
                           dimnames = list(voter_ID, "t1"))
  add_field(G, State(voter_voted_to))

  party_posit_diff <- matrix(0, nrow = n_party, ncol = 2,
                             dimnames = list(party_ID, c("X1", "X2")))
  add_field(G, State(party_posit_diff))

  #===== Active ====================
  posit_distance <- function() {
    vp <- self$voter_posit
    pp <- self$party_posit

    out <- sqrt(
      (vp[, 1] - rep(pp[, 1], each = nrow(vp)))^2 +
        (vp[, 2] - rep(pp[, 2], each = nrow(vp)))^2
    )

    matrix(out, nrow = nrow(vp), ncol = nrow(pp),
           dimnames = list(rownames(vp), rownames(pp)))
  }
  add_field(G, Active(posit_distance))

  party_stat <- function() {
    voted <- self$voter_voted_to[, ncol(self$voter_voted_to)]
    table(factor(voted, levels = seq_len(self$settings$n_party)))
  }
  add_field(G, Active(party_stat))

  #===== Plot ======================
  plot_posit <- function() {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)

    x_max <- max(self$voter_posit[, 1], self$party_posit[, 1])
    x_min <- min(self$voter_posit[, 1], self$party_posit[, 1])
    y_max <- max(self$voter_posit[, 2], self$party_posit[, 2])
    y_min <- min(self$voter_posit[, 2], self$party_posit[, 2])

    plot(self$voter_posit,
         xlim = c(x_min, x_max), ylim = c(y_min, y_max),
         main = paste0("time = ", self$time),
         col = "darkgray")
    par(new = TRUE)
    plot(self$party_posit,
         xlim = c(x_min, x_max), ylim = c(y_min, y_max),
         col = "red",
         ann = FALSE, axes = FALSE, pch = 15)
    car::pointLabel(self$party_posit, labels = self$settings$party_ID,
                    col = "red")
  }
  add_field(G, Plot(plot_posit))

  #===== Act ======================
  voter_vote <- function() {
    dist_mat <- self$posit_distance

    voted <- apply(dist_mat, 1, function(v_i) {
      candidates <- which(v_i == min(v_i))
      sample(candidates, size = 1)
    })

    voted_to <- cbind(self$voter_voted_to, voted)
    colnames(voted_to)[ncol(voted_to)] <- paste0("t", self$time)
    self$voter_voted_to <- voted_to
  }
  add_field(G, Act(voter_vote))

  aggregator <- function(party_ID = 1, self) {
    latest_vote <- self$voter_voted_to[, ncol(self$voter_voted_to)]
    supporters <- latest_vote == party_ID

    if (!any(supporters)) {
      return(self$party_posit[party_ID, ])
    }

    colMeans(self$voter_posit[supporters, , drop = FALSE])
  }

  sticker <- function(party_ID = 1, self) {
    self$party_posit[party_ID, ]
  }

  predator <- function(party_ID = 1, self) {
    voted <- self$voter_voted_to[, ncol(self$voter_voted_to)]
    stat <- table(factor(voted, levels = seq_len(self$settings$n_party)))
    largest_party <- which(stat == max(stat))

    if (length(largest_party) > 1) {
      largest_party <- sample(largest_party, size = 1)
    }

    if (party_ID == largest_party) {
      return(self$party_posit[party_ID, ])
    }

    move_coord(
      agent_posit  = self$party_posit[party_ID, , drop = FALSE],
      target_posit = self$party_posit[largest_party, , drop = FALSE],
      speed        = self$settings$speed
    )
  }

  hunter <- function(party_ID = 1, self) {
    voted <- self$voter_voted_to
    self_posit <- self$party_posit[party_ID, ]
    self_posit_diff <- self$party_posit_diff[party_ID, ]
    n_col_voted <- ncol(voted)

    is_first_move <- (self$time <= 2)

    if (n_col_voted < 2 || is_first_move) {
      radian <- runif(n = 1, min = 0, max = 2 * pi)
      return(move_coord(self_posit, speed = self$settings$speed, radian = radian))
    }

    vote_result <- apply(
      voted[, c(n_col_voted - 1, n_col_voted), drop = FALSE],
      2,
      function(x) sum(x == party_ID)
    )
    vote_diff <- vote_result[2] - vote_result[1]

    if (vote_diff >= 0) {
      return(self_posit + self_posit_diff)
    }

    current_radian <- atan2(self_posit_diff[2], self_posit_diff[1])
    new_radian <- current_radian + pi + runif(1, -pi / 2, pi / 2)
    move_coord(self_posit, speed = speed, radian = new_radian)
  }

  party_move_rules <- list(
    aggregator = aggregator,
    sticker    = sticker,
    predator   = predator,
    hunter     = hunter
  )
  add_field(G, State(party_move_rules))

  party_move <- function() {
    rules <- self$party_rule
    new_destination <- matrix(
      0, nrow = self$settings$n_party, ncol = 2,
      dimnames = list(self$settings$party_ID, c("X1", "X2"))
    )

    for (i in seq_len(self$settings$n_party)) {
      fun <- self$party_move_rules[[rules[i]]]
      if (!is.function(fun)) {
        stop(sprintf("Unknown party rule: %s", rules[i]))
      }
      new_destination[i, ] <- fun(party_ID = i, self = self)
    }

    self$party_posit_diff <- new_destination - self$party_posit
    self$party_posit <- new_destination
  }
  add_field(G, Act(party_move))

  #=========== Report ==========
  report_party_move <- function(party_ID = 1) {
    party_posit_log <- value_of(self, "party_posit", log = "all")
    time <- value_of(self, "time", log = "all")

    posit <- do.call(
      rbind,
      lapply(party_posit_log, function(x) x[party_ID, , drop = FALSE])
    )

    all_pos <- do.call(rbind, party_posit_log)
    xmin <- min(all_pos[, 1])
    xmax <- max(all_pos[, 1])
    ymin <- min(all_pos[, 2])
    ymax <- max(all_pos[, 2])

    plot(posit,
         xlim = c(xmin, xmax), ylim = c(ymin, ymax),
         type = "l", col = "darkgray",
         main = paste0("party ", party_ID, " (", self$party_rule[party_ID], ")"))
    text(posit, labels = time)

    df <- data.frame(
      time = time,
      party_ID = party_ID,
      x = posit[, 1],
      y = posit[, 2],
      row.names = NULL
    )
    df
  }
  add_field(G, Report(report_party_move))

  report_party_stat <- function(party_ID = NULL) {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)

    party_stat <- do.call(rbind, value_of(self, "party_stat", log = "all"))
    time <- unlist(value_of(self, "time", log = "all"))

    if (!is.null(party_ID)) {
      party_stat <- party_stat[, party_ID, drop = FALSE]
    }

    ymax <- max(party_stat)
    ymin <- min(party_stat)

    cols <- RColorBrewer::brewer.pal(max(ncol(party_stat), 3), "Set1")[
      seq_len(ncol(party_stat))
    ]

    plot(
      x = time[-1],
      y = party_stat[-1, 1],
      type = "l",
      xlab = "time",
      ylab = "number of votes obtained",
      col = cols[1],
      lwd = 2,
      ylim = c(ymin, ymax)
    )

    if (ncol(party_stat) > 1) {
      for (i in 2:ncol(party_stat)) {
        par(new = TRUE)
        plot(
          x = time[-1],
          y = party_stat[-1, i],
          type = "l",
          col = cols[i],
          lwd = 2,
          ylim = c(ymin, ymax),
          ann = FALSE,
          axes = FALSE
        )
      }
      legend("topleft", legend = colnames(party_stat), col = cols, lty = 1)
    }

    df <- as.data.frame(cbind(time = time, party_stat))
    rownames(df) <- NULL
    df
  }
  add_field(G, Report(report_party_stat))

  #=========== save & run ==========
  G_init <- copy_obj(G)
  G_finished <- run_Game(
    G,
    plan = c("voter_vote", "party_move"),
    times = sim_times,
    seed = seed
  )

  list(G_init = G_init, G_finished = G_finished)
}
