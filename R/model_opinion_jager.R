#' Opinion Dynamics Simulation Based on the Jager-Amblard Model
#'
#' @description
#' Simulates opinion dynamics among agents based on the bounded confidence
#' model proposed by Jager and Amblard (2005). In the original model, each
#' agent holds a single continuous opinion on \eqn{[-1, 1]}. This
#' implementation extends the framework to a two-dimensional opinion space,
#' allowing the user to examine how opinion formation on a major issue
#' (\code{x1}) propagates to a minor issue (\code{x2}) via
#' \code{influence_opinion}.
#'
#' At each time step, agents interact with a randomly selected network
#' neighbour. Opinion updating follows three regimes based on the absolute
#' opinion difference \eqn{|o_i - o_j|}:
#' \itemize{
#'   \item \strong{Assimilation} (difference \eqn{<} acceptance threshold):
#'     \eqn{o_i \leftarrow o_i + \lambda (o_j - o_i)}
#'   \item \strong{Contrast} (difference \eqn{>} rejection threshold):
#'     \eqn{o_i \leftarrow o_i + \lambda (o_i - o_j)}
#'   \item \strong{No change} (otherwise): \eqn{o_i} stays unchanged
#' }
#' where \eqn{\lambda} is the learning rate.
#'
#' @param n_agent Integer. Number of agents in the simulation. Default is
#'   \code{10}.
#'
#' @param agent_opinion A numeric data frame with two columns (\code{x1},
#'   \code{x2}), each ranging from \eqn{-1} to \eqn{1}, with \code{n_agent}
#'   rows representing the initial opinions of each agent. If \code{NULL}
#'   (default), opinions are drawn independently from
#'   \eqn{\mathrm{Uniform}(-1, 1)}.
#'
#' @param agent_accept Numeric vector of length \code{n_agent}. Acceptance
#'   threshold for each agent: opinion differences below this value trigger
#'   assimilation. Default is \code{rep(0.3, n_agent)}.
#'
#' @param agent_reject Numeric vector of length \code{n_agent}. Rejection
#'   threshold for each agent: opinion differences above this value trigger
#'   contrast (i.e., opinion movement away from the alter). Should satisfy
#'   \code{agent_reject > agent_accept} for sensible dynamics. Default is
#'   \code{rep(0.5, n_agent)}.
#'
#' @param agent_learn Numeric vector of length \code{n_agent}. Learning rate
#'   \eqn{\lambda \in (0, 1]} controlling the step size of opinion updating.
#'   Default is \code{rep(0.1, n_agent)}.
#'
#' @param agent_self_influence Numeric vector of length \code{n_agent}.
#'   Self-influence rate governing how strongly an agent's opinion on \code{x1}
#'   (the major issue) pulls their opinion on \code{x2} (the minor issue) via
#'   \code{influence_opinion}. This parameter has no counterpart in the original
#'   Jager-Amblard model and is specific to this two-dimensional extension.
#'   Default is \code{rep(0.1, n_agent)}.
#'
#' @param net_agent An \code{n_agent} by \code{n_agent} numeric adjacency
#'   matrix. If provided, \code{net_type} and \code{tprob} are ignored and
#'   a warning is issued if \code{net_type} is also specified. Default is
#'   \code{NULL}.
#'
#' @param net_type Character string specifying the network topology to generate
#'   when \code{net_agent} is \code{NULL}. One of \code{"complete"} (default),
#'   \code{"random"}, or \code{"lattice"}. For \code{"lattice"},
#'   \code{n_agent} must be a perfect square.
#'
#' @param tprob Numeric scalar in \eqn{[0, 1]}. Edge probability used when
#'   \code{net_type = "random"}. Ignored for other network types. Default is
#'   \code{0.5}.
#'
#' @param issue_change_prob Numeric scalar in \eqn{[0, 1]}. Probability that
#'   the discussion topic switches from the current issue to the other at each
#'   time step, used by the optional action \code{change_issue}. This argument
#'   has no effect unless \code{"change_issue"} is included in \code{plan}.
#'   Default is \code{0.3}.
#'
#' @param plan Character vector specifying which actions to execute at each
#'   time step and in what order. Available actions are:
#'   \describe{
#'     \item{\code{"select_alter"}}{Each agent randomly selects one network
#'       neighbour to interact with.}
#'     \item{\code{"update_opinion"}}{Agents update their opinion on the
#'       currently discussed issue according to the Jager-Amblard rule.}
#'     \item{\code{"influence_opinion"}}{Opinion on \code{x1} influences
#'       opinion on \code{x2} proportionally to \code{agent_self_influence}.}
#'     \item{\code{"change_issue"}}{With probability \code{issue_change_prob},
#'       the discussion topic switches to the other issue. This action is
#'       \emph{not} included in the default \code{plan}; add it explicitly
#'       when needed.}
#'   }
#'   Default is \code{c("select_alter", "update_opinion")}.
#'
#' @param sim_times Integer. Number of simulation time steps. Default is
#'   \code{30}.
#'
#' @param seed Integer or \code{NULL}. Random seed passed to
#'   \code{\link{set.seed}} for reproducibility. Default is \code{NULL}
#'   (no seed set).
#'
#' @details
#' The simulation is built on a \code{Game} object that stores agent states
#' and executes actions in the order specified by \code{plan}. The social
#' network is initialised once at the start of the simulation and remains
#' fixed throughout.
#'
#' \strong{Network types.} When \code{net_agent} is \code{NULL}, the network
#' is generated internally according to \code{net_type}: a fully connected
#' graph (\code{"complete"}), an Erdos-Renyi random graph with edge probability
#' \code{tprob} (\code{"random"}, via \code{\link[sna]{rgraph}}), or a regular
#' two-dimensional square lattice (\code{"lattice"}, via
#' \code{\link[igraph]{make_lattice}}). For the lattice option, \code{n_agent}
#' must be a perfect square.
#'
#' \strong{Two-dimensional extension.} The original Jager-Amblard (2005) model
#' is strictly one-dimensional. This implementation adds a second opinion
#' dimension (\code{x2}) to explore cross-issue influence: the action
#' \code{influence_opinion} shifts each agent's \code{x2} opinion toward their
#' \code{x1} opinion at a rate governed by \code{agent_self_influence}. The
#' labels \emph{major} (\code{x1}) and \emph{minor} (\code{x2}) reflect the
#' intended use case, but the roles can be reversed by adjusting \code{plan}
#' and \code{agent_self_influence}.
#'
#' \strong{Topic switching.} When \code{"change_issue"} is included in
#' \code{plan}, the discussion alternates stochastically between \code{x1} and
#' \code{x2} at each time step with probability \code{issue_change_prob}. This
#' requires \code{ncol(agent_opinion) >= 2}; a warning is issued otherwise.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{\code{G_init}}{A copy of the \code{Game} object capturing the
#'     initial state before any simulation steps.}
#'   \item{\code{G_finished}}{The \code{Game} object after all
#'     \code{sim_times} steps have been executed, including logged trajectories
#'     accessible via \code{value_of}.}
#' }
#'
#' @references
#' Jager, W., & Amblard, F. (2005). Uniformity, bipolarization and pluriformity
#' captured as generic stylized behavior with an agent-based simulation model of
#' attitude change. Computational & Mathematical Organization Theory,
#' 10(4), 295--303. DOI: 10.1007/s10588-005-6282-2.
#'
#' @seealso
#' \code{\link[sna]{rgraph}} for random network generation;
#' \code{\link[igraph]{make_lattice}} for lattice network generation;
#' \code{value_of}, \code{run_Game}, \code{copy_obj} for game utilities.
#'
#' @examples
#' ## ------------------------------------------------------------------
#' ## Example 1: Basic simulation with default settings (complete network)
#' ## ------------------------------------------------------------------
#' result <- model_opinion_jager(n_agent = 20, sim_times = 50, seed = 42)
#'
#' ## Plot opinion trajectories for issue x1
#' result$G_finished$report_trajectory(issue_idx = 1)
#'
#' ## Plot number of opinion clusters over time
#' result$G_finished$report_n_groups(issue_idx = 1)
#'
#' ## ------------------------------------------------------------------
#' ## Example 2: Cross-issue influence (two-dimensional extension)
#' ## ------------------------------------------------------------------
#' result2 <- model_opinion_jager(
#'   n_agent   = 30,
#'   sim_times = 80,
#'   plan      = c("select_alter", "update_opinion", "influence_opinion"),
#'   seed      = 123
#' )
#'
#' result2$G_finished$report_trajectory(issue_idx = 1)
#' result2$G_finished$report_trajectory(issue_idx = 2)
#'
#' ## ------------------------------------------------------------------
#' ## Example 3: Lattice network (n_agent must be a perfect square)
#' ## ------------------------------------------------------------------
#' result3 <- model_opinion_jager(
#'   n_agent  = 25,
#'   net_type = "lattice",
#'   sim_times = 50,
#'   seed     = 7
#' )
#'
#' ## ------------------------------------------------------------------
#' ## Example 4: Enabling stochastic topic switching
#' ## ------------------------------------------------------------------
#' result4 <- model_opinion_jager(
#'   n_agent           = 25,
#'   sim_times         = 60,
#'   issue_change_prob = 0.5,
#'   plan              = c("select_alter", "update_opinion", "change_issue"),
#'   seed              = 7
#' )
#'
#' ## ------------------------------------------------------------------
#' ## Example 5: Heterogeneous agents
#' ## ------------------------------------------------------------------
#' set.seed(1)
#' n <- 40
#' result5 <- model_opinion_jager(
#'   n_agent      = n,
#'   agent_accept = runif(n, 0.1, 0.4),
#'   agent_reject = runif(n, 0.5, 0.9),
#'   agent_learn  = runif(n, 0.05, 0.3),
#'   sim_times    = 100,
#'   seed         = 99
#' )
#'
#' @export
model_opinion_jager <- function(
    n_agent              = 10,
    agent_opinion        = NULL,
    agent_accept         = rep(0.4, n_agent),
    agent_reject         = rep(0.6, n_agent),
    agent_learn          = rep(0.1, n_agent),
    agent_self_influence = rep(0.1, n_agent),
    net_agent            = NULL,
    net_type             = c("complete", "random", "lattice"),
    tprob                = 0.5,
    issue_change_prob    = 0.3,
    plan                 = c("select_alter", "update_opinion"),
    sim_times            = 30,
    seed                 = NULL
){
  #======= validation =====
  if(is.null(agent_opinion)){
    agent_opinion <- data.frame(x1 = runif(n_agent, min = -1, max = 1),
                                x2 = runif(n_agent, min = -1, max = 1))
  } else {
    stopifnot("'agent_opinion' must range from -1 to 1." =
                all(agent_opinion <= 1) && all(agent_opinion >= -1))
  }

  stopifnot("'tprob' must range between 0 and 1." = tprob >= 0 && tprob <= 1)

  if(length(agent_accept)==1) agent_accept <- rep(agent_accept, n_agent)
  if(length(agent_reject)==1) agent_reject <- rep(agent_reject, n_agent)
  if(length(agent_learn)==1) agent_learn <- rep(agent_learn, n_agent)
  if(length(agent_self_influence)==1) agent_self_influence <- rep(agent_self_influence, n_agent)

  stopifnot("'agent_accept' must be the length of 'n_agent'. " = length(agent_accept) == n_agent)
  stopifnot("'agent_reject' must be the length of 'n_agent'. " = length(agent_reject) == n_agent)
  stopifnot("'agent_learn' must be the length of 'n_agent'. " = length(agent_learn) == n_agent)
  stopifnot("'agent_self_influence' must be the length of 'n_agent'. " = length(agent_self_influence) == n_agent)

  stopifnot("Each of 'agent_accept' must be smaller or equal to 'agent_reject'. " = all(agent_accept <= agent_reject))

  #======= seed ===========
  if(!is.null(seed)){
    set.seed(seed = seed)
  }

  #====== network validation & creation =========
  if(!is.null(net_agent)){
    if(!missing(net_type)){
      warning("'net_type' is ignored when 'net_agent' is provided directly.")
    }
  } else {
    net_type  <- match.arg(net_type)

    is_perfect_square <- function(n){ s <- round(sqrt(n)); s^2 == n }

    net_agent <- switch(net_type,
                        "complete" = {
                          mat <- matrix(1, n_agent, n_agent)
                          diag(mat) <- 0
                          mat
                        },
                        "random" = {
                          sna::rgraph(n_agent, tprob = tprob)
                        },
                        "lattice" = {
                          stopifnot("'n_agent' must be a perfect square for lattice network." =
                                      is_perfect_square(n_agent))
                          n_side <- round(sqrt(n_agent))
                          g <- igraph::make_lattice(dimvector = c(n_side, n_side))
                          as.matrix(igraph::as_adjacency_matrix(g))
                        }
    )
  }

  stopifnot("Number of rows and columns of 'net_agent' must equal 'n_agent'." =
              nrow(net_agent) == n_agent && ncol(net_agent) == n_agent)

  #======= State ==========
  settings <- list(n_agent          = n_agent,
                   agent_idx        = seq_len(n_agent),
                   issue_change_prob = issue_change_prob)

  G <- Game(State(agent_opinion), State(agent_accept), State(agent_reject),
            State(agent_learn), State(agent_self_influence),
            State(net_agent),
            State(settings))

  selected_alter <- apply(net_agent, 1, function(net_i){
    sample2((1:n_agent)[net_i == 1], size = 1)
  })
  add_field(G, State(selected_alter))

  issue_discussing <- 1
  add_field(G, State(issue_discussing))

  #======= Plot ==========
  plot_opinion_hist <- function(issue_idx = 1){
    hist(self$agent_opinion[ , issue_idx],
         xlim = c(-1, 1),
         xlab = colnames(agent_opinion)[issue_idx],
         main = paste0("time = ", self$time))
  }
  add_field(G, Plot(plot_opinion_hist))

  plot_network <- function(issue_idx = 1){
    opinion      <- self$agent_opinion[ , issue_idx]
    opinion_attr <- ifelse(opinion > 0, "blue", ifelse(opinion == 0, "gray", "red"))
    sna::gplot(self$net_agent, edge.col = "gray",
               vertex.col = opinion_attr,
               main = paste0("time = ", self$time),
               sub  = paste0("Node color based on opinion: ",
                             colnames(self$agent_opinion)[issue_idx]))
  }
  add_field(G, Plot(plot_network))

  plot_opinion <- function(){
    plot(x = self$agent_opinion[ ,1], y = self$agent_opinion[ ,2],
         xlim = c(-1, 1), ylim = c(-1, 1),
         main = paste0("time = ", self$time))
  }
  add_field(G, Plot(plot_opinion))

  plot_learning <- function(issue_idx = 1){
    opinion      <- self$agent_opinion[ , issue_idx]
    edges        <- cbind(self$selected_alter, self$settings$agent_idx)
    mat          <- matrix(0, self$settings$n_agent, self$settings$n_agent)
    mat[edges]   <- 1
    opinion_attr <- ifelse(opinion > 0, "blue", ifelse(opinion == 0, "gray", "red"))
    sna::gplot(mat, edge.col = "gray", coord = self$agent_opinion,
               vertex.col = opinion_attr,
               main = paste0("time = ", self$time),
               sub  = paste0("Node color based on opinion: ",
                             colnames(self$agent_opinion)[issue_idx]))
  }
  add_field(G, Plot(plot_learning))

  #======= Act ===========
  select_alter <- function(){
    selected_alter <- apply(self$net_agent, 1, function(net_i){
      sample2(self$settings$agent_idx[net_i == 1], size = 1)})
    self$selected_alter <- selected_alter
  }
  add_field(G, Act(select_alter))

  update_opinion <- function(){
    opinion <- self$agent_opinion[ , self$issue_discussing]
    accept  <- self$agent_accept
    reject  <- self$agent_reject
    learn   <- self$agent_learn

    new_opinion <- rep(NA, self$settings$n_agent)
    for(i in self$settings$agent_idx){
      opinion_ego   <- opinion[i]
      opinion_alter <- opinion[self$selected_alter[i]]
      opinion_abs   <- abs(opinion_ego - opinion_alter)

      if(opinion_abs < accept[i]){
        new_opinion[i] <- opinion_ego + learn[i] * (opinion_alter - opinion_ego)
      } else if(opinion_abs > reject[i]){
        new_opinion[i] <- opinion_ego + learn[i] * (opinion_ego - opinion_alter)
      } else {
        new_opinion[i] <- opinion_ego
      }
    }

    # truncate the opinion to [-1:+1]
    new_opinion[new_opinion < -1] <- -1
    new_opinion[new_opinion > 1] <- 1

    # update
    self$agent_opinion[ , self$issue_discussing] <- new_opinion
  }
  add_field(G, Act(update_opinion))

  influence_opinion <- function(from = 1, to = 2){
    opinion_from <- self$agent_opinion[ , from]
    opinion_to   <- self$agent_opinion[ , to]
    opinion_diff <- opinion_from - opinion_to
    opinion_to   <- opinion_to + self$agent_self_influence * opinion_diff
    self$agent_opinion[ , to] <- opinion_to
  }
  add_field(G, Act(influence_opinion))

  change_issue <- function(){
    if(ncol(self$agent_opinion) == 1){
      warning("This action only works when number of columns of 'agent_opinion' is greater than or equal to 2.")
      return(invisible(NULL))
    }

    prob      <- self$settings$issue_change_prob
    do_change <- sample(c(TRUE, FALSE), size = 1, prob = c(prob, 1 - prob))
    if(do_change){
      next_issue_candid    <- seq_len(ncol(self$agent_opinion))[-self$issue_discussing]
      new_issue            <- sample(next_issue_candid, size = 1)
      self$issue_discussing <- new_issue
    } else {
      invisible(NULL)
    }
  }
  add_field(G, Act(change_issue))

  #========= report ===============
  report_trajectory <- function(n = NULL, issue_idx = 1){
    # old_par
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)

    if(is.null(n)){
      agent_idx <- self$settings$agent_idx
    } else {
      agent_idx <- sample2(self$settings$agent_idx, n)
    }

    opinion_list <- value_of(self, "agent_opinion", log = "all",
                             return_FUN = function(x){x[ , issue_idx]})
    opinion <- do.call(rbind, opinion_list)
    times   <- seq_len(nrow(opinion))
    nm      <- colnames(self$agent_opinion)[issue_idx]

    plot(x = times, y = opinion[ , 1], type = "l", ylab = nm,
         ylim = c(-1, 1), xlab = "time",
         sub  = paste0("Acceptance(mean) = ", mean(self$agent_accept), "; ",
                       "Rejection(mean) = ",  mean(self$agent_reject)))
    par(new = TRUE)

    for(i in agent_idx[2:length(agent_idx)]){
      plot(x = times, y = opinion[ , i], type = "l", xlab = "", ylab = "",
           ylim = c(-1, 1), ann = FALSE, axes = FALSE)
      par(new = TRUE)
    }

    colnames(opinion) <- paste0("agent_", agent_idx)
    return(opinion)
  }
  add_field(G, Report(report_trajectory))

  report_n_groups <- function(issue_idx = 1, interval = 0.05){
    opinion_list <- value_of(self, "agent_opinion", log = "all",
                             return_FUN = function(x_t){x_t[ , issue_idx]})
    opinion     <- do.call(rbind, opinion_list)
    opinion_cat <- cut(opinion, breaks = seq(-1, 1, interval))
    opinion_cat <- array(opinion_cat, dim = dim(opinion))

    n_clust <- apply(opinion_cat, 1, function(x_i){
      length(unique(x_i))
    })

    plot(x = 1:nrow(opinion), y = n_clust, type = "l",
         xlab = "time", ylab = "number of groups")

    names(n_clust) <- rownames(opinion)
    n_clust
  }
  add_field(G, Report(report_n_groups))

  #======== G_init ==================
  G_init <- copy_obj(G)

  #======= run ======================
  G_finished <- run_Game(G,
                         plan  = plan,
                         times = sim_times,
                         seed  = seed)
  #====== output ====================
  out <- list(G_init     = G_init,
              G_finished = G_finished)
  out
}
