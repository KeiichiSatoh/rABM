#' Agent-Based Model of Opinion Dynamics and Cultural Polarization
#'
#' @description
#' Implements the agent-based model of opinion dynamics and cultural polarization
#' proposed by Flache and Macy (2011). Agents are embedded in a clustered
#' ("caveman") network and update their continuous opinions on multiple issues
#' through social influence. Influence can be purely assimilative (positive
#' weights only) or bivalent---combining assimilation with differentiation and
#' xenophobia (positive and negative weights). The model examines how the
#' addition of random long-range ties between otherwise disconnected network
#' clusters (caves) affects population-level polarization.
#'
#' @param n_agent Integer. Number of agents. Default is \code{100}, matching
#'   the baseline specification in Flache and Macy (2011).
#' @param n_agent_per_cave Integer. Number of agents per cave (network cluster).
#'   Each cave is initialized as a complete subgraph (clique). Default is
#'   \code{5}. If \code{n_agent} is not divisible by \code{n_agent_per_cave},
#'   a warning is issued and the remaining agents form a smaller final cave.
#' @param n_issue Integer. Number of opinion dimensions (K in the paper).
#'   Each agent holds one continuous opinion per issue, ranging from \code{-1}
#'   to \code{+1}. Default is \code{2}.
#' @param negative_weight Logical. If \code{TRUE} (default), relationship
#'   weights can be negative, enabling differentiation and xenophobia
#'   (Equation 1 in Flache & Macy, 2011). If \code{FALSE}, weights are
#'   constrained to \eqn{[0, 1]}, allowing only assimilation and homophily
#'   (Equation 1a).
#' @param random_tie_prop_outset Numeric in \eqn{[0, 1]}. Proportion of
#'   non-existing tie slots (lower triangle) used to determine the number of
#'   random inter-cave ties added at initialization. The number of ties added
#'   is \code{ceiling(n_slots * random_tie_prop_outset)}. Default is
#'   \code{0.003}. Set to \code{0} to start from a purely disconnected caveman
#'   graph. Ignored if \code{agent_network} is supplied.
#' @param random_tie_prop_midpoint Numeric in \eqn{[0, 1]}. Proportion of
#'   non-existing tie slots used to determine the number of random inter-cave
#'   ties added at iteration \code{random_tie_adding_at}. Applied in the same
#'   manner as \code{random_tie_prop_outset}. Default is \code{0.003}.
#' @param random_tie_adding_at Integer greater than 1, or \code{NULL}
#'   (default). The iteration at which additional random ties are injected
#'   into the network via \code{random_tie_prop_midpoint}. Must be \code{> 1}
#'   because iteration 1 is the initial state in rABM. Set to \code{NULL} to
#'   disable mid-simulation tie addition.
#' @param times Integer. Number of iterations to run. One iteration corresponds
#'   to N time steps, where each time step selects one agent at random
#'   (with replacement) and updates their opinions. Default is \code{100}.
#'   Set to \code{0} to return the initialized \code{Game} object without
#'   running the simulation.
#' @param agent_opinion Data frame or matrix, or \code{NULL} (default). An
#'   \code{n_agent}-by-\code{n_issue} matrix of initial opinion values, each
#'   in \eqn{[-1, +1]}. If \code{NULL}, opinions are drawn independently from
#'   a uniform distribution on \eqn{[-1, +1]}.
#' @param agent_network Matrix or \code{NULL} (default). An
#'   \code{n_agent}-by-\code{n_agent} binary adjacency matrix specifying the
#'   initial access network. If \code{NULL}, the network is constructed as a
#'   disconnected caveman graph (see Details), optionally with random ties
#'   added according to \code{random_tie_prop_outset}.
#'
#' @details
#' ## Model Overview
#'
#' The model follows Flache and Macy (2011), who extend Hopfield's attractor
#' network to study opinion dynamics on a small-world access network.
#'
#' ## Network Initialization
#'
#' When \code{agent_network = NULL}, the access network is initialized as a
#' \emph{disconnected caveman graph}: agents are partitioned into caves of size
#' \code{n_agent_per_cave}, and each cave is a complete subgraph with no ties
#' to other caves. If \code{random_tie_prop_outset > 0}, a fixed number of
#' undirected ties is drawn uniformly at random from all non-existing tie slots
#' (lower triangle only, excluding existing cave ties) and added symmetrically.
#' This approximates the Watts-Strogatz small-world construction while
#' preserving local cave structure.
#'
#' ## Relationship Weights (Active State)
#'
#' Relationship weights \eqn{w_{ij}} are not stored as a static state but
#' computed dynamically as an active state (\code{network_weight}) from current
#' opinions. When \code{negative_weight = TRUE} (Equation 1):
#' \deqn{w_{ij} = 1 - \frac{\sum_{k=1}^{K} |s_{jk} - s_{ik}|}{K}}
#' When \code{negative_weight = FALSE} (Equation 1a):
#' \deqn{w_{ij} = 1 - \frac{\sum_{k=1}^{K} |s_{jk} - s_{ik}|}{2K}}
#' Weights are set to zero for pairs not connected in the access network.
#'
#' Note: In the original model, weights are updated as a separate stochastic
#' step. Here, weights are derived deterministically from current opinions as
#' an active state, which is theoretically equivalent given that weights are a
#' strict function of opinions.
#'
#' ## Opinion Update Rule
#'
#' Each iteration consists of N time steps. In each time step, one agent
#' \eqn{i} is selected with replacement. The raw state change on issue \eqn{k}
#' is (Equation 2):
#' \deqn{\Delta s_{ik} = \frac{1}{2 N_l} \sum_{j \neq i} w_{ij}(s_{jk} - s_{ik})}
#' where \eqn{N_l} is the number of \eqn{i}'s neighbors. To keep opinions
#' within \eqn{[-1, +1]}, the update is smoothed near the boundaries
#' (Equation 2a):
#' \deqn{s_{ik, t+1} = \begin{cases}
#'   s_{ik} + \Delta s_{ik}(1 - s_{ik}) & \text{if } s_{ik} > 0 \\
#'   s_{ik} + \Delta s_{ik}(1 + s_{ik}) & \text{if } s_{ik} \leq 0
#' \end{cases}}
#' Agents with no neighbors are skipped.
#'
#' ## Polarization Measure
#'
#' The \code{report_polarization()} method computes the variance of pairwise
#' Manhattan distances (averaged across issues) between all agent pairs,
#' excluding self-distances. This corresponds to the polarization measure P
#' in Equation 3 of Flache and Macy (2011), with the minor difference that
#' R's \code{var()} uses \eqn{n-1} in the denominator rather than \eqn{n}.
#' This difference is negligible for \code{n_agent = 100}.
#'
#' ## Plan Execution Order
#'
#' Each iteration executes \code{add_random_tie} before \code{update_opinion}.
#' \code{add_random_tie} is a no-op unless
#' \code{self$time == random_tie_adding_at}.
#'
#' @return
#' If \code{times > 0}, returns the \code{Game} object \code{G2} after running
#' \code{times} iterations via \code{run_Game()}. If \code{times == 0}, returns
#' the initialized \code{Game} object \code{G} without running the simulation.
#'
#' The returned \code{Game} object contains the following fields:
#' \describe{
#'   \item{\code{agent_opinion}}{Data frame (\code{n_agent} x \code{n_issue})
#'     of current agent opinions.}
#'   \item{\code{agent_network}}{Matrix (\code{n_agent} x \code{n_agent})
#'     binary adjacency matrix of the current access network.}
#'   \item{\code{settings}}{List of model parameters and derived quantities
#'     (\code{n_agent}, \code{n_issue}, \code{n_cave}, \code{cave_residue},
#'     \code{negative_weight}, \code{n_agent_per_cave},
#'     \code{random_tie_adding_at}, \code{random_tie_prop_outset},
#'     \code{random_tie_prop_midpoint}).}
#'   \item{\code{network_weight}}{Active state. Matrix (\code{n_agent} x
#'     \code{n_agent}) of current relationship weights, computed from
#'     \code{agent_opinion} and \code{agent_network}.}
#' }
#'
#' The following methods are also attached to the returned object:
#' \describe{
#'   \item{\code{update_opinion()}}{Act. Runs one iteration of N asynchronous
#'     opinion updates.}
#'   \item{\code{add_random_tie()}}{Act. Adds random ties at the specified
#'     iteration. No-op at all other times.}
#'   \item{\code{plot_network(opinion_dim, ...)}}{Plot. Visualizes the access
#'     network with vertex colors reflecting agent opinions on dimension
#'     \code{opinion_dim} (blue = \eqn{-1}, white = \eqn{0}, red = \eqn{+1};
#'     default \code{opinion_dim = 1}). Network statistics (density,
#'     clustering, mean path length) are shown in the subtitle. Additional
#'     arguments are passed to \code{sna::gplot()}.
#'     Note: \code{plot_network()} internally calls
#'     \code{self$report_net_stat(print = FALSE)}, so \code{net_stat} must be
#'     computed before the subtitle is drawn.}
#'   \item{\code{plot_opinion(x, y, pch, time, xlim, ylim, ...)}}{Plot.
#'     Scatter plot of agent opinions on dimensions \code{x} and \code{y}
#'     (defaults \code{1} and \code{2}). \code{pch} defaults to \code{1}
#'     (open circle). \code{xlim} and \code{ylim} both default to
#'     \code{c(-1, 1)}. If \code{time} is specified, retrieves the logged
#'     state at that iteration. Additional arguments are passed to
#'     \code{plot()}.}
#'   \item{\code{report_polarization(log, show_plot, ylim)}}{Report. Returns
#'     the polarization measure. If \code{log = NULL}, returns a single numeric
#'     value for the current state. If \code{log} is specified, returns a data
#'     frame with columns \code{time} and \code{polarization} across logged
#'     iterations, and optionally plots the time series (\code{show_plot =
#'     TRUE}). \code{ylim} controls the y-axis range of the time-series plot
#'     (default \code{c(0, 1.25)}).}
#'   \item{\code{report_net_stat(print)}}{Report. Computes and optionally
#'     prints network density, average local clustering coefficient, and mean
#'     geodesic path length. Returns a named numeric vector
#'     \code{c(density, clustering, path_length)}.}
#'   \item{\code{report_group_opinion(opinion_dim, log, show_plot, ylim)}}{
#'     Report. Detects network communities via
#'     \code{igraph::cluster_walktrap()} and summarizes mean opinion per
#'     community. \code{opinion_dim} selects the opinion dimension to report
#'     (default \code{1}). If \code{log = NULL}, displays a boxplot of current
#'     opinions by community and returns a named vector of community means. If
#'     \code{log} is specified (default \code{"all"}), returns a matrix of
#'     community mean opinions over time and optionally plots trajectories
#'     (\code{show_plot = TRUE}). \code{ylim} controls the y-axis range
#'     (default \code{c(-1, 1)}).}
#' }
#'
#' @references
#' Flache, A., & Macy, M. W. (2011). Small worlds and cultural polarization.
#' \emph{Journal of Mathematical Sociology}, 35(1-3), 146--176.
x#'
#' Huberman, B. A., & Glance, N. S. (1993). Evolutionary games and computer
#' simulations. \emph{Proceedings of the National Academy of Sciences}, 90,
#' 7716--7718.
#'
#' Watts, D. J., & Strogatz, S. (1998). Collective dynamics of "small-world"
#' networks. \emph{Nature}, 393, 440--442.
#'
#' @seealso \code{\link[rABM]{Game}}, \code{\link[rABM]{run_Game}},
#'   \code{\link[rABM]{value_of}}
#'
#' @examples
#' \dontrun{
#' # Replicate baseline condition of Experiment 1 in Flache & Macy (2011):
#' # disconnected caveman graph, no random ties, negative weights allowed
#' G_base <- model_opinion_flache(
#'   n_agent                = 100,
#'   n_agent_per_cave       = 5,
#'   n_issue                = 2,
#'   negative_weight        = TRUE,
#'   random_tie_prop_outset = 0,
#'   times                  = 100
#' )
#'
#' # Polarization at the end state
#' G_base$report_polarization()
#'
#' # Add random ties at the outset (small-world condition)
#' G_sw <- model_opinion_flache(
#'   n_agent                = 100,
#'   n_agent_per_cave       = 5,
#'   n_issue                = 2,
#'   negative_weight        = TRUE,
#'   random_tie_prop_outset = 0.003,
#'   times                  = 100
#' )
#'
#' # Add random ties mid-simulation (iteration 50)
#' G_mid <- model_opinion_flache(
#'   n_agent                  = 100,
#'   n_agent_per_cave         = 5,
#'   n_issue                  = 2,
#'   negative_weight          = TRUE,
#'   random_tie_prop_outset   = 0,
#'   random_tie_prop_midpoint = 0.003,
#'   random_tie_adding_at     = 50,
#'   times                    = 100
#' )
#'
#' # Scatter plot of opinions on dimensions 1 and 2
#' G_mid$plot_opinion()
#'
#' # Network plot with vertex colours reflecting dimension-1 opinions
#' G_mid$plot_network(opinion_dim = 1)
#'
#' # Polarization trajectory (requires logging; see run_Game)
#' G_mid$report_polarization(log = 1:100, show_plot = TRUE)
#'
#' # Mean opinion per community over time
#' G_mid$report_group_opinion(opinion_dim = 1, log = "all", show_plot = TRUE)
#'
#' # Return initialized object without running (for inspection)
#' G_init <- model_opinion_flache(times = 0)
#' G_init$plot_network()
#' }
#'
#' @importFrom sna gplot gden geodist
#' @importFrom igraph graph_from_adjacency_matrix transitivity cluster_walktrap
#'
#' @export
model_opinion_flache <- function(
  n_agent = 100,
  n_agent_per_cave = 5,
  n_issue = 2,
  negative_weight = TRUE,
  random_tie_prop_outset = 0.003,
  random_tie_prop_midpoint = 0.003,
  random_tie_adding_at = NULL,
  times = 100,
  agent_opinion = NULL,
  agent_network = NULL
){
  # check arguments
    stopifnot("'n_agent' must be a positive integer of length 1." = n_agent > 0 && length(n_agent)==1)
    stopifnot("'n_agent_per_cave' must be a positive integer of length 1." = n_agent_per_cave > 0 && length(n_agent_per_cave)==1)
    stopifnot("'n_issue' must be a positive integer of length 1." = n_issue > 0 && length(n_issue)==1)
    stopifnot("'negative_weight' must be a logical vector of length 1." = is.logical(negative_weight) && length(negative_weight)==1)
    stopifnot("'random_tie_prop_outset' must be between 0 and 1." = length(random_tie_prop_outset)==1 && random_tie_prop_outset >= 0 && random_tie_prop_outset <= 1)
    stopifnot("'random_tie_prop_midpoint' must be between 0 and 1." = length(random_tie_prop_midpoint)==1 && random_tie_prop_midpoint >= 0 && random_tie_prop_midpoint <= 1)
    if(!is.null(random_tie_adding_at)){
      stopifnot("'random_tie_adding_at' must be an integer of length 1 and greater than 1." = length(random_tie_adding_at)==1 && random_tie_adding_at > 1)
    }

    if(!is.null(agent_opinion)){
      stopifnot("Number of rows of 'agent_opinion' must be 'n_agent'." = nrow(agent_opinion) == n_agent)
      if(any(agent_opinion > 1) || any(agent_opinion < -1))
        stop("'agent_opinion' must be between -1 and +1.")
    }

    if(!is.null(agent_network)){
      stopifnot("The number of rows and columns of the 'agent_network' must be the 'n_agent'." =
                  nrow(agent_network) == n_agent && ncol(agent_network) == n_agent)
    }

    # number of caves
    if(n_agent %% n_agent_per_cave > 0){
      warning("A cave does not have the required number of agents because 'n_agent' is not divided by 'n_agent_per_cave' without residue.")
    }

    n_cave <- n_agent %/% n_agent_per_cave
    cave_residue <- n_agent %% n_agent_per_cave


    #==============================
    # State
    #==============================

    # Game
    G <- Game()

    # opinion
    if(is.null(agent_opinion)){
      agent_opinion <- data.frame(
        matrix(runif(n_agent * n_issue, min = -1, max = 1), n_agent, n_issue)
      )
    }
    add_field(G, State(agent_opinion))


    # network
    if(is.null(agent_network)){
      agent_network <- matrix(0, nrow = n_agent, ncol = n_agent,
                              dimnames = list(1:n_agent, 1:n_agent))
      for(i in 1:n_cave){
        agent_start <- n_agent_per_cave * (i - 1) + 1
        agent_end   <- n_agent_per_cave * i
        agent_network[(agent_start:agent_end), (agent_start:agent_end)] <- 1
      }
      diag(agent_network) <- 0

      if(cave_residue > 0){
        agent_start <- n_agent_per_cave * n_cave + 1
        agent_end   <- n_agent_per_cave * n_cave + cave_residue
        agent_network[(agent_start:agent_end), (agent_start:agent_end)] <- 1
      }

      # random tie
      if(random_tie_prop_outset > 0){
        idx <- which(agent_network == 0 & lower.tri(agent_network), arr.ind = TRUE)
        if(nrow(idx) > 0){
          n_random_tie <- ceiling(nrow(idx) * random_tie_prop_outset)
          selected_row <- sample(nrow(idx), size = n_random_tie)
          selected_idx <- rbind(
            idx[selected_row, ,drop = FALSE],
            idx[selected_row, c(2,1),drop = FALSE]
          )

          # add
          agent_network[selected_idx] <- 1
        }
      }
    }
    add_field(G, State(agent_network))

    # settings
    settings <- list(n_agent = n_agent,
                     n_issue = n_issue,
                     n_cave = n_cave,
                     cave_residue = cave_residue,
                     negative_weight = negative_weight,
                     n_agent_per_cave = n_agent_per_cave,
                     random_tie_adding_at = random_tie_adding_at,
                     random_tie_prop_outset = random_tie_prop_outset,
                     random_tie_prop_midpoint = random_tie_prop_midpoint)

    add_field(G, State(settings))

    #===================================
    # Active State
    #===================================

    if(negative_weight){
      network_weight <- function(){
        weight <- 1 - (as.matrix(dist(self$agent_opinion, method = "manhattan")))/self$settings$n_issue
        weight[self$agent_network==0] <- 0
        diag(weight) <- 0
        weight
      }
    }else{
      network_weight <- function(){
        weight <- 1 - (as.matrix(dist(self$agent_opinion, method = "manhattan")))/(self$settings$n_issue * 2)
        weight[self$agent_network==0] <- 0
        diag(weight) <- 0
        weight
      }
    }
    add_field(G, Active(network_weight))

    #====================================
    # Act
    #====================================

    # update_opinion
    update_opinion <- function(){
      # sample the egos
      egos <- sample(seq_len(self$settings$n_agent), replace = TRUE, size = self$settings$n_agent)

      for(ego in egos){
        # alters
        alter <- which(self$agent_network[ego, ]==1)
        if(length(alter)==0) next

        # amount of influence
        S <- sweep(as.matrix(self$agent_opinion[alter, , drop = FALSE]), 2,
                   as.numeric(self$agent_opinion[ego, ]), "-")
        delta_S <- (self$network_weight[ego, alter, drop = FALSE] %*% S) / (2 * length(alter))

        for(k in 1:self$settings$n_issue){
          ego_opinion <- self$agent_opinion[ego, k]
          if(ego_opinion > 0){
            self$agent_opinion[ego, k] <- ego_opinion + delta_S[,k]*(1 - ego_opinion)
          }else{
            self$agent_opinion[ego, k] <- ego_opinion + delta_S[,k]*(1 + ego_opinion)
          }
        }
      }
    }

    add_field(G, Act(update_opinion))

    # add random tie
    add_random_tie <- function(){
      # return NULL if it is not the time to add ties
      if(is.null(self$settings$random_tie_adding_at)) return(NULL)
      if(self$time != self$settings$random_tie_adding_at) return(NULL)
      if(self$settings$random_tie_prop_midpoint==0) return(NULL)

      idx <- which(self$agent_network == 0 & lower.tri(self$agent_network), arr.ind = TRUE)
      if(nrow(idx) > 0){
        n_random_tie <- ceiling(nrow(idx) * self$settings$random_tie_prop_midpoint)
        selected_row <- sample(nrow(idx), size = n_random_tie)
        selected_idx <- rbind(
          idx[selected_row, ,drop = FALSE],
          idx[selected_row, c(2,1),drop = FALSE]
        )

        # add
        self$agent_network[selected_idx] <- 1
      }
    }
    add_field(G, Act(add_random_tie))


    #=====================================
    # Plot
    #=====================================

    # plot_network
    plot_network <- function(opinion_dim = 1, ...){
      opinion <- self$agent_opinion[ ,opinion_dim]

      ramp <- colorRamp(c("blue", "white", "red"))
      z_norm <- (opinion + 1) / 2  # -1→0, 0→0.5, +1→1
      col_vec <- rgb(ramp(z_norm), maxColorValue = 255)

      net_stat <- self$report_net_stat(print = FALSE)

      sna::gplot(self$agent_network, gmode = "graph", vertex.col = col_vec,
                 sub = paste0("Density: ", round(net_stat[1], 3), "; ",
                              "Clustering: ", round(net_stat[2], 3), "; ",
                              "Path (avg): ", round(net_stat[3], 3), "\n",
                              "Vertex colour: blue = -1, white = 0, red = +1"),
                 ...)
    }
    add_field(G, Plot(plot_network))

    # opinion
    plot_opinion <- function(x = 1, y = 2, pch = 1, time = NULL,
                             xlim = c(-1, 1), ylim = c(-1, 1), ...){
      if(!is.null(time)){
        stopifnot("'time' must be a positive interger of length 1." = time > 0 && length(time) == 1)
        x_val <- value_of(self, "agent_opinion", log = time)[[1]][ ,x]
        y_val <- value_of(self, "agent_opinion", log = time)[[1]][ ,y]
        pol   <- self$report_polarization(log = time, show_plot = FALSE)["polarization"]
      }else{
        x_val <- self$agent_opinion[ ,x]
        y_val <- self$agent_opinion[ ,y]
        pol <- self$report_polarization(show_plot = FALSE)
        time <- self$time
      }

      plot(x = x_val, y = y_val,
           xlab = paste0("Dim: ", x), ylab = paste0("Dim: ", y),
           pch = pch,
           main = paste("time =", time), xlim = xlim, ylim = ylim,
           sub = paste0("Polarization = ", round(pol, 3)),
           ...)
    }
    add_field(G, Plot(plot_opinion))

    #=====================================
    # Report
    #=====================================

    report_polarization <- function(log = NULL, show_plot = TRUE, ylim = c(0, 1.25)){
      if(is.null(log)){
        d <- as.matrix(dist(self$agent_opinion, method = "manhattan"))
        diag(d) <- NA
        var(as.vector(d), na.rm = TRUE)
      }else{
        opinion_list <- value_of(self, "agent_opinion", log = log)
        time_vect    <- unlist(value_of(self, "time", log = log))
        d_vect <- vapply(opinion_list, FUN.VALUE = numeric(1), FUN = function(opinion){
          d <- as.matrix(dist(opinion, method = "manhattan"))
          diag(d) <- NA
          var(as.vector(d), na.rm  = TRUE)
        })

        # plot
        if(isTRUE(show_plot)){
          plot(x = time_vect, y = d_vect, type = "l", xlab = "time", ylim = ylim,
               ylab = "polarization")
        }

        # output
        data.frame(time = as.numeric(time_vect),
                   polarization = d_vect)
      }
    }
    add_field(G, Report(report_polarization))

    report_net_stat <- function(print = TRUE){
      net <- self$agent_network

      clust <- igraph::transitivity(igraph::graph_from_adjacency_matrix(net), type = "localaverage")
      den <- sna::gden(net)

      geo <- as.vector(sna::geodist(net, count.paths = FALSE)$gdist)
      geo_mean <- mean(geo)

      # print
      if(isTRUE(print)){
        cat("Density           : ", den, "\n")
        cat("Clustering        : ", clust, "\n")
        cat("Path length (avg.): ", geo_mean, "\n")
      }

      # return
      c(density = den, clustering = clust, path_length = geo_mean)
    }
    add_field(G, Report(report_net_stat))

    # report_group_opinion
    report_group_opinion <- function(opinion_dim = 1, log = "all", show_plot = TRUE,
                                     ylim = c(-1, 1)){
      # grouping
      net <- igraph::graph_from_adjacency_matrix(self$agent_network)
      wt <- cluster_walktrap(net)
      gr <- wt$membership

      # current state
      if(is.null(log)){
        opinion <- self$agent_opinion[ ,opinion_dim]
        boxplot(opinion ~ gr, xlab = "caves", ylab = paste("Dim:", opinion_dim))
        return(tapply(opinion, gr, mean))
      }else{
      # from log
        time <- as.numeric(unlist(value_of(self, "time", log = log)))
        opinion_list0 <- value_of(self, "agent_opinion", log = log)
        opinion_list <- lapply(opinion_list0, function(x){x[,opinion_dim]})

        opinion_mean_list <- lapply(opinion_list, function(opinion){
          tapply(opinion, gr, mean)})
        opinion_mean <- do.call(rbind, opinion_mean_list)

        if(isTRUE(show_plot)){
          plot(x = time, y = opinion_mean[ ,1], type = "l", ylim = ylim,
               ylab = paste0("Dim: ", opinion_dim))
          for(i in 2:ncol(opinion_mean)){
            par(new = TRUE)
            plot(x = time, y = opinion_mean[ ,i], type = "l", ann = FALSE, ylim = ylim, axes = FALSE)
          }
        }
        return(opinion_mean)
        }
    }
    add_field(G, Report(report_group_opinion))

    #=====================================
    # RUN
    #=====================================

    # return initial object if required
    if(times == 0){return(G)}

    # RUN
    G2 <- run_Game(G, plan = c("add_random_tie", "update_opinion"), times = times)
    return(G2)
}
