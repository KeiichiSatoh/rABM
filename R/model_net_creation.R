#' Network Formation Simulation Model
#'
#' Simulates a directed (or undirected) network formation process among agents
#' using a configurable set of network mechanisms (popularity, reciprocity,
#' homophily, transitivity, common-source, common-target). Each agent
#' sequentially selects new ties according to a weighted probability derived
#' from the specified mechanisms until all agents reach their target number of
#' friends.
#'
#' @param n_agent A positive integer of length 1. Number of agents (nodes) in
#'   the network. Default is \code{100}.
#' @param n_friend A non-negative integer or an integer vector of length
#'   \code{n_agent}. Target number of outgoing ties per agent. If a scalar is
#'   supplied, it is recycled to length \code{n_agent}. Default is \code{5}.
#' @param group_prop A numeric vector of group proportions that sums to 1.
#'   Used to assign agents to groups via \code{\link{make_group_labels}}.
#'   Default is \code{c(0.5, 0.5)}.
#' @param net_logic A character vector specifying which network mechanisms to
#'   activate. Any subset of \code{c("popl", "recip", "homoph", "transiv",
#'   "cmsource", "cmtarget")} may be supplied (partial matching via
#'   \code{\link{match.arg}} applies). Pass \code{NULL} to use a uniform random
#'   tie-formation rule. Default activates all six mechanisms.
#'   \describe{
#'     \item{\code{"popl"}}{Popularity: agents with higher in-degree are
#'       preferred.}
#'     \item{\code{"recip"}}{Reciprocity: agents who already point to ego are
#'       preferred.}
#'     \item{\code{"homoph"}}{Homophily: agents in the same group are
#'       preferred.}
#'     \item{\code{"transiv"}}{Transitivity: friends-of-friends are preferred.}
#'     \item{\code{"cmsource"}}{Common source: agents who share in-neighbours
#'       with ego are preferred.}
#'     \item{\code{"cmtarget"}}{Common target: agents who share out-neighbours
#'       with ego are preferred.}
#'   }
#' @param net_effect A numeric vector of the same length as \code{net_logic},
#'   giving the weight (effect size) of each mechanism. Positive values
#'   increase preference; negative values decrease it. Pass \code{NULL} when
#'   \code{net_logic} is \code{NULL}. Default is \code{c(1,1,1,1,1,1)}.
#' @param seed An integer passed to \code{\link{set.seed}} for reproducibility,
#'   or \code{NULL} (default) to use the current RNG state.
#' @param undirected Logical of length 1. If \code{TRUE}, each drawn edge is
#'   symmetrised immediately via \code{\link{symmetrize}}. Default is
#'   \code{FALSE}.
#' @param return_init_G Logical of length 1. If \code{TRUE}, the function
#'   returns the initialised \code{Game} object \emph{before} running the
#'   simulation, which is useful for debugging or manual stepping. Default is
#'   \code{FALSE}.
#'
#' @return
#' If \code{return_init_G = FALSE} (default), a \code{Game} object \code{G2}
#' after the simulation has run to completion (i.e., all agents have reached
#' their target number of friends). The object exposes the following fields and
#' methods:
#' \describe{
#'   \item{\code{$net}}{Integer matrix (\code{n_agent} × \code{n_agent}).
#'     Final adjacency matrix.}
#'   \item{\code{$group}}{Integer vector of length \code{n_agent}. Group
#'     membership labels.}
#'   \item{\code{$settings}}{Named list of simulation parameters.}
#'   \item{\code{$time}}{Integer. Number of tie-formation steps executed.}
#'   \item{\code{$plot_net(displaylabels, show_time)}}{Plots the network using
#'     \code{\link[sna]{gplot}}.}
#'   \item{\code{$update_net()}}{Executes one tie-formation step manually.}
#'   \item{\code{$report_net_stat(log, show_print)}}{Returns a
#'     \code{data.frame} of graph-level statistics. See Details.}
#'   \item{\code{$report_gini(log, show_plot)}}{Returns a \code{data.frame}
#'     of in-degree Gini coefficients over time. See Details.}
#' }
#' If \code{return_init_G = TRUE}, the initialised (pre-run) \code{Game}
#' object \code{G} is returned instead.
#'
#' @details
#' \strong{Tie-formation mechanism.}
#' At each step, one agent \eqn{i} is sampled uniformly from those who have
#' not yet reached their target out-degree. A score matrix is computed as
#' \deqn{S = 1 + \sum_k w_k \cdot M_k,}
#' where \eqn{M_k} is the matrix produced by the \eqn{k}-th mechanism and
#' \eqn{w_k} is the corresponding element of \code{net_effect}. Existing edges
#' and the diagonal are masked to zero. A new partner \eqn{j} is then sampled
#' from row \eqn{i} of \eqn{S} with probability proportional to the scores
#' (via \code{\link{sample_weighted}}).
#'
#' \strong{\code{$report_net_stat(log, show_print)}.}
#' Computes graph-level statistics using \pkg{igraph}:
#' edge density, mean in-degree, degree centralisation, global transitivity,
#' nominal assortativity (by group), and dyad census proportions (mutual,
#' asymmetric, null). When \code{log} is supplied, statistics are computed for
#' every snapshot in the log and returned as a multi-row \code{data.frame};
#' the result is returned invisibly with a \code{message}.
#'
#' \strong{\code{$report_gini(log, show_plot)}.}
#' Computes the Gini coefficient of the in-degree distribution:
#' \deqn{G = \frac{\sum_{i}\sum_{j}|d_i - d_j|}{2n^2\bar{d}},}
#' where \eqn{d_i} is the in-degree of node \eqn{i} and \eqn{\bar{d}} is the
#' mean in-degree. Returns 0 when the network has no edges. When \code{log} is
#' supplied, a time-series \code{data.frame} and (optionally) a line plot are
#' produced.
#'
#' @seealso
#' \code{\link{run_Game}}, \code{\link{make_group_labels}},
#' \code{\link{sample_weighted}}, \code{\link{symmetrize}},
#' \code{\link[sna]{gplot}}, \code{\link[igraph]{graph_from_adjacency_matrix}}
#'
#' @examples
#' # --- Basic usage (all defaults) ---
#' G2 <- model_net_creation(n_agent = 20, n_friend = 3, seed = 42)
#' G2$plot_net()
#' G2$report_net_stat()
#' G2$report_gini()
#'
#' # --- Uniform random network (no mechanism) ---
#' G2 <- model_net_creation(
#'   n_agent = 50,
#'   n_friend = 5,
#'   net_logic = NULL,
#'   net_effect = NULL,
#'   seed = 1
#' )
#'
#' # --- Homophily only ---
#' G2 <- model_net_creation(
#'   n_agent = 30,
#'   n_friend = 4,
#'   group_prop = c(0.4, 0.6),
#'   net_logic  = "homoph",
#'   net_effect = 2,
#'   seed = 7
#' )
#'
#' # --- Undirected network ---
#' G2 <- model_net_creation(
#'   n_agent    = 20,
#'   n_friend   = 3,
#'   undirected = TRUE,
#'   seed       = 99
#' )
#'
#' # --- Return initialised object for manual stepping ---
#' G <- model_net_creation(n_agent = 10, n_friend = 2, return_init_G = TRUE)
#' G$update_net()
#' G$plot_net()
#'
#' @importFrom igraph graph_from_adjacency_matrix edge_density degree
#'   centr_degree transitivity assortativity_nominal dyad_census
#' @importFrom sna gplot
#'
#' @export

model_net_creation <- function(
    n_agent = 100,
    n_friend = 5,
    group_prop = c(0.5, 0.5),
    net_logic = c("popl", "recip", "homoph", "transiv", "cmsource","cmtarget"),
    net_effect = c(1,1,1,1,1,1),
    seed = NULL,
    undirected = FALSE,
    return_init_G = FALSE
){

  # =========================================================
  # validation
  # =========================================================

  stopifnot("'n_agent' must be a positive integer of length 1." =
              length(n_agent) == 1 && n_agent > 0 && n_agent == as.integer(n_agent))

  if(length(n_friend) == 1){
    n_friend <- rep(n_friend, n_agent)
  }
  stopifnot("The length of 'n_friend' must be 'n_agent'." = length(n_friend) == n_agent)
  stopifnot("'n_friend' must be a positive integer or 0." = all(n_friend >= 0))

  if(!is.null(net_logic)){
    net_logic <- match.arg(net_logic, several.ok = TRUE)
  }

  # net_effect
  if(!is.null(net_effect)){
    stopifnot("The length of 'net_effect' must be equal to the length of 'net_logic'." = length(net_logic) == length(net_effect))
  }

  # seed
  if(!is.null(seed)){
    set.seed(seed = seed)
  }

  # undirected
  stopifnot("'undirected' must be a logical vector of length 1." = length(undirected) == 1 && is.logical(undirected))


  # =========================================================
  # Initialize Game
  # =========================================================

  G <- Game()

  # =========================================================
  # state
  # =========================================================
  # node idx
  node_idx <- seq_len(n_agent)

  # initial network
  net <- matrix(
    0L,
    nrow = n_agent,
    ncol = n_agent,
    dimnames = list(node_idx, node_idx)
  )

  # group
  group <- make_group_labels(n_agent, group_prop)

  # settings
  settings <- list(
    n_agent = n_agent,
    n_friend = n_friend,
    node_idx = node_idx,
    undirected = undirected,
    group_prop = group_prop,
    net_logic = net_logic,
    net_effect = net_effect
  )

  add_field(G, State(net), State(group), State(settings))

  # =========================================================
  # plot_FUN
  # =========================================================

  plot_net <- function(displaylabels = TRUE, show_time = TRUE) {
    gmode <- if(self$settings$undirected) "graph" else "digraph"
    main_title <- if(isTRUE(show_time)) paste0("time = ", self$time) else NULL
    sna::gplot(self$net, gmode = gmode, displaylabels = displaylabels,
               main = main_title, edge.col = "gray")
  }

  add_field(G, Plot(plot_net))

  # =========================================================
  # act_FUN
  # =========================================================

  #--------------------------------------------
  # net logic
  #--------------------------------------------
  # popularity
  calc_popl <- function(net, group){
    n <- nrow(net)
    mat <- matrix(colSums(net), n, n, byrow = TRUE)
    diag(mat) <- 0
    mat
  }

  # recip
  calc_recip <- function(net, group){
    t(net)
  }


  # homoph
  calc_homoph <- function(net, group){
    n <- length(group)
    gr <- unique(group)
    mat <- matrix(0, n, length(gr))
    for(k in seq_along(gr)){
      mat[group == gr[k], k] <- 1
    }

    mat_mlt <- mat %*% t(mat)
    diag(mat_mlt) <- 0
    mat_mlt
  }


  # transiv
  calc_transiv <- function(net, group){
    mat <- net %*% net
    diag(mat) <- 0
    mat
  }

  # cmsource
  calc_cmsource <- function(net, group){
    mat <- t(net) %*% net
    diag(mat) <- 0
    mat
  }

  # cmtarget
  calc_cmtarget <- function(net, group){
    mat <- net %*% t(net)
    diag(mat) <- 0
    mat
  }

  net_calc_FUN <- list(popl = calc_popl,
                       recip = calc_recip,
                       homoph = calc_homoph,
                       transiv = calc_transiv,
                       cmsource = calc_cmsource,
                       cmtarget = calc_cmtarget)
  add_field(G, State(net_calc_FUN))
  #----------------------------------
  # net_update
  #----------------------------------
  update_net <- function(){
    n <- self$settings$n_agent
    net_logic <- self$settings$net_logic
    net_effect <- self$settings$net_effect

    if(is.null(net_logic)){
      score <- matrix(1, n, n)
    }else{
      calc_array <- array(0, dim = c(n, n, length(net_logic)))
      for(i in seq_along(net_logic)){
        calc_array[,,i] <- net_effect[i] * self$net_calc_FUN[[net_logic[i]]](net = self$net,
                                                                             group = self$group)
      }
      score <- apply(calc_array, MARGIN = c(1,2), sum) + 1   # 1 = intercept
    }

    # mask the score of the already existed edges
    score[self$net == 1] <- 0
    diag(score) <- 0

    # select the agent
    candid <- self$settings$node_idx[rowSums(self$net) < self$settings$n_friend]
    if(length(candid)==0) return(NULL)
    agent <- sample2(candid, size = 1)

    # select a new partner
    new_partner <- sample_weighted(score[agent, ,drop = FALSE], size = 1)

    # draw edges
    self$net[agent, new_partner] <- 1

    # if symmetric
    if(isTRUE(self$settings$undirected)){
      self$net <- symmetrize(self$net)
    }
  }

  add_field(G, Act(update_net))


  # =========================================================
  # stop_FUN
  # =========================================================

  n_friend_reached <- function() {
    current_n_friends <- rowSums(self$net)
    all(current_n_friends >= self$settings$n_friend)
  }

  add_field(G, Stop(n_friend_reached))


  # =========================================================
  # report_FUN
  # =========================================================

  report_net_stat <- function(log = NULL, show_print = TRUE){
    # internal function
    graph_stat <- function(net, undirected, time, show_print) {
      graph_mode <- if (undirected) "undirected" else "directed"

      net_igraph <- igraph::graph_from_adjacency_matrix(
        net,
        mode = graph_mode,
        diag = FALSE
      )

      dens <- igraph::edge_density(graph = net_igraph)
      indeg <- igraph::degree(net_igraph, mode = "in")
      centr <- igraph::centr_degree(net_igraph, mode = "in")$centralization
      trans <- igraph::transitivity(net_igraph, type = "global")
      assort <- igraph::assortativity_nominal(
        net_igraph,
        types = self$group,
        directed = !self$settings$undirected
      )
      dyad_cens <- unlist(igraph::dyad_census(net_igraph))
      dyad_cens_p <- dyad_cens / sum(dyad_cens)

      if(isTRUE(show_print)){
        cat("[summary of the graph]", "\n")
        cat("density                 : ", round(dens, 2), "\n", sep = "")
        cat("centralization          : ", round(centr, 2), "\n", sep = "")
        cat("transitivity            : ", round(trans, 2), "\n", sep = "")
        cat("assortativity (by group): ", round(assort, 2), "\n", sep = "")
        cat("dyad census (%)", "\n")
        cat("  mutual                : ", round(dyad_cens_p[1], 2), "\n", sep = "")
        cat("  asymmetric            : ", round(dyad_cens_p[2], 2), "\n", sep = "")
        cat("  null                  : ", round(dyad_cens_p[3], 2), "\n", sep = "")
      }

      out <- data.frame(
        time = time,
        density = dens,
        indegree_avg = mean(indeg),
        centralization = centr,
        transitivity = trans,
        assortativity = assort,
        dyad_census_mutual = dyad_cens_p[1],
        dyad_census_asymmetric = dyad_cens_p[2],
        dyad_census_null = dyad_cens_p[3]
      )
      invisible(out)
    }

    if(is.null(log)){
      out <- graph_stat(net = self$net, undirected = self$settings$undirected, time = self$time, show_print = show_print)
      invisible(out)
    }else{
      net_list <- value_of(G = self, field_name = "net", log = log)
      time_list <- as.numeric(unlist(value_of(G = self, field_name = "time", log = log)))

      stat_list <- vector("list", length(net_list))
      for(i in seq_along(stat_list)){
        stat_list[[i]] <- graph_stat(net = net_list[[i]],
                                     undirected = self$settings$undirected,
                                     time = time_list[i], show_print = FALSE)
      }
      out <- do.call(rbind, stat_list)

      message("Returning the statistics invisibly.")
      invisible(out)
    }
  }

  add_field(G, Report(report_net_stat))


  # gini index
  report_gini <- function(log = NULL, show_plot = TRUE) {

    calc_net_gini <- function(net) {
      indeg     <- colSums(net)
      indeg_avg <- mean(indeg)
      if (indeg_avg == 0) return(NaN)
      n <- length(indeg)
      # outer()で全ペア差分行列を一括生成 → 二重ループを排除
      sum(abs(outer(indeg, indeg, "-"))) / (2 * n^2 * indeg_avg)
    }

    if (is.null(log)) {
      calc_net_gini(net = self$net)
    } else {
      time_list <- as.numeric(unlist(value_of(self, field_name = "time", log = log)))
      net_list  <- value_of(self, field_name = "net", log = log)
      gini_list <- vapply(net_list, calc_net_gini, numeric(1))
      gini_list[is.nan(gini_list)] <- 0

      if (isTRUE(show_plot)) {
        plot(x = time_list, y = gini_list, type = "l",
             xlab = "time", ylab = "Gini")
      }
      data.frame(time = time_list, gini = gini_list)
    }
  }

  add_field(G, Report(report_gini))

  # =========================================================
  # run
  # =========================================================

  if(isTRUE(return_init_G)){
    return(G)
  }

  G2 <- run_Game(G = G, plan = "update_net",
                 nm_stop_FUN = "n_friend_reached")
  return(G2)
}

