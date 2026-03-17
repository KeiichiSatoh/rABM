#' Score-to-probability transformation rules
#'
#' Convert a numeric score vector or score matrix into a probability distribution.
#'
#' These functions implement several common probability transformation rules used
#' in agent-based modeling, reinforcement learning, and evolutionary computation.
#' Each function maps scores to probabilities that sum to one.
#'
#' If `x` is a matrix, each row is treated as an independent choice set and
#' probabilities are normalized row-wise.
#'
#' Missing values (`NA`) are treated as unavailable choices. In the returned
#' probabilities, such entries are assigned zero.
#'
#' @name prob_rules
#' @aliases prob_linprop
#' @aliases prob_power
#' @aliases prob_softmax
#' @aliases prob_rank
#' @aliases prob_eps_greedy
#'
#' @param x A numeric vector or numeric matrix of scores.
#'
#'   If `x` is a matrix, each row is interpreted as a separate decision context.
#'
#' @param alpha A non-negative exponent used in [prob_power()]. Larger values
#'   place more probability mass on higher scores. `alpha = 1` corresponds to
#'   linear proportional transformation.
#'
#' @param beta A non-negative sensitivity parameter used in [prob_softmax()].
#'   Larger values make the distribution more concentrated on higher scores.
#'
#' @param eps An exploration parameter used in [prob_eps_greedy()]. With
#'   probability `1 - eps`, the best option is chosen; with probability `eps`,
#'   exploration is introduced uniformly across options.
#'
#' @param ties.method A character string specifying how ties are handled in
#'   [prob_rank()]. Passed to [rank()] or `matrixStats::rowRanks()`.
#'
#' @details
#' The implemented rules are as follows.
#'
#' **Linear proportional transformation**
#'
#' Scores are shifted if necessary so that all values become non-negative, and
#' then normalized:
#'
#' \deqn{p_i = x_i / \sum_j x_j}
#'
#' In [prob_linprop()], if any score is negative, the global minimum of `x` is
#' subtracted from all scores before normalization.
#'
#' **Power transformation**
#'
#' Scores are shifted to non-negative values if necessary, raised to the power
#' `alpha`, and normalized:
#'
#' \deqn{p_i = x_i^\alpha / \sum_j x_j^\alpha}
#'
#' In [prob_power()], `alpha = 1` gives the same result as linear proportional
#' transformation. Larger values increase selection pressure; values between
#' `0` and `1` flatten the distribution.
#'
#' **Softmax transformation**
#'
#' Scores are exponentiated after scaling by `beta`:
#'
#' \deqn{p_i = \exp(\beta x_i) / \sum_j \exp(\beta x_j)}
#'
#' [prob_softmax()] corresponds to the standard softmax (or Boltzmann / logit)
#' rule. Larger `beta` gives stronger concentration on high-score options.
#'
#' **Rank-based transformation**
#'
#' Scores are converted to ranks and normalized:
#'
#' \deqn{p_i = r_i / \sum_j r_j}
#'
#' where \eqn{r_i} denotes the rank of score \eqn{x_i}.
#'
#' **Epsilon-greedy transformation**
#'
#' The highest-score option receives probability
#'
#' \deqn{1 - \varepsilon + \varepsilon / n}
#'
#' while all options receive a uniform exploration mass
#'
#' \deqn{\varepsilon / n}
#'
#' before unavailable (`NA`) choices are set to zero and the result is
#' renormalized if needed.
#'
#' @return
#' A numeric vector or numeric matrix of probabilities with the same shape as
#' `x`.
#'
#' If `x` is a matrix, each row sums to one unless all entries in that row are
#' unavailable.
#'
#' @section Included functions:
#' \describe{
#'   \item{[prob_linprop()]}{Linear proportional transformation.}
#'   \item{[prob_power()]}{Power transformation.}
#'   \item{[prob_softmax()]}{Softmax transformation.}
#'   \item{[prob_rank()]}{Rank-based transformation.}
#'   \item{[prob_eps_greedy()]}{Epsilon-greedy transformation.}
#' }
#'
#' @examples
#' x <- c(1, 2, 3, 4)
#'
#' prob_linprop(x)
#' prob_power(x, alpha = 2)
#' prob_softmax(x, beta = 1)
#' prob_rank(x)
#' prob_eps_greedy(x, eps = 0.1)
#'
#' m <- matrix(c(
#'   1, 2, 3, 4,
#'   4, 3, 2, 1
#' ), nrow = 2, byrow = TRUE)
#'
#' prob_linprop(m)
#' prob_power(m, alpha = 2)
#' prob_softmax(m, beta = 1)
#' prob_rank(m)
#' prob_eps_greedy(m, eps = 0.1)
#'
#' m2 <- matrix(c(
#'   1, NA, 3, 4,
#'   NA, 3, 2, 1
#' ), nrow = 2, byrow = TRUE)
#'
#' prob_softmax(m2, beta = 1)
#' prob_rank(m2)
#' prob_eps_greedy(m2, eps = 0.1)
#'
#' @export
NULL


#===============================================================================
# linear proportion
#===============================================================================

#' @rdname prob_rules
#' @export
prob_linprop <- function(x) {
  if (is.matrix(x)) {
    xmin <- min(x, na.rm = TRUE)
    if (xmin < 0) {
      x <- x - xmin
    }

    out <- x/rowSums(x, na.rm = TRUE)

  } else if (is.numeric(x) && is.null(dim(x))) {
    xmin <- min(x, na.rm = TRUE)
    if (xmin < 0) {
      x <- x - xmin
    }

    out <- x / sum(x, na.rm = TRUE)

  } else {
    stop("'x' must be a numeric matrix or numeric vector.")
  }

  out[!is.finite(out)] <- 0
  out
}


#===============================================================================
# prob_power
#===============================================================================

#' @rdname prob_rules
#' @export

prob_power <- function(x, alpha = 1) {
  if (is.matrix(x)) {
    xmin <- min(x, na.rm = TRUE)
    if (xmin < 0) x <- x - xmin
    z <- x^alpha
    out <- z / rowSums(z, na.rm = TRUE)

  } else if (is.numeric(x) && is.null(dim(x))) {
    xmin <- min(x, na.rm = TRUE)
    if (xmin < 0) x <- x - xmin
    z <- x^alpha
    out <- z / sum(z, na.rm = TRUE)

  } else {
    stop("'x' must be a numeric matrix or numeric vector.")
  }

  out[!is.finite(out)] <- 0
  out
}


#===============================================================================
# softmax
#===============================================================================

#' @rdname prob_rules
#' @export
prob_softmax <- function(x, beta = 1) {
  if (is.matrix(x)) {
    tx <- beta * x
    xmax <- matrixStats::rowMaxs(tx, na.rm = TRUE)
    z <- exp(tx - xmax)
    out <- z / rowSums(z, na.rm = TRUE)
  } else if (is.numeric(x) && is.null(dim(x))) {
    tx <- beta * x
    z <- exp(tx - max(tx, na.rm = TRUE))
    out <- z / sum(z, na.rm = TRUE)
  } else {
    stop("'x' must be a numeric matrix or numeric vector.")
  }

  out[!is.finite(out)] <- 0
  out
}



#===============================================================================
# prob_rank
#===============================================================================

#' @rdname prob_rules
#' @export
prob_rank <- function(x, ties.method = c("max", "average", "first", "last", "random", "min")) {
  ties.method <- match.arg(ties.method)

  if (is.matrix(x)) {
    r <- matrixStats::rowRanks(x, ties.method = ties.method)
    out <- r / rowSums(r, na.rm = TRUE)

  } else if (is.numeric(x) && is.null(dim(x))) {
    r <- rank(x, ties.method = ties.method)
    r[is.na(x)] <- NA
    out <- r / sum(r, na.rm = TRUE)

  } else {
    stop("'x' must be a numeric matrix or numeric vector.")
  }

  out[!is.finite(out)] <- 0
  out
}


#===============================================================================
# Epsilon-Greedy
#===============================================================================

#' @rdname prob_rules
#' @export

prob_eps_greedy <- function(x, eps = 0.1) {
  if (is.matrix(x)) {
    n_row <- nrow(x)
    n_col <- ncol(x)
    na_cell <- is.na(x)

    if (any(na_cell)) {
      x2 <- x
      x2[na_cell] <- -Inf
      best_idx <- max.col(x2)
    } else {
      best_idx <- max.col(x)
    }

    out <- matrix(eps / n_col, n_row, n_col)
    out[cbind(seq_len(n_row), best_idx)] <- 1 - eps + eps / n_col

    if (any(na_cell)) {
      out[na_cell] <- 0
      out <- out / rowSums(out)
    }

  } else if (is.numeric(x) && is.null(dim(x))) {
    n <- length(x)
    na_cell <- is.na(x)

    if (any(na_cell)) {
      x2 <- x
      x2[na_cell] <- -Inf
      best_idx <- which.max(x2)
    } else {
      best_idx <- which.max(x)
    }

    out <- rep(eps / n, n)
    out[best_idx] <- 1 - eps + eps / n

    if (any(na_cell)) {
      out[na_cell] <- 0
      out <- out / sum(out)
    }

  } else {
    stop("'x' must be a numeric matrix or numeric vector.")
  }

  out[!is.finite(out)] <- 0
  out
}


