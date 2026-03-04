#' @title Coordinate Movement Functions for rABM
#'
#' @description
#' A family of functions for moving agent coordinates toward target coordinates
#' under different spatial boundary conditions.
#'
#' The main wrapper function is \code{act_coord_move()}, which dispatches to
#' specific movement implementations. All movement functions can also be
#' called directly by the user.
#'
#' @details
#' ## General movement rule
#' Agents move from \code{agent_posit} toward \code{target_posit} by
#' \code{movement_speed} in one step. If an agent and its target coincide
#' (distance = 0), the agent does not move.
#'
#' ## Boundary assumptions
#' When bounded movement is used (\code{"torus"}, \code{"torus_netlogo"},
#' \code{"reflect"}, or \code{"stop"}), the space is assumed to have lower
#' bounds fixed at 0. That is, \code{space_max = c(x_max, y_max)} defines
#' a rectangle:
#'
#' \itemize{
#'   \item Torus: coordinates wrapped into \code{[0, x_max)} and \code{[0, y_max)}
#'   \item Reflect / Stop: bounded in \code{[0, x_max]} × \code{[0, y_max]}
#' }
#'
#' \code{space_max} must be non-negative.
#'
#' ## Movement types
#' \itemize{
#'   \item \code{"unlimit"}: Unbounded Euclidean movement.
#'   \item \code{"torus"}: Move in Euclidean space, then wrap.
#'   \item \code{"torus_netlogo"}: Compute direction using torus shortest difference
#'     (minimum image), then wrap (NetLogo-like behavior).
#'   \item \code{"reflect"}: Mirror (specular) reflection at boundaries.
#'   \item \code{"stop"}: Stop at the first boundary encountered.
#' }
#'
#' @param agent_posit Numeric matrix with two columns \code{c(x, y)}.
#' @param target_posit Numeric matrix with two columns \code{c(x, y)}.
#' @param movement_speed Numeric scalar.
#' @param space_max Numeric vector \code{c(x_max, y_max)}.
#' @param movement_type Character scalar specifying boundary type.
#'
#' @return A numeric matrix with two columns \code{c(x, y)}.
#'
#' @examples
#' agent  <- rbind(c(0, 0), c(2, 2))
#' target <- rbind(c(3, 4), c(5, 6))
#'
#' # Unbounded
#' act_coord_move(agent, target, movement_type = "unlimit")
#'
#' # Torus
#' act_coord_move(agent, target,
#'                movement_type = "torus",
#'                space_max = c(10, 10))
#'
#' # NetLogo-like torus
#' act_coord_move(agent, target,
#'                movement_type = "torus_netlogo",
#'                space_max = c(10, 10))
#'
#' # Reflection
#' act_coord_move(agent, target,
#'                movement_type = "reflect",
#'                space_max = c(10, 10))
#'
#' # Stop at boundary
#' act_coord_move(agent, target,
#'                movement_type = "stop",
#'                space_max = c(10, 10))
#'
#' @name act_coord_move_family
NULL


#' @rdname act_coord_move_family
#' @export
act_coord_move <- function(agent_posit, target_posit,
                           movement_speed = 0.1,
                           space_max = NULL,
                           movement_type = c("unlimit", "torus", "torus_netlogo",
                                             "reflect", "stop")) {
  movement_type <- match.arg(movement_type)

  if (movement_type != "unlimit") {
    if (is.null(space_max) || length(space_max) < 2L)
      stop("`space_max` must be length 2.")
    if (any(space_max < 0))
      stop("`space_max` must be non-negative.")
  }

  switch(
    movement_type,
    "unlimit" = act_coord_move_unlimit(agent_posit, target_posit, movement_speed),
    "torus"   = act_coord_move_torus(agent_posit, target_posit, movement_speed, space_max),
    "torus_netlogo" = act_coord_move_torus_netlogo(agent_posit, target_posit, movement_speed, space_max),
    "reflect" = act_coord_move_reflect(agent_posit, target_posit, movement_speed, space_max),
    "stop"    = act_coord_move_stop(agent_posit, target_posit, movement_speed, space_max)
  )
}


#' @rdname act_coord_move_family
#' @export
act_coord_move_unlimit <- function(agent_posit, target_posit, movement_speed) {
  dx <- target_posit[, 1] - agent_posit[, 1]
  dy <- target_posit[, 2] - agent_posit[, 2]
  L  <- sqrt(dx*dx + dy*dy)

  ok <- (L > 0)

  x <- agent_posit[, 1]
  y <- agent_posit[, 2]
  if (any(ok)) {
    invL <- 1 / L[ok]
    x[ok] <- x[ok] + dx[ok] * invL * movement_speed
    y[ok] <- y[ok] + dy[ok] * invL * movement_speed
  }

  cbind(x, y)
}


#' @rdname act_coord_move_family
#' @export
act_coord_move_torus <- function(agent_posit, target_posit, movement_speed, space_max) {
  dx <- target_posit[, 1] - agent_posit[, 1]
  dy <- target_posit[, 2] - agent_posit[, 2]
  L  <- sqrt(dx*dx + dy*dy)

  ok <- (L > 0)

  x <- agent_posit[, 1]
  y <- agent_posit[, 2]

  if (any(ok)) {
    invL <- 1 / L[ok]
    x[ok] <- x[ok] + dx[ok] * invL * movement_speed
    y[ok] <- y[ok] + dy[ok] * invL * movement_speed
  }

  x <- x %% space_max[1]
  y <- y %% space_max[2]

  cbind(x, y)
}


#' @rdname act_coord_move_family
#' @export
act_coord_move_torus_netlogo <- function(agent_posit, target_posit, movement_speed, space_max) {
  x_max <- space_max[1]
  y_max <- space_max[2]

  dx <- (target_posit[,1] - agent_posit[,1] + x_max/2) %% x_max - x_max/2
  dy <- (target_posit[,2] - agent_posit[,2] + y_max/2) %% y_max - y_max/2

  L <- sqrt(dx*dx + dy*dy)
  ok <- (L > 0)

  x <- agent_posit[, 1]
  y <- agent_posit[, 2]

  if (any(ok)) {
    invL <- 1 / L[ok]
    x[ok] <- x[ok] + dx[ok] * invL * movement_speed
    y[ok] <- y[ok] + dy[ok] * invL * movement_speed
  }

  x <- x %% x_max
  y <- y %% y_max

  cbind(x, y)
}


#' @rdname act_coord_move_family
#' @export
act_coord_move_reflect <- function(agent_posit, target_posit, movement_speed, space_max) {
  dx <- target_posit[, 1] - agent_posit[, 1]
  dy <- target_posit[, 2] - agent_posit[, 2]
  L  <- sqrt(dx*dx + dy*dy)

  ok <- (L > 0)

  x <- agent_posit[, 1]
  y <- agent_posit[, 2]
  if (any(ok)) {
    invL <- 1 / L[ok]
    x[ok] <- x[ok] + dx[ok] * invL * movement_speed
    y[ok] <- y[ok] + dy[ok] * invL * movement_speed
  }

  p_x <- 2 * space_max[1]
  p_y <- 2 * space_max[2]

  x <- {
    r <- x %% p_x
    r <- r + (r < 0) * p_x
    r - (r > space_max[1]) * (2 * (r - space_max[1]))
  }

  y <- {
    r <- y %% p_y
    r <- r + (r < 0) * p_y
    r - (r > space_max[2]) * (2 * (r - space_max[2]))
  }

  cbind(x, y)
}


#' @rdname act_coord_move_family
#' @export
act_coord_move_stop <- function(agent_posit, target_posit, movement_speed, space_max) {
  dx <- target_posit[, 1] - agent_posit[, 1]
  dy <- target_posit[, 2] - agent_posit[, 2]
  L  <- sqrt(dx*dx + dy*dy)

  x <- agent_posit[, 1]
  y <- agent_posit[, 2]

  ok <- (L > 0)
  if (!any(ok)) return(cbind(x, y))

  ux <- dx[ok] / L[ok]
  uy <- dy[ok] / L[ok]

  x1 <- x[ok]; y1 <- y[ok]
  x_max <- space_max[1]; y_max <- space_max[2]

  t <- rep(movement_speed, length(ux))

  tx <- rep(Inf, length(ux))
  pos <- (ux > 0); neg <- (ux < 0)
  if (any(pos)) tx[pos] <- (x_max - x1[pos]) / ux[pos]
  if (any(neg)) tx[neg] <- (0 - x1[neg]) / ux[neg]

  ty <- rep(Inf, length(uy))
  pos <- (uy > 0); neg <- (uy < 0)
  if (any(pos)) ty[pos] <- (y_max - y1[pos]) / uy[pos]
  if (any(neg)) ty[neg] <- (0 - y1[neg]) / uy[neg]

  t <- pmin(t, pmin(tx, ty))

  x[ok] <- x1 + t * ux
  y[ok] <- y1 + t * uy

  cbind(x, y)
}

