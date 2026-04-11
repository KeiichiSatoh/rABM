#' @title Coordinate Movement Functions for rABM
#'
#' @description
#' A family of functions for moving agent coordinates toward target coordinates
#' (or in a specified direction) under different spatial boundary conditions.
#'
#' The main wrapper function is \code{move_coord()}, which dispatches to
#' specific movement implementations. All movement functions can also be
#' called directly by the user and they are slightly faster because they have fewer validations.
#'
#' @details
#' ## General movement rule
#' Agents move from \code{agent_posit} toward \code{target_posit} by
#' \code{movement_speed} in one step. If an agent and its target coincide
#' (distance = 0), the agent does not move.
#'
#' Alternatively, movement direction can be specified via \code{radian}
#' instead of \code{target_posit}. Exactly one of \code{target_posit} or
#' \code{radian} must be provided.
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
#' @param target_posit Numeric matrix with two columns \code{c(x, y)}, or
#'   \code{NULL} if \code{radian} is specified.
#' @param speed Numeric scalar (or vector of length equal to the
#'   number of agents).
#' @param space_max Numeric vector \code{c(x_max, y_max)}. Required for all
#'   bounded movement types.
#' @param movement_type Character scalar specifying boundary type.
#'   One of \code{"unlimit"}, \code{"torus"}, \code{"torus_netlogo"},
#'   \code{"reflect"}, or \code{"stop"}.
#' @param radian Numeric scalar or vector of radians specifying movement
#'   direction. Used when \code{target_posit = NULL}.
#'
#' @return A numeric matrix with two columns \code{c(x, y)}.
#'
#' @examples
#' agent  <- rbind(c(0, 0), c(2, 2))
#' target <- rbind(c(3, 4), c(5, 6))
#'
#' # Unbounded, target_posit
#' move_coord(agent, target, movement_speed = 1, movement_type = "unlimit")
#'
#' # Unbounded, radian
#' move_coord(agent, target_posit = NULL, speed = 1,
#'            movement_type = "unlimit", radian = pi / 4)
#'
#' # Torus, target_posit
#' move_coord(agent, target, speed = 1,
#'            movement_type = "torus",
#'            space_max = c(10, 10))
#'
#' # Torus, radian
#' act_move_coord(agent, target_posit = NULL, speed = 1,
#'                movement_type = "torus",
#'                space_max = c(10, 10), radian = pi / 4)
#'
#' # NetLogo-like torus
#' move_coord(agent, target, speed = 1,
#'            movement_type = "torus_netlogo",
#'            space_max = c(10, 10))
#'
#' # Reflection
#' move_coord(agent, target, speed = 1,
#'            movement_type = "reflect",
#'            space_max = c(10, 10))
#'
#' # Stop at boundary
#' move_coord(agent, target, speed = 1,
#'            movement_type = "stop",
#'            space_max = c(10, 10))
#'
#' @name move_coord_family
NULL


#' @rdname move_coord_family
#' @export

agent_posit <- c(1,1)
move_coord <- function(agent_posit,
                       target_posit = NULL,
                       speed = 0.1,
                       radian = NULL,
                       space_max = NULL,
                       movement_type = c("unlimit", "torus", "torus_netlogo",
                                         "reflect", "stop")) {
  movement_type <- match.arg(movement_type)


  # resque a vector
  if(!is.matrix(agent_posit)){
    if(length(agent_posit)==2 && !is.list(agent_posit)){
      agent_posit <- matrix(agent_posit, ncol = 2)
    }else{
      stop("'agent_posit' must be a matrix with two columns.")
    }
  }

  if(!is.null(target_posit) && !is.matrix(target_posit)){
    if(length(target_posit)==2 && !is.list(target_posit)){
      target_posit <- matrix(target_posit, ncol = 2)
    }else{
      stop("'target_posit' must be a matrix with two columns.")
    }
  }


  if (movement_type != "unlimit") {
    if (is.null(space_max) || length(space_max) < 2L)
      stop("`space_max` must be length 2.")
    if (any(space_max < 0))
      stop("`space_max` must be non-negative.")
  }

  switch(
    movement_type,
    "unlimit" = move_coord_unlimit(agent_posit = agent_posit, target_posit = target_posit, speed = speed, radian = radian),
    "torus"   = move_coord_torus(agent_posit = agent_posit, target_posit = target_posit, speed = speed, space_max = space_max, radian = radian),
    "torus_netlogo" = move_coord_torus_netlogo(agent_posit = agent_posit, target_posit = target_posit, speed = speed, space_max = space_max, radian = radian),
    "reflect" = move_coord_reflect(agent_posit = agent_posit, target_posit = target_posit, speed = speed, space_max = space_max, radian = radian),
    "stop"    = move_coord_stop(agent_posit = agent_posit, target_posit = target_posit, speed = speed, space_max = space_max, radaian = radian)
  )
}


#' @rdname move_coord_family
#' @export
move_coord_unlimit <- function(agent_posit, target_posit = NULL, speed = 0.1, radian = NULL) {
  x <- agent_posit[, 1]
  y <- agent_posit[, 2]

  # radian
  if(is.null(target_posit)){
    x <- x + cos(radian) * speed
    y <- y + sin(radian) * speed
  }else{
    # normal target position
    dx <- target_posit[, 1] - agent_posit[, 1]
    dy <- target_posit[, 2] - agent_posit[, 2]
    L  <- sqrt(dx*dx + dy*dy)

    ok <- (L > 0)

    if (any(ok)) {
      invL <- 1 / L[ok]
      step <- pmin(speed, L[ok])
      x[ok] <- x[ok] + dx[ok] * invL * step
      y[ok] <- y[ok] + dy[ok] * invL * step
    }
  }

  cbind(x, y)
}


#' @rdname move_coord_family
#' @export
move_coord_torus <- function(agent_posit, target_posit = NULL, speed = 0.1, space_max, radian = NULL) {
  x <- agent_posit[, 1]
  y <- agent_posit[, 2]

  if (is.null(target_posit)) {
    # radian
    x <- x + cos(radian) * speed
    y <- y + sin(radian) * speed
  } else {
    # normal target position
    dx <- target_posit[, 1] - agent_posit[, 1]
    dy <- target_posit[, 2] - agent_posit[, 2]
    L  <- sqrt(dx*dx + dy*dy)

    ok <- (L > 0)

    if (any(ok)) {
      invL <- 1 / L[ok]
      step <- pmin(speed, L[ok])
      x[ok] <- x[ok] + dx[ok] * invL * step
      y[ok] <- y[ok] + dy[ok] * invL * step
    }
  }

  x <- x %% space_max[1]
  y <- y %% space_max[2]

  cbind(x, y)
}


#' @rdname move_coord_family
#' @export
move_coord_torus_netlogo <- function(agent_posit, target_posit = NULL, speed = 0.1, space_max, radian = NULL) {
  x_max <- space_max[1]
  y_max <- space_max[2]

  x <- agent_posit[, 1]
  y <- agent_posit[, 2]

  if (is.null(target_posit)) {
    # radian
    x <- x + cos(radian) * speed
    y <- y + sin(radian) * speed
  } else {
    # normal target position
    dx <- (target_posit[,1] - agent_posit[,1] + x_max/2) %% x_max - x_max/2
    dy <- (target_posit[,2] - agent_posit[,2] + y_max/2) %% y_max - y_max/2

    L <- sqrt(dx*dx + dy*dy)
    ok <- (L > 0)

    if (any(ok)) {
      invL <- 1 / L[ok]
      step <- pmin(speed, L[ok])
      x[ok] <- x[ok] + dx[ok] * invL * step
      y[ok] <- y[ok] + dy[ok] * invL * step
    }
  }

  x <- x %% x_max
  y <- y %% y_max

  cbind(x, y)
}


#' @rdname move_coord_family
#' @export
move_coord_reflect <- function(agent_posit, target_posit = NULL, speed = 0.1, space_max, radian = NULL) {
  x <- agent_posit[, 1]
  y <- agent_posit[, 2]

  if (is.null(target_posit)) {
    # radian
    x <- x + cos(radian) * speed
    y <- y + sin(radian) * speed
  } else {
    # normal target position
    dx <- target_posit[, 1] - agent_posit[, 1]
    dy <- target_posit[, 2] - agent_posit[, 2]
    L  <- sqrt(dx*dx + dy*dy)

    ok <- (L > 0)

    if (any(ok)) {
      invL <- 1 / L[ok]
      step <- pmin(speed, L[ok])
      x[ok] <- x[ok] + dx[ok] * invL * step
      y[ok] <- y[ok] + dy[ok] * invL * step
    }
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

#' @rdname move_coord_family
#' @export
move_coord_stop <- function(agent_posit, target_posit = NULL, speed = 0.1, space_max, radian = NULL) {
  x <- agent_posit[, 1]
  y <- agent_posit[, 2]

  x_max <- space_max[1]
  y_max <- space_max[2]

  if (is.null(target_posit)) {
    # radian
    ux <- cos(radian)
    uy <- sin(radian)
    x1 <- x; y1 <- y

    t <- rep(speed, length(ux))

    tx <- rep(Inf, length(ux))
    pos <- (ux > 0); neg <- (ux < 0)
    if (any(pos)) tx[pos] <- (x_max - x1[pos]) / ux[pos]
    if (any(neg)) tx[neg] <- (0 - x1[neg]) / ux[neg]

    ty <- rep(Inf, length(uy))
    pos <- (uy > 0); neg <- (uy < 0)
    if (any(pos)) ty[pos] <- (y_max - y1[pos]) / uy[pos]
    if (any(neg)) ty[neg] <- (0 - y1[neg]) / uy[neg]

    t <- pmin(t, pmin(tx, ty))

    x <- x1 + t * ux
    y <- y1 + t * uy

  } else {
    # normal target position
    dx <- target_posit[, 1] - agent_posit[, 1]
    dy <- target_posit[, 2] - agent_posit[, 2]
    L  <- sqrt(dx*dx + dy*dy)

    ok <- (L > 0)
    if (!any(ok)) return(cbind(x, y))

    ux <- dx[ok] / L[ok]
    uy <- dy[ok] / L[ok]

    x1 <- x[ok]; y1 <- y[ok]

    t <- rep(speed, length(ux))

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
  }

  cbind(x, y)
}
