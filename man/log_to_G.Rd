% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/log_to_G.R
\name{log_to_G}
\alias{log_to_G}
\title{Restore ABM_G object from log}
\usage{
log_to_G(G, which_time)
}
\arguments{
\item{G}{An \code{ABM_G} object that contains a \code{log} field.}

\item{which_time}{Integer. The time point (as stored in \code{G$log[[t]]$time}) to restore.}
}
\value{
A reconstructed \code{ABM_G} object representing the state at the specified simulation time.
The returned object includes reconstructed agents, attributes, methods, and global functions,
as well as stage variables.
}
\description{
Reconstructs an \code{ABM_G} object at a specific simulation time
by retrieving the state stored in the \code{G$log} field.
This is useful for debugging, inspection, or replaying past simulation states.
}
\details{
This function assumes that only agent attributes and stage fields are stored in the log,
while act_FUNs and active bindings are retained from the current environment.

If some agent IDs present in the log are no longer found in the current \code{G} object,
a warning is issued and the act_FUNs of the first available agent are used as a fallback.
}
\examples{
act1 <- function(){ self$x <- self$x + 1 }
act2 <- function(){ self$y <- self$y + 1 }
ab   <- function(){ self$x^2 }
g1   <- function(){ print(1) }

agents <- init_agents(
  n = 3,
  attr_df = data.frame(x = 1:3, y = 4:6),
  act_FUN = list(act1 = act1, act2 = act2),
  active_binding = ab
)

G <- setABM(
  agents = list(agents1 = agents, agents2 = agents),
  stage = matrix(1:9, 3, 3),
  global_FUN = g1
)

G <- runABM(G = G, plan = c("act1", "act2", "g1"), times = 3)

# Restore G at time = 2
G2 <- log_to_G(G = G, which_time = 2)

}
