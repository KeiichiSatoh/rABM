% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_agents_by_idx.R
\name{get_agents_by_idx}
\alias{get_agents_by_idx}
\title{Select Agents by Index}
\usage{
get_agents_by_idx(
  G = NULL,
  G_agents_name = NULL,
  idx,
  agents = NULL,
  deep = TRUE
)
}
\arguments{
\item{G}{An object of class \code{ABM_G}. If supplied, agents will be extracted
from its field specified by \code{G_agents_name}.}

\item{G_agents_name}{A string giving the name of the field in \code{G} that stores
the agents (e.g., "agents"). Required if \code{G} is provided.}

\item{idx}{An integer vector of indices specifying which agents to extract.}

\item{agents}{A list of \code{ABM_Agent} objects. Used if \code{G} is not supplied.}

\item{deep}{Logical. If \code{TRUE} (default), a deep clone of each agent is returned;
if \code{FALSE}, a shallow copy is returned.}
}
\value{
A list of agents (either shallow or deep cloned).
}
\description{
This function extracts agents from an \strong{\code{ABM_G}} object's agent
list or from a standalone list of \strong{\code{ABM_Agent}} objects.

It is functionally equivalent to the \verb{\%ai\%} (shallow copy) and
\verb{\%aidp\%} (deep copy) operators. The main difference is that this function
is safer due to its validation checks and uses deep copy by default, while the
operators are slightly faster due to their lightweight design.

You can achieve similar behavior to the \verb{\%ai\%<-} replacement operator
by using \code{modify_agents()} with \code{method = "replace_agent"}.
}
\examples{
# Create and extract agents
agents <- init_agents(n = 3, attr_df = data.frame(x = 1:3))
get_agents_by_idx(agents = agents, idx = c(1, 3))

# Use within ABM_G object
G <- setABM(agents = agents)
get_agents_by_idx(G = G, G_agents_name = "agents", idx = c(1, 2))

}
\seealso{
\link{\%ai\%}, \link{\%aidp\%}, \code{\link[=modify_agents]{modify_agents()}}
}
