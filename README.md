
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rABM:A flexible and extensible agent-based modeling package for exploratory social science research

<!-- badges: start -->
<!-- badges: end -->

rABM is an R package for agent-based modeling (ABM), specifically tailored to social science applications. It offers a flexible and extensible framework that allows users to build a variety of ABMs using any type of R object. The framework supports interactions among multiple agent types governed by diverse behavioral rules. Moreover, users can easily experiment with different parameters or rules without rewriting the entire model.

Currently, rABM is still a private test version and may have several issues to be improved. Therefore, **please do not use for the actual research!.**

## Installation

You can install the development version of rABM from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
library(remotes)
remotes::install_github("KeiichiSatoh/rABM")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# read package
library(rABM)

# setup agents
agent_attr <- data.frame(age = c(0, 1, 2))
get_older <- function() { self$age <- self$age + 1 }
agents <- init_agent(attr_df = agent_attr, act_FUN = get_older)

# Initialize the ABM environment
G <- setABM(agents = agents)

# Run simulation for 5 steps
result <- runABM(G = G, schedule = "get_older", times = 5)

# Check each agent's age
result$agents
```
