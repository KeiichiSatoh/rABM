
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
# install.packages("pak")
pak::pak("KeiichiSatoh/rABM")
```
## Example

The basic procedure of implementing simulation with rABM is following the three steps:
1) Create an object about the agents' state object and the functions to update it.
2) Put all the objects into the 'Game' class object.
3) Run the simulations with 'run_Game' function. 

The following code shows the simplest example of this procedure.
```
# read package
library(rABM)

# Step 1: define the agent's state and functions to update it.
age <- c(A1 = 1, A2 = 2, A3 = 3)   # the age agent A1, A2, and A3
get_older <- function(){self$age <- self$age + 1} # functions to update

# Step 2: put them into Game object with defining the each fields with State(...) and Act(...)
G_init <- Game(State(age), Act(get_older))

# Step 3: run the simulation
G_finished <- run_Game(G = G_init,         # the initial Game object 
                       plan = "get_older",  # the name of the function to update 
                       times = 3)          # how many times the update should be called.
```

To see the content of the Game object, just print it.

```
G_finished
```

Other than 'State' and 'Act' field, there are 'Active'
 (i.e., active bindings), 'Stop' (defining the condition to stop the simulation), 'Plot' (plot function), and 'Summary' (summary function) fields to be used. 
 
The package also contains several predefined models, with which users can play with. (still updating now...)
```
# Let's use a famous toy model of NetLogo "heros and cowards": 
G_hero <- model_hero_coward(n = 30,
                            sim_time = 30)         # just run 30 times here

# G_finished is the Game object that has already simulated
G <- G_hero$G_finished

# Examine the current position of each agents
G$posit

# Or plot the current position
G$plot_space()

# The previous states are saved in 'log' field
G$log

# if you have "gifski" package, you can also create a gif-movie easily
library(gifski)
animate_log(G = G, name = "plot_space", delay = 0.2)
```
