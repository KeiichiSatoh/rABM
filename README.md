
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rABM: A flexible and extensible agent-based modeling package for exploratory social science research

<!-- badges: start -->

<!-- badges: end -->

**rABM** is an R package for agent-based modeling (ABM), specifically
tailored to social science applications. It offers a flexible and
extensible framework that allows users to build a variety of ABMs using
any type of R object. Users can easily experiment with different
parameters or rules without rewriting the entire model.

Currently, rABM is still a private test version and may have several
issues to be improved. Therefore, **please do not use for the actual
research!**

**Visit Tutorial Page**: <https://keiichisatoh.github.io/rABM/>

## Installation

You can install the development version of rABM from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("KeiichiSatoh/rABM")
```

## Example

The basic procedure of implementing simulation with rABM is following
the three steps: 1) Create an object about the agents’ state object and
the functions to update it. 2) Put all the objects into the ‘Game’ class
object. 3) Run the simulations with ‘run_Game’ function.

The following code shows the simplest example of this procedure.

    #> [plan] 
    #> get_older
    #>  
    #> [stop_FUN] 
    #> stop times at 4
    #> 
    #> The initial values at time 1 were saved.
    #> 
    #> Ready to run......
    #>    start time  : 1
    #>    current time: 2
    #>    current time: 3
    #>    current time: 4
    #> Finished at time 4 
    #> 
    #> Simulation took 00:00:00.009 (hh:mm:ss.mmm)

To see the content of the Game object, just print it.

    #> <Game>
    #> $age(state)
    #> A1 A2 A3 
    #>  4  5  6 
    #> 
    #> $get_older(act_FUN)
    #> function () 
    #> {
    #>     self$age <- self$age + 1
    #> }
    #> 
    #> ------------------- 
    #> time         : 4 
    #> n of logs    : 4 
    #> n of notes   : 2 
    #> n of fields  : 2 
    #>  state       : age
    #>  act_FUN     : get_older
    #> -------------------

Other than ‘State’ and ‘Act’ field, there are ‘Active’ (i.e., active
bindings), ‘Stop’ (defining the condition to stop the simulation),
‘Plot’ (plot function), and ‘Summary’ (summary function) fields to be
used.

The package also contains several predefined models, with which users
can play with. (still updating now…)

``` r
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
