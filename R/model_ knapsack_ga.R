#' Solve the Knapsack Problem Using a Genetic Algorithm
#'
#' @description
#' Solves the integer knapsack problem using a genetic algorithm (GA).
#' The function searches for the combination of item quantities that maximises
#' the total value subject to a weight constraint.
#'
#' @param goods_names A \code{character} vector of item names.
#'   Default: \code{c("A", "B", "C", "D", "E")}.
#' @param goods_value A \code{numeric} vector of values for each item.
#'   Must be the same length as \code{goods_names}.
#'   Default: \code{c(10, 5, 7, 2, 1)}.
#' @param goods_weight A \code{numeric} vector of weights for each item.
#'   Must be the same length as \code{goods_names}.
#'   Default: \code{c(12, 1, 4, 2, 1)}.
#' @param weight_limit A positive \code{numeric} scalar specifying the
#'   maximum total weight the knapsack can carry.
#'   Default: \code{50}.
#' @param n_goods_min An \code{integer} vector specifying the minimum quantity
#'   allowed for each item (must be \eqn{\geq 0}).
#'   Must be the same length as \code{goods_names} and element-wise
#'   \eqn{\leq} \code{n_goods_max}.
#'   Default: \code{c(0, 0, 0, 0, 0)}.
#' @param n_goods_max An \code{integer} vector specifying the maximum quantity
#'   allowed for each item.
#'   Must be the same length as \code{goods_names}.
#'   Default: \code{c(10, 10, 10, 10, 10)}.
#' @param sim_times A positive \code{integer} specifying the number of
#'   generations (iterations) to run the GA.
#'   Default: \code{100}.
#' @param n_solutions A positive \code{integer} \eqn{\geq 2} specifying the
#'   number of candidate solutions (individuals) per generation.
#'   Default: \code{10}.
#' @param n_elite A non-negative \code{integer} specifying the number of
#'   elite solutions carried over to the next generation without modification.
#'   Must be strictly less than \code{n_solutions}.
#'   Set to \code{0} to disable elitism.
#'   Default: \code{2}.
#' @param prob_crossover A \code{numeric} scalar in \eqn{[0, 1]} specifying
#'   the crossover probability.
#'   Default: \code{0.8}.
#' @param prob_mutation A \code{numeric} scalar in \eqn{[0, 1]} specifying
#'   the mutation probability.
#'   Default: \code{0.1}.
#' @param type_mutation A \code{character} string specifying the type of
#'   mutation operator passed to \code{act_ga()}.
#'   Default: \code{"uniform"}.
#' @param seed An \code{integer} scalar or \code{NULL}. Random seed for
#'   reproducibility. If \code{NULL}, no seed is set.
#'   Default: \code{NULL}.
#'
#' @return A named \code{list} with two elements:
#' \describe{
#'   \item{\code{G_init}}{A \code{Game} object containing the initial state
#'     of the GA before any generations have been evaluated.}
#'   \item{\code{G_finished}}{A \code{Game} object containing the final state
#'     of the GA after \code{sim_times} generations.}
#' }
#'
#' @details
#' ## Algorithm
#' An integer-type GA is used to solve the knapsack problem. Each solution is
#' represented as a vector of item quantities. The fitness score
#' (\code{fit_values}) equals the total value of a solution if its total
#' weight is within \code{weight_limit}, and \code{0} otherwise.
#' Selection, one-point crossover, and mutation are applied at each generation
#' via \code{\link{act_ga}}.
#'
#' ## Fields of the \code{Game} object
#'
#' **State fields**
#' \describe{
#'   \item{\code{solutions}}{An integer matrix of dimensions
#'     \code{n_solutions} \eqn{\times} \code{length(goods_names)}.
#'     Each row is one candidate solution.}
#'   \item{\code{settings}}{A list storing all GA parameters and item
#'     attributes passed to the function.}
#' }
#'
#' **Active fields** (computed on access)
#' \describe{
#'   \item{\code{total_weights}}{Numeric vector of total weights for each
#'     solution in the current generation.}
#'   \item{\code{total_values}}{Numeric vector of total values for each
#'     solution in the current generation.}
#'   \item{\code{fit_values}}{Numeric vector of fitness scores. Solutions
#'     exceeding \code{weight_limit} receive a score of \code{0}.}
#' }
#'
#' **Plot fields**
#' \describe{
#'   \item{\code{plot_solutions()}}{Plots the fitness score of every solution
#'     in the current generation.}
#'   \item{\code{plot_best_solution()}}{Plots the item quantities of the
#'     best solution in the current generation. If multiple solutions share
#'     the highest fitness score, one is selected at random and a warning is
#'     issued.}
#' }
#'
#' **Report fields**
#' \describe{
#'   \item{\code{report_trace_solution()}}{Plots the mean, standard deviation,
#'     and maximum fitness score across generations using the saved log.
#'     Returns \code{NULL} with a warning if no log has been saved.}
#'   \item{\code{report_best_solution()}}{Prints the best solution(s) found
#'     in the current generation, together with their total values, total
#'     weights, and the search conditions.}
#'   \item{\code{report_solutions(exclude_duplicated_solution = TRUE)}}{Prints
#'     all solutions in the current generation, sorted by fitness score in
#'     descending order. Duplicate solutions are removed by default.}
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with default settings
#' result <- model_napsack_ga(seed = 42)
#'
#' # Inspect the best solution after the run
#' result$G_finished$report_best_solution()
#'
#' # Visualise the fitness trajectory across generations
#' result$G_finished$report_trace_solution()
#'
#' # Custom items and constraints
#' result2 <- model_napsack_ga(
#'   goods_names  = c("X", "Y", "Z"),
#'   goods_value  = c(5, 8, 3),
#'   goods_weight = c(3, 6, 2),
#'   weight_limit = 10,
#'   n_goods_min  = c(0, 0, 0),
#'   n_goods_max  = c(5, 5, 5),
#'   sim_times    = 200,
#'   n_solutions  = 20,
#'   n_elite      = 3,
#'   seed         = 123
#' )
#'
#' # Plot the best solution
#' result2$G_finished$plot_best_solution()
#'
#' # Print all solutions at the final generation
#' result2$G_finished$report_solutions()
#' }
#'
#' @seealso
#' \code{\link{act_ga}} for the underlying GA operators
#' (selection, crossover, and mutation).
#'
#' @importFrom stats sd
#' @importFrom graphics axis par plot text
#'
#' @export
model_knapsack_ga <- function(
    goods_names = c("A","B","C","D","E"),
    goods_value = c(10, 5, 7, 2, 1),
    goods_weight = c(12, 1, 4, 2, 1),
    weight_limit = 50,
    n_goods_min = c(0,0,0,0,0),
    n_goods_max = c(10,10,10,10,10),
    sim_times = 100,
    n_solutions = 10,
    n_elite = 2,
    prob_crossover = 0.8,
    prob_mutation = 0.1,
    type_mutation = "uniform",
    seed = NULL
  ){
# 関数内部
#===== seed ===============
if(!is.null(seed)){
  set.seed(seed = seed)
}

#===== Validation =========
  stopifnot("Length of 'goods_value' and `goods_names` must be the same." = length(goods_value)  == length(goods_names))
  stopifnot("Length of 'goods_weight' and `goods_names` must be the same." = length(goods_weight)  == length(goods_names))
  stopifnot("Length of 'n_goods_min' and `goods_names` must be the same." = length(n_goods_min)  == length(goods_names))
  stopifnot("Length of 'n_goods_max' and `goods_names` must be the same." = length(n_goods_max)  == length(goods_names))
  stopifnot("'n_goods_min' must be smaller than 'n_good_max'." = all(n_goods_min <= n_goods_max))
  stopifnot("'n_goods_min' must be equal to or greater than 0."  = all(n_goods_min >= 0))
  stopifnot("'weight_limit' must be greater than 0."  = weight_limit > 0)
  stopifnot("'n_solutions' must be equal to or greater than 2."  = n_solutions >= 2)
  stopifnot("'n_elite' must be smaller than n_solutions."  = n_elite < n_solutions)
  stopifnot("'n_elite' must be equal to or greater than 0."  = n_elite >= 0)

#===== State =========
# create a vacant solution box
solutions <- matrix(0, n_solutions, length(goods_weight),
                    dimnames = list(paste0("S", seq_len(n_solutions)),
                                    goods_names))

# put a random solution
for(j in seq_len(length(goods_names))){
  solutions[ ,j] <- sample(n_goods_min[j]:n_goods_max[j],
         size = n_solutions, replace = TRUE)
}

settings <- list(goods_names = goods_names,
                 goods_weight = goods_weight,
                 goods_value = goods_value,
                 n_goods_min = n_goods_min,
                 n_goods_max = n_goods_max,
                 weight_limit = weight_limit,
                 n_elite = n_elite,
                 prob_crossover = prob_crossover,
                 prob_mutation = prob_mutation,
                 type_mutation = type_mutation)

G <- Game(State(solutions),
          State(settings))

#==== Active State =====
total_weights <- function(){
  vec <- as.vector(self$solutions %*% self$settings$goods_weight)
  vec
}

total_values <- function(){
  vec <- as.vector(self$solutions %*% self$settings$goods_value)
  vec
}

fit_values <- function(){
  val <- self$total_values
  val[self$total_weights > self$settings$weight_limit] <- 0
  val
}

add_field(G, Active(total_weights), Active(total_values), Active(fit_values))

#==== plot_FUN =========
plot_solutions <- function(){
  plot(x = seq_along(self$fit_values), y = self$fit_values,
       xaxt = "n", ylim = c(0, max(self$fit_values, 1)),
       xlab = "solutions", ylab = "fit values",
       main = paste("time = ", self$time))
  axis(1,
       at = seq_along(self$fit_values),
       labels = 1:nrow(self$solutions))
}

plot_best_solution <- function(){
  fit_values <- self$fit_values

  best_idx <- which(max(fit_values) == fit_values)
  if(length(best_idx) > 1){
    warning("Multiple best solutions found. The result shows only one of them.")
    best_idx <- sample(best_idx, 1)
  }

  best_sol <- self$solutions[best_idx, ]

  ymax <- max(self$settings$n_goods_max)
  plot(seq_along(best_sol), best_sol,
       xaxt = "n", ylim = c(0, ymax),
       xlab = "", ylab = "number of each good",
       pch = 19,
       main = paste("time =", self$time),
       sub = paste("Solution index =", best_idx, "; ",
                   "fit =", round(fit_values[best_idx], 3)))
  axis(1,
       at = seq_along(best_sol),
       labels = names(best_sol))
}

add_field(G, Plot(plot_solutions), Plot(plot_best_solution))

#==== act_FUN ==========
update_solution <- function(){
  self$solutions <- act_ga(parents = self$solutions,
                           fit_score = self$fit_values,
                           ga_type = "integer",
                           n_elite = self$settings$n_elite,
                           prob_crossover = self$settings$prob_crossover,
                           prob_mutation = self$settings$prob_mutation,
                           values_min = self$settings$n_goods_min,
                           values_max = self$settings$n_goods_max,
                           type_crossover = "one",
                           type_mutation = self$settings$type_mutation)
}

add_field(G, Act(update_solution))


#==== report_FUN ======
report_trace_solution <- function(){
  # number of logs
  n_log <- length(self$log)
  if(n_log==0){
    warning("No `log` has been saved.")
    return(NULL)
  }

  # exctract the fit_values in logs
  fit_list <- value_of(G = self, field_name = "fit_values", log = 1:n_log)

  # mean
  trace_mean  <- unlist(lapply(fit_list, mean))
  trace_sd    <- unlist(lapply(fit_list, sd))
  trace_max   <- unlist(lapply(fit_list, max))

  # plot_max
  max_val <- max(trace_mean, trace_sd, trace_max)

  plot(x = 1:n_log, y = trace_mean, type = "l",
       xlab = "time", ylab = "fit values", ylim = c(0, max_val),
       main = "Change of fit values among solutions",
       sub = "black = average; blue = SD; red = max (i.e., the best)")

  par(new=TRUE)

  plot(x = 1:n_log, y = trace_sd, type = "l", axes = FALSE, ann = FALSE,
       xlab = "", ylab = "", ylim = c(0, max_val), col = "blue")

  par(new=TRUE)

  plot(x = 1:n_log, y = trace_max, type = "l", axes = FALSE, ann = FALSE,
       xlab = "", ylab = "", ylim = c(0, max_val), col = "red")
}

report_best_solution <- function(){
  fit_max <- max(self$fit_values)
  best_idx_candid <- which(fit_max==self$fit_values)
  dup <- duplicated(self$solutions[best_idx_candid, ])
  best_idx <- best_idx_candid[!dup]
  best_solution <- self$solutions[best_idx, ]
  df_best_summary <- data.frame(
    self$total_values[best_idx],
    self$total_weights[best_idx])
  colnames(df_best_summary) <- c("total values", "total weights")
  rownames(df_best_summary) <- NULL

  # printing
  cat("[Latest best solutions]", "\n")
  print(best_solution)
  cat("\n")
  cat("[summary of the best solution]","\n")
  print(df_best_summary)
  cat("\n")
  cat("[Meta data]","\n")
  cat("time: ", self$time,"\n")
  cat("search condition: ", "\n")
  df <- data.frame(colnames(self$solutions),
             self$settings$n_goods_min,
             self$settings$n_goods_max)
  colnames(df) <- c("goods name", "min", "max")
  print(df)
}


report_solutions <- function(exclude_duplicated_solution = TRUE){
  df <- data.frame(self$solutions,
                   total_weights = self$total_weights,
                   total_values = self$total_values,
                   fit_values = self$fit_values)
  if(exclude_duplicated_solution){
    dup <- duplicated(df)
    df <- df[!dup, ]
    if(isTRUE(any(dup))) warning("Duplicated solutions were excluded from printing.")
  }

  odr <- order(df$fit_values, decreasing = TRUE)

  # print
  cat("[Solutions at time: ", self$time, "]", "\n", sep = "")
  print(df[odr, ])

  # return
  df[odr, ]
}

add_field(G,
          Report(report_trace_solution),
          Report(report_best_solution),
          Report(report_solutions))

#==== save the initial object =======
G_init <- copy_obj(G)
#==== run =========
G_finished <- run_Game(G = G, plan = "update_solution", times = sim_times, seed = seed)
#==== out =========
out <- list(G_init = G_init,
            G_finished = G_finished)
out
}

