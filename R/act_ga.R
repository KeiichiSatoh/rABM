#' Genetic algorithm update
#'
#' Perform one generation update of a genetic algorithm population.
#'
#' Each row of `parents` represents one individual, and each column
#' represents one DNA element. The function supports binary, integer,
#' and numeric search spaces through `ga_type`.
#'
#' The update consists of:
#' \enumerate{
#'   \item elite preservation,
#'   \item fitness-based parent selection,
#'   \item crossover,
#'   \item mutation.
#' }
#'
#' @param parents A matrix or data.frame in which each row represents one
#'   parent individual and each column represents one DNA element.
#' @param fit_score A numeric vector of fitness scores for each parent.
#'   Larger values indicate better fitness.
#' @param ga_type A character string specifying the genetic representation.
#'   Must be one of `"binary"`, `"integer"`, or `"numeric"`.
#' @param n_elite A single non-negative integer giving the number of elite
#'   parents preserved unchanged in the next generation.
#' @param prob_crossover A single numeric value between 0 and 1 giving the
#'   probability of crossover for each child.
#' @param prob_mutation A single numeric value between 0 and 1 giving the
#'   mutation probability for each DNA element.
#' @param values_min Optional lower bounds used during mutation for
#'   `"integer"` and `"numeric"` types. Must be `NULL`, length 1, or
#'   length `ncol(parents)`. Ignored when `ga_type = "binary"`.
#' @param values_max Optional upper bounds used during mutation for
#'   `"integer"` and `"numeric"` types. Must be `NULL`, length 1, or
#'   length `ncol(parents)`. Ignored when `ga_type = "binary"`.
#' @param type_selection_prob Method used to convert fitness scores into
#'   parent-selection probabilities. Currently only `"fit_prop"` is supported.
#' @param type_crossover Type of crossover. Allowed values depend on `ga_type`:
#'   \describe{
#'     \item{`"binary"`}{`"one"`}
#'     \item{`"integer"`}{`"one"`}
#'     \item{`"numeric"`}{`"one"` or `"prop"`}
#'   }
#'   If `NULL`, a default is chosen automatically.
#' @param type_mutation Type of mutation. Allowed values depend on `ga_type`:
#'   \describe{
#'     \item{`"binary"`}{ignored}
#'     \item{`"integer"`}{`"uniform"` or `"walk"`}
#'     \item{`"numeric"`}{`"walk"` or `"uniform"`}
#'   }
#'   If `NULL`, a default is chosen automatically for non-binary types.
#' @param parent_info Logical. If `TRUE`, return a list containing both the
#'   children matrix and a two-column matrix indicating the parent indices used
#'   to generate each row.
#' @param ... Reserved for future extensions.
#'
#' @return
#' If `parent_info = FALSE`, a matrix representing the next generation.
#'
#' If `parent_info = TRUE`, a list with the following elements:
#' \describe{
#'   \item{children}{A matrix representing the next generation.}
#'   \item{parent_info}{A two-column integer matrix with columns `parent1`
#'   and `parent2`, indicating the parent indices used to generate each row
#'   of `children`. For elite rows, the same parent index is repeated twice.}
#' }
#'
#' @details
#' `act_ga()` performs a single-generation GA update rather than a full
#' optimization run. It is intended for situations where users want to
#' control the update cycle explicitly.
#'
#' The interpretation of crossover and mutation depends on `ga_type`:
#' \describe{
#'   \item{Binary}{One-point crossover and bit-flip mutation.}
#'   \item{Integer}{One-point crossover and either integer uniform-replacement
#'   mutation or integer random-walk mutation.}
#'   \item{Numeric}{One-point or proportional crossover, and either numeric
#'   random-walk mutation or numeric uniform-replacement mutation.}
#' }
#'
#' When `ga_type = "binary"`, `values_min`, `values_max`, and
#' `type_mutation` are ignored.
#'
#' @seealso
#' \code{\link[GA]{ga}} in the \pkg{GA} package for a full genetic algorithm
#' implementation.
#'
#' @examples
#' set.seed(1)
#'
#' #--------------------------------
#' # binary example
#' #--------------------------------
#' parents_bin <- matrix(sample(0:1, size = 20, replace = TRUE), nrow = 5)
#' fit_bin <- c(3.2, 1.5, 4.8, 2.0, 3.9)
#'
#' act_ga(
#'   parents = parents_bin,
#'   fit_score = fit_bin,
#'   ga_type = "binary"
#' )
#'
#' #--------------------------------
#' # integer example
#' #--------------------------------
#' parents_int <- matrix(sample(1:5, size = 24, replace = TRUE), nrow = 6)
#' fit_int <- c(2.1, 5.4, 1.3, 4.0, 3.7, 2.8)
#'
#' act_ga(
#'   parents = parents_int,
#'   fit_score = fit_int,
#'   ga_type = "integer",
#'   type_mutation = "walk",
#'   values_min = 1,
#'   values_max = 5
#' )
#'
#' #--------------------------------
#' # numeric example
#' #--------------------------------
#' parents_num <- matrix(stats::runif(30), nrow = 6)
#' fit_num <- c(1.1, 2.3, 4.8, 3.5, 2.7, 5.0)
#'
#' act_ga(
#'   parents = parents_num,
#'   fit_score = fit_num,
#'   ga_type = "numeric",
#'   type_crossover = "prop",
#'   type_mutation = "walk",
#'   values_min = 0,
#'   values_max = 1
#' )
#'
#' #--------------------------------
#' # return parent information
#' #--------------------------------
#' res <- act_ga(
#'   parents = parents_bin,
#'   fit_score = fit_bin,
#'   ga_type = "binary",
#'   parent_info = TRUE
#' )
#'
#' str(res)
#'
#' @export
act_ga <- function(parents,
                   fit_score,
                   ga_type = c("binary", "integer", "numeric"),
                   n_elite = 2,
                   prob_crossover = 0.8,
                   prob_mutation = 0.1,
                   values_min = NULL,
                   values_max = NULL,
                   type_selection_prob = c("fit_prop"),
                   type_crossover = NULL,
                   type_mutation = NULL,
                   parent_info = FALSE,
                   ...) {
  ga_type <- match.arg(ga_type)
  type_selection_prob <- match.arg(type_selection_prob)

  spec <- .ga_spec(ga_type)

  if (is.null(type_crossover)) {
    type_crossover <- spec$default_crossover
  }

  if (is.null(type_mutation)) {
    type_mutation <- spec$default_mutation
  }

  if (!(type_crossover %in% spec$allowed_crossover)) {
    stop(sprintf(
      "'type_crossover' must be one of %s when ga_type = '%s'.",
      paste(sprintf("'%s'", spec$allowed_crossover), collapse = ", "),
      ga_type
    ))
  }

  if (ga_type == "binary") {
    if (!is.null(type_mutation)) {
      warning("'type_mutation' is ignored when ga_type = 'binary'.")
    }
    type_mutation <- NULL
    values_min <- NULL
    values_max <- NULL
  } else {
    if (is.null(type_mutation) || !(type_mutation %in% spec$allowed_mutation)) {
      stop(sprintf(
        "'type_mutation' must be one of %s when ga_type = '%s'.",
        paste(sprintf("'%s'", spec$allowed_mutation), collapse = ", "),
        ga_type
      ))
    }
  }

  .ga_validate_input(
    parents = parents,
    fit_score = fit_score,
    n_elite = n_elite,
    prob_crossover = prob_crossover,
    prob_mutation = prob_mutation
  )

  parents <- .ga_normalize_parents(parents)
  spec$validate_parents(parents)

  n_population <- nrow(parents)
  n_children   <- n_population - n_elite
  n_dna        <- ncol(parents)

  values_min <- .ga_recycle_bounds(values_min, n_dna, "values_min")
  values_max <- .ga_recycle_bounds(values_max, n_dna, "values_max")

  sel <- .ga_select(
    parents = parents,
    fit_score = fit_score,
    n_elite = n_elite,
    n_children = n_children,
    type_selection_prob = type_selection_prob
  )

  cross <- .ga_crossover(
    parents = parents,
    selected_pairs = sel$selected_pairs,
    type_crossover = type_crossover,
    prob_crossover = prob_crossover
  )

  children <- .ga_mutate(
    children = cross$children,
    ga_type = ga_type,
    type_mutation = type_mutation,
    prob_mutation = prob_mutation,
    values_min = values_min,
    values_max = values_max
  )

  children_comb <- rbind(sel$elite_parents, children)
  rownames(children_comb) <- NULL

  if (!isTRUE(parent_info)) {
    return(children_comb)
  }

  list(
    children = children_comb,
    parent_info = .ga_make_parent_info(
      elite_idx = sel$elite_idx,
      selected_pairs = sel$selected_pairs
    )
  )
}
