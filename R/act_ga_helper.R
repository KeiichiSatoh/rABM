# Internal helper functions for act_ga()
# (not exported)

#===============================================================================
# GA spec
#===============================================================================

.ga_spec <- function(ga_type) {
  switch(
    ga_type,
    "binary" = list(
      default_crossover = "one",
      default_mutation = NULL,
      allowed_crossover = c("one"),
      allowed_mutation = NULL,
      validate_parents = .validate_ga_binary_parents
    ),
    "integer" = list(
      default_crossover = "one",
      default_mutation = "uniform",
      allowed_crossover = c("one"),
      allowed_mutation = c("uniform", "walk"),
      validate_parents = .validate_ga_integer_parents
    ),
    "numeric" = list(
      default_crossover = "one",
      default_mutation = "walk",
      allowed_crossover = c("one", "prop"),
      allowed_mutation = c("walk", "uniform"),
      validate_parents = .validate_ga_numeric_parents
    ),
    stop("Unknown 'ga_type'.")
  )
}

#===============================================================================
# validation / normalization
#===============================================================================

.ga_normalize_parents <- function(parents) {
  if (!(is.matrix(parents) || is.data.frame(parents))) {
    stop("'parents' must be a matrix or data.frame.")
  }

  out <- as.matrix(parents)
  storage.mode(out) <- "numeric"
  out
}

.ga_validate_input <- function(parents,
                               fit_score,
                               n_elite,
                               prob_crossover,
                               prob_mutation) {
  stopifnot(
    "'parents' must be a matrix or data.frame." =
      is.matrix(parents) || is.data.frame(parents),

    "'parents' must have at least one row." =
      nrow(parents) >= 1L,

    "'parents' must have at least one column." =
      ncol(parents) >= 1L,

    "'fit_score' must be numeric." =
      is.numeric(fit_score),

    "length('fit_score') must equal nrow(parents)." =
      length(fit_score) == nrow(parents),

    "'fit_score' must not contain NA." =
      all(!is.na(fit_score)),

    "'n_elite' must be a single non-negative integer." =
      length(n_elite) == 1L &&
      !is.na(n_elite) &&
      n_elite >= 0 &&
      n_elite %% 1 == 0,

    "'n_elite' must not exceed the population size." =
      n_elite <= nrow(parents),

    "'prob_crossover' must be a single number between 0 and 1." =
      length(prob_crossover) == 1L &&
      !is.na(prob_crossover) &&
      prob_crossover >= 0 &&
      prob_crossover <= 1,

    "'prob_mutation' must be a single number between 0 and 1." =
      length(prob_mutation) == 1L &&
      !is.na(prob_mutation) &&
      prob_mutation >= 0 &&
      prob_mutation <= 1
  )
}

.validate_ga_binary_parents <- function(parents) {
  if (any(!(parents %in% c(0, 1)))) {
    stop("'parents' must contain only 0 and 1 when ga_type = 'binary'.")
  }
}

.validate_ga_integer_parents <- function(parents) {
  if (any(is.na(parents)) || any(parents %% 1 != 0)) {
    stop("'parents' must contain only integers when ga_type = 'integer'.")
  }
}

.validate_ga_numeric_parents <- function(parents) {
  if (!is.numeric(parents) || any(is.na(parents))) {
    stop("'parents' must be numeric and must not contain NA when ga_type = 'numeric'.")
  }
}

.ga_recycle_bounds <- function(values, n_dna, arg_name) {
  if (is.null(values)) return(NULL)

  if (length(values) == 1L) {
    return(rep(values, n_dna))
  }

  if (length(values) != n_dna) {
    stop(sprintf("'%s' must be NULL, length 1, or length ncol(parents).", arg_name))
  }

  values
}

#===============================================================================
# selection
#===============================================================================

.ga_selection_fit_prop <- function(fit_score) {
  if (any(fit_score < 0)) {
    fit_score <- fit_score - min(fit_score)
  }

  s <- sum(fit_score)

  if (s == 0) {
    return(rep(1 / length(fit_score), length(fit_score)))
  }

  fit_score / s
}

.ga_get_elite <- function(parents, fit_score, n_elite) {
  if (n_elite == 0L) {
    return(list(
      elite_parents = parents[integer(0), , drop = FALSE],
      elite_idx = integer(0)
    ))
  }

  elite_idx <- order(fit_score, decreasing = TRUE)[seq_len(n_elite)]

  list(
    elite_parents = parents[elite_idx, , drop = FALSE],
    elite_idx = elite_idx
  )
}

.ga_select_pairs <- function(n_population,
                             n_children,
                             prob_selection,
                             replace = TRUE) {
  if (n_children == 0L) {
    return(list())
  }

  lapply(seq_len(n_children), function(i) {
    sample.int(
      n = n_population,
      size = 2L,
      replace = replace,
      prob = prob_selection
    )
  })
}

.ga_select <- function(parents,
                       fit_score,
                       n_elite,
                       n_children,
                       type_selection_prob = c("fit_prop")) {
  type_selection_prob <- match.arg(type_selection_prob)

  elite <- .ga_get_elite(
    parents = parents,
    fit_score = fit_score,
    n_elite = n_elite
  )

  prob_selection <- switch(
    type_selection_prob,
    "fit_prop" = .ga_selection_fit_prop(fit_score),
    stop("Unknown 'type_selection_prob'.")
  )

  selected_pairs <- .ga_select_pairs(
    n_population = nrow(parents),
    n_children = n_children,
    prob_selection = prob_selection,
    replace = TRUE
  )

  list(
    elite_parents = elite$elite_parents,
    elite_idx = elite$elite_idx,
    prob_selection = prob_selection,
    selected_pairs = selected_pairs
  )
}

#===============================================================================
# crossover
#===============================================================================

.ga_make_child_one <- function(parent1, parent2, prob_crossover) {
  n_dna <- length(parent1)

  if (length(parent2) != n_dna) {
    stop("'parent1' and 'parent2' must have the same length.")
  }

  if (n_dna == 1L || stats::runif(1) >= prob_crossover) {
    return(if (stats::runif(1) < 0.5) parent1 else parent2)
  }

  div <- sample.int(n_dna - 1L, size = 1L)
  c(parent1[seq_len(div)], parent2[(div + 1L):n_dna])
}

.ga_make_child_prop <- function(parent1, parent2, prob_crossover) {
  n_dna <- length(parent1)

  if (length(parent2) != n_dna) {
    stop("'parent1' and 'parent2' must have the same length.")
  }

  if (stats::runif(1) >= prob_crossover) {
    return(if (stats::runif(1) < 0.5) parent1 else parent2)
  }

  prop <- stats::runif(n_dna)
  parent1 * prop + parent2 * (1 - prop)
}

.ga_apply_crossover_one <- function(parents, selected_pairs, prob_crossover) {
  n_children <- length(selected_pairs)

  if (n_children == 0L) {
    return(parents[integer(0), , drop = FALSE])
  }

  out <- t(vapply(
    X = selected_pairs,
    FUN = function(idx) {
      .ga_make_child_one(
        parent1 = parents[idx[1], ],
        parent2 = parents[idx[2], ],
        prob_crossover = prob_crossover
      )
    },
    FUN.VALUE = numeric(ncol(parents))
  ))

  rownames(out) <- NULL
  out
}

.ga_apply_crossover_prop <- function(parents, selected_pairs, prob_crossover) {
  n_children <- length(selected_pairs)

  if (n_children == 0L) {
    return(parents[integer(0), , drop = FALSE])
  }

  out <- t(vapply(
    X = selected_pairs,
    FUN = function(idx) {
      .ga_make_child_prop(
        parent1 = parents[idx[1], ],
        parent2 = parents[idx[2], ],
        prob_crossover = prob_crossover
      )
    },
    FUN.VALUE = numeric(ncol(parents))
  ))

  rownames(out) <- NULL
  out
}

.ga_crossover <- function(parents,
                          selected_pairs,
                          type_crossover,
                          prob_crossover) {
  children <- switch(
    type_crossover,
    "one" = .ga_apply_crossover_one(
      parents = parents,
      selected_pairs = selected_pairs,
      prob_crossover = prob_crossover
    ),
    "prop" = .ga_apply_crossover_prop(
      parents = parents,
      selected_pairs = selected_pairs,
      prob_crossover = prob_crossover
    ),
    stop("Unknown 'type_crossover'.")
  )

  list(
    children = children,
    selected_pairs = selected_pairs
  )
}

#===============================================================================
# mutation helpers
#===============================================================================

.ga_make_mutation_mask <- function(n_row, n_col, prob_mutation) {
  matrix(
    stats::runif(n_row * n_col) < prob_mutation,
    nrow = n_row,
    ncol = n_col
  )
}

.ga_clip_children <- function(children,
                              values_min = NULL,
                              values_max = NULL) {
  n_dna <- ncol(children)

  if (!is.null(values_min)) {
    for (j in seq_len(n_dna)) {
      children[children[, j] < values_min[j], j] <- values_min[j]
    }
  }

  if (!is.null(values_max)) {
    for (j in seq_len(n_dna)) {
      children[children[, j] > values_max[j], j] <- values_max[j]
    }
  }

  children
}

.ga_default_integer_bounds <- function(children) {
  sds <- round(matrixStats::colSds(children, na.rm = TRUE))
  sds[is.na(sds)] <- 0L

  list(
    values_min = matrixStats::colMins(children, na.rm = TRUE) - sds,
    values_max = matrixStats::colMaxs(children, na.rm = TRUE) + sds
  )
}

.ga_default_numeric_bounds <- function(children) {
  sds <- matrixStats::colSds(children, na.rm = TRUE)
  sds[is.na(sds)] <- 0

  list(
    values_min = matrixStats::colMins(children, na.rm = TRUE) - sds,
    values_max = matrixStats::colMaxs(children, na.rm = TRUE) + sds
  )
}

.ga_mutation_binary <- function(children, prob_mutation) {
  if (nrow(children) == 0L) return(children)

  do_mutate <- .ga_make_mutation_mask(
    n_row = nrow(children),
    n_col = ncol(children),
    prob_mutation = prob_mutation
  )

  children[do_mutate] <- 1 - children[do_mutate]
  children
}

.ga_mutation_integer_walk <- function(children,
                                      prob_mutation,
                                      values_min = NULL,
                                      values_max = NULL) {
  if (nrow(children) == 0L) return(children)

  do_mutate <- .ga_make_mutation_mask(
    n_row = nrow(children),
    n_col = ncol(children),
    prob_mutation = prob_mutation
  )

  idx <- which(do_mutate, arr.ind = TRUE)
  if (nrow(idx) == 0L) {
    return(children)
  }

  steps <- round(matrixStats::colSds(children, na.rm = TRUE))
  steps[is.na(steps) | steps < 1L] <- 1L

  step_each <- steps[idx[, 2]]

  delta <- vapply(
    step_each,
    FUN = function(s) sample.int(2L * s + 1L, size = 1L) - (s + 1L),
    FUN.VALUE = integer(1)
  )

  children[idx] <- children[idx] + delta

  .ga_clip_children(children, values_min, values_max)
}

.ga_mutation_integer_uniform <- function(children,
                                         prob_mutation,
                                         values_min = NULL,
                                         values_max = NULL) {
  if (nrow(children) == 0L) return(children)

  do_mutate <- .ga_make_mutation_mask(
    n_row = nrow(children),
    n_col = ncol(children),
    prob_mutation = prob_mutation
  )

  idx <- which(do_mutate, arr.ind = TRUE)
  if (nrow(idx) == 0L) {
    return(children)
  }

  if (is.null(values_min) || is.null(values_max)) {
    defaults <- .ga_default_integer_bounds(children)
    if (is.null(values_min)) values_min <- defaults$values_min
    if (is.null(values_max)) values_max <- defaults$values_max
  }

  mins <- values_min[idx[, 2]]
  maxs <- values_max[idx[, 2]]

  children[idx] <- floor(stats::runif(nrow(idx), min = mins, max = maxs + 1))

  children
}

.ga_mutation_numeric_walk <- function(children,
                                      prob_mutation,
                                      values_min = NULL,
                                      values_max = NULL) {
  if (nrow(children) == 0L) return(children)

  do_mutate <- .ga_make_mutation_mask(
    n_row = nrow(children),
    n_col = ncol(children),
    prob_mutation = prob_mutation
  )

  idx <- which(do_mutate, arr.ind = TRUE)
  if (nrow(idx) == 0L) {
    return(children)
  }

  sds <- matrixStats::colSds(children, na.rm = TRUE)
  sds[is.na(sds) | sds == 0] <- 1

  children[idx] <- children[idx] +
    stats::rnorm(nrow(idx), mean = 0, sd = sds[idx[, 2]])

  .ga_clip_children(children, values_min, values_max)
}

.ga_mutation_numeric_uniform <- function(children,
                                         prob_mutation,
                                         values_min = NULL,
                                         values_max = NULL) {
  if (nrow(children) == 0L) return(children)

  do_mutate <- .ga_make_mutation_mask(
    n_row = nrow(children),
    n_col = ncol(children),
    prob_mutation = prob_mutation
  )

  idx <- which(do_mutate, arr.ind = TRUE)
  if (nrow(idx) == 0L) {
    return(children)
  }

  if (is.null(values_min) || is.null(values_max)) {
    defaults <- .ga_default_numeric_bounds(children)
    if (is.null(values_min)) values_min <- defaults$values_min
    if (is.null(values_max)) values_max <- defaults$values_max
  }

  children[idx] <- stats::runif(
    n = nrow(idx),
    min = values_min[idx[, 2]],
    max = values_max[idx[, 2]]
  )

  children
}

.ga_mutate <- function(children,
                       ga_type,
                       type_mutation,
                       prob_mutation,
                       values_min = NULL,
                       values_max = NULL) {
  if (nrow(children) == 0L || prob_mutation <= 0) {
    return(children)
  }

  switch(
    ga_type,
    "binary" = .ga_mutation_binary(
      children = children,
      prob_mutation = prob_mutation
    ),
    "integer" = switch(
      type_mutation,
      "uniform" = .ga_mutation_integer_uniform(
        children = children,
        prob_mutation = prob_mutation,
        values_min = values_min,
        values_max = values_max
      ),
      "walk" = .ga_mutation_integer_walk(
        children = children,
        prob_mutation = prob_mutation,
        values_min = values_min,
        values_max = values_max
      ),
      stop("Unknown integer 'type_mutation'.")
    ),
    "numeric" = switch(
      type_mutation,
      "walk" = .ga_mutation_numeric_walk(
        children = children,
        prob_mutation = prob_mutation,
        values_min = values_min,
        values_max = values_max
      ),
      "uniform" = .ga_mutation_numeric_uniform(
        children = children,
        prob_mutation = prob_mutation,
        values_min = values_min,
        values_max = values_max
      ),
      stop("Unknown numeric 'type_mutation'.")
    ),
    stop("Unknown 'ga_type'.")
  )
}

#===============================================================================
# output
#===============================================================================

.ga_make_parent_info <- function(elite_idx, selected_pairs) {
  elite_parent_info <- if (length(elite_idx) == 0L) {
    NULL
  } else {
    cbind(elite_idx, elite_idx)
  }

  child_parent_info <- if (length(selected_pairs) == 0L) {
    NULL
  } else {
    do.call(rbind, selected_pairs)
  }

  out <- rbind(elite_parent_info, child_parent_info)

  if (!is.null(out)) {
    rownames(out) <- NULL
    colnames(out) <- c("parent1", "parent2")
  }

  out
}
