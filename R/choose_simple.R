#' @title Random Samples with Weighted Probability
#' @description
#' A streamlined version of \code{sample} designed to speed up selection
#' during simulations. Sampling is performed without replacement.
#'
#' @details
#' This function selects one or more items from a set of candidates based on
#' weighted probabilities, useful in simulations where agents choose an option.
#'
#' Functions that accommodate a wide range of inputs often run slower due to
#' extensive checks. Reducing such checks and pre-processing steps
#' typically increases execution speed.
#'
#' This function bypasses certain preparatory procedures within "sample.int" and
#'  is specifically optimized for scalar selection,
#'  achieving approximately 1.5 to 2 times faster performance.
#'
#' @seealso [sample(base)]
#' @family FUN tools
#' @param x A vector of elements to choose from.
#' @param size A non-negative integer indicating the number of items to select. Default is 1.
#' @param prob A vector of probability weights for selecting elements from the vector.
#' If set to \code{NULL} (the default), all elements have an equal chance of selection.
#' @export
#' @examples
#' x <- c("A", "B", "C")
#' prob <- c(0.3, 0.6, 0.1)
#' choose_simple(x = x, prob = prob)

choose_simple <- function(x, size = 1, prob = NULL){
  x[.Internal(sample(length(x), size = size, replace = FALSE, prob = prob))]
}

