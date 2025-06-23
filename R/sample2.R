#' Sample Elements from a Vector without Implicit Coercion
#'
#' @description
#' A lightweight wrapper around integer-based sampling that avoids the implicit behavior of
#' treating numeric scalars as sequences. This function ensures that the input
#' \code{x} is always treated as a vector of values to sample from.
#'
#' @param x A vector of elements to sample from. Unlike \code{\link{sample}}, numeric scalars such as \code{3}
#' are treated as values (e.g., \code{sample2(3)} returns \code{3}) rather than implicitly converted to \code{1:3}.
#' @param size A non-negative integer giving the number of items to choose. Defaults to 1.
#' @param replace Logical: should sampling be with replacement? Defaults to \code{FALSE}.
#' @param prob A vector of probability weights for obtaining the elements of the vector being sampled. Defaults to \code{NULL}.
#'
#' @return A vector of sampled values from \code{x}, with names preserved if present.
#'
#' @details
#' This function is designed for situations where numeric values are used as categorical labels or identifiers,
#' and where \code{sample()}'s automatic conversion of numeric scalars to \code{1:n} is undesirable.
#'
#' For example:
#' \code{sample(3)} returns a permutation of \code{1:3}, whereas \code{sample2(3)} returns \code{3}.
#'
#' @seealso \code{\link{sample}}, \code{\link{sample.int}}
#'
#' @examples
#' sample(3)       # Returns a permutation of 1:3
#' sample2(3)      # Returns 3
#'
#' sample2(letters[1:5], size = 2)
#'
#' x <- c(a = "apple", b = "banana", c = "cherry")
#' sample2(x, 2)   # Names are preserved
#'
#' @export
sample2 <- function(x, size = 1, replace = FALSE, prob = NULL){
  x[sample.int(length(x), size = size, replace = replace, prob = prob)]
}


