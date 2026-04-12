#' Weighted Sampling for Multiple Cases
#'
#' Sample column labels for each row of a probability-weight matrix, where the
#' probability of selecting each column is proportional to its weight.
#' This function is designed for agent-based models (ABMs) and similar settings
#' where many cases make weighted probabilistic choices simultaneously.
#'
#' @param prob_mat A numeric matrix of non-negative weights. Each row represents
#'   a case (e.g., an agent), and each column represents an option. Row sums do
#'   not need to equal 1, as rows are normalized internally. Missing values
#'   (\code{NA}) are not allowed.
#' @param size A positive integer specifying the number of options to sample per
#'   case. Defaults to \code{1}. When \code{replace = FALSE}, \code{size} must
#'   not exceed \code{ncol(prob_mat)}.
#' @param replace Logical scalar indicating whether sampling is with replacement.
#'   Defaults to \code{FALSE}. If \code{FALSE}, once an option is drawn for a row,
#'   its weight is set to 0 for that row and the remaining weights are
#'   re-normalized before the next draw.
#' @param return_as_numeric Logical scalar indicating whether output values should
#'   be returned as numeric. Defaults to \code{FALSE}. This is only used when
#'   \code{prob_mat} has column names. If \code{TRUE}, column names are coerced to
#'   numeric and returned as numeric values.
#'
#' @return
#' If \code{size == 1}, a vector of length \code{nrow(prob_mat)} containing the
#' selected option for each row.
#'
#' If \code{size >= 2}, a matrix with \code{nrow(prob_mat)} rows and \code{size}
#' columns, where each row contains the selected options in draw order. Row names
#' are inherited from \code{prob_mat}, and column names are \code{1}, \code{2},
#' ..., \code{size}.
#'
#' If \code{colnames(prob_mat)} is \code{NULL}, the function returns column
#' indices (\code{1}, \code{2}, ..., \code{ncol(prob_mat)}). Otherwise, it
#' returns column names, unless \code{return_as_numeric = TRUE}, in which case
#' column names are coerced to numeric.
#'
#' @details
#' For each row of \code{prob_mat}, weights are normalized to sum to 1, and a
#' cumulative sum is computed. A uniform random number is then drawn and compared
#' against the cumulative probabilities to determine the selected column using
#' \code{\link[max.col]{max.col}} with \code{ties.method = "first"}.
#'
#' When \code{replace = TRUE}, each row is normalized only once, and repeated
#' draws are made independently from the same row-wise distribution.
#'
#' When \code{replace = FALSE}, sampling is performed without replacement within
#' each row: after each draw, the selected column is assigned weight 0 for that
#' row, and the remaining weights are re-normalized before the next draw.
#'
#' @note
#' To ensure reproducibility, call \code{\link{set.seed}} before this function.
#'
#' @examples
#' prob_mat <- matrix(c(1, 2, 3,
#'                      4, 5, 6),
#'                    nrow = 2, byrow = TRUE)
#' colnames(prob_mat) <- c("A", "B", "C")
#'
#' # One draw per row
#' set.seed(1)
#' sample_weighted(prob_mat, size = 1)
#'
#' # Two draws per row without replacement
#' set.seed(1)
#' sample_weighted(prob_mat, size = 2, replace = FALSE)
#'
#' # Two draws per row with replacement
#' set.seed(1)
#' sample_weighted(prob_mat, size = 2, replace = TRUE)
#'
#' # Return numeric values when column names are numeric
#' prob_mat2 <- prob_mat
#' colnames(prob_mat2) <- c("10", "20", "30")
#' set.seed(1)
#' sample_weighted(prob_mat2, size = 1, return_as_numeric = TRUE)
#'
#' # If column names are absent, column indices are returned
#' prob_mat3 <- prob_mat
#' colnames(prob_mat3) <- NULL
#' set.seed(1)
#' sample_weighted(prob_mat3, size = 1)
#'
#' @importFrom matrixStats rowSums2 rowCumsums
#' @export
sample_weighted <- function(prob_mat, size = 1, replace = FALSE, return_as_numeric = FALSE){
  size <- as.integer(size)
  # validation
  stopifnot("'prob_mat' must be a matrix." = is.matrix(prob_mat))
  stopifnot("'size' must be a positive integer." =
              length(size) == 1 && size >= 1)
  if (any(prob_mat < 0)) {
    stop("'prob_mat' contains negative values. Provide 'prob_mat' whose minimum value is 0.")
  }
  if (any(is.na(prob_mat))) {
    stop("'prob_mat' contains NA. Set 0 if the probability of choosing this option should be 0.")
  }

  if (is.null(colnames(prob_mat))){
    col_names <- seq_len(ncol(prob_mat))
  }else{
    if(return_as_numeric){
      col_names <- as.numeric(colnames(prob_mat))
      stopifnot("Some 'colnames' cannot be coerced into numeric." =
                  all(!is.na(col_names)))
    }else{
      col_names <- colnames(prob_mat)
    }
  }

  n_cases <- nrow(prob_mat)

  if(size == 1){
    # standardize prob_mat
    x_std <- prob_mat / matrixStats::rowSums2(prob_mat)
    x_csm <- matrixStats::rowCumsums(x_std)
    # select
    th <- runif(n_cases, 0, 1)
    choice <- max.col(th <= x_csm, ties.method = "first")
    return(col_names[choice])
  }else{
    choice_mat <- matrix(0L, n_cases, size)
    x_std <- prob_mat

    if(replace){
      x_std <- x_std / matrixStats::rowSums2(x_std)
      x_csm <- matrixStats::rowCumsums(x_std)

      for(s in seq_len(size)){
        # select
        th <- runif(n_cases, 0, 1)
        choice <- max.col(th <= x_csm, ties.method = "first")
        choice_mat[ ,s] <- choice
      }
    }else{
      stopifnot("'size' must be <= ncol(prob_mat)." = size <= ncol(prob_mat))

      for(s in seq_len(size)){
        # standardize
        x_std <- x_std / matrixStats::rowSums2(x_std)
        x_csm <- matrixStats::rowCumsums(x_std)

        # select
        th <- runif(n_cases, 0, 1)
        choice <- max.col(th <= x_csm, ties.method = "first")
        choice_mat[ ,s] <- choice

        # update the choice
        if(s < size) x_std[cbind(1:n_cases, choice)] <- 0
      }
    }

    out <- matrix(col_names[choice_mat], nrow = n_cases, ncol = size)
    dimnames(out) <- list(rownames(prob_mat), 1:size)
    out
  }
}

