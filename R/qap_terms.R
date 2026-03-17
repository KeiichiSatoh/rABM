# =========================================================
# qap terms based on a matrix or node attributes
# =========================================================

#' QAP terms derived from a matrix
#'
#' These functions generate matrix-valued QAP terms from an input matrix.
#'
#' `qap_popularity()` returns a matrix in which each row repeats the column sums
#' of `data`.
#'
#' `qap_activity()` returns a matrix in which each column repeats the row sums
#' of `data`.
#'
#' `qap_reciprocity()` returns the transpose of `data`.
#'
#' `qap_transitivity()` returns the matrix product `data %*% data`.
#'
#' `qap_common_source()` returns `t(data) %*% data`.
#'
#' `qap_common_target()` returns `data %*% t(data)`.
#'
#' @param data A matrix, typically an adjacency matrix.
#'
#' @return A matrix.
#'
#' @examples
#' mat <- matrix(
#'   c(0, 1, 1,
#'     1, 0, 0,
#'     0, 1, 0),
#'   nrow = 3, byrow = TRUE
#' )
#'
#' qap_popularity(mat)
#' qap_activity(mat)
#' qap_reciprocity(mat)
#' qap_transitivity(mat)
#' qap_common_source(mat)
#' qap_common_target(mat)
#'
#' @name qap_matrix_terms
NULL

#' @rdname qap_matrix_terms
#' @export
qap_popularity <- function(data) {
  .qap_check_matrix(data)

  matrix(
    rep(colSums(data), each = nrow(data)),
    nrow = nrow(data),
    ncol = ncol(data)
  )
}

#' @rdname qap_matrix_terms
#' @export
qap_activity <- function(data) {
  .qap_check_matrix(data)

  matrix(
    rep(rowSums(data), ncol(data)),
    nrow = nrow(data),
    ncol = ncol(data)
  )
}

#' @rdname qap_matrix_terms
#' @export
qap_reciprocity <- function(data) {
  .qap_check_matrix(data)
  t(data)
}

#' @rdname qap_matrix_terms
#' @export
qap_transitivity <- function(data) {
  .qap_check_matrix(data)
  data %*% data
}

#' @rdname qap_matrix_terms
#' @export
qap_common_source <- function(data) {
  .qap_check_matrix(data)
  t(data) %*% data
}

#' @rdname qap_matrix_terms
#' @export
qap_common_target <- function(data) {
  .qap_check_matrix(data)
  data %*% t(data)
}


# =========================================================
# qap terms based on node attributes
# =========================================================

#' QAP terms derived from node attributes
#'
#' These functions generate matrix-valued QAP terms from a node-level attribute
#' vector.
#'
#' `qap_homophily()` returns a binary matrix indicating whether sender and
#' receiver have the same attribute value.
#'
#' `qap_sender()` returns a matrix in which each column contains sender-side
#' attribute values.
#'
#' `qap_receiver()` returns a matrix in which each row contains receiver-side
#' attribute values.
#'
#' `qap_attrcross()` combines sender and receiver attributes using either one of
#' the built-in rules (`"sum"`, `"max"`, `"min"`, `"prod"`) or a user-supplied
#' function.
#'
#' @param data A matrix, typically an adjacency matrix. Its dimensions are used
#'   to shape the output.
#' @param attribute A vector of node attributes. Its length must equal
#'   `nrow(data)`.
#' @param calc A character string specifying how sender and receiver attributes
#'   are combined. One of `"sum"`, `"max"`, `"min"`, or `"prod"`.
#' @param FUN An optional function used to combine sender and receiver
#'   attributes element-wise. If supplied, `calc` is ignored.
#' @param ... Additional arguments passed to `FUN`.
#'
#' @return A matrix with dimensions `nrow(data)` by `ncol(data)`.
#'
#' @examples
#' mat <- matrix(
#'   c(0, 1, 1,
#'     1, 0, 0,
#'     0, 1, 0),
#'   nrow = 3, byrow = TRUE
#' )
#'
#' attr <- c(1, 2, 1)
#'
#' qap_homophily(mat, attr)
#' qap_sender(mat, attr)
#' qap_receiver(mat, attr)
#' qap_attrcross(mat, attr, calc = "sum")
#' qap_attrcross(mat, attr, calc = "max")
#' qap_attrcross(mat, attr, calc = "prod")
#' qap_attrcross(mat, attr, FUN = function(x, y) abs(x - y))
#'
#' @name qap_attribute_terms
NULL

#' @rdname qap_attribute_terms
#' @export
qap_homophily <- function(data, attribute) {
  x <- .qap_expand_attribute(data, attribute)

  matrix(
    as.numeric(x$sender_mat == x$receiver_mat),
    nrow = x$n_row,
    ncol = x$n_col
  )
}

#' @rdname qap_attribute_terms
#' @export
qap_sender <- function(data, attribute) {
  .qap_expand_attribute(data, attribute)$sender_mat
}

#' @rdname qap_attribute_terms
#' @export
qap_receiver <- function(data, attribute) {
  .qap_expand_attribute(data, attribute)$receiver_mat
}

#' @rdname qap_attribute_terms
#' @export
qap_attrcross <- function(data, attribute,
                          calc = c("sum", "max", "min", "prod"),
                          FUN = NULL, ...) {
  x <- .qap_expand_attribute(data, attribute)

  if (!is.null(FUN)) {
    stopifnot("'FUN' must be a function." = is.function(FUN))
    vec <- mapply(
      FUN,
      x$sender_vec,
      x$receiver_vec,
      ...,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
  } else {
    calc <- match.arg(calc)

    vec <- switch(
      calc,
      "sum"  = x$sender_vec + x$receiver_vec,
      "max"  = pmax(x$sender_vec, x$receiver_vec),
      "min"  = pmin(x$sender_vec, x$receiver_vec),
      "prod" = x$sender_vec * x$receiver_vec
    )
  }

  matrix(vec, nrow = x$n_row, ncol = x$n_col)
}


# =========================================================
# internal helpers
# =========================================================

#' Validate matrix input for QAP terms
#'
#' @param data A matrix.
#'
#' @return Invisibly returns `TRUE`.
#' @noRd
.qap_check_matrix <- function(data) {
  stopifnot("'data' must be a matrix." = is.matrix(data))
  invisible(TRUE)
}

#' Validate attribute input for QAP terms
#'
#' @param data A matrix.
#' @param attribute A vector of node attributes.
#'
#' @return Invisibly returns `TRUE`.
#' @noRd
.qap_check_attribute <- function(data, attribute) {
  .qap_check_matrix(data)
  stopifnot(
    "'attribute' must have the same length as nrow(data)." =
      length(attribute) == nrow(data)
  )
  invisible(TRUE)
}

#' Expand node attributes into sender and receiver forms
#'
#' @param data A matrix.
#' @param attribute A vector of node attributes.
#'
#' @return A list containing sender/receiver vectors and matrices, along with
#'   matrix dimensions.
#' @noRd
.qap_expand_attribute <- function(data, attribute) {
  .qap_check_attribute(data, attribute)

  n_row <- nrow(data)
  n_col <- ncol(data)

  list(
    n_row = n_row,
    n_col = n_col,
    sender_vec = rep(attribute, n_row),
    receiver_vec = rep(attribute, each = n_row),
    sender_mat = matrix(rep(attribute, n_row), nrow = n_row, ncol = n_col),
    receiver_mat = matrix(rep(attribute, each = n_row), nrow = n_row, ncol = n_col)
  )
}
