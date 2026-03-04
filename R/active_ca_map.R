#' Create an active-binding CA map function (factory)
#'
#' @description
#' `active_ca_map()` is a factory that returns a **no-argument function** intended to be
#' registered as an `"active_state"` field (i.e., an active binding) in an [`ABM_Game`].
#' When evaluated, the returned function creates a fresh `nrow` x `ncol` matrix filled with
#' `vacant_val` and then writes agent attributes into the matrix according to the positions
#' stored in `self`.
#'
#' This function is designed to be compatible with the current **rABM** internal design:
#' active-binding functions are evaluated with **no arguments**, and their environments may be
#' overwritten when registered into an `ABM_Game`. Therefore, the returned function does not
#' rely on closure state and builds the matrix on each evaluation.
#'
#' @details
#' ## Position specification (`posit_type`)
#' Users can store positions in either of the following formats:
#'
#' - `posit_type = "lin"`:
#'   A vector of **linear indices** (1..`nrow*ncol`) in **column-major** order (the same as R matrices).
#' - `posit_type = "rc"`:
#'   A `K x 2` matrix (or data.frame) whose columns are `(row, col)` (1-based).
#'   These are converted internally to linear indices by
#'   \deqn{idx = (col - 1) * nrow + row}
#'
#' ## Safety option (`safe`)
#' - `safe = FALSE` (default): minimal checks for speed.
#'   Out-of-range indices may error (especially for `rc`).
#' - `safe = TRUE`: ignore `NA` and out-of-bounds positions (do not assign those entries).
#'   This is useful when user input may contain invalid positions.
#'
#' @param posit_type Character. Position format used in `self[[nm_posit]]`.
#'   One of `c("lin", "rc")`.
#' @param nrow Integer (>= 1). Number of rows in the grid.
#' @param ncol Integer (>= 1). Number of columns in the grid.
#' @param nm_posit Character scalar. Field name in `self` that stores positions.
#' @param nm_attr Character scalar. Field name in `self` that stores attribute values to write.
#' @param vacant_val Scalar. Value used to initialize the matrix (default: `0`).
#' @param safe Logical. If `TRUE`, ignore `NA` and out-of-bounds positions.
#'   If `FALSE`, perform minimal checks for speed (default).
#'
#' @return
#' A function with no required arguments. When evaluated in an `ABM_Game` context
#' (where `self` is available), it returns an `nrow` x `ncol` matrix in which the
#' entries at specified positions are replaced with the corresponding attribute values.
#'
#' @examples
#' # Example 1: linear indices (column-major)
#' city_map <- active_ca_map(
#'   posit_type = "lin",
#'   nrow = 5, ncol = 5,
#'   nm_posit = "posit",
#'   nm_attr  = "attr",
#'   vacant_val = 0
#' )
#'
#' # In an ABM_Game, 'city_map' is typically registered as active_state.
#' # Here we only show the expected 'self' structure.
#' self <- list(posit = c(1, 7, 13), attr = c(10, 20, 30))
#' # If 'self' were available in the evaluation environment:
#' # city_map()
#'
#' # Example 2: (row, col) matrix positions
#' city_map_rc <- active_ca_map(
#'   posit_type = "rc",
#'   nrow = 5, ncol = 5,
#'   nm_posit = "posit_rc",
#'   nm_attr  = "attr",
#'   vacant_val = 0,
#'   safe = TRUE
#' )
#' self <- list(
#'   posit_rc = matrix(c(1,1,  2,1,  6,2), ncol = 2, byrow = TRUE), # last row is out-of-bounds
#'   attr     = c(1, 1, 2)
#' )
#' # city_map_rc()  # out-of-bounds row would be ignored when safe = TRUE
#'
#' @export
active_ca_map <- function(posit_type = c("lin", "rc"),
                          nrow, ncol,
                          nm_posit,
                          nm_attr,
                          vacant_val = 0,
                          safe = FALSE) {

  posit_type <- match.arg(posit_type)

  stopifnot(
    "'nrow' must be a single positive integer." =
      is.numeric(nrow) && length(nrow) == 1L && is.finite(nrow) &&
      !is.na(nrow) && nrow >= 1 && (nrow %% 1 == 0),
    "'ncol' must be a single positive integer." =
      is.numeric(ncol) && length(ncol) == 1L && is.finite(ncol) &&
      !is.na(ncol) && ncol >= 1 && (ncol %% 1 == 0),
    "'nm_posit' must be a single non-empty string." =
      is.character(nm_posit) && length(nm_posit) == 1L && !is.na(nm_posit) && nzchar(nm_posit),
    "'nm_attr' must be a single non-empty string." =
      is.character(nm_attr) && length(nm_attr) == 1L && !is.na(nm_attr) && nzchar(nm_attr),
    "'safe' must be TRUE or FALSE." =
      is.logical(safe) && length(safe) == 1L && !is.na(safe)
  )

  nrow <- as.integer(nrow)
  ncol <- as.integer(ncol)

  P <- as.name(nm_posit)
  A <- as.name(nm_attr)

  body_expr <- if (identical(posit_type, "lin")) {

    if (isTRUE(safe)) {
      substitute({
        mat <- matrix(VAC, nrow = NROW, ncol = NCOL)

        posit <- self$P
        attr  <- self$A

        ok <- !is.na(posit) & (posit >= 1L) & (posit <= (NROW * NCOL))
        if (any(ok)) mat[posit[ok]] <- attr[ok]

        mat
      }, list(VAC = vacant_val, NROW = nrow, NCOL = ncol, P = P, A = A))
    } else {
      substitute({
        mat <- matrix(VAC, nrow = NROW, ncol = NCOL)
        mat[self$P] <- self$A
        mat
      }, list(VAC = vacant_val, NROW = nrow, NCOL = ncol, P = P, A = A))
    }

  } else { # "rc"

    if (isTRUE(safe)) {
      substitute({
        mat <- matrix(VAC, nrow = NROW, ncol = NCOL)

        posit <- self$P
        attr  <- self$A

        if (is.data.frame(posit)) posit <- as.matrix(posit)
        if (!is.matrix(posit) || ncol(posit) != 2L) stop("posit must be Kx2 (row, col).")

        rr <- posit[, 1L]
        cc <- posit[, 2L]

        ok <- !is.na(rr) & !is.na(cc) &
          (rr >= 1L) & (rr <= NROW) &
          (cc >= 1L) & (cc <= NCOL)

        if (any(ok)) {
          idx <- (cc[ok] - 1L) * NROW + rr[ok]
          mat[idx] <- attr[ok]
        }

        mat
      }, list(VAC = vacant_val, NROW = nrow, NCOL = ncol, P = P, A = A))
    } else {
      substitute({
        mat <- matrix(VAC, nrow = NROW, ncol = NCOL)

        posit <- self$P
        attr  <- self$A

        if (is.data.frame(posit)) posit <- as.matrix(posit)
        if (!is.matrix(posit) || ncol(posit) != 2L) stop("posit must be Kx2 (row, col).")

        rr <- posit[, 1L]
        cc <- posit[, 2L]

        idx <- (cc - 1L) * NROW + rr
        mat[idx] <- attr

        mat
      }, list(VAC = vacant_val, NROW = nrow, NCOL = ncol, P = P, A = A))
    }
  }

  f <- function() {}
  body(f) <- body_expr
  f
}


