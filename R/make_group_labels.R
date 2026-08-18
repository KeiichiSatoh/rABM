#' @title Generate Group Labels Based on Proportions with Adjustment
#' @description
#' This function generates a vector of group labels based on
#' given proportions and adjusts the allocation if the total count
#' differs from the expected size. Adjustments can be made randomly
#' or by prioritizing a specific group.
#'
#' @param n Integer. The total number of items to allocate.
#' @param prop Numeric vector. Proportions for each group. Must be
#' non-negative and must not sum to zero. If unnamed, groups will be
#' labeled as integers (1, 2, ...).
#' @param adjustment_by Optional. A single group label to prioritize
#' when adjusting counts. Must match exactly one of the group labels;
#' this is validated immediately, even if rounding happens not to
#' require any adjustment.
#' @param random_adjustment Logical. If `TRUE`, adjustments are
#' made randomly across groups when `adjustment_by` is not specified.
#' Default is \code{TRUE}. If `FALSE` and `adjustment_by` is `NULL`,
#' no adjustment is made; a warning explaining why is issued and the
#' returned vector's length will not equal `n`.
#' @param from_zero Logical. If `TRUE`, group labels are returned as
#' 0-based integers (`0, 1, 2, ...`, in the order the groups appear
#' in `prop`) instead of the default 1-based integer or name-based
#' labels. If `adjustment_by` is also used together with `from_zero`,
#' it must refer to the 0-based label (e.g. `0`), not the original
#' name. Default is \code{FALSE}.
#'
#' @return A character, integer, or numeric vector of length `n`
#' (except in the one case described under `random_adjustment`
#' where the length may differ from `n`), containing the group
#' labels for each item.
#'
#' @details
#' The function starts by scaling the proportions to sum to 1 and
#' calculates the initial allocation using `round(n * prop)`.
#' If the total count (`sum(gr_n)`) does not equal `n`,
#' adjustments are made:
#'
#' - If `adjustment_by` is provided, the specified group is adjusted
#' to correct the total.
#' - Otherwise, adjustments are made randomly across groups when
#'  `random_adjustment = TRUE`.
#' - If neither is available, no adjustment is made and a warning
#'  explains why the returned vector's length differs from `n`.
#'
#' The function includes safeguards to prevent infinite loops during
#' adjustments, and to prevent any group's count from being adjusted
#' below zero.
#'
#' @examples
#' # Basic usage with named proportions
#' make_group_labels(10, c(A = 0.4, B = 0.6))
#'
#' # Adjust prioritizing group "A"
#' make_group_labels(11, c(A = 0.4, B = 0.6), adjustment_by = "A")
#'
#' # Random adjustment without specifying a group (the default)
#' make_group_labels(11, c(A = 0.4, B = 0.6))
#'
#' # Returned as 0-based integers instead of 1-based (works for any
#' # number of groups)
#' make_group_labels(10, c(A = 0.4, B = 0.6), from_zero = TRUE)
#' make_group_labels(10, c(A = 0.2, B = 0.3, C = 0.5), from_zero = TRUE)
#'
#' @export
make_group_labels <- function(n, prop, adjustment_by = NULL,
                               random_adjustment = TRUE,
                               from_zero = FALSE) {
  # --- input validation ---
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 0 || n != round(n)) {
    stop("'n' must be a single non-negative integer")
  }
  if (!is.numeric(prop) || length(prop) == 0) {
    stop("'prop' must be a non-empty numeric vector")
  }
  if (any(is.na(prop)) || any(prop < 0)) {
    stop("'prop' must not contain negative values or NAs")
  }
  if (sum(prop) == 0) stop("'prop' must not sum to zero")
  prop <- prop / sum(prop)

  # retrieve group names
  if (is.null(names(prop))) {
    label <- seq_along(prop)
  } else {
    label <- names(prop)
  }
  if (anyDuplicated(label) > 0) {
    stop("Group labels must be unique (check 'names(prop)')")
  }

  # optionally relabel as 0-based integers (0, 1, 2, ...) instead of 1-based
  if (isTRUE(from_zero)) {
    label <- seq_along(prop) - 1
  }

  # validate adjustment_by unconditionally, even if rounding happens not to
  # require any adjustment this time -- an invalid label should always error
  if (!is.null(adjustment_by)) {
    adjust_index <- which(label == adjustment_by)
    if (length(adjust_index) == 0) stop("'adjustment_by' must be a valid group label")
  }

  # generate counts according to the proportions
  gr_n <- round(n * prop)

  # when the total differs from n, adjust
  if (sum(gr_n) != n) {
    max_iter <- 1000
    iter <- 0
    if (!is.null(adjustment_by)) {
      # adjust by a specific group (single-step: the full gap is corrected at once)
      diff <- sum(gr_n) - n
      if (gr_n[adjust_index] - diff < 0) {
        stop(
          "Cannot adjust group '", adjustment_by, "' without its count going below zero; ",
          "choose a different 'adjustment_by' group or use random_adjustment = TRUE"
        )
      }
      gr_n[adjust_index] <- gr_n[adjust_index] - diff
    } else if (isTRUE(random_adjustment)) {
      # adjust randomly, never letting a group's count drop below zero
      while (sum(gr_n) != n) {
        iter <- iter + 1
        if (iter > max_iter) stop("Failed to adjust 'gr_n' within the maximum number of iterations")
        need_decrease <- sum(gr_n) > n
        candidates <- if (need_decrease) which(gr_n > 0) else seq_along(label)
        if (length(candidates) == 0) {
          stop("Failed to adjust 'gr_n': no group has a count available to decrease")
        }
        # sample() on a length-1 numeric vector treats it as a range (1:x), not
        # as "pick this one element" -- select by index and guard length 1 explicitly.
        selected_index <- if (length(candidates) == 1) candidates else sample(candidates, size = 1)
        if (need_decrease) {
          gr_n[selected_index] <- gr_n[selected_index] - 1
        } else {
          gr_n[selected_index] <- gr_n[selected_index] + 1
        }
      }
    } else {
      warning(
        "sum(gr_n) (", sum(gr_n), ") does not equal 'n' (", n, "), but no adjustment ",
        "was made because 'adjustment_by' is NULL and 'random_adjustment' is FALSE. ",
        "Returning a vector of length ", sum(gr_n), " instead of ", n, "."
      )
    }
  }

  # build the group label vector
  gr <- unlist(lapply(seq_along(gr_n), function(i) rep(label[i], gr_n[i])))
  # output
  gr
}
