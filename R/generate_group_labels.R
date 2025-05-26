#' @title Generate Group labels Based on Proportions with Adjustment
#' @description
#' This function generates a vector of group labels based on
#' given proportions and adjusts the allocation if the total count
#' differs from the expected size. Adjustments can be made randomly
#' or by prioritizing a specific group.
#'
#' @param n Integer. The total number of items to allocate.
#' @param prop Numeric vector. Proportions for each group.
#' If unnamed, groups will be labeled as integers (1, 2, ...).
#' @param adjustment_by Optional. A single group label to prioritize
#' when adjusting counts. Must match one of the group labels.
#' @param random_adjustment Logical. If `TRUE`, adjustments are
#' made randomly across groups when `adjustment_by` is not specified.
#' Default is \code{TRUE}.
#'
#' @return A character or integer vector of length `n`,
#' containing the group labels for each item.
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
#'
#' The function includes safeguards to prevent infinite loops during
#' adjustments.
#'
#' @examples
#' # Basic usage with named proportions
#' generate_group_labels(10, c(A = 0.4, B = 0.6))
#'
#' # Adjust prioritizing group "A"
#' generate_group_labels(11, c(A = 0.4, B = 0.6), adjustment_by = "A")
#'
#' # Random adjustment without specifying a group
#' generate_group_labels(11, c(A = 0.4, B = 0.6), random_adjustment = TRUE)
#'
#' @export

generate_group_labels <- function(n, prop, adjustment_by = NULL, random_adjustment = TRUE) {
  # 割合を合計が1になるように調整
  if (sum(prop) == 0){stop("'prop' must not sum to zero")}
  prop <- prop / sum(prop)

  # 各グループ名を取得
  if (is.null(names(prop))) {
    label <- 1:length(prop)
  } else {
    label <- names(prop)
  }

  # ひとまず割合に従って、ベクトルを生成する
  gr_n <- round(n * prop)
  gr <- unlist(lapply(1:length(label), function(i) { rep(label[i], gr_n[i]) }))

  # 数が合わない場合
  if (sum(gr_n) != n) {
    max_iter <- 1000
    iter <- 0
    if (!is.null(adjustment_by)) {
      # 特定のグループで合わせる
      adjust_index <- which(label == adjustment_by)
      if (length(adjust_index) == 0) stop("'adjustment_by' must be a valid group label")
      while (sum(gr_n) != n) {
        iter <- iter + 1
        if (iter > max_iter) stop("Failed to adjust 'gr_n' within the maximum number of iterations")
        if (sum(gr_n) > n) {
          gr_n[adjust_index] <- gr_n[adjust_index] - (sum(gr_n) - n)
        } else {
          gr_n[adjust_index] <- gr_n[adjust_index] + (sum(gr_n) - n)
        }
      }
    } else {
      if (random_adjustment == TRUE) {
        # ランダムに調整
        while (sum(gr_n) != n) {
          iter <- iter + 1
          if (iter > max_iter) stop("Failed to adjust 'gr_n' within the maximum number of iterations")
          gr_selected <- sample(label, size = 1)
          selected_index <- which(label == gr_selected)
          if (sum(gr_n) > n) {
            gr_n[selected_index] <- gr_n[selected_index] - 1
          } else {
            gr_n[selected_index] <- gr_n[selected_index] + 1
          }
        }
      }
    }
  }

  # グループを生成
  gr <- unlist(lapply(1:length(gr_n), function(i) { rep(label[i], gr_n[i]) }))
  # アウトプット
  gr
}
