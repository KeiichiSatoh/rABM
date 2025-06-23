utils::globalVariables(c("G","E"))

#' @keywords internal
.DollarNames.ABM_G <- function(x, pattern = "") {
  all <- names(x)
  hidden <- c("initialize", "clone", "print")
  deprioritized <- c("time", "notes", "log")

  # exclude those fields with "." at the beggining and depriotized fields
  visible <- setdiff(all, c(hidden, deprioritized))
  visible <- grep("^\\.", visible, invert = TRUE, value = TRUE)

  ordered <- c(visible, intersect(deprioritized, all))

  grep(pattern, ordered, value = TRUE)
}

#' @keywords internal
.DollarNames.ABM_Agent <- function(x, pattern = "") {
  all <- names(x)
  hidden <- c("initialize", "clone", "print")
  shown <- setdiff(all, hidden)
  shown <- grep("^\\.", shown, invert = TRUE, value = TRUE)  # "."始まりも除外
  grep(pattern, shown, value = TRUE)
}


