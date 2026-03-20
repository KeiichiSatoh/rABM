#' Summarize an ABM game object
#'
#' S3 method for `summary()` on [`ABM_Game`] objects.
#' This method returns a lightweight summary object (class `summary.ABM_Game`)
#' created by the internal R6 method `$ .summary()`.
#'
#' @param object An [`ABM_Game`] object.
#' @param ... Reserved for future extensions.
#'
#' @return
#' An object of class `summary.ABM_Game`. It typically contains:
#' - `n_fields`: Number of fields,
#' - `field_table`: summaries of fields and categories,
#' - `field_list`: a flattened field list,
#' - metadata such as `time`, `n_log`, and `note_names`.
#'
#' @seealso [`ABM_Game`], [`Game()`], [`print.summary.ABM_Game`]
#'
#' @export
summary.ABM_Game <- function(object, ...) {
  object$.summary()
}



#' Print a summary of an ABM game object
#'
#' S3 method for printing objects returned by `summary()` on [`ABM_Game`] objects.
#' The output is organized into sections for groups, stage, registered functions,
#' and metadata.
#'
#' @param x A `summary.ABM_Game` object.
#' @param ... Currently unused.
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [`summary.ABM_Game`], [`ABM_Game`]
#'
#' @export
print.summary.ABM_Game <- function(x, ...) {
  # internal function
  show <- function(cat_name, ft, fl) {
    n <- unname(ft[cat_name])
    if (isTRUE(n > 0)) {
      nm <- fl$name[fl$category == cat_name]
      cat(sprintf("  %-12s: %s\n", cat_name, paste(nm, collapse = ", ")))
    }
  }

  cat("<ABM_Game>\n")

  # [stage]
  if(length(x$field_table["state"]) > 0 || (length(x$field_table["active_state"]) > 0)){
    cat("[state]\n")
    show("state", ft = x$field_table, fl = x$field_list)
    show("active_state", ft = x$field_table, fl = x$field_list)
    cat("\n")
  }

  # [FUN]
  is.exist.FUN <- any(x$field_table[c("act_FUN", "stop_FUN",
                                      "report_FUN", "plot_FUN")] > 0)

  if(is.exist.FUN > 0){
    cat("[FUN]\n")
    show("act_FUN",  ft = x$field_table, fl = x$field_list)
    show("stop_FUN", ft = x$field_table, fl = x$field_list)
    show("report_FUN", ft = x$field_table, fl = x$field_list)
    show("plot_FUN", ft = x$field_table, fl = x$field_list)
    cat("\n")
  }

  # [Meta data]
  cat("[Meta data]", "\n")
  cat("  current time:", x$time, "\n")
  cat("  n of log    :", x$n_log, "\n")
  cat("  note names  :", x$note_names, "\n")
  cat("\n")
}
