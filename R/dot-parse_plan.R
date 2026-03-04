#-------------------------------------------------------------------------------
# .parse_plan
#-------------------------------------------------------------------------------
#' Parse a plan specification into function list and arguments (internal)
#'
#' This internal helper parses a character vector \code{plan} that specifies
#' which registered functions (FUNs) to use, optionally with a group prefix and
#' call-style arguments.
#'
#' Each element of \code{plan} can be either:
#' \itemize{
#'   \item \code{"FUN"} (a function name), or
#'   \item \code{"FUN(arg1, arg2, ...)"} (with arguments).
#' }
#'
#' Spaces are removed before parsing. Function expressions are parsed with
#' \code{parse()}, and arguments (if any) are stored as language objects in a list.
#'
#' @param plan A character vector specifying functions to be used.
#'   Validation (character vector, non-list) is assumed to be done upstream.
#' @param field_list A data.frame that registers available functions.
#'   It must have at least columns \code{name}, \code{category}.
#'
#' @return A named list with:
#'   \describe{
#'     \item{\code{plan_list}}{A subset of \code{field_list} containing only the rows
#'     referenced by \code{plan} (in the same order as \code{plan}).}
#'     \item{\code{FUN_args}}{A list of argument lists for each parsed FUN.
#'     The names of \code{FUN_args} are the parsed function names.}
#'   }
#'
#' @details
#' If any FUN specified in \code{plan} cannot be found in \code{field_list},
#' the function stops with an error. When multiple rows in \code{field_list}
#' match the same key, the first match is used (depending on the implementation).
#'
#' @keywords internal

.parse_plan <- function(plan, field_list){

  # remove spaces
  plan <- stringr::str_remove_all(plan, pattern = " ")

  n_plan <- length(plan)

  # parse FUN arguments
  FUN_args <- vector("list", n_plan)

  for(i in seq_len(n_plan)){
    parsed_obs <- tryCatch(
      parse(text = plan[i])[[1]],
      error = function(e) {
        stop(sprintf("Invalid expression: %s", plan[i]), call. = FALSE)
      }
    )

    if (is.symbol(parsed_obs)) {
      names(FUN_args)[i] <- as.character(parsed_obs)
      FUN_args[[i]] <- list()
    } else {
      names(FUN_args)[i] <- as.character(parsed_obs[[1]])
      FUN_args[[i]] <- as.list(parsed_obs[-1])
    }
  }

  # locate functions in field_list
  idx <- rep(NA_integer_, n_plan)

  for (i in seq_len(n_plan)) {
    FUN_name <- names(FUN_args)[i]
    w <- which(field_list$name == FUN_name)
    idx[i] <- if (length(w)) w[1] else NA_integer_
  }

  # stop if there is NA
  if (anyNA(idx)) {
    missing_i <- which(is.na(idx))
    missing_txt <- plan[missing_i]
    stop(
      paste0("The following FUN was not found: ",
             paste(missing_txt, collapse = ", ")),
      call. = FALSE
    )
  }

  # output
  list(
    plan_list = field_list[idx, , drop = FALSE],
    FUN_args  = FUN_args
  )
}
