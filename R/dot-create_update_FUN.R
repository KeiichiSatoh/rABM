#-------------------------------------------------------------------------------
# .create_update_FUN
#-------------------------------------------------------------------------------
#' Create update_FUN from plan_list using language objects (no parse) (internal)
#'
#' This internal helper constructs an \code{update_FUN} function from a flattened
#' \code{plan_list} by building language objects via \code{call()} /
#' \code{substitute()} (instead of string generation + \code{parse()}).
#'
#' @param plan_list A data.frame with at least columns \code{name}, \code{category},
#'   and \code{group}, ordered by execution.
#' @param add_tryCatch Logical; if \code{TRUE}, wrap function calls by \code{tryCatch()}
#'   so that errors do not stop the update cycle.
#'
#' @return A function \code{update_FUN()} whose body executes the plan.
#'
#' @keywords internal


.create_update_FUN <- function(plan_list, add_tryCatch = TRUE) {
  stopifnot(is.data.frame(plan_list))
  if (!nrow(plan_list)) {
    f <- function() {}
    body(f) <- quote({})
    return(f)
  }

  #===========================================================================
  # Builders
  #===========================================================================

  # tryCatch(<expr>, error = function(e){ <body>; NULL })
  wrap_tryCatch <- function(expr, msg_prefix = NULL) {
    handler_body <- if (is.null(msg_prefix)) {
      quote({ message(e); NULL })
    } else {
      substitute({ message(paste0(PREFIX, e)); NULL }, list(PREFIX = msg_prefix))
    }
    handler_fun <- eval(substitute(function(e) BODY, list(BODY = handler_body)))
    call("tryCatch", expr, error = handler_fun)
  }

  # global_FUN expression: G[[fname]]() (optionally tryCatch)
  build_global_expr <- function(fname) {
    expr <- substitute(G[[FUN]](), list(FUN = fname))
    if (add_tryCatch) {
      wrap_tryCatch(expr, msg_prefix = paste0("error occured for ", shQuote(fname), ": "))
    } else {
      expr
    }
  }

  #===========================================================================
  # Main loop
  #===========================================================================

  parts <- list()
  k <- 0L

  for (j in seq_len(nrow(plan_list))) {
    cat_j <- plan_list$category[j]

    if (identical(cat_j, "act_FUN")) {
      fname <- plan_list$name[j]
      k <- k + 1L
      parts[[k]] <- build_global_expr(fname)
      next
    }

    stop("Unsupported category in plan_list: ", cat_j)
  }

  #===========================================================================
  # Assemble update_FUN
  #===========================================================================

  update_FUN <- function(G = G) {}
  body(update_FUN) <- as.call(c(list(as.name("{")), parts))
  update_FUN
}
