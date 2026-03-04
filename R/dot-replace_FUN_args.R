#-------------------------------------------------------------------------------
# .replace_FUN_args
#-------------------------------------------------------------------------------
#' Replace default arguments of planned functions (internal)
#'
#' This internal helper injects argument values specified in \code{FUN_args}
#' into the formals (default arguments) of functions referenced by \code{plan_list},
#' and then updates those functions in the given \code{ABM_Game} object via
#' \code{G$.replace()}.
#'
#' The function assumes that \code{plan_list} and \code{FUN_args} were created
#' by \code{.parse_plan()} so that their orders correspond one-to-one.
#'
#' @param G An \code{ABM_Game} object to be updated.
#' @param FUN_args A list of arguments for each planned function.
#'   Each element must be a named list of language objects (parsed from \code{plan}).
#' @param plan_list A data.frame describing planned functions, typically the
#'   \code{plan_list} component returned by \code{.parse_plan()}.
#'   It must contain at least column \code{name} .
#'
#' @return The updated \code{ABM_Game} object \code{G}.
#'
#' @details
#' For each row of \code{plan_list}, if the corresponding \code{FUN_args[[i]]}
#' is non-empty, the function:
#' \enumerate{
#'   \item Retrieves the target function from \code{G} (global FUN) or from each
#'   agent in the specified group (agent FUN).
#'   \item Replaces the default values in \code{formals(FUN)} using the names in
#'   \code{FUN_args[[i]]}.
#'   \item Writes the modified function back using \code{.replace()}.
#' }
#'
#' All arguments in \code{FUN_args} are expected to be \strong{named}.
#' If unknown argument names (not present in the target function's formals)
#' are supplied, the function should error (depending on the implementation).
#'
#' @keywords internal


.replace_FUN_args <- function(G, FUN_args, plan_list){

  stopifnot("Length mismatch between 'plan_list' and 'FUN_args'." =
              nrow(plan_list) == length(FUN_args))

  for(i in seq_len(nrow(plan_list))){

    args <- FUN_args[[i]]
    if (!length(args)) next

    # keep only named args (positional args are ignored for safety)
    nms <- names(args)
    if (is.null(nms) || anyNA(nms) || any(nms == "")) {
      stop("All FUN arguments in 'plan' must be named (e.g., fun(a=1, b=2)).")
    }

    name <- plan_list$name[i]
    FUN  <- G[[name]]

    fml <- formals(FUN)
    bad <- setdiff(nms, names(fml))
    if (length(bad)) {
      stop("Unknown argument(s) for ", name, ": ", paste(bad, collapse = ", "))
    }

    fml[nms] <- args
    formals(FUN) <- fml
    G$.replace(name = name, x = FUN)
  }

  G
}

