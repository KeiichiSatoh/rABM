#-------------------------------------------------------------------------------
# add_field / remove_field
#-------------------------------------------------------------------------------

#' Add Fields to an ABM_Game Object
#'
#' A user-facing wrapper for the internal \code{.add()} method of
#' an \code{ABM_Game} object.
#'
#' This function forwards the input to \code{G$.add()} and returns
#' the modified \code{ABM_Game} object invisibly.
#' Validation of field structure, naming rules, and category consistency
#' is handled internally by the \code{ABM_Game} method.
#'
#' @param G An object of class \code{ABM_Game}.
#' @param ... Fields to be added. The interpretation of the input
#'   depends on the internal implementation of \code{G$.add()}.
#'
#' @return The modified \code{ABM_Game} object (invisibly).
#'
#' @details
#' This function is a thin wrapper and performs only minimal validation
#' (checking that \code{G} inherits from \code{ABM_Game}).
#' All structural and semantic validation is delegated to
#' \code{ABM_Game}'s internal \code{.add()} method.
#'
#' @seealso \code{\link{remove_field}}
#'
#' @export
add_field <- function(G, ...){
  stopifnot("'G' must be a 'ABM_Game' class object." =
              inherits(G, "ABM_Game"))
  G$.add(...)
  invisible(G)
}


#' Remove Fields from an ABM_Game Object
#'
#' A user-facing wrapper for the internal \code{.remove()} method of
#' an \code{ABM_Game} object.
#'
#' This function forwards the input to \code{G$.remove()} and returns
#' the modified \code{ABM_Game} object invisibly.
#'
#' Validation of field existence and structural consistency
#' is handled internally by the \code{ABM_Game} method.
#'
#' @param G An object of class \code{ABM_Game}.
#' @param ... Field names to remove. The interpretation of the input
#'   depends on the internal implementation of \code{G$.remove()}.
#'
#' @return The modified \code{ABM_Game} object (invisibly).
#'
#' @details
#' This function is a thin wrapper and performs only minimal validation
#' (checking that \code{G} inherits from \code{ABM_Game}).
#' All structural and semantic validation is delegated to
#' \code{ABM_Game}'s internal \code{.remove()} method.
#'
#' @seealso \code{\link{add_field}}
#'
#' @export
remove_field <- function(G, ...){
  stopifnot("'G' must be a 'ABM_Game' class object." =
              inherits(G, "ABM_Game"))
  G$.remove(...)
  invisible(G)
}
