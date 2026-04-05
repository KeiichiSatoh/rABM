#-------------------------------------------------------------------------------
# add_field / remove_field / replace_field
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
#' @seealso \code{\link{remove_field}}, \code{\link{replace_field}}
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
#' @seealso \code{\link{add_field}}, \code{\link{replace_field}}
#'
#' @export
remove_field <- function(G, ...){
  stopifnot("'G' must be a 'ABM_Game' class object." =
              inherits(G, "ABM_Game"))
  G$.remove(...)
  invisible(G)
}


#' Replace Fields in an ABM_Game Object
#'
#' A user-facing wrapper for the internal \code{.replace()} method of
#' an \code{ABM_Game} object.
#'
#' This function forwards the input to \code{G$.replace()} and returns
#' the modified \code{ABM_Game} object invisibly.
#' Validation of field existence, naming rules, and class consistency
#' is handled internally by the \code{ABM_Game} method.
#'
#' @param G An object of class \code{ABM_Game}.
#' @param ... Fields to replace. Each element must be an \code{ABM_Field}
#'   object whose name matches an existing field in \code{G}.
#'
#' @return The modified \code{ABM_Game} object (invisibly).
#'
#' @details
#' This function is a thin wrapper and performs only minimal validation
#' (checking that \code{G} inherits from \code{ABM_Game}).
#' All structural and semantic validation is delegated to
#' \code{ABM_Game}'s internal \code{.replace()} method.
#'
#' Note that replacing a field removes and re-adds it internally,
#' so the field's position in the field list may change.
#' The category of the replacement field may differ from the original,
#' allowing a field to be reassigned to a different category (e.g.,
#' from \code{"state"} to \code{"active_state"}).
#' Note that replacing a field removes and re-adds it internally,
#' so the field's position in the field list may change.
#'
#' @examples
#' # prepare a Game object
#' x <- 1
#' y <- 2
#' G <- Game(State(x), State(y))
#'
#' # replace value of x
#' x <- 3
#' replace_field(G, State(x))
#'
#' # replace state field y to be an active state
#' y <- function(){ self$x^2 }
#' replace_field(G, Active(y))
#'
#' @seealso \code{\link{add_field}}, \code{\link{remove_field}}
#'
#' @export
replace_field <- function(G, ...){
  stopifnot("'G' must be a 'ABM_Game' class object." =
              inherits(G, "ABM_Game"))
  G$.replace(...)
  invisible(G)
}
