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
#' @param ... One or more \code{ABM_Field} objects to add -- typically
#'   created with \code{\link{State}}, \code{\link{Active}}, \code{\link{Act}},
#'   \code{\link{Stop}}, \code{\link{Report}}, or \code{\link{Plot}} -- or
#'   \code{\link{Zip}} bundles thereof (e.g. \code{add_field(G, State(pop),
#'   Act(reproduce))}). At least one field must be supplied.
#'
#' @return The modified \code{ABM_Game} object (invisibly).
#'
#' @details
#' This function is a thin wrapper and performs only minimal validation
#' (checking that \code{G} inherits from \code{ABM_Game} and that \code{...}
#' is not empty).
#' All structural and semantic validation is delegated to
#' \code{ABM_Game}'s internal \code{.add()} method.
#'
#' @examples
#' pop <- 100
#' G <- Game(State(pop))
#' reproduce <- function() { self$pop <- self$pop * 1.1 }
#' add_field(G, Act(reproduce))
#'
#' @seealso \code{\link{remove_field}}, \code{\link{replace_field}}
#'
#' @export
add_field <- function(G, ...){
  stopifnot("'G' must be a 'ABM_Game' class object." =
              inherits(G, "ABM_Game"))
  stopifnot("'...' must not be empty: supply at least one 'ABM_Field' object (or 'Zip()' bundle) to add." =
              length(list(...)) > 0L)
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
#' @param ... One or more field names (character strings) to remove, e.g.
#'   \code{remove_field(G, "pop")} or \code{remove_field(G, "pop",
#'   "reproduce")}. At least one name must be supplied.
#'
#' @return The modified \code{ABM_Game} object (invisibly).
#'
#' @details
#' This function is a thin wrapper and performs only minimal validation
#' (checking that \code{G} inherits from \code{ABM_Game} and that \code{...}
#' is not empty).
#' All structural and semantic validation is delegated to
#' \code{ABM_Game}'s internal \code{.remove()} method.
#'
#' @examples
#' pop <- 100
#' G <- Game(State(pop))
#' remove_field(G, "pop")
#'
#' @seealso \code{\link{add_field}}, \code{\link{replace_field}}
#'
#' @export
remove_field <- function(G, ...){
  stopifnot("'G' must be a 'ABM_Game' class object." =
              inherits(G, "ABM_Game"))
  stopifnot("'...' must not be empty: supply at least one field name to remove." =
              length(list(...)) > 0L)
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
#' @param ... One or more \code{ABM_Field} objects whose names match
#'   existing fields in \code{G} -- typically created with
#'   \code{\link{State}}, \code{\link{Active}}, \code{\link{Act}},
#'   \code{\link{Stop}}, \code{\link{Report}}, or \code{\link{Plot}} -- or
#'   \code{\link{Zip}} bundles thereof (e.g. \code{replace_field(G,
#'   State(pop))}). At least one field must be supplied.
#'
#' @return The modified \code{ABM_Game} object (invisibly).
#'
#' @details
#' This function is a thin wrapper and performs only minimal validation
#' (checking that \code{G} inherits from \code{ABM_Game} and that \code{...}
#' is not empty).
#' All structural and semantic validation is delegated to
#' \code{ABM_Game}'s internal \code{.replace()} method.
#'
#' Note that replacing a field removes and re-adds it internally,
#' so the field's position in the field list may change.
#' The category of the replacement field may differ from the original,
#' allowing a field to be reassigned to a different category (e.g.,
#' from \code{"state"} to \code{"active_state"}).
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
  stopifnot("'...' must not be empty: supply at least one 'ABM_Field' object (or 'Zip()' bundle) to replace." =
              length(list(...)) > 0L)
  G$.replace(...)
  invisible(G)
}
