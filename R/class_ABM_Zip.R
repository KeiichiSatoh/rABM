#-------------------------------------------------------------------------------
# Zip / Unzip ABM_Field objects
#-------------------------------------------------------------------------------

#' Bundle and unbundle ABM_Field objects
#'
#' \code{Zip()} groups multiple objects (typically \code{"ABM_Field"} objects)
#' into a single container of class \code{"ABM_Zip"}.
#' \code{Unzip()} flattens such containers back into a simple list, recursively
#' expanding nested \code{"ABM_Zip"} objects.
#'
#' @details
#' \itemize{
#'   \item \code{ABM_Zip()} is a lightweight internal constructor that stores the
#'   supplied objects in a list with class \code{"ABM_Zip"}.
#'   \item \code{Zip()} is the user-facing wrapper for creating \code{"ABM_Zip"} objects.
#'   \item \code{Unzip()} takes one or more objects and returns a flat list:
#'   \itemize{
#'     \item If an element inherits from \code{"ABM_Zip"}, it is recursively expanded.
#'     \item Otherwise, the element is returned as-is.
#'   }
#' }
#'
#' This utility is mainly intended to help combine multiple \code{"ABM_Field"}
#' objects and pass them as a single argument (e.g., into \code{Game(...)}),
#' while still allowing easy flattening back to individual fields.
#'
#' @param ... Objects to be bundled or unbundled. Typically \code{"ABM_Field"}
#'   objects, but \code{Unzip()} will accept any objects and only expand those
#'   inheriting from \code{"ABM_Zip"}.
#'
#' @return
#' \itemize{
#'   \item \code{ABM_Zip()} and \code{Zip()} return an object of class
#'   \code{"ABM_Zip"} (a list).
#'   \item \code{Unzip()} returns a list containing the flattened elements.
#' }
#'
#' @examples
#' # Create ABM_Field objects
#' a <- 1
#' b <- 2
#'
#' P <- Zip(State(a), State(b))
#'
#' # Flatten
#' Unzip(State(a), State(b), P)
#' # -> list(State(a), State(b), State(a), State(b))
#'
#' # Nested Zip objects are also flattened
#' Q <- Zip(P, State(a))
#' Unzip(Q)
#' # -> list(State(a), State(b), State(a))
#'
#' @name Zip
NULL


#-------------------------------------------------------------------------------
# Internal constructor
#-------------------------------------------------------------------------------

#' Construct an ABM_Zip object (internal)
#'
#' Internal low-level constructor for objects of class \code{"ABM_Zip"}.
#'
#' @param ... Objects to be bundled.
#'
#' @return An object of class \code{"ABM_Zip"} (a list).
#' @keywords internal
ABM_Zip <- function(...) {
  x_list <- list(...)

  structure(x_list, class = "ABM_Zip")
}


#-------------------------------------------------------------------------------
# User-facing API
#-------------------------------------------------------------------------------

#' @rdname Zip
#' @export
Zip <- function(...) ABM_Zip(...)


#' @rdname Zip
#' @export
Unzip <- function(...) {
  xs <- list(...)

  out <- list()

  push <- function(x) {
    if (inherits(x, "ABM_Zip")) {
      # recursive flatten (also handles nested Zip objects)
      for (el in unclass(x)) push(el)
    } else {
      out[[length(out) + 1L]] <<- x
    }
  }

  for (x in xs) push(x)

  out
}

