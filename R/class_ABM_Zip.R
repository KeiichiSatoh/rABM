#-------------------------------------------------------------------------------
# Zip / Unzip ABM_Field objects
#-------------------------------------------------------------------------------
#' Bundle and unbundle ABM_Field objects
#'
#' \code{Zip()} groups multiple objects (typically [`ABM_Field()`] objects)
#' into a single container of class [`ABM_Zip()`].
#' \code{Unzip()} flattens such containers back into a simple list, recursively
#' expanding nested [`ABM_Zip()`] objects.
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
#'   \item If arguments are supplied with names (e.g. \code{Zip(a = State(x))}),
#'   those names are not preserved through \code{Unzip()}; only the values are
#'   retained. This is usually inconsequential for \code{"ABM_Field"} objects,
#'   which already carry their own \code{name} element.
#' }
#'
#' This utility is mainly intended to help combine multiple \code{"ABM_Field"}
#' objects and pass them as a single argument (e.g., into \code{Game(...)}),
#' while still allowing easy flattening back to individual fields.
#'
#' @param ... Objects to be bundled or unbundled. Typically \code{"ABM_Field"}
#'   objects, but neither \code{Zip()} nor \code{Unzip()} enforces this: any
#'   object is accepted, and \code{Unzip()} only recursively expands elements
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
#' # Unzip
#' Unzip(P)
#'
#' # Nested Zip objects are also flattened
#' Q <- Zip(P, State(a))
#' Unzip(Q)
#' # Returns a list of 3 ABM_Field objects, in this order:
#' #   State(a), State(b), State(a)
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
      # Note: NULL is preserved as a genuine list element, matching the
      # documented behavior ("the element is returned as-is").
      out <<- c(out, list(x))
    }
  }
  for (x in xs) push(x)
  out
}
