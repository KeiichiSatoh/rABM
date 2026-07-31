#-------------------------------------------------------------------------------
# copy_obj()
#-------------------------------------------------------------------------------

#' Deep copy ABM and R6 objects
#'
#' Create a deep copy of an \code{ABM_Game} or other \code{R6} object.
#'
#' This function provides a unified interface for cloning objects used in
#' rABM, dispatching on the class of \code{x}.
#'
#' @param x An object to copy. Supported classes are:
#'   \itemize{
#'     \item \code{ABM_Game}
#'     \item \code{R6} (any other R6 object)
#'   }
#'
#' @details
#' For \code{ABM_Game} objects, \code{copy_obj()} calls
#' \code{x$clone(deep = TRUE)} and then \code{$.rebind_dynamic_fields()} on
#' the clone, since R6's default \code{clone()} does not correctly carry
#' over the dynamically added methods and active bindings (\code{act_FUN},
#' \code{stop_FUN}, \code{report_FUN}, \code{plot_FUN}, \code{active_state})
#' that \code{ABM_Game} attaches at runtime (see \code{ABM_Game}).
#'
#' For any other \code{R6} object, \code{copy_obj()} simply calls
#' \code{x$clone(deep = TRUE)}.
#'
#' Objects that do not belong to the supported classes are not handled by
#' this function and will result in an error.
#'
#' @return
#' A deep-cloned object of the same class as \code{x}.
#'
#' @export
#'
#' @examples
#' pop <- 100
#' reproduce <- function() { self$pop <- self$pop * 1.1 }
#'
#' G  <- Game(State(pop), Act(reproduce))
#' G2 <- copy_obj(G)
copy_obj <- function(x) {
  UseMethod("copy_obj")
}

#' @rdname copy_obj
#' @export
copy_obj.ABM_Game <- function(x){
  new_obj <- x$clone(deep = TRUE)
  new_obj$.rebind_dynamic_fields()
  new_obj
}

#' @rdname copy_obj
#' @export
copy_obj.R6 <- function(x){
  x$clone(deep = TRUE)
}
