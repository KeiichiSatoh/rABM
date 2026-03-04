#' Deep copy ABM and R6 objects
#'
#' Create a deep copy of an ABM or R6 object.
#'
#' This function provides a unified interface for cloning objects used in
#' rABM. For ABM and R6 objects, it performs a deep clone using
#' \code{$clone(deep = TRUE)}.
#'
#' @param x An object to copy. Supported classes are:
#'   \itemize{
#'     \item \code{ABM_Agent}
#'     \item \code{ABM_Group}
#'     \item \code{ABM_Game}
#'     \item \code{R6}
#'   }
#'
#' @details
#' For \code{ABM_Agent}, \code{ABM_Game}, and other \code{R6} objects,
#' \code{copy_obj()} calls \code{$clone(deep = TRUE)}.
#'
#' For \code{ABM_Group}, which is internally represented as a list of agents,
#' each agent is deep-cloned individually, and the group structure (including
#' element names) is preserved.
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
#' # Pseudo-code (depends on constructors):
#' # A  <- Agent(state = list(a = 1))
#' # A2 <- copy_obj(A)
#'
#' # Gr  <- Group(list(ID1 = A, ID2 = A))
#' # Gr2 <- copy_obj(Gr)
#'
#' # G  <- Game(stage = list(x = 1), group = Gr)
#' # G2 <- copy_obj(G)
copy_obj <- function(x) {
  UseMethod("copy_obj")
}

#' @rdname copy_obj
#' @export
copy_obj <- function(x) {
  UseMethod("copy_obj")
}

#' @rdname copy_obj
#' @export
copy_obj.ABM_Agent <- function(x){
  x$clone(deep = TRUE)
}

#' @rdname copy_obj
#' @export
copy_obj.ABM_Group <- function(x){
  new_group <- lapply(x, function(a) a$clone(deep = TRUE))
  names(new_group) <- names(x)
  structure(new_group, class = class(x))
}

#' @rdname copy_obj
#' @export
copy_obj.ABM_Game <- function(x){
  x$clone(deep = TRUE)
}

#' @rdname copy_obj
#' @export
copy_obj.R6 <- function(x){
  x$clone(deep = TRUE)
}


