#' Validate a field name
#'
#' Check whether \code{field_name} is a valid single, non-empty character string.
#' This function is intended for internal use to ensure that field identifiers
#' used in ABM-related objects and methods are properly specified.
#'
#' @param field_name A character string representing a field name.
#'
#' @return The validated \code{field_name} (invisibly).
#'
#' @details
#' The function verifies that:
#' \itemize{
#'   \item \code{field_name} is of type character,
#'   \item its length is exactly one,
#'   \item it is not \code{NA},
#'   \item it is not an empty string.
#' }
#' An error is thrown if any of these conditions are not satisfied.
#'
#' @keywords internal
#'

.validate_field_name <- function(field_name) {
  stopifnot(
    "'field_name' must be a single non-empty character string." =
      is.character(field_name) && length(field_name) == 1L &&
      !is.na(field_name) && nzchar(field_name)
  )
  invisible(field_name)
}
