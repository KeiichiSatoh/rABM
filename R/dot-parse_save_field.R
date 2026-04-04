#' Parse field specifiers for snapshot/log saving
#'
#' @description
#' Internal helper to parse a character vector that specifies which fields should be saved
#' (e.g., for snapshot/logging). Each element of \code{fields_to_save} can be either:
#' \itemize{
#'   \item \code{"field"}: a field name (must be unique across groups), or
#'   \item \code{"group:field"}: a field name qualified by a group name.
#' }
#' Spaces in \code{fields_to_save} are removed before parsing.
#'
#' The function validates that each specified field exists in \code{field_list}.
#' If a non-qualified \code{"field"} matches multiple groups, an error is raised
#' and the user is asked to specify the group.
#'
#' If \code{fields_to_save} is \code{NULL}, the function returns default targets:
#' global fields in categories \code{"stage"} and \code{"active_state"}, and
#' group-level fields in categories \code{"state"} and \code{"active_state"}.
#'
#' Depending on the input, \code{global_field_names}, \code{group_names}, and
#' \code{group_field_names} may be \code{NULL}.

#' @param fields_to_save A character vector of field specifiers. Each element is either
#'   \code{"field"} or \code{"group:field"}. Spaces are removed internally.
#' @param field_list A data.frame (or tibble) that defines available fields.
#'   Must include at least columns \code{name} and \code{group}.
#'   \code{group} should be \code{NA} for global fields.
#'
#' @return
#' A list with the following elements:
#' \describe{
#'   \item{global_field_names}{Character vector of global field names (where \code{group} is \code{NA}).}
#'   \item{group_names}{Character vector of group names that appeared in \code{fields_to_save}.}
#'   \item{group_field_names}{A list of character vectors. Each element contains the field names
#'     to be saved for the corresponding group in \code{group_names}.}
#' }
#'
#' @details
#' This function is designed to be called after input validation has been performed elsewhere,
#' so it keeps checks minimal and focuses on parsing and existence checks.
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' # (internal) Example of expected inputs:
#' field_list <- data.frame(
#'   name  = c("time", "x", "y"),
#'   group = c(NA, "agents", "agents"),
#'   stringsAsFactors = FALSE
#' )
#' .parse_save_field(c("time", "agents:x", "agents:y"), field_list)
#'

.parse_save_field <- function(fields_to_save, field_list){
  # in case field_to_save is NULL
  if(is.null(fields_to_save)){
    field_names <- field_list$name[field_list$category %in% c("state", "active_state")]
    return(field_names)
  }

  # remove spaces
  fields_to_save <- stringr::str_remove_all(fields_to_save, pattern = " ")

  # match
  looked <- match(fields_to_save, field_list$name)
  if(any(is.na(looked))){
    stop(c("The following field name(s) were not found in 'G':", paste0(looked[is.na(looked)], collapse = ", ")))
  }

  # output
  fields_to_save
}
