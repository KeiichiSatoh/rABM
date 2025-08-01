% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify_G.R
\name{modify_G}
\alias{modify_G}
\title{Modify fields in an ABM_G object}
\usage{
modify_G(
  G,
  field_name,
  method = c("add_agents", "add_stage", "add_global_FUN", "add_select_FUN",
    "add_stop_FUN", "add_update_FUN", "add_summary_FUN", "add_plot_FUN",
    "add_active_binding", "rename", "replace", "delete"),
  new_obj = NULL,
  new_field_name = NULL,
  deep = TRUE
)
}
\arguments{
\item{G}{An object of class \code{ABM_G}, created by \code{setABM()}.}

\item{field_name}{A string indicating the name of the field to modify, delete, or rename.}

\item{method}{A string specifying the operation to perform.
One of \code{"add_agents"}, \code{"add_stage"}, \code{"add_global_FUN"}, \code{"add_select_FUN"},
\code{"add_stop_FUN"}, \code{"add_update_FUN"},
\code{"add_summary_FUN"}, \code{"add_plot_FUN"},
\code{"add_active_binding"},
\code{"rename"}, \code{"replace"}, or \code{"delete"}.}

\item{new_obj}{The object to assign to the field (e.g., a set of 'ABM_Agents',
vector, matrix, data.frame, list or a function). Required for \code{"add_*"} and
\code{"replace"} methods.}

\item{new_field_name}{A string specifying the new name of the field,
used when \code{method = "rename"}.}

\item{deep}{Logical. If \code{TRUE} (default),
the function will operate on a deep clone of \code{G} and return the modified
object. If \code{FALSE}, \code{G} will be modified in place and \code{NULL}
will be returned.}
}
\value{
If \code{deep = TRUE}, returns a modified \code{ABM_G} object.
If \code{FALSE}, returns \code{NULL} and modifies the object in place.
}
\description{
This function allows users to modify, rename, delete, or add fields
(i.e., stage, agents, global_FUN/select_FUN/update_FUN/stop_FUN/summary_FUN/plot_FUN, and active bindings)
to an \code{ABM_G} object.
}
\details{
\itemize{
\item For \code{method = "delete"}, the specified field will be removed.
\item For \code{method = "rename"}, the field will be renamed.
\item For \code{method = "replace"}, the specified field will be replaced.
\item For \code{method = "add_*"}, a new field will be added to the object.
}

\strong{Note}: If the user wants to modify the contents of \code{agents}, use
\code{\link[=modify_agents]{modify_agents()}} instead.
}
\examples{
G <- setABM(agents = 2, stage = data.frame(age = c(1, 2)),
            global_FUN = function(){NULL},
            select_FUN = function(){NULL},
            update_FUN = function(){NULL},
            stop_FUN = function(){TRUE},
            active_binding_field = function(){1})

# Add a new stage
G2 <- modify_G(G, field_name = "stage2", method = "add_stage", new_obj = matrix(0, 2, 2))

# Rename an existing field
G2 <- modify_G(G, field_name = "stage", method = "rename", new_field_name = "stage_renamed")

# Replace a function
new_FUN <- function() print("new")
G2 <- modify_G(G, field_name = "global_FUN", method = "replace", new_obj = new_FUN)

}
\seealso{
\code{\link[=modify_agents]{modify_agents()}}
}
