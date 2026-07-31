#-------------------------------------------------------------------------------
# run_Game() and its internal helpers
#
# This file consolidates run_Game() together with the internal helper
# functions it depends on (previously split across separate files):
#   - .parse_plan()          : parse a 'plan'/'nm_stop_FUN' character vector
#   - .replace_FUN_args()    : inject argument overrides into planned FUNs
#   - .create_update_FUN()   : build the per-step update_FUN from plan_list
#   - .parse_save_field()    : resolve which fields to save in the log
#   - .format_FUN_formals()  : normalize formals of user-supplied functions
#
# copy_obj() is a standalone, general-purpose utility (not specific to
# run_Game()) and is kept in its own file, copy_obj.R.
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# .parse_plan
#-------------------------------------------------------------------------------
#' Parse a plan specification into function list and arguments (internal)
#'
#' This internal helper parses a character vector \code{plan} that specifies
#' which registered functions (FUNs) to use, optionally with call-style
#' arguments.
#'
#' Each element of \code{plan} can be either:
#' \itemize{
#'   \item \code{"FUN"} (a function name), or
#'   \item \code{"FUN(arg1, arg2, ...)"} (with arguments).
#' }
#'
#' Spaces are removed before parsing. Function expressions are parsed with
#' \code{parse()}, and arguments (if any) are stored as language objects in a list.
#'
#' @param plan A character vector specifying functions to be used.
#'   Validation (character vector, non-list) is assumed to be done upstream.
#' @param field_list A data.frame that registers available functions.
#'   It must have at least columns \code{name}, \code{category}.
#'
#' @return A named list with:
#'   \describe{
#'     \item{\code{plan_list}}{A subset of \code{field_list} containing only the rows
#'     referenced by \code{plan} (in the same order as \code{plan}).}
#'     \item{\code{FUN_args}}{A list of argument lists for each parsed FUN.
#'     The names of \code{FUN_args} are the parsed function names.}
#'   }
#'
#' @details
#' If any FUN specified in \code{plan} cannot be found in \code{field_list}
#' (matched by \code{field_list$name}), the function stops with an error.
#' If a name in \code{plan} matches multiple rows in \code{field_list}, the
#' first match is used.
#'
#' @keywords internal
.parse_plan <- function(plan, field_list){

  # remove spaces
  plan <- stringr::str_remove_all(plan, pattern = " ")

  n_plan <- length(plan)

  # parse FUN arguments
  FUN_args <- vector("list", n_plan)

  for(i in seq_len(n_plan)){
    parsed_obs <- tryCatch(
      parse(text = plan[i])[[1]],
      error = function(e) {
        stop(sprintf("Invalid expression: %s", plan[i]), call. = FALSE)
      }
    )

    if (is.symbol(parsed_obs)) {
      names(FUN_args)[i] <- as.character(parsed_obs)
      FUN_args[[i]] <- list()
    } else {
      names(FUN_args)[i] <- as.character(parsed_obs[[1]])
      FUN_args[[i]] <- as.list(parsed_obs[-1])
    }
  }

  # locate functions in field_list
  idx <- rep(NA_integer_, n_plan)

  for (i in seq_len(n_plan)) {
    FUN_name <- names(FUN_args)[i]
    w <- which(field_list$name == FUN_name)
    idx[i] <- if (length(w)) w[1] else NA_integer_
  }

  # stop if there is NA
  if (anyNA(idx)) {
    missing_i <- which(is.na(idx))
    missing_txt <- plan[missing_i]
    stop(
      paste0("The following FUN was not found: ",
             paste(missing_txt, collapse = ", ")),
      call. = FALSE
    )
  }

  # output
  list(
    plan_list = field_list[idx, , drop = FALSE],
    FUN_args  = FUN_args
  )
}


#-------------------------------------------------------------------------------
# .replace_FUN_args
#-------------------------------------------------------------------------------
#' Replace default arguments of planned functions (internal)
#'
#' This internal helper injects argument values specified in \code{FUN_args}
#' into the formals (default arguments) of functions referenced by \code{plan_list},
#' and then updates those functions in the given \code{ABM_Game} object via
#' \code{G$.replace()}.
#'
#' The function assumes that \code{plan_list} and \code{FUN_args} were created
#' by \code{.parse_plan()} so that their orders correspond one-to-one.
#'
#' @param G An \code{ABM_Game} object to be updated.
#' @param FUN_args A list of arguments for each planned function.
#'   Each element must be a named list of language objects (parsed from \code{plan}).
#' @param plan_list A data.frame describing planned functions, typically the
#'   \code{plan_list} component returned by \code{.parse_plan()}.
#'   It must contain at least columns \code{name} and \code{category}.
#'
#' @return The updated \code{ABM_Game} object \code{G}.
#'
#' @details
#' For each row of \code{plan_list}, if the corresponding \code{FUN_args[[i]]}
#' is non-empty, the function:
#' \enumerate{
#'   \item Retrieves the target function \code{G[[name]]}.
#'   \item Replaces the default values in \code{formals(FUN)} using the names in
#'   \code{FUN_args[[i]]}.
#'   \item Wraps the modified function in an \code{ABM_Field} (using the same
#'   \code{category} recorded in \code{plan_list}) and writes it back via
#'   \code{G$.replace()}.
#' }
#'
#' All arguments in \code{FUN_args} are expected to be \strong{named}.
#' If unknown argument names (not present in the target function's formals)
#' are supplied, the function errors.
#'
#' @keywords internal
.replace_FUN_args <- function(G, FUN_args, plan_list){

  stopifnot("Length mismatch between 'plan_list' and 'FUN_args'." =
              nrow(plan_list) == length(FUN_args))

  for(i in seq_len(nrow(plan_list))){

    args <- FUN_args[[i]]
    if (!length(args)) next

    # keep only named args (positional args are ignored for safety)
    nms <- names(args)
    if (is.null(nms) || anyNA(nms) || any(nms == "")) {
      stop("All FUN arguments in 'plan' must be named (e.g., fun(a=1, b=2)).")
    }

    name <- plan_list$name[i]
    FUN  <- G[[name]]

    fml <- formals(FUN)
    bad <- setdiff(nms, names(fml))
    if (length(bad)) {
      stop("Unknown argument(s) for ", name, ": ", paste(bad, collapse = ", "))
    }

    fml[nms] <- args
    formals(FUN) <- fml

    # G$.replace() requires its '...' to be ABM_Field object(s) (it runs
    # Unzip() and checks inherits(x_i, "ABM_Field") on every element), so
    # the modified function must be wrapped in one, tagged with the same
    # category it already had in 'plan_list' (e.g. "act_FUN", "stop_FUN").
    new_field <- ABM_Field(x = FUN, name = name, category = plan_list$category[i])
    G$.replace(new_field)
  }

  G
}


#-------------------------------------------------------------------------------
# .create_update_FUN
#-------------------------------------------------------------------------------
#' Create update_FUN from plan_list using language objects (no parse) (internal)
#'
#' This internal helper constructs an \code{update_FUN} function from a
#' flattened \code{plan_list} (as produced by \code{.parse_plan()}) by
#' building language objects via \code{call()}/\code{substitute()} (instead
#' of string generation + \code{parse()}). Every row of \code{plan_list}
#' must have category \code{"act_FUN"}; each is translated into a call of
#' the form \code{G[[name]]()}, optionally wrapped in \code{tryCatch()}.
#'
#' @param plan_list A data.frame with at least columns \code{name} and
#'   \code{category}, ordered by execution (as returned by
#'   \code{.parse_plan()}). Every row's \code{category} must be
#'   \code{"act_FUN"}; any other value raises an error.
#' @param add_tryCatch Logical; if \code{TRUE}, wrap each function call in
#'   \code{tryCatch()} so that an error inside one \code{act_FUN} is reported
#'   via \code{message()} (prefixed with the field name) and does not stop
#'   the rest of the update cycle. If \code{FALSE}, errors propagate normally.
#'
#' @return A function \code{update_FUN(G)} whose body calls each planned
#'   \code{act_FUN} field on \code{G} in order, as \code{G[[name]]()}.
#'   \code{act_FUN} fields are already bound to \code{self}/\code{private}
#'   by \code{ABM_Game$.add_method()} (see \code{class_ABM_Game.R}), so no
#'   arguments need to be passed at the call site.
#'
#' @keywords internal
.create_update_FUN <- function(plan_list, add_tryCatch = TRUE) {
  stopifnot(is.data.frame(plan_list))
  if (!nrow(plan_list)) {
    f <- function() {}
    body(f) <- quote({})
    return(f)
  }

  #===========================================================================
  # Builders
  #===========================================================================

  # tryCatch(<expr>, error = function(e){ <body>; NULL })
  wrap_tryCatch <- function(expr, msg_prefix = NULL) {
    handler_body <- if (is.null(msg_prefix)) {
      quote({ message(e); NULL })
    } else {
      substitute({ message(paste0(PREFIX, e)); NULL }, list(PREFIX = msg_prefix))
    }
    handler_fun <- eval(substitute(function(e) BODY, list(BODY = handler_body)))
    call("tryCatch", expr, error = handler_fun)
  }

  # act_FUN expression: G[[fname]]() (optionally tryCatch)
  build_global_expr <- function(fname) {
    expr <- substitute(G[[FUN]](), list(FUN = fname))
    if (add_tryCatch) {
      wrap_tryCatch(expr, msg_prefix = paste0("error occured for ", shQuote(fname), ": "))
    } else {
      expr
    }
  }

  #===========================================================================
  # Main loop
  #===========================================================================

  parts <- list()
  k <- 0L

  for (j in seq_len(nrow(plan_list))) {
    cat_j <- plan_list$category[j]

    if (identical(cat_j, "act_FUN")) {
      fname <- plan_list$name[j]
      k <- k + 1L
      parts[[k]] <- build_global_expr(fname)
      next
    }

    stop("Unsupported category in plan_list: ", cat_j)
  }

  #===========================================================================
  # Assemble update_FUN
  #===========================================================================

  update_FUN <- function(G) {}
  body(update_FUN) <- as.call(c(list(as.name("{")), parts))
  update_FUN
}


#-------------------------------------------------------------------------------
# .parse_save_field
#-------------------------------------------------------------------------------
#' Parse field specifiers for snapshot/log saving
#'
#' @description
#' Internal helper that resolves which fields should be included in each
#' saved simulation snapshot (see \code{run_Game(fields_to_save = ...)}).
#'
#' If \code{fields_to_save} is \code{NULL}, all \code{"state"} and
#' \code{"active_state"} fields registered in \code{field_list} are returned
#' by default. Otherwise, each element of \code{fields_to_save} (with spaces
#' removed) is validated against \code{field_list$name} and returned as-is.
#'
#' @param fields_to_save A character vector of field names to save. Spaces
#'   are removed internally. If \code{NULL}, the default described above is used.
#' @param field_list A \code{data.frame} that defines available fields, as
#'   returned by \code{G$.get_flist()}. Must include at least columns
#'   \code{name} and \code{category}.
#'
#' @return
#' A character vector of field names to include in each saved snapshot.
#'
#' @details
#' This function is designed to be called after input validation has been
#' performed elsewhere, so it keeps checks minimal and focuses on parsing
#' and existence checks against \code{field_list$name}.
#'
#' @keywords internal
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
    stop(
      "The following field name(s) were not found in 'G': ",
      paste(fields_to_save[is.na(looked)], collapse = ", ")
    )
  }

  # output
  fields_to_save
}


#-------------------------------------------------------------------------------
# .format_FUN_formals
#-------------------------------------------------------------------------------
#' Normalize function formals for ABM execution
#'
#' This internal helper standardizes the formal arguments of a function.
#' When \code{include_self = TRUE}, any existing \code{self} argument is
#' dropped and re-added as the first formal, with an unevaluated \code{self}
#' symbol as its default value (i.e. the default is the promise
#' \code{self = self}, not a concrete value). When \code{include_self = FALSE}
#' (the default), \code{fun} is returned unmodified.
#'
#' @param fun A function whose formal arguments are to be reformatted.
#' @param include_self Logical; if \code{TRUE}, ensure \code{fun} has a
#'   leading \code{self} formal as described above. Defaults to \code{FALSE}.
#'
#' @return \code{fun}, with formals modified as described above when
#'   \code{include_self = TRUE}; unmodified otherwise.
#'
#' @details
#' Because the injected \code{self} formal's default is the unevaluated
#' symbol \code{self} rather than a concrete value, any caller of the
#' resulting function must always pass \code{self} explicitly (e.g.
#' \code{fn(self = G)}). Calling the result with no arguments will raise a
#' "promise already under evaluation" error, since the default would try to
#' resolve itself. \code{run_Game()} no longer relies on this function for
#' its \code{nm_stop_FUN} handling (it calls the already environment-bound
#' \code{G[[nm]]} directly instead); it is retained here for other internal
#' callers (e.g. \code{.create_update_FUN()}) that may still depend on it.
#'
#' @keywords internal
.format_FUN_formals = function(fun, include_self = FALSE) {
  f <- formals(fun)
  if (is.null(f)) f <- pairlist()

  # drop existing G/E (if any)
  nms <- names(f)

  # include self?
  if(isTRUE(include_self)){
    if (!is.null(nms)) {
      f <- f[!nms %in% c("self")]
    }
    formals(fun) <- c(alist(self = self), f)
  }

  fun
}


#-------------------------------------------------------------------------------
# run_Game
#-------------------------------------------------------------------------------

#' Run the ABM simulation
#'
#' @description
#' Runs a simulation using an \code{ABM_Game} object by constructing an
#' update function from a user-defined execution plan (\code{plan}) and
#' repeatedly applying it. The simulation proceeds step by step and can
#' include logging, a custom stop condition, and RDS file output.
#'
#' @param G An \code{ABM_Game} object representing the simulation state and structure.
#' @param plan A character vector giving the sequence of \code{act_FUN}
#' fields (registered in \code{G} via \code{Act()}) to run at each time
#' step, in the exact order supplied. Each element is one of:
#' \itemize{
#'   \item a bare field name, e.g. \code{"add_money"}, which runs that
#'   \code{act_FUN} with its currently stored default arguments; or
#'   \item a field name with call-style arguments, e.g.
#'   \code{"add_money(b = 2)"}, which runs that \code{act_FUN} with the
#'   given argument(s) overriding its defaults for this run only (the
#'   original \code{G} passed in is never modified -- see Details).
#' }
#' If \code{plan} has more than one element, every element is executed, in
#' the order given, at every time step -- e.g.
#' \code{plan = c("select_agent", "add_money")} runs \code{select_agent}
#' and then \code{add_money} at each step. Every element must name an
#' \code{act_FUN} field already present in \code{G}; other field categories
#' (\code{state}, \code{active_state}, \code{stop_FUN}, ...) are not valid
#' entries here.
#' @param nm_stop_FUN A character scalar naming a \code{stop_FUN} field
#' already registered in \code{G} (see \code{Stop()}) to use as the stopping
#' condition. If supplied, \code{times} is ignored entirely and the
#' simulation runs until that \code{stop_FUN} returns \code{TRUE}. If
#' \code{NULL} (the default), the simulation instead stops after exactly
#' \code{times} steps.
#' @param times Integer specifying the number of steps to run the simulation.
#' Only used when \code{nm_stop_FUN} is \code{NULL}; ignored otherwise.
#' Defaults to \code{1}.
#' @param save_log Logical; if \code{TRUE}, saves a snapshot of the selected fields
#' at each saved time step. Defaults to \code{TRUE}.
#' @param save_interval Positive integer; when \code{save_log = TRUE}, a snapshot
#' is saved once every \code{save_interval} steps. Defaults to \code{1} (every step).
#' @param fields_to_save A character vector of field names to include in each saved
#' snapshot. If \code{NULL}, all \code{"state"} and \code{"active_state"} fields
#' registered in \code{G} are saved. Defaults to \code{NULL}.
#' @param seed Optional integer; if \code{NULL}, a seed is generated automatically
#' and recorded (see Details).
#' @param add_tryCatch Logical; if \code{TRUE}, each function call is wrapped in \code{tryCatch()}
#' to allow simulations to continue in case of errors. Defaults to \code{TRUE}.
#' @param return_update_FUN Logical; if \code{TRUE}, the body of the final \code{update_FUN} is saved in
#' \code{G$notes$update_FUN_used}. Defaults to \code{FALSE}.
#' @param saveRDS_inbetween Logical; if \code{TRUE}, saves the entire \code{G} object as an RDS file
#' at every simulation step. Defaults to \code{FALSE}.
#' @param verbose Logical; if \code{TRUE}, prints progress messages (the plan,
#' the stop condition, the current time step, and elapsed time) during the run.
#' Defaults to \code{TRUE}.
#' @param RDS_file_name A character scalar specifying the file name for saving RDS
#' objects if \code{saveRDS_inbetween = TRUE}. Defaults to \code{"G_temp.rds"}.
#' @param beep Logical; if \code{TRUE}, plays a notification sound via
#' \code{beepr::beep()} once the simulation finishes. Defaults to \code{FALSE}.
#'
#' @return The updated \code{ABM_Game} object after running the simulation.
#' If \code{return_update_FUN = TRUE}, the update function used is stored as a character
#' vector in \code{G$notes$update_FUN_used}.
#'
#' @details
#' \code{run_Game()} proceeds through the following steps:
#' \enumerate{
#'   \item \code{G} is deep-cloned internally (via \code{copy_obj()}) into a
#'   fresh object; the \code{G} the caller passed in is never modified, so
#'   it can safely be reused as a starting point for other runs (as in the
#'   examples below).
#'   \item If any element of \code{plan} (or \code{nm_stop_FUN}) includes
#'   call-style arguments, e.g. \code{"add_money(b = 2)"}, those arguments
#'   are used to override that field's defaults on the internal clone only.
#'   \item An \code{update_FUN} is built from \code{plan}, to be run once
#'   per time step, executing each named \code{act_FUN} in the given order.
#'   \item If \code{nm_stop_FUN} is supplied, that \code{stop_FUN} field is
#'   used as the stopping rule and \code{times} is ignored; otherwise, a
#'   stopping rule based on \code{times} steps is generated automatically.
#'   \item \code{update_FUN} is called repeatedly until the stopping
#'   condition is met.
#' }
#'
#' If \code{add_tryCatch = TRUE}, runtime errors in user-defined functions are caught
#' and logged as warnings, and simulation continues.
#'
#' If \code{seed} is \code{NULL}, a seed is generated automatically and
#' passed to \code{set.seed()} before the run, so that any randomness in
#' \code{act_FUN}s is reproducible from the returned object; the actual seed
#' used is recorded in \code{G$notes$seed} regardless of whether it was
#' supplied or auto-generated.
#'
#' To examine the simulation runtime, the total elapsed time is printed and also saved in
#' \code{G$notes$simulation_took}.
#'
#' @examples
#' # A simple example: agents accumulate money over several time steps.
#' money          <- 1:5
#' selected_agent <- 1:5
#' add_money <- function(b = 1) {
#'   self$money[self$selected_agent] <- self$money[self$selected_agent] + b
#' }
#' select_agent <- function() {
#'   self$selected_agent <- sample(1:5, size = 2)
#' }
#' G <- Game(State(money), State(selected_agent), Act(add_money), Act(select_agent))
#'
#' # Each step: pick two agents, then give them money.
#' G2 <- run_Game(G = G, plan = c("select_agent", "add_money"), times = 3)
#'
#' # Skip select_agent entirely -- since 'selected_agent' still holds its
#' # initial value (all 5 agents), everyone gets money each step.
#' G3 <- run_Game(G = G, plan = "add_money", times = 3)
#'
#' # Override add_money's default argument for this run only; G itself is
#' # untouched, so it can still be reused as above.
#' G4 <- run_Game(G = G, plan = c("select_agent", "add_money(b = 2)"), times = 3)
#'
#' # Use a custom stop_FUN instead of a fixed number of steps. Note that
#' # 'times' is ignored once 'nm_stop_FUN' is supplied.
#' wealthy_enough <- function() { sum(self$money) >= 100 }
#' G5 <- Game(
#'   State(money), State(selected_agent),
#'   Act(add_money), Act(select_agent), Stop(wealthy_enough)
#' )
#' G6 <- run_Game(
#'   G = G5, plan = c("select_agent", "add_money"),
#'   nm_stop_FUN = "wealthy_enough"
#' )
#'
#' # For long runs, turn off console output and logging for speed, and
#' # inspect the generated update_FUN afterwards.
#' G7 <- run_Game(
#'   G = G, plan = "add_money", times = 100,
#'   verbose = FALSE, save_log = FALSE, return_update_FUN = TRUE
#' )
#' cat(G7$notes$update_FUN_used, sep = "\n")
#'
#' @importFrom beepr beep
#' @export
run_Game <- function(G,
                   plan,
                   nm_stop_FUN = NULL,
                   times = 1,
                   save_log = TRUE,
                   save_interval = 1,
                   fields_to_save = NULL,
                   seed = NULL,
                   add_tryCatch = TRUE,
                   return_update_FUN = FALSE,
                   saveRDS_inbetween = FALSE,
                   verbose = TRUE,
                   RDS_file_name = "G_temp.rds",
                   beep = FALSE) {
  # deep clone the G
  stopifnot(inherits(G, "ABM_Game"))
  G <- copy_obj(G)

  # validate plan
  if (is.list(plan)) plan <- unlist(plan, use.names = FALSE)
  stopifnot("'plan' must be a character vector." =
              is.character(plan) && is.vector(plan) && !is.list(plan))

  # validate nm_stop_FUN
  stopifnot("'nm_stop_FUN' must be a character of length 1." =
              is.null(nm_stop_FUN) || (is.character(nm_stop_FUN) && length(nm_stop_FUN) == 1L))

  # validate save_interval
  stopifnot(
    "'save_interval' must be a positive integer." =
      is.numeric(save_interval) && length(save_interval) == 1L &&
      !is.na(save_interval) && save_interval >= 1 && save_interval %% 1 == 0
  )
  save_interval <- as.integer(save_interval)

  # validate saveRDS
  if (isTRUE(saveRDS_inbetween)) {
    stopifnot("'RDS_file_name' must be a character of length 1." =
                is.character(RDS_file_name) && length(RDS_file_name) == 1L && nzchar(RDS_file_name))
  }

  # ensure notes exists
  if (is.null(G$notes)) G$notes <- list()

  # Retrieve the field_list
  field_list <- G$.get_flist()

  #----- parse 'plan'-------------------
  parse_plan_out <- .parse_plan(plan = plan, field_list = field_list)
  plan_list <- parse_plan_out$plan_list
  FUN_args <- parse_plan_out$FUN_args

  # Check if FUNs in the plan_list are either "global_FUN", "act_FUN", or "select_FUN".
  stopifnot(
    "All elements in 'plan' must refer to 'act_FUN' fields." =
      all(plan_list$category == "act_FUN"))


  #----- Replace FUN if required (in case length(FUN_args[[i]]))--------
  G <- .replace_FUN_args(G = G, FUN_args = FUN_args, plan_list = plan_list)

  #----- create a body of update_FUN based on plan
  update_FUN <- .create_update_FUN(plan_list = plan_list,
                                   add_tryCatch = add_tryCatch)

  ## announce the plan
  if(isTRUE(verbose)){
    cat("[plan]", "\n")
    cat(paste0(plan, collapse = " -> "))
    cat("\n","\n")
  }

  #------ stop FUN------------------------
  if (!is.null(nm_stop_FUN)) {
    parse_stop_out <- .parse_plan(plan = nm_stop_FUN, field_list = field_list)
    stopFUN_list <- parse_stop_out$plan_list
    stopFUN_args <- parse_stop_out$FUN_args

    stopifnot(
      "'nm_stop_FUN' must refer to a 'stop_FUN' field in G." =
        all(stopFUN_list$category == "stop_FUN")
    )

    G <- .replace_FUN_args(G = G, FUN_args = stopFUN_args, plan_list = stopFUN_list)

    nm <- stopFUN_list$name
    stopifnot("'nm_stop_FUN' must refer to a function in G." = is.function(G[[nm]]))

    # G[[nm]] is already re-bound to self/private by .add_method() (see
    # ABM_Game's initialize()/.add()), so it can be called directly here --
    # no need to reconstruct its body/formals as before. Reconstructing a
    # fresh function from body()/formals() alone would have dropped its
    # original enclosing environment, silently breaking any self$/private$
    # references or closures over the user's own helper functions.
    stop_FUN <- G[[nm]]

    if(isTRUE(verbose)){
      cat("[stop_FUN]","\n")
      cat(nm_stop_FUN, "\n")
    }
  } else {
    stopifnot(
      "'times' must be a positive integer." =
        is.numeric(times) && length(times) == 1L && !is.na(times) && times >= 1 && times %% 1 == 0
    )
    times <- as.integer(times)
    sim_time <- G$time + times
    stop_FUN <- function() G$time >= sim_time

    if(isTRUE(verbose)){
      cat("[stop_FUN]","\n")
      cat(paste0("stop times at ", sim_time, "\n"))
      cat("\n")
    }
  }

  # seed (set once)
  # sample.int() draws from the RNG's own auto-initialized state (seeded from
  # time + process ID the first time R's RNG is touched), so it yields a
  # valid integer in [1, .Machine$integer.max] without the Y2038-style
  # overflow risk of as.integer(Sys.time()).
  if (is.null(seed)) seed <- sample.int(.Machine$integer.max, 1L)
  set.seed(seed = seed)
  G$notes$seed <- seed

  #---- save_log -------
  if (isTRUE(save_log)) {
    # validate and parse the fields to save
    parsed_save_field <- .parse_save_field(fields_to_save = fields_to_save,
                                           field_list = field_list)

    # create the index
    log_idx <- 1L
    t_init <- G$time   # initial time

    # estimate number of saves; if nm_stop_FUN is used, 'times' may not reflect the true steps
    n_save_est <- if (is.null(nm_stop_FUN)) {
      (as.integer(times) %/% save_interval) + 2L
    } else {
      1000L
    }

    log <- vector("list", n_save_est)
    log_time <- rep.int(NA_integer_, n_save_est)

    # save the current snapshot
    log[[log_idx]] <- G$.snapshot(field_names = parsed_save_field,
                                  add_tryCatch = add_tryCatch)
    log_time[[log_idx]] <- G$time

    if(isTRUE(verbose)){
      cat("The initial values at time ", G$time, " were saved.\n", sep = "")
    }
  }

  # Ready to run
  if(isTRUE(verbose)){
    cat("\n")
    cat("Ready to run......\n")
    cat(paste0("   start time  : ", G$time, "\n"))
  }

  # start_time
  start_time <- Sys.time()

  # implement the update (CORE PART!!)
  repeat {
    # check the end condition(put here to prepare for the case where initial condition already meet the stop-condition)
    if (isTRUE(stop_FUN())) break

    # update time
    G$time <- G$time + 1L

    if(isTRUE(verbose)) cat(paste0("   current time: ", G$time, "\n"))

    update_FUN(G = G)

    # save by interval
    if (isTRUE(save_log)) {
      if (((G$time - t_init) %% save_interval) == 0L) {
        log_idx <- log_idx + 1L
        log[[log_idx]] <- G$.snapshot(field_names = parsed_save_field,
                                      add_tryCatch = add_tryCatch)
        log_time[[log_idx]] <- G$time

        # save RDS
        if (isTRUE(saveRDS_inbetween)) saveRDS(G, file = RDS_file_name)

        # extend holders if necessary
        if (log_idx >= length(log)) {
          log <- c(log, vector("list", n_save_est))
          log_time <- c(log_time, rep.int(NA_integer_, n_save_est))
        }
      }
    }
    # save by interval----

  } #//----repeat-------//

  # end_time
  end_time <- Sys.time()

  # Finished
  if(isTRUE(verbose)){
    cat("Finished at time", G$time, "\n\n")
  }

  # implementation time（in seconds）
  time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))

  hours <- floor(time_taken / 3600)
  minutes <- floor((time_taken %% 3600) / 60)
  seconds <- floor(time_taken %% 60)
  milliseconds <- round((time_taken %% 1) * 1000)

  time_hms <- sprintf("%02d:%02d:%02d.%03d", hours, minutes, seconds, milliseconds)

  if(isTRUE(verbose)){
    cat(paste("Simulation took", time_hms, "(hh:mm:ss.mmm)\n"))
  }

  # write simulation time to 'notes'
  G$notes$simulation_took <- time_hms

  # trim log
  if (isTRUE(save_log)) {
    log <- log[seq_len(log_idx)]
    log_time <- log_time[seq_len(log_idx)]
    names(log) <- paste0("t", log_time)
    G$log <- log
  }

  # record update_FUN if requested
  if (isTRUE(return_update_FUN)) {
    G$notes$update_FUN_used <- deparse(body(update_FUN))
  }

  # beep?
  if(isTRUE(beep)){
    beep()
  }

  G
}
