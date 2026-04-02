#' Run the ABM simulation
#'
#' @description
#' Runs a simulation using an \code{ABM_G} object, either by executing a predefined update
#' function or by constructing an update function from a user-defined execution plan.
#' The simulation proceeds step by step and can include logging, custom stop conditions,
#' and RDS file output.
#'
#' @param G An \code{ABM_Game} object representing the simulation state and structure.
#' @param plan A character vector specifying the sequence of functions to execute
#' in each simulation step. Can include agent-level functions using the syntax \code{agent:fun(arg=val)}.
#' Defaults to \code{NULL}.
#' @param update_FUN_name A character scalar specifying the name of an \code{update_FUN}
#' already stored in \code{G}. If \code{NULL}, an update function is generated from \code{plan}.
#' @param stop_FUN_name A character scalar indicating the name of a stop function stored in \code{G}.
#' If \code{NULL}, the simulation stops after \code{times} steps.
#' @param times Integer specifying the number of steps to run the simulation
#' (used only if \code{stop_FUN_name} is not provided). Defaults to \code{1}.
#' @param save_log Logical; if \code{TRUE}, logs the state at each time step using \code{G$.save()}.
#' Defaults to \code{TRUE}.
#' @param seed Optional integer; if \code{NULL}, the seed is derived from \code{Sys.time()}.
#' @param add_tryCatch Logical; if \code{TRUE}, each function call is wrapped in \code{tryCatch()}
#' to allow simulations to continue in case of errors. Defaults to \code{TRUE}.
#' @param return_update_FUN Logical; if \code{TRUE}, the body of the final \code{update_FUN} is saved in
#' \code{G$notes$update_FUN_used}. Defaults to \code{FALSE}.
#' @param temp_E Logical; if \code{TRUE}, creates a temporary environment \code{E} passed
#' to each function during simulation. Defaults to \code{TRUE}.
#' @param saveRDS_inbetween Logical; if \code{TRUE}, saves the entire \code{G} object as an RDS file
#' at every simulation step. Defaults to \code{FALSE}.
#' @param RDS_file_name A character scalar specifying the file name for saving RDS
#' objects if \code{saveRDS_inbetween = TRUE}. Defaults to \code{"G_temp.rds"}.
#'
#' @return The updated \code{ABM_G} object after running the simulation.
#' If \code{return_update_FUN = TRUE}, the update function used is stored as a character
#' vector in \code{G$notes$update_FUN_used}.
#'
#' @details
#' The simulation proceeds by either calling a registered \code{update_FUN} stored in \code{G},
#' or generating a new one based on a user-defined \code{plan}.
#' Each element of \code{plan} can be a function name (e.g., \code{"fun_name"})
#' or a namespaced string specifying the target agent group (e.g., \code{"agents:fun_name"}).
#'
#' If multiple elements are provided in \code{plan}, they are executed sequentially
#' in the specified order at each time step. For example,
#' \code{c("children:get_older", "parents:get_older")} applies the \code{get_older}
#' function to both children and parents agents in order.
#'
#' If no \code{select_FUN} is provided prior to an \code{act_FUN}, a default
#' \code{[select_all]} is automatically inserted for convenience.
#' When using \code{plan}, each entry can reference a global or agent-level function,
#' and arguments can be passed via standard R function syntax. Agent-level functions must be
#' preceded by a valid \code{select_FUN}, which selects the agents to which the function applies.
#' If no \code{select_FUN} is present, a default \code{[select_all]} is added automatically.
#'
#' If \code{add_tryCatch = TRUE}, runtime errors in user-defined functions are caught
#' and logged as warnings, and simulation continues.
#'
#' To examine the simulation runtime, the total elapsed time is printed and also saved in
#' \code{G$notes$simulation_took}.
#'
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
  G <- G$clone(deep = TRUE)

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
    "'plan' must be 'act_FUN'." =
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
    G <- .replace_FUN_args(G = G, FUN_args = stopFUN_args, plan_list = stopFUN_list)

    nm <- stopFUN_list$name
    stopifnot("nm_stop_FUN must refer to a function in G." = is.function(G[[nm]]))

    stop_FUN <- function(){}
    body(stop_FUN) <- body(G[[nm]])
    formals(stop_FUN) <- formals(.format_FUN_formals(fun = G[[nm]],
                                                     include_self = TRUE))

    cat("[stop_FUN]","\n")
    cat(nm_stop_FUN, "\n")
  } else {
    stopifnot(
      "times must be a positive integer." =
        is.numeric(times) && length(times) == 1L && !is.na(times) && times >= 1 && times %% 1 == 0
    )
    times <- as.integer(times)
    sim_time <- G$time + times
    stop_FUN <- function(self = self) { self$time >= sim_time }

    cat("[stop_FUN]","\n")

    cat(paste0("stop times at ", sim_time, "\n"))
    cat("\n")
  }

  # seed (set once)
  if (is.null(seed)) seed <- as.integer(Sys.time())
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

    cat("The initial values at time ", G$time, " were saved.\n", sep = "")
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
    if (isTRUE(stop_FUN(self = G))) break

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
    beepr::beep()
  }

  G
}


