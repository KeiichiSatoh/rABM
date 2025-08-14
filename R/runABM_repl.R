#' @title Run an Interactive REPL for Agent-Based Simulation
#'
#' @description
#' This function launches an interactive REPL (Read-Eval-Print Loop) interface
#' for stepwise execution of an agent-based model (ABM) using the rABM framework.
#' It allows flexible execution of `act_FUN`, `plot_FUN`, `global_FUN`, and
#' other functions associated with the simulation object \code{G}.
#' Users can issue commands one at a time, inspect simulation status,
#' and visualize or summarize results step by step.
#'
#' @param G An ABM_G class object representing the simulation environment.
#' @param plot_FUN_name Character string specifying the name of the plot function stored in \code{G}.
#'   Either this or \code{plot_FUN} must be provided (but not both).
#' @param plot_FUN_args A named list of additional arguments passed to \code{plot_FUN}.
#' @param plot_FUN A function used to visualize the current simulation state.
#' @param summary_FUN_name Character string specifying the name of the summary function stored in \code{G}.
#'   Either this or \code{summary_FUN} must be provided (but not both).
#' @param summary_FUN_args A named list of additional arguments passed to \code{summary_FUN}.
#' @param summary_FUN A function used to summarize the current simulation state.
#' @param stop_FUN_name Character string specifying the name of the stop function stored in \code{G}.
#'   Either this or \code{stop_FUN} must be provided (but not both).
#' @param stop_FUN_args A named list of additional arguments passed to \code{stop_FUN}.
#' @param stop_FUN A function used to stop the simulation.
#' @param save_log Logical. If \code{TRUE}, the internal state of \code{G} will be saved at each step using \code{G$.save()}.
#'   If provided, this overrides \code{plot_FUN_name}.
#' @param return_prompt_log Logical. If \code{TRUE}, a list containing \code{G} and the prompt history will be returned.
#' @param return_log_file Logical. If \code{TRUE}, the prompt history will be saved to a log file.
#' @param log_file_name Optional character string specifying the file name for saving the prompt log.
#'   If \code{NULL}, the name is automatically generated using the current timestamp.
#'
#' @return If \code{return_prompt_log = TRUE}, a list with elements:
#'   \item{G}{The updated \code{ABM_G} object after interactive execution.}
#'   \item{prompt_log}{A character vector of all commands entered during the REPL session.}
#'   Otherwise, only the updated \code{G} object is returned.
#'
#' @details
#' This REPL supports several command formats:
#' \itemize{
#'   \item{\code{agents:idx:act_FUN} — Applies an action function to specified agents.}
#'   \item{\code{select_FUN -> agents_name::act_FUN} — Applies an action to agents selected by a selector function.}
#'   \item{\code{FUN} — Executes a function (e.g., plot_FUN, global_FUN) with optional arguments.}
#'   \item{\code{EXPR 1 + 1} — Evaluates an arbitrary R expression.}
#'   \item{\code{G} — Prints the current simulation object.}
#'   \item{\code{HELP} — Displays usage instructions.}
#'   \item{\code{END} — Ends the session.}
#' }
#' Pressing ENTER without typing a command re-executes the previous command.
#'
#' @importFrom rlang new_environment
#' @importFrom emoji zoo
#' @importFrom stringr str_detect str_split str_trim str_remove
#' @importFrom cli cli_h1 cli_h2 cli_text cli_ul cli_li cli_end
#' @examples
#' \dontrun{
#' agents <- init_agents(n = 10,
#' act_FUN = list(move = function(step = 1){
#'   G$posit[self$ID,"x"] <- G$posit[self$ID,"x"] + step
#' }))
#'
#' plot_map <- function(){
#'   plot(x = self$posit$x, y = self$posit$y, xlim = c(0, 11),
#'        pch = 15, main = paste("t =", self$time), xlab = "x-posit", ylab = "agents")
#'   abline(v=10)
#' }
#'
#'
#' G <- setABM(
#'   agents = agents,
#'   stage = list(posit = data.frame(x = 0, y = 1:10)),
#'   select_FUN = list(random_select = function(){sample.int(10, size = 1)}),
#'   global_FUN = list(retreat_all = function(){self$posit[,"x"] <- self$posit[,"x"] - 1}),
#'   stop_FUN = list(reached_goal = function(){any(self$posit[,"x"] >= 10)}),
#'   plot_FUN = list(plot_map = plot_map),
#'   summary_FUN = list(current_stage = function(){cat("Current posit \n"); print(self$posit)})
#' )
#'
#' reached_goal = function(){any(G$posit[,"x"] >= 10)}
#'
#' # Run REPL interactively
#' G2 <- runABM_repl(G = G, summary_FUN_name = "current_stage",
#'                   plot_FUN_name = "plot_map", stop_FUN = reached_goal)
#' }
#'
#' @seealso \code{\link{runABM}}, \code{\link{init_agents}}, \code{\link{setABM}}
#' @export
runABM_repl <- function(G,
                        plot_FUN_name = NULL,
                        plot_FUN_args = list(),
                        plot_FUN = NULL,
                        summary_FUN_name = NULL,
                        summary_FUN_args = list(),
                        summary_FUN = NULL,
                        times = NULL,
                        stop_FUN_name = NULL,
                        stop_FUN_args = list(),
                        stop_FUN = NULL,
                        save_log = TRUE,
                        return_prompt_log = FALSE,
                        return_log_file = TRUE,
                        log_file_name = NULL){
  # clone G
  stopifnot("'G' must be the class of 'ABM_G'." = inherits(G, "ABM_G"))
  G <- G$clone(deep = TRUE)

  # create E(temporal_environment)
  E <- new_environment()
  on.exit(rm(E, envir = environment()), add = TRUE)

  # summary_FUN
  if(is.null(summary_FUN_name) && is.null(summary_FUN)){
    summary_FUN <- function(G = G, E = E){}
    is_exist_summary_FUN <- FALSE
  }else if(!is.null(summary_FUN_name)){
    stopifnot("Either 'summary_FUN_name' or 'summary_FUN' can be provided, not both" = is.null(summary_FUN))
    summary_FUN <- G[[summary_FUN_name]]
    is_exist_summary_FUN <- TRUE
  }else{
    stopifnot("'summary_FUN' must be a function." = is.function(summary_FUN))
    # Delete G or E if the user already set
    current_formals <- formals(summary_FUN)
    current_formals[which(names(current_formals)=="G")|which(names(current_formals)=="E")] <- NULL
    formals(summary_FUN) <- c(alist(G = G, E = E), current_formals) # add G = G, E = E
  }

  # plot_FUN
  if(is.null(plot_FUN_name) && is.null(plot_FUN)){
    plot_FUN <- function(G = G, E = E){}
    is_exist_plot_FUN <- FALSE
  }else if(!is.null(plot_FUN_name)){
    stopifnot("Either 'plot_FUN_name' or 'plot_FUN' can be provided, not both" = is.null(plot_FUN))
    plot_FUN <- G[[plot_FUN_name]]
    is_exist_plot_FUN <- TRUE
  }else{
    stopifnot("'plot_FUN' must be a function." = is.function(plot_FUN))
    # Delete G or E if the user already set
    current_formals <- formals(plot_FUN)
    current_formals[which(names(current_formals)=="G")|which(names(current_formals)=="E")] <- NULL
    formals(plot_FUN) <- c(alist(G = G, E = E), current_formals) # add G = G, E = E
    is_exist_plot_FUN <- TRUE
  }

  # stop_FUN
  if(!is.null(times)){
    stopifnot("'times' must be a positive integer." = (is.numeric(times) && times >= 1))
    stopifnot("Only one of the 'times', 'stop_FUN_name' or 'stop_FUN' can be provided" = is.null(stop_FUN_name) && is.null(stop_FUN))
  sim_time <- G$time + times
  stop_FUN <- function(G, E = NULL){G$time >= sim_time}
  is_exist_stop_FUN <- TRUE
  }else if(is.null(stop_FUN_name) && is.null(stop_FUN)){
    stop_FUN <- function(G = G, E = E){}
    is_exist_stop_FUN <- FALSE
  }else if(!is.null(stop_FUN_name)){
    stopifnot("Either 'stop_FUN_name' or 'stop_FUN' can be provided, not both" = is.null(stop_FUN))
    stop_FUN <- G[[stop_FUN_name]]
    is_exist_stop_FUN <- TRUE
  }else{
    stopifnot("'stop_FUN' must be a function." = is.function(stop_FUN))
    # Delete G or E if the user already set
    current_formals <- formals(stop_FUN)
    current_formals[which(names(current_formals)=="G")|which(names(current_formals)=="E")] <- NULL
    formals(stop_FUN) <- c(alist(G = G, E = E), current_formals) # add G = G, E = E
    is_exist_stop_FUN <- TRUE
  }

  # create an temporal common tryCatch function
  .tc <- function(expr) {
    tryCatch(
      expr = expr,
      error = function(e) {
        message("[ERROR] ", e$message)
        NULL
      }
    )
  }

  # create an temporal function to get argument and call
  .parse_call <- function(code_string) {
    expr <- tryCatch(parse(text = code_string)[[1]], error = function(e) return(NULL))
    if (is.null(expr)) return(NULL)
    if (is.call(expr)) {
      fun_name <- as.character(expr[[1]])
      args <- c(as.list(expr)[-1])
    } else if (is.symbol(expr)) {
      fun_name <- as.character(expr)
      args <- list()
    } else {
      return(NULL)
    }
    list(fun_name = fun_name, args = args)
  }


  # help file
  print_help <- function(){
    cli_h1("[REPL HELP]")
    cli_text("During the REPL, type the command next to >>>.")
    cli_text("Available commands and their formats are explained below.")
    cli_text("Pressing ENTER with no input will re-execute the previous command.")

    cli_h2("Commands for the ABM execution")
    cli_text("Type in the following format, depending on which FUN you want to use.")
    cli_text("All functions (FUN) can have custom arguments...")
    cli_text("Here, FUN refers to any defined function (e.g., act_FUN, plot_FUN, global_FUN, etc.)")
    ulid <- cli_ul()
    cli_li("act_FUN: agents/idx/act_FUN")
    cli_li("act_FUN with select_FUN: select_FUN -> agents_name//act_FUN (applies act_FUN to selected agents)")
    cli_li("Other *_FUN: Just type the function name (e.g., plot_map)")
    cli_end(ulid)

    cli_h2("Other Commands")
    cli_text("These commands below do not update 'G$time'")
    ulid <- cli_ul()
    cli_li("HELP: Type 'HELP' to show this document again")
    cli_li("G: Type 'G' to print the current simulation object (G)")
    cli_li("EXPR: Type 'EXPR' at the beginning followed by your R expression")
    cli_li("LOG: Show prompt log. Use 'LOG ALL' for full history (latest first), or 'LOG 1:2' for selected entries.")
    cli_li("END: Type 'END' to stop the REPL session")
    cli_end(ulid)

    cli_h2("Examples")
    ulid <- cli_ul()
    cli_li("[act_FUN] >>> agents/1/move(a = 1)")
    cli_li("[act_FUN] >>> agents/c(1,2)/move(a = 1)")
    cli_li("[act_FUN with select_FUN] >>> select_top_5 -> agents//move")
    cli_li("[global_FUN] >>> add_1")
    cli_li("[EXPR] >>> EXPR 1 + 1")
    cli_li("[EXPR] >>> LOG")
    cli_li("[EXPR] >>> LOG ALL")
    cli_li("[END] >>> END")
    cli_end(ulid)
  }

  #--------------------------------------
  # start REPL
  #--------------------------------------
  # prompt_log holder
  prompt_log <- c()

  # Welcoming
  animal <- zoo(1)
  cli_alert_success(paste(animal, "Welcome to REPL mode", animal))

  # print help
  print_help()

  # showing the initial condition
  if(any(c(is_exist_summary_FUN, is_exist_plot_FUN))){
    cli_h1("Showing initial condition")
    if(is_exist_summary_FUN){
      # summary
      .tc(do.call(summary_FUN, args = c(summary_FUN_args, G = G, E = E)))
    }

    if(is_exist_plot_FUN){
      # plot
      .tc(do.call(plot_FUN, args = c(plot_FUN_args, G = G, E = E)))
    }
  }

  # start
  animal <- zoo(1)
  cli_h1(paste(animal, "Starting REPL...", animal))

  # in REPL----->
  repeat{
    # imput prompt
    prompt <- readline(">>> ")

    # if prompt == "", insert the previous prompt
    if (prompt == "") {
      if (length(prompt_log) == 0) {
        message("No previous command to repeat.")
        next
      }
      prompt <- prompt_log[length(prompt_log)]
    }

    #-------------------------------------
    # react to the prompt
    #-------------------------------------

    if(prompt == "END"){
      animal <- zoo(1)
      cli_h1(paste("Ending REPL. Bye!", animal))
      break
    }else if(prompt == "HELP"){
      print_help()
      next
    }else if(prompt == "G"){
      print(G)
      next
    }else if(startsWith(prompt, "LOG")){
      log_time <- trimws(str_remove(prompt, "LOG"))
      if(log_time == ""){
        log_time <- length(prompt_log)
      }else if(log_time == "ALL"){
        log_time <- length(prompt_log):1
      }else{
        log_time <- eval(parse(text = log_time)[[1]])
      }
      # return log
      .tc(print(prompt_log[log_time]))
      next
    }else if(startsWith(prompt, "EXPR ")){
      expr_str <- str_trim(str_remove(prompt, "^EXPR\\s+"))
      if (nzchar(expr_str)) {
        parsed_expr <- .tc(parse(text = expr_str)[[1]])
        result <- .tc(eval(parsed_expr))
        print(result)
        prompt_log <- c(prompt_log, prompt)
        next
      } else {
        message("[WARNING] No expression provided after EXPR.")
        next
      }
    }else if(str_detect(prompt, "->")){
      # update time
      G$time <- G$time + 1

      # select_FUN -> agents' act_FUN
      FUN_parts <- str_split(prompt, "->")[[1]]
      # select_FUN
      select_fun_args <- .tc(.parse_call(FUN_parts[1]))
      # act_FUN
      agents_parts <- str_split(FUN_parts[2], "//")[[1]]
      agents_name <- trimws(agents_parts[1])
      act_fun_args <- .tc(.parse_call(agents_parts[2]))
      # agent_num
      agents_idx <- .tc(do.call(G[[select_fun_args$fun_name]],
                                args = c(select_fun_args$args, G = G, E = E)))
      # act
      lapply(agents_idx, function(i){
        .tc(do.call(G[[agents_name]][[i]][[act_fun_args$fun_name]],
                    args = c(act_fun_args$args, G = G, E = E)))
      })
    }else if(str_detect(prompt, "/")){
      # update time
      G$time <- G$time + 1

      # agents to act
      prompt_parts <- str_split(prompt, "/")[[1]]
      if (length(prompt_parts) != 3) {
        message("[WARNING] Invalid format. Use 'agents:idx:act_FUN'")
        next
      }
      agents_name <- prompt_parts[1]
      agents_idx <- prompt_parts[2]
      agents_act <- prompt_parts[3]
      # decode agents_idx
      if(agents_idx == ""){
        agents_idx <- sample.int(length(G[[agents_name]]))
      }else{
        agents_idx <- .tc(eval(parse(text = agents_idx)[[1]]))
      }
      # decode agents_act
      agents_fun_args <- .tc(.parse_call(agents_act))
      # execute
      lapply(agents_idx, function(i){
        .tc(do.call(G[[agents_name]][[i]][[agents_fun_args$fun_name]],
                args = c(agents_fun_args$args, G = G, E = E)))
      })
    }else{
      # update time
      G$time <- G$time + 1
      # Other FUN
      g_fun_args <- .tc(.parse_call(prompt))
      # execute
      error_occurred <- FALSE
      tryCatch(
        {
          do.call(G[[g_fun_args$fun_name]], args = c(g_fun_args$args, G = G, E = E))
        },
        error = function(e){
          message("[ERROR] ", e$message)
          G$time <- G$time - 1
          error_occurred <<- TRUE
          NULL
        }
      )
      # do not proceed to update if an error occurs
      if (error_occurred) next
    }

    #---------------------------------------------
    # print, plot, and update time
    #---------------------------------------------
    # summary
    .tc(do.call(summary_FUN, args = c(summary_FUN_args, G = G, E = E)))
    # plot
    .tc(do.call(plot_FUN, args = c(plot_FUN_args, G = G, E = E)))

    #---------------------------------------------
    # log and save
    #---------------------------------------------
    if(save_log){
      G$.save()
    }

    # take a log
    prompt_log <- c(prompt_log, prompt)

    # check the stop condition
    if(is_exist_stop_FUN){
      stop_judge <- .tc(do.call(stop_FUN, args = c(stop_FUN_args, G = G, E = E)))
      if(stop_judge){
        cli_alert_success("Stop condition satisfied")
        animal <- zoo(1)
        cli_h1(paste("Ending REPL. Bye!", animal))
        break
      }
    }

  }#//-----end of REPL-----//

  # clean-up E
  E <- NULL

  # Write log file
  if(return_log_file){
    if(is.null(log_file_name)){
      log_file_name <- paste0(
        "REPL_log_",
        format(Sys.time(), "%y%m%d_%H%M"),
        ".txt"
      )
    }
    writeLines(prompt_log, con = log_file_name)
    cli_text("Prompt log saved to: ", log_file_name)
  }

  # return
  if(return_prompt_log){
    out <- list(G = G,
                prompt_log = prompt_log)
    return(out)
  }else{
    return(G)
  }
} #//--------END OF FUNCTION------------//


