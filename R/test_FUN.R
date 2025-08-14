#' Interactively test and refine a new function against an ABM environment
#'
#' @description
#' `test_FUN()` adds a new function (agent action or global function) to an
#' existing ABM environment `G`, runs it once, diffs the changes with
#' [`compare_G()`], and optionally opens an interactive REPL to debug, edit,
#' re‑run, or reset the function and/or `G`. When you end the REPL, the function
#' (possibly edited during the session) is returned as a plain function with
#' a cleaned environment, ready to be stored or reused.
#'
#' @details
#' The workflow is:
#' 1. Validate inputs and **clone** the provided `G` for safety (`G_original`, `G_new`).
#' 2. Insert `new_FUN` into `G` at the specified location depending on `FUN_type`.
#' 3. **Run once** and show changes with [`compare_G()`] (optional).
#' 4. If `with_repl = TRUE`, enter an interactive REPL that supports:
#'    - Running the function again,
#'    - One‑shot debugging (`DEBUG`) via `utils::debugonce()`,
#'    - In‑place editing of the function body (`EDIT`) via `edit()`,
#'    - Resetting `G` or the function to original state,
#'    - Printing the current function, or arbitrary R evaluation.
#'
#' During setup, the function’s formal arguments `G` and `E` are normalized:
#' user‑supplied `G`/`E` formals are temporarily removed and replaced with
#' internally managed `G` (the working clone) and `E` (a fresh
#' [`rlang::new_environment()`] used as a scratch pad). The `E` environment is
#' removed on exit. The **returned function** is re‑parsed to strip srcref and
#' injected bindings so it can be saved cleanly in user code.
#'
#' @section Supported locations (`FUN_type`) and effects:
#' \describe{
#'   \item{`"act_FUN"`}{Adds a method named `new_FUN_name` to selected agents
#'   under `G[[agents_name]]`. For agents not listed in `agents_idx`, a dummy
#'   placeholder is added so that the field exists on all agents.}
#'   \item{`"global_FUN"`, `"select_FUN"`, `"stop_FUN"`, `"update_FUN"`,
#'   `"summary_FUN"`, `"plot_FUN"`}{Adds a new function field named
#'   `new_FUN_name` onto `G` via [`modify_G()`] with the corresponding
#'   `add_*_FUN` method.}
#' }
#'
#' @section REPL commands:
#' \describe{
#'   \item{`RUN`}{Execute the current function against the working `G` and print diffs (if `print_changes`).}
#'   \item{`DEBUG`}{Debug the function once with browser commands (`n`, `s`, `c`, `where`, etc.). For `act_FUN`, you will be prompted for a single `idx`.}
#'   \item{`EDIT`}{Open an editor for the function, save, and replace the version on `G`.}
#'   \item{`RESET_G`}{Restore `G` to the original input (`G_original`) and re‑attach the current function.}
#'   \item{`RESET_FUN`}{Restore the function body to the initially provided `new_FUN`.}
#'   \item{`PRINT`}{Print the current function object.}
#'   \item{`HELP`}{Show the REPL help again.}
#'   \item{`END`}{Exit the REPL and return the (possibly edited) function.}
#'   \item{Any R expression}{Evaluate in the current session (e.g., `G$time`).}
#' }
#'
#' @param G An `ABM_G` object. The target ABM environment to which the new
#'   function will be attached and tested. Must inherit from class `"ABM_G"`.
#' @param new_FUN_name A length‑1 character string. The new field/method
#'   name to create. Must not already exist in the target location (checked).
#' @param new_FUN A function object (or a list of functions if you extend this
#'   behavior) to be attached. Its `G`/`E` formals—if present—are ignored and
#'   replaced by internally managed ones during testing.
#' @param agents_name Optional character scalar. Required when
#'   `FUN_type = "act_FUN"`. The name of the agent collection inside `G`
#'   (e.g., `"children"`).
#' @param agents_idx Optional integer vector. Only for `FUN_type = "act_FUN"`.
#'   Indices of agents within `G[[agents_name]]` that should receive the new
#'   method. Defaults to **all** agents in that collection. Must be within
#'   bounds.
#' @param FUN_type Character scalar; one of
#'   `c("act_FUN","global_FUN","select_FUN","stop_FUN","update_FUN","summary_FUN","plot_FUN")`.
#'   Determines where and how the function is attached.
#' @param with_repl Logical (default `TRUE`). If `FALSE`, the function is added,
#'   run once, and the function is returned **without**
#'   entering the interactive REPL.
#' @param print_changes Logical (default `TRUE`). If `TRUE`, prints the result
#'   of [`compare_G()`] after each run to highlight changes to `G`.
#'
#' @return A function object. Specifically, the final version of the function
#' (potentially edited during the REPL) with cleaned formals and environment,
#' suitable for saving (e.g., assigning into your package or writing to file).
#' Side effects occur only on **cloned** `G` objects within the session; the
#' original `G` passed in is not mutated.
#'
#' @section Validation rules:
#' - `G` must inherit from `"ABM_G"`.
#' - `new_FUN_name` must be a length‑1 character and must **not** already exist
#'   at the target location.
#' - For `FUN_type = "act_FUN"`, `agents_name` is required and
#'   `G[[agents_name]][[1]]` must inherit from `"ABM_Agent"`.
#' - For `agents_idx` (when provided), indices must be numeric and within range.
#'
#' @examples
#' \dontrun{
#' # Assume you have an ABM with children agents:
#' children_attr <- data.frame(age = c(0, 1, 2))
#' get_older <- function(){ self$age <- self$age + 1 }
#' children <- init_agents(attr_df = children_attr, act_FUN = list(get_older = get_older))
#' G <- setABM(agents = children)
#'
#' # Define a new action that adds 2 years:
#' add_two_years <- function(G, E){ self$age <- self$age + 2 }
#'
#' # Interactively attach and test it on all children:
#' f <- test_FUN(
#'   G = G,
#'   new_FUN_name = "add_two_years",
#'   new_FUN = add_two_years,
#'   agents_name = "children",
#'   FUN_type = "act_FUN",
#'   with_repl = TRUE,
#'   print_changes = TRUE
#' )
#'
#' # After the REPL ends, `f` holds the finalized function.
#' }
#'
#' @seealso [compare_G()], [modify_G()], [setABM()], [init_agents()],
#'   [base::debugonce()], [utils::edit()]
#'
#' @family rABM developer tools
#'
#' @importFrom rlang new_environment
#' @importFrom cli cli_h1 cli_h2 cli_text cli_ul cli_end cli_alert_info
#' @importFrom utils edit
#'
#' @export

test_FUN <- function(
    G,
    new_FUN_name,
    new_FUN,
    agents_name = NULL,
    agents_idx = NULL,
    FUN_type = c("act_FUN", "global_FUN", "select_FUN",
                 "stop_FUN", "update_FUN", "summary_FUN", "plot_FUN"),
    with_repl = TRUE,
    print_changes = TRUE
){
  #----------------------------
  # validation of the input
  #----------------------------
  # G
  stopifnot("'G' must be the class of 'ABM_G'" = inherits(G, "ABM_G"))
  G_original <- copy_G(G)
  G_new <- copy_G(G)
  # new_FUN
  stopifnot("new_FUN_name should be a character of length 1." = is.character(new_FUN_name) && length(new_FUN_name) == 1)
  stopifnot("'new_FUN' must be a function or a list of functions."
            = all(unlist(lapply(new_FUNs, function(FUN){is.function(FUN)}))))
  # FUN_type
  FUN_type <- match.arg(FUN_type)
  # agents_name
  if(FUN_type == "act_FUN"){
    stopifnot("'agents_name' should be provided if 'FUN_type' is 'act_FUN'." = !is.null(agents_name))
    stopifnot("Agents indicated in 'agents_name' do not exist in 'G' or not the class of 'ABM_Agent'"
              = inherits(G_new[[agents_name]][[1]], "ABM_Agent"))
  }
  # agents_idx
  if(FUN_type == "act_FUN"){
    if(is.null(agents_idx)){
      agents_idx <- 1:length(G[[agents_name]])
    }else{
      judge <- all(vapply(agents_idx, function(idx) is.numeric(idx), logical(1)))
      if(judge==FALSE) stop("All 'agents_idx' must be an integer.")
      stopifnot("'agents_idx' must be within the length of 'agents'." = max(agents_idx) <= length(G[[agents_name]]))
    }
  }
  # existence new_FUN name
  if(FUN_type=="act_FUN"){
    idx <- agents_idx[1]
    stopifnot("'new_FUN_name' already exists in the provided in 'agents'. Please use another name." = !new_FUN_name %in% ls(G[[agents_name]][[idx]]))
  }else{
    stopifnot("'new_FUN_name' already exists in the provided 'G'. Please use another name." = !new_FUN_name %in% ls(G))
  }

  # NEW FUN
  # temporary delete the user's G and E arguments
  GE_posit <- which(names(formals(new_FUN)) == "G" | names(formals(new_FUN)) == "E")
  formals(new_FUN)[GE_posit] <- NULL
  # and rewrite G and E arguments
  formals(new_FUN) <- c(alist(G = G, E = E), formals(new_FUN))


  #---------------------------
  # [internal] .add_act_FUN
  #---------------------------
  .add_act_FUN <- function(G, new_FUN_name, new_FUN, agents_name, agents_idx){
    # copy for safe-guard
    G <- copy_G(G)
    # add a new FUN to the defined agents
    for(i in agents_idx){
      G[[agents_name]][[i]]$.add_method(name = new_FUN_name,
                                       method = new_FUN)
    }
    # add a dummy act_FUN to other agents
    remaining_idx <- setdiff(1:length(G[[agents_name]]), agents_idx)
    dummy_FUN <- function(G = G, E = E){
      # This is a dummy function imputed for those agents which are not specified in 'agents_idx'
      }
    for(j in remaining_idx){
      G[[agents_name]][[j]]$.add_method(name = new_FUN_name,
                                        method = dummy_FUN)
    }

    # return
    G
  }

  #---------------------------
  # [internal] .add_new_FUN
  #---------------------------
  .add_new_FUN <- function(G, agents_name = NULL, new_FUN = new_FUN,
                              FUN_type = FUN_type){
    switch(FUN_type,
           "act_FUN" = {
             G <- .add_act_FUN(G = G, new_FUN_name = new_FUN_name,
                                   new_FUN = new_FUN,
                                   agents_name =  agents_name,
                                   agents_idx = agents_idx)
           },
           "global_FUN" = {
             G <- modify_G(G = G, field_name = new_FUN_name,
                               method = "add_global_FUN",
                               new_obj = new_FUN)
           },
           "select_FUN" = {
             G <- modify_G(G = G, field_name = new_FUN_name,
                               method = "add_select_FUN",
                               new_obj = new_FUN)
           },
           "stop_FUN" = {
             G <- modify_G(G = G, field_name = new_FUN_name,
                               method = "add_stop_FUN",
                               new_obj = new_FUN)
           },
           "update_FUN" = {
             G <- modify_G(G = G, field_name = new_FUN_name,
                               method = "add_update_FUN",
                               new_obj = new_FUN)
           },
           "summary_FUN" = {
             G <- modify_G(G = G, field_name = new_FUN_name,
                               method = "add_summary_FUN",
                               new_obj = new_FUN)
           },
           "plot_FUN" = {
             G <- modify_G(G = G, field_name = new_FUN_name,
                               method = "add_plot_FUN",
                               new_obj = new_FUN)
           }

    )
    # return
    G
  }
  #//----end of .switch_modif_G-----------//


  #-----------------------------
  # create a G_new and G for testing
  #-----------------------------
  G_new <- .add_new_FUN(G = G_new, agents_name = agents_name,
                           new_FUN = new_FUN, FUN_type = FUN_type)

  # create a G_test for testing
  G <- copy_G(G_new)
  G_before <- copy_G(G_new)
  E <- new_environment()
  on.exit(rm(E, envir = environment()), add = TRUE)

  #------------------------------
  # [internal function] implement the new_FUN
  #------------------------------
  .test_run <- function(G, FUN_type, new_FUN_name, agents_name = NULL, agents_idx = NULL){
    if(FUN_type == "act_FUN"){
      lapply(agents_idx, function(i){
        tryCatch(
          G[[agents_name]][[i]][[new_FUN_name]](G = G, E = E),
          error = function(e){
            print("ERROR for agent", i, e$message)
            NULL
          })
      })
    }else{
      tryCatch(G[[new_FUN_name]](G = G, E = E),
               error = function(e){
                 print("ERROR: ", e$message)
                 NULL
               })
    }
  }
  #//------.test_run-------------//

  # message
  cli_par()
  cli_h1("Running the 'new_FUN' given 'G' once.")
  cli_end()

  # Implement the test_run
  print(.test_run(G = G, FUN_type = FUN_type,
            new_FUN_name = new_FUN_name,
            agents_name = agents_name,
            agents_idx = agents_idx))

  # print changes
  if(print_changes){
    cli_par()
    cli_h1("Changes made by the 'new_FUN' to 'G'.")
    cli_text("'G1' represents the state of 'G' before the 'new_FUN' was implemented, and 'G2' after.")
    res <- suppressMessages(compare_G(G = G_before, G2 = G))
    if(nrow(res$stage)==0 && nrow(res$agents)==0){
      cli_alert_warning("No change has been made by 'new_FUN' on 'G'.")
    }else{
      print(res)
    }
    cli_end()
    cat("\n")
  }

  # End the session if with_repl == FALSE
  if(with_repl == FALSE){
    return(new_FUN)
  }

  # proceed to REPL session
  cli_par()
  cli_alert_info("Proceeding to the REPL session.")
  # put back the G_new to G
  cli_text("The state of the 'G' is now the one you provided.")
  cli_end()
  cat("\n")

  G <- copy_G(G_new)


  #------------------------------
  # REPL help
  #------------------------------
  print_help <- function(){
    cli_h1("[Mode Selection Help]")
    cli_alert_info("Type one of the following commands at the >>> prompt.")
    cli_text("Available commands and their formats are explained below.")

    cli_h2("Mode Commands")
    cli_text("Use one of the following to test or modify the current function:")
    cli_ul()
    cli_li("'RUN': Execute the current 'new_FUN'.")
    cli_li("'DEBUG': Enter debug mode for 'new_FUN'.")
    cli_li("'EDIT': Edit the function body of 'new_FUN' interactively.")
    cli_li("'RESET_G': Restore the ABM environment 'G' to its initial state.")
    cli_li("'RESET_FUN': Restore the original version of 'new_FUN'.")
    cli_li("'PRINT': Print the current 'new_FUN' definition.")
    cli_li("'END': Exit the REPL session.")
    cli_end()

    cli_h2("Other Commands")
    cli_ul()
    cli_li("'HELP': Show this help menu again.")
    cli_li("Any valid R expression (e.g., 'G$time') can be evaluated directly.")
    cli_end()
  }
  print_help()

  #------------------------------
  # Debug help
  #------------------------------
  debug_help <- function(){
    cli_h2("[Debug Commands help]")
    cli_ul()
    cli_li("'c': exit the browser and continue execution at the next statement.")
    cli_li("'cont': synonym for c.")
    cli_li("'f': finish execution of the current loop or function.")
    cli_li("'help': print this list of commands.")
    cli_li("'n': evaluate the next statement, stepping over function calls. ")
    cli_li("'s': evaluate the next statement, stepping into function calls. ")
    cli_li("'where': print a stack trace of all active function calls.")
    cli_li("'r': invoke a 'resume' restart if one is available.")
    cli_li("'Q': exit the browser and the current evaluation and return to the top-level prompt.")
    cli_end()
  }

  #--------------------------------------------
  # [internal] to keep the source for debug
  #--------------------------------------------
  rebuild_with_srcref <- function(FUN) {
    txt <- paste(deparse(FUN), collapse = "\n")
    # parse(text=..., keep.source=TRUE) keeps srcref
    FUN2 <- eval(parse(text = txt, keep.source = TRUE)[[1]], envir = environment(FUN))
    environment(FUN2) <- environment(FUN)  # keep the original environment
    FUN2
  }


  #==============================
  # in REPL...
  #==============================
  repeat{
    prompt <- readline(">>> ")
    switch(prompt,
           "RUN" = {
             G_before <- copy_G(G)
             .test_run(G = G, FUN_type = FUN_type,
                       new_FUN_name = new_FUN_name,
                       agents_name = agents_name,
                       agents_idx = agents_idx)
             # print compare_G
             if(print_changes){
               cli_h1("Changes made by the 'new_FUN' to 'G'.")
               cli_text("'G1' represents the state of 'G' before the 'new_FUN' was implemented, and 'G2' after.")
               res <- suppressMessages(compare_G(G = G_before, G2 = G))
               print(res)
             }
           },
           "DEBUG" = {
             if(FUN_type == "act_FUN"){
               repeat{
                 idx <- readline("Which agent idx? >>> ")
                 idx <- suppressWarnings(as.numeric(idx))
                 if(is.na(idx)){
                   message("The agent's idx in debug mode must be an integer of length 1.")
                   next
                 }
                 if(idx > length(G[[agents_name]])||idx <= 0){
                   message("The provided 'idx' is outside of the 'G[[agents_name]]'.")
                   next
                 }
                   break
               }

               # Rebuild G and FUN
               FUN <- G[[agents_name]][[idx]][[new_FUN_name]]
             }else{
               FUN <- G[[new_FUN_name]]
             }
             FUN2 <- tryCatch(rebuild_with_srcref(FUN),
                              error = function(e){
                                message("[ERROR] ", e$message)
                                next})
             tryCatch(debugonce(FUN2),
                      error = function(e){
                        message("[ERROR] ", e$message)
                        next})
             debug_help()
             do.call(FUN2, list(G = G, E = E))
           },
           "EDIT" = {
             if(FUN_type == "act_FUN"){
               idx <- agents_idx[1]
               FUN <- G[[agents_name]][[idx]][[new_FUN_name]]
               FUN <- edit(FUN)
               # replace
               for(i in agents_idx){
                 G[[agents_name]][[i]]$.remove_field(name = new_FUN_name)
                 G[[agents_name]][[i]]$.add_method(name = new_FUN_name,
                                                   method = FUN)
               }
               # message
               cat(paste0("The function '", new_FUN_name, "' was updated if it was edited; otherwise, it remains unchanged.\n"))
             }else{
               FUN <- G[[new_FUN_name]]
               FUN <- edit(FUN)
               # replace G
               G <- modify_G(G = G, field_name = new_FUN_name, method = "replace",
                             new_obj = FUN)
               # message
               cat(paste0("The function '", new_FUN_name, "' was updated if it was edited; otherwise, it remains unchanged.\n"))
             }
           },
           "RESET_G" = {
             G_current <- copy_G(G)
             G <- copy_G(G_original)
             if(FUN_type == "act_FUN"){
               for(i in agents_idx){
                 FUN <- G_current[[agents_name]][[i]][[new_FUN_name]]
                 G[[agents_name]][[i]]$.add_method(name = new_FUN_name,
                                                   method = FUN)
               }
             }else{
               FUN <- G_current[[new_FUN_name]]
               G <- .add_new_FUN(G = G, agents_name = agents_name,
                                 new_FUN = FUN, FUN_type = FUN_type)
             }
             cat("The state of the 'G' is backed to the original input.")
           },
           "PRINT" = {
             if(FUN_type=="act_FUN"){
               idx <- agents_idx[1]
               print(G[[agents_name]][[idx]][[new_FUN_name]])
             }else{
               print(G[[new_FUN_name]])
             }
           },
           "RESET_FUN" = {
             if(FUN_type == "act_FUN"){
               for(i in agents_idx){
                 G[[agents_name]][[i]]$.remove_field(name = new_FUN_name)
                 G[[agents_name]][[i]]$.add_method(name = new_FUN_name,
                                                   method = new_FUN)
               }
             }else{
               G$.remove_field(new_FUN_name)
               G <- .add_new_FUN(G = G, agents_name = agents_name,
                                 new_FUN = new_FUN, FUN_type = FUN_type)
             }
             cat("The state of the 'new_FUN' is backed to the original input.")
           },
           "HELP" = {
             print_help()
             next
           },
           "END" = {
             cli_par()
             cli_text("Ending the REPL session. Returning the 'new_FUN'.")
             cli_end()
             break
           },
           {parsed_expr <- tryCatch(parse(text = prompt)[[1]],
                                    error = function(e){
                                      message("[ERROR] ", e$message)
                                      NULL
                                    })
            result <- tryCatch(eval(parsed_expr),
                               error = function(e){
                                 message("[ERROR] ", e$message)
                                 NULL
                               })
            tryCatch(print(result),
                     error = function(e){
                       message("[ERROR] ", e$message)
                       NULL
                     })
           }
    )
  }#//--------end of REPL--------//

  #=======================================
  # finalization
  #=======================================

  if(FUN_type == "act_FUN"){
    idx <- agents_idx[1]
    return_FUN <- G[[agents_name]][[idx]][[new_FUN_name]]
  }else{
    return_FUN <- G[[new_FUN_name]]
  }

  return_FUN_code <- paste(deparse(return_FUN), collapse = "\n")
  return_FUN_clean <- eval(parse(text = return_FUN_code), envir = globalenv())

  return(return_FUN_clean)
}#//------end of function-------//
