#' Add a convergence-based stop condition to a Game object
#'
#' Constructs a \code{stop_FUN} that halts simulation once a watched field's
#' recent history satisfies a convergence criterion (or once \code{max_times}
#' is reached), and adds both the tracking state and the \code{stop_FUN} to a
#' (deep-copied) \code{\link{Game}} object.
#'
#' @details
#' A new \code{state} field named \code{state_name} is added to \code{G},
#' holding a list with elements \code{values} (the rolling history of the
#' watched field, initially \code{NULL}), \code{eval_by}, \code{eval_length},
#' \code{thresh}, \code{watching}, and — only when \code{include_max_times =
#' TRUE} — \code{max_times}. Each simulation step, the generated
#' \code{stop_FUN} first appends the current value of
#' \code{self[[watching_field]]} to \code{values} (sliding the window — i.e.
#' dropping the oldest value — once \code{values} has reached length
#' \code{eval_length}), so the convergence criterion is always evaluated on a
#' window that includes the value from the current step. The criterion
#' selected by \code{eval_by} is:
#' \itemize{
#'   \item \code{"absdiff"}: all absolute differences between consecutive
#'   values in the window are below \code{thresh}. Requires
#'   \code{eval_length >= 2}.
#'   \item \code{"reldiff"}: all absolute differences between consecutive
#'   values, relative to the preceding value, are below \code{thresh}.
#'   Requires \code{eval_length >= 2}.
#'   \item \code{"sd"}: the standard deviation of the window is below
#'   \code{thresh}. Requires \code{eval_length >= 2}.
#'   \item \code{"range"}: every value in the window falls within
#'   \code{c(thresh[1], thresh[2])}. \code{thresh} must be a numeric vector
#'   of length 2 with \code{thresh[1] < thresh[2]}.
#'   \item \code{"upper"}: every value in the window exceeds \code{thresh}.
#'   \item \code{"lower"}: every value in the window is below \code{thresh}.
#' }
#' \code{"absdiff"}, \code{"reldiff"}, and \code{"sd"} require
#' \code{eval_length >= 2} (with \code{eval_length == 1} these criteria are
#' either vacuously satisfied on the very first value, for the diff-based
#' ones, or produce \code{NA} and error, for \code{"sd"}); this is enforced
#' by an upfront check.
#'
#' When \code{include_max_times = TRUE}, the simulation also stops once
#' \code{self$time >= max_times}, regardless of \code{eval_by}.
#'
#' If a custom \code{FUN} is supplied, the convergence-check block is
#' appended after its body (via \code{\link{append_to_body}}), so \code{FUN}
#' can perform additional stop-condition logic before the convergence check
#' runs. When \code{include_max_times = TRUE}, a \code{max_times} check is
#' inserted before \code{FUN}'s existing body (or, when \code{FUN = NULL}, a
#' skeleton function consisting of just that check is created). When
#' \code{include_max_times = FALSE} and \code{FUN = NULL}, the skeleton
#' starts as an empty function and only the convergence-check block is
#' appended.
#'
#' The state and threshold values referenced inside the generated function
#' are spliced in via \code{\link{bquote}} using \code{state_name}, so the
#' resulting \code{stop_FUN} does not depend on \code{state_name} being any
#' particular value (e.g. \code{"convergence"}) — it will correctly read and
#' update whichever field name was actually used.
#'
#' @param G A \code{\link{Game}} object to add the convergence state and
#'   \code{stop_FUN} to. \code{G} is deep-copied internally, so the original
#'   object passed in is left unmodified. \code{watching_field} must already
#'   be a registered field on \code{G}.
#' @param watching_field A single character string: the name of the state
#'   (or active_state) field on \code{G} whose value should be tracked for
#'   convergence. Must already exist as a field on \code{G}.
#' @param stop_FUN_name A single character string: the field name under
#'   which the generated \code{stop_FUN} is registered on \code{G}. Defaults
#'   to \code{"converged"}.
#' @param state_name A single character string: the field name under which
#'   the convergence-tracking state (history, settings) is registered on
#'   \code{G}. Defaults to \code{"convergence"}.
#' @param eval_by A single character string selecting the convergence
#'   criterion. One of \code{"absdiff"}, \code{"sd"}, \code{"range"},
#'   \code{"lower"}, \code{"upper"}, or \code{"reldiff"}. See Details.
#' @param eval_length A single positive integer: the size of the rolling
#'   window of recent \code{watching_field} values used to evaluate
#'   convergence. Must be \code{>= 2} when \code{eval_by} is
#'   \code{"absdiff"}, \code{"reldiff"}, or \code{"sd"}. Defaults to
#'   \code{10}.
#' @param max_times A single positive integer: when \code{include_max_times
#'   = TRUE}, the simulation always stops once \code{self$time} reaches this
#'   value, regardless of \code{eval_by}. Not used (and not stored) when
#'   \code{include_max_times = FALSE}. Defaults to \code{100}.
#' @param thresh A numeric value (or, for \code{eval_by = "range"}, a numeric
#'   vector of length 2 giving lower and upper bounds with
#'   \code{thresh[1] < thresh[2]}) used as the convergence threshold.
#'   Defaults to \code{0.01}.
#' @param FUN An optional existing \code{stop_FUN}-style function (taking no
#'   arguments and using \code{self} internally). If supplied, the
#'   convergence-check block is appended after its body instead of building a
#'   new skeleton function. Defaults to \code{NULL}.
#' @param include_max_times A single logical value. If \code{TRUE}, a
#'   \code{max_times} check is inserted before the (possibly
#'   user-supplied) \code{FUN}'s existing body. Defaults to \code{TRUE}.
#'
#' @return A modified (deep-copied) \code{\link{Game}} object with the
#'   convergence-tracking state field and the \code{stop_FUN} added.
#'
#' @seealso \code{\link{append_to_body}}, \code{\link{Game}},
#'   \code{\link{State}}, \code{\link{Stop}}
#'
#' @examples
#' \dontrun{
#' x <- 10
#' subtract_x <- function(){
#'   self$x <- self$x - runif(n = 1, min = 0, max = 1)
#' }
#' G <- Game(State(x), Act(subtract_x))
#' G2 <- add_stop_convergence(
#'   G,
#'   watching_field = "x",
#'   eval_by        = "lower",
#'   eval_length    = 1,
#'   thresh         = 0,
#'   max_times      = 500
#' )
#' G_out <- run_Game(G = G2, plan = "subtract_x", nm_stop_FUN = "stop_converged")
#' }
#'
#' @export
add_stop_convergence <- function(G,
                                 watching_field,
                                 stop_FUN_name = "converged",
                                 state_name = "convergence",
                                 eval_by = c("absdiff", "sd", "range",
                                             "lower", "upper","reldiff"),
                                 eval_length = 10,
                                 max_times = 100,
                                 thresh = 0.01,
                                 FUN = NULL,
                                 include_max_times = TRUE){
  # ---- validation ----------------------------------------------------
  stopifnot(
    "'watching_field' must be a single, non-empty character string." =
      is.character(watching_field) && length(watching_field) == 1L &&
      !is.na(watching_field) && nzchar(watching_field),
    "'stop_FUN_name' must be a single, non-empty character string." =
      is.character(stop_FUN_name) && length(stop_FUN_name) == 1L &&
      !is.na(stop_FUN_name) && nzchar(stop_FUN_name),
    "'state_name' must be a single, non-empty character string." =
      is.character(state_name) && length(state_name) == 1L &&
      !is.na(state_name) && nzchar(state_name),
    "'eval_length' must be a single positive integer." =
      is.numeric(eval_length) && length(eval_length) == 1L && !is.na(eval_length) &&
      eval_length >= 1 && eval_length == as.integer(eval_length),
    "'include_max_times' must be a single TRUE/FALSE value." =
      is.logical(include_max_times) && length(include_max_times) == 1L &&
      !is.na(include_max_times),
    "'thresh' must not contain NA." = !anyNA(thresh)
  )

  eval_by <- match.arg(eval_by)

  if(eval_by == "range"){
    if(!is.numeric(thresh)) stop("'thresh' must be a numeric vector.")
    if(length(thresh) != 2) stop("'thresh' must be a vector of length 2.")
    if(!(thresh[1] < thresh[2])) stop("The first value of 'thresh' must be less than the second.")
  }else{
    stopifnot("'thresh' must be a numeric value of length 1." = length(thresh) == 1 && is.numeric(thresh))
  }

  if(eval_by %in% c("absdiff", "reldiff", "sd") && eval_length < 2){
    stop("'eval_length' must be >= 2 when eval_by is \"absdiff\", \"reldiff\", or \"sd\".")
  }

  if(isTRUE(include_max_times)){
    stopifnot(
      "'max_times' must be a single positive integer when include_max_times = TRUE." =
        is.numeric(max_times) && length(max_times) == 1L && !is.na(max_times) &&
        max_times >= 1 && max_times == as.integer(max_times)
    )
  }

  if(!is.null(FUN)){
    stopifnot("'FUN' must be a function if supplied." = is.function(FUN))
  }

  # deep copy the current G
  G <- copy_obj(G)

  existing_fields <- G$.get_flist()$name
  if(!(watching_field %in% existing_fields)){
    stop("'watching_field' (\"", watching_field, "\") is not a field of 'G'. ",
         "Add it first (e.g. via State() or Active()) before calling add_stop_convergence().")
  }

  # state name (to be embedded)
  sn <- state_name

  # add convergence-tracking state (max_times is only stored when used)
  convergence <- list(values = NULL,
                      eval_by = eval_by,
                      eval_length = eval_length,
                      thresh = thresh,
                      watching = watching_field)
  if(isTRUE(include_max_times)){
    convergence$max_times <- max_times
  }
  add_field(G, State(convergence, name = state_name))

  # create a skeleton of the stop_FUN
  if(!is.null(FUN)){
    if(isTRUE(include_max_times)){
      max_times_block <- bquote({
        if(self$time >= self[[.(sn)]]$max_times){
          return(TRUE)
        }
      })
      FUN <- append_to_body(FUN = FUN, expr = max_times_block, posit = "before")
    }
  }else{
    if(isTRUE(include_max_times)){
      FUN <- eval(bquote(function(){
        if(self$time >= self[[.(sn)]]$max_times) return(TRUE)
      }))
    }else{
      FUN <- function(){}
    }
  }

  # add stop FUN
  switch(eval_by,
         "absdiff" = {
           extra_block <- bquote({
             if(length(self[[.(sn)]]$values) >= self[[.(sn)]]$eval_length){
               # update
               self[[.(sn)]]$values <- c(self[[.(sn)]]$values[-1], self[[self[[.(sn)]]$watching]])
               # evaluation
               if(all(abs(diff(self[[.(sn)]]$values)) < self[[.(sn)]]$thresh)) return(TRUE)
             } else {
               # update
               self[[.(sn)]]$values <- c(self[[.(sn)]]$values, self[[self[[.(sn)]]$watching]])
             }
             return(FALSE)
           })
         },
         "sd" = {
           extra_block <- bquote({
             if(length(self[[.(sn)]]$values) >= self[[.(sn)]]$eval_length){
               self[[.(sn)]]$values <- c(self[[.(sn)]]$values[-1], self[[self[[.(sn)]]$watching]])
               if(sd(self[[.(sn)]]$values) < self[[.(sn)]]$thresh) return (TRUE)
             }else{
               self[[.(sn)]]$values <- c(self[[.(sn)]]$values, self[[self[[.(sn)]]$watching]])
             }
             return(FALSE)
           })
         },
         "range" = {
           extra_block <- bquote({
             if(length(self[[.(sn)]]$values) >= self[[.(sn)]]$eval_length){
               self[[.(sn)]]$values <- c(self[[.(sn)]]$values[-1], self[[self[[.(sn)]]$watching]])
               if(all(self[[.(sn)]]$values >= self[[.(sn)]]$thresh[1]) &
                  all(self[[.(sn)]]$values <= self[[.(sn)]]$thresh[2])){
                 return (TRUE)
               }
             }else{
               self[[.(sn)]]$values <- c(self[[.(sn)]]$values, self[[self[[.(sn)]]$watching]])
             }
             return(FALSE)
           })
         },
         "upper" = {
           extra_block <- bquote({
             if(length(self[[.(sn)]]$values) >= self[[.(sn)]]$eval_length){
               self[[.(sn)]]$values <- c(self[[.(sn)]]$values[-1], self[[self[[.(sn)]]$watching]])
               if(all(self[[.(sn)]]$values > self[[.(sn)]]$thresh)) return (TRUE)
             }else{
               self[[.(sn)]]$values <- c(self[[.(sn)]]$values, self[[self[[.(sn)]]$watching]])
             }
             return(FALSE)
           })
         },
         "lower" = {
           extra_block <- bquote({
             if(length(self[[.(sn)]]$values) >= self[[.(sn)]]$eval_length){
               self[[.(sn)]]$values <- c(self[[.(sn)]]$values[-1], self[[self[[.(sn)]]$watching]])
               if(all(self[[.(sn)]]$values < self[[.(sn)]]$thresh)) return (TRUE)
             }else{
               self[[.(sn)]]$values <- c(self[[.(sn)]]$values, self[[self[[.(sn)]]$watching]])
             }
             return(FALSE)
           })
         },
         "reldiff" = {
           extra_block <- bquote({
             if(length(self[[.(sn)]]$values) >= self[[.(sn)]]$eval_length){
               self[[.(sn)]]$values <- c(self[[.(sn)]]$values[-1], self[[self[[.(sn)]]$watching]])
               if(all(abs(diff(self[[.(sn)]]$values)) / abs(self[[.(sn)]]$values[-length(self[[.(sn)]]$values)]) < self[[.(sn)]]$thresh)) return(TRUE)
             } else {
               self[[.(sn)]]$values <- c(self[[.(sn)]]$values, self[[self[[.(sn)]]$watching]])
             }
             return(FALSE)
           })
         },
         stop("eval_by = \"", eval_by, "\" is not yet implemented.", call. = FALSE)
  )
  # update function
  FUN <- append_to_body(FUN = FUN, expr = extra_block, posit = "after")
  # add to G
  add_field(G, Stop(FUN, name = stop_FUN_name))

  # Message
  message(paste0("Added a State field ", "'", state_name, "'", " and ",
                 "a Stop field ", "'", stop_FUN_name, "'."))
  # return
  G
}

#=========== HELPERS ==============================
#' Append a new code block to the body of a function
#'
#' Internal helper used by \code{\link{add_stop_convergence}} (and
#' potentially other \code{add_*} generators) to splice an additional
#' \code{{...}} block of statements into an existing function's body,
#' either before or after its current statements.
#'
#' @param FUN The original function.
#' @param expr A new code block created by \code{quote({...})} or
#'   \code{bquote({...})}.
#' @param posit Whether to insert \code{expr} \code{"before"} or
#'   \code{"after"} the existing body. Defaults to \code{"after"}.
#' @return A new function with the combined body. \code{FUN} itself is not
#'   modified in place.
#' @seealso \code{\link{add_stop_convergence}}
#' @keywords internal
append_to_body <- function(FUN, expr, posit = c("after", "before")) {
  posit <- match.arg(posit)
  old_body <- body(FUN)
  old_stmts <- as.list(old_body)[-1]
  new_stmts <- as.list(expr)[-1]
  new_body <- if (posit == "before") {
    as.call(c(as.name("{"), new_stmts, old_stmts))
  } else {
    as.call(c(as.name("{"), old_stmts, new_stmts))
  }
  new_FUN <- FUN
  body(new_FUN) <- new_body
  new_FUN
}
