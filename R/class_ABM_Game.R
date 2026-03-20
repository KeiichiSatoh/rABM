#' @title ABM_Game (internal R6 class)
#' @name ABM_Game
#' @docType class
#'
#' @description
#' Internal R6 class used by **rABM** to represent a game (global state, groups,
#' and model-level functions) during ABM execution.
#'
#' @details
#' This class is implemented using the `R6` class system and is primarily intended
#' for internal infrastructure. For user-facing workflows, use [`Game()`] as the
#' recommended constructor and interact with objects through the public API
#' functions (e.g., add/remove/get/save/summary wrappers), rather than calling
#' R6 methods directly.
#'
#' The object manages multiple field categories:
#' `"state"`, `"active_state"`, `"act_FUN"`, `"stop_FUN"`,
#' `"report_FUN"`, and `"plot_FUN"`. Field names must be unique across categories.
#'
#' A key internal operation is taking snapshots of the current state.
#' The `.snapshot()` method retrieves only user-selected non-function fields
#' (global fields and/or group-level agent fields) in a lightweight manner.
#' To maximize speed during simulation, name validation and consistency checks
#' are expected to be performed in `run_Game()` (or an equivalent runner) before
#' simulation starts; `.snapshot()` itself avoids expensive checks.
#'
#' @section Public fields:
#' @field time Integer time step (default: `1`).
#' @field log List of saved snapshots (default: `NULL`).
#' @field notes List of notes (default: `NULL`).
#'
#' @section Public methods (internal):
#' - `initialize(...)`: Initialize an `ABM_Game` object.
#' - `.add(name, x, category)`: Register a field/method/active binding.
#' - `.remove(name)`: Remove a field and deactivate an active binding if needed.
#' - `.replace(name, x)`: Replace a field with x.
#' - `.get_category()`: Return the named vector of field categories.
#' - `.get_flist()`: Return a flattened data.frame of fields (including group fields).
#' - `.snapshot(global_field_names = NULL, group_names = NULL, group_field_names = NULL, add_tryCatch = FALSE)`:
#'   Retrieve a snapshot of selected non-function fields.
#' - `print(max_lines = 6, ...)`: Print a preview of fields with truncation.
#' - `.summary()`: Create a `summary.ABM_Game` object.
#'
#' @section `.snapshot()` (internal) details:
#' `.snapshot()` is designed to be called repeatedly during simulation.
#' It returns a list that combines:
#' \itemize{
#'   \item Selected global fields (named list).
#'   \item Selected group fields (nested as group -> agent -> field).
#'   \item `time` (current time step).
#' }
#'
#' The group snapshot is structured as:
#' `snapshot[[<group_name>]][[<agent_id>]][[<field_name>]]`.
#'
#' When `add_tryCatch = TRUE`, field access is wrapped in `tryCatch()`. If an error
#' occurs while retrieving a field, the corresponding value becomes
#' `structure(NULL, class = "error")`. This option is intended for debugging or
#' robust logging; for maximum performance, keep it `FALSE`.
#'
#' @section Parameters (for internal methods):
#' @param stage Global (non-function) fields to be stored as `"stage"`. Functions are not allowed.
#' @param active_stage Functions registered as active bindings (`"active_stage"`).
#' @param group A list of [`ABM_Group`] objects registered as `"group"`.
#' @param global_FUN Functions registered as model-level methods.
#' @param select_FUN Functions registered as model-level methods.
#' @param stop_FUN Functions registered as model-level methods.
#' @param report_FUN Functions registered as model-level methods.
#' @param plot_FUN Functions registered as model-level methods.
#' @param time A positive integer time step. If `NULL`, the default (`1`) is used.
#' @param log A list of saved snapshots (default: `NULL`).
#' @param notes A list of notes (default: `NULL`).
#' @param name A single field/method name (used in `.add()` / `.remove()`).
#' @param x An object to be registered (used in `.add()`).
#' @param category Field category to register `x` under (used in `.add()`).
#' @param max_lines Maximum number of lines to print per field (used in `print()`).
#' @param global_field_names Character vector of global field names to snapshot
#' (names in `"stage"` or other non-function global fields).
#' If `NULL`, no global fields are included.
#' @param group_names Character vector of group field names (i.e., names of groups
#' registered under category `"group"`). If `NULL`, no group fields are included.
#' @param group_field_names Named list of character vectors specifying, for each group,
#' which agent fields to snapshot. Must be keyed by `group_names`.
#' @param add_tryCatch Logical; if `TRUE`, wrap field access in `tryCatch()` and store
#' `structure(NULL, class = "error")` on error. Default is `FALSE` for performance.
#' @param ... Additional arguments (used in `print()`).
#'
#' @return
#' An `ABM_Game` object (R6).
#'
#' @seealso
#' [`Game()`], [`as.Game()`], [`summary.ABM_Game`]
#'
#' @keywords internal
#' @import R6
NULL


ABM_Game <- R6::R6Class(
  "ABM_Game", lock_objects = FALSE, cloneable = TRUE,
  public = list(
    time = 1,
    log = NULL,
    notes = NULL,
    #===========================================================================
    # initialize
    #===========================================================================
    initialize = function(...,
                          time = NULL,
                          log = NULL,
                          notes = NULL){
      # quick input validation-------------------
      ## time
      if (!is.null(time)) {
        if (!is.numeric(time) || length(time) != 1L || is.na(time) || time < 1 || time %% 1 != 0) {
          stop("'time' must be a positive integer of length 1.")
        }
      }

      ## log
      if(!is.null(log)){
        log_check <- is.list(log)
        if(isFALSE(log_check)){
          stop("'log' must be a list.")
        }
      }

      # create environment for active bindings
      self$.__enclos_env__$.__active__ <- list()
      # ensure the existence of field_category
      private$field_category <- character(0)

      # preparation of x
      x <- Unzip(...)
      if(all(vapply(x, is.null, FUN.VALUE = logical(1)))){
        return(self)
      }

      # check x to be the ABM_Field class
      field_check <- vapply(x, inherits, logical(1), what = "ABM_Field")
      stopifnot("Some elements in the input '...' are not 'ABM_Field' class objects." = all(field_check))

      # retrieve field name/category/value
      x_names <- vapply(x, function(x_i) x_i$name, character(1))
      stopifnot(
        "All ABM_Field objects must have a non-empty name." =
          all(!is.na(x_names) & nzchar(x_names))
      )

      dup <- unique(x_names[duplicated(x_names)])
      if (length(dup)) stop("Duplicated field names: ", paste(dup, collapse = ", "))

      # retrieve field category
      x_category <- vapply(x, function(x_i) x_i$category, character(1))
      names(x_category) <- x_names

      # retrieve field values
      x_values <- lapply(x, function(x_i) x_i$value)
      names(x_values) <- x_names

      # register the category
      private$field_category <- x_category

      # add-----------------------------------
      for (nm in x_names[x_category == "state"]) {
        private$.add_state(name = nm, x = x_values[[nm]])
      }

      for (nm in x_names[x_category == "active_state"]) {
        private$.add_active(name = nm, x = x_values[[nm]])
      }

      for (nm in x_names[x_category %in% c("act_FUN", "stop_FUN", "report_FUN", "plot_FUN")]) {
        private$.add_method(name = nm, x = x_values[[nm]])
      }


      # time
      if(!is.null(time)){
        self$time <- time
      }

      # log
      if(!is.null(log)){
        self$log <- log
      }

      # notes
      if (!is.null(notes)) {
        self$notes <- if (is.list(notes) && !is.object(notes)) notes else list(notes)
      }

      invisible(self)
    },
    #============================================================
    # add
    #============================================================
    .add = function(...){
      x <- Unzip(...)
      if(all(vapply(x, is.null, FUN.VALUE = logical(1)))){
        return(self)
      }

      # check x to be the ABM_Field class
      field_check <- vapply(x, inherits, logical(1), what = "ABM_Field")
      stopifnot("Some elements in the input '...' are not 'ABM_Field' class objects." = all(field_check))

      # retrieve field name/category/value
      x_names <- vapply(x, function(x_i) x_i$name, character(1))
      stopifnot(
        "All ABM_Field objects must have a non-empty name." =
          all(!is.na(x_names) & nzchar(x_names))
      )

      dup <- unique(x_names[duplicated(x_names)])
      if (length(dup)) stop("Duplicated field names: ", paste(dup, collapse = ", "))

      # check with the duplication
      fc_names <- names(private$field_category)
      dup_existed <- unique(x_names[x_names %in% fc_names])
      if (length(dup_existed)) stop("Duplicated field names with the existing names in 'G': ", paste(dup_existed, collapse = ", "))

      # retrieve field category
      x_category <- vapply(x, function(x_i) x_i$category, character(1))
      names(x_category) <- x_names

      # retrieve field values
      x_values <- lapply(x, function(x_i) x_i$value)
      names(x_values) <- x_names

      # register the category
      private$field_category <- c(private$field_category, x_category)

      # add-----------------------------------
      for (nm in x_names[x_category == "state"]) {
        private$.add_state(name = nm, x = x_values[[nm]])
      }

      for (nm in x_names[x_category == "active_state"]) {
        private$.add_active(name = nm, x = x_values[[nm]])
      }

      for (nm in x_names[x_category %in% c("act_FUN", "stop_FUN", "report_FUN", "plot_FUN")]) {
        private$.add_method(name = nm, x = x_values[[nm]])
      }

      invisible(self)
    },
    #================================================================
    # remove
    #================================================================
    .remove = function(...){
      field_names <- unlist(list(...))
      stopifnot("All elements in '...' must be a character." = all(is.character(field_names)))

      # retrieve the field_category
      fc <- private$field_category
      indices <- match(field_names, names(fc))
      if (any(is.na(indices))){
        stop("The following 'name' does not exist in the fields: ", paste0(field_names[is.na(indices)], collapse = ", "))
      }

      for(i in seq_along(field_names)){
        name <- field_names[i]
        idx <- indices[i]
        cat_i <- unname(fc[name])

      # 1) remove from self
      if (exists(name, envir = self, inherits = FALSE)) {
        rm(list = name, envir = self)
      }

      # 2) if active_stage, also remove from registry of active functions
      if (identical(cat_i, "active_state")) {
        if (!is.null(self$.__enclos_env__$.__active__)) {
          self$.__enclos_env__$.__active__[[name]] <- NULL
        }
      }

      # 3) update field_category
      idx <- which(names(private$field_category)==name)
      private$field_category <- private$field_category[-idx]
      }

      invisible(self)
      },

    #================================================================
    # replace
    #================================================================
    .replace = function(name, x){
      if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
        stop("'name' must be a single non-empty character string.")
      }

      fc <- private$field_category
      idx <- match(name, names(fc))
      if (is.na(idx)) stop("'name' does not exist in the fields.")

      cat_i <- unname(fc[idx])
      if (is.na(cat_i) || !cat_i %in% c("state", "active_stage",
                                        "act_FUN", "stop_FUN",
                                        "plot_FUN", "report_FUN")) {
        stop("Unknown field category: ", cat_i)
      }

      if (cat_i %in% c("act_FUN", "stop_FUN", "plot_FUN", "report_FUN")) {
        stopifnot("Put a function to the '*_FUN' category field." = is.function(x))
        unlockBinding(name, self)
        on.exit(lockBinding(name, self), add = TRUE)

        x <- .format_FUN_formals(x)
        environment(x) <- self$.__enclos_env__

        self[[name]] <- x
        return(invisible(self))
      }

      switch(cat_i,
             "state" = {
               stopifnot("Do not put a function to the 'state' category field." = !is.function(x))
               self[[name]] <- x
             },
             "active_state" = {
               stopifnot("Put a function to the 'active_state' category field." = is.function(x))
               self$.remove(name = name)      # remove binding + registry
               private$.add_active(name, x)   # recreate binding
             }
      )

      invisible(self)
    },
    #==================================================
    # get_category
    #==================================================
    .get_category = function(){private$field_category},
    #==================================================
    # get_flist
    #==================================================
    .get_flist = function() {
      fc <- private$field_category

      df <- data.frame(
        name     = names(fc),
        category = unname(fc),
        stringsAsFactors = FALSE
      )

      df
    },
    #=====================================================
    # snapshot
    #=====================================================
    .snapshot = function(field_names,
                         add_tryCatch = FALSE){
      if (add_tryCatch) {
        value <- setNames(vector("list", length(field_names)), field_names)
        for (nm in field_names) {
          value[[nm]] <- tryCatch(
            self[[nm]],
            error = function(e) structure(NULL, class = "error")
          )
        }
      } else {
        value <- setNames(vector("list", length(field_names)), field_names)
        for (nm in field_names) {
          value[[nm]] <- self[[nm]]
        }
      }

      # combine
      c(value, list(time = self$time))
    },
    #=====================================================
    # print
    #=====================================================
    print = function(max_lines = 6, ...) {
      stopifnot(
        "'max_lines' must be a single non-negative integer" =
          is.numeric(max_lines) &&
          length(max_lines) == 1L &&
          !is.na(max_lines) &&
          max_lines >= 0 &&
          max_lines == floor(max_lines)
      )
      max_lines <- as.integer(max_lines)

      truncated_any <- FALSE

      .truncate <- function(lines, max_lines) {
        truncated <- FALSE

        if (max_lines == 0L) {
          truncated <- length(lines) > 0
          return(list(lines = character(0), truncated = truncated))
        }

        if (length(lines) > max_lines) {
          truncated <- TRUE
          lines <- c(lines[seq_len(max_lines)], "  ---- (truncated) ----")
        }

        list(lines = lines, truncated = truncated)
      }

      .preview <- function(x, max_lines, ...) {
        if (is.function(x)) {
          out <- .truncate(deparse(x), max_lines)
        } else {
          lines <- capture.output(base::print(x, ...))
          if (!length(lines)) {
            lines <- capture.output(utils::str(x))
          }
          out <- .truncate(lines, max_lines)
        }

        if (length(out$lines)) {
          cat(paste(out$lines, collapse = "\n"), "\n", sep = "")
        }

        out$truncated
      }

      cat("<Game>\n")

      fc <- private$field_category
      for (nm in names(fc)) {
        cat("$", nm, "(", fc[[nm]], ")\n", sep = "")
        if (.preview(self[[nm]], max_lines, ...)) {
          truncated_any <- TRUE
        }
        cat("\n")
      }

      cat("-------------------", "\n")
      cat("time         :", self$time, "\n")
      cat("n of logs    :", length(self$log), "\n")
      cat("n of notes   :", length(self$notes), "\n")
      cat("n of fields  :", length(fc), "\n")

      cat_label <- c(state        = "state       : ",
                     active_state = "active_state: ",
                     act_FUN      = "act_FUN     : ",
                     stop_FUN     = "stop_FUN    : ",
                     plot_FUN     = "plot_FUN    : ",
                     report_FUN   = "report_FUN  : ")

      for(category in c("state","active_state","act_FUN","stop_FUN","plot_FUN","report_FUN")){
        if(any(fc == category)){
          cat(" ", unname(cat_label[category]), paste0(names(fc[fc==category]), collapse = ", "), "\n", sep = "")
        }
      }
      cat("-------------------", "\n")

      if (isTRUE(truncated_any)) {
        cat(
          "*Some fields are truncated. ",
          "Increase 'max_lines' to display more.\n",
          sep = ""
        )
      }

      invisible(NULL)
    },
    #==============================================
    # summary
    #==============================================
    .summary = function() {
      fl <- self$.get_flist()
      field_table <- table(factor(fl$category,
                                  c("state", "active_state",
                                    "act_FUN", "stop_FUN",
                                    "report_FUN", "plot_FUN")),
                                  useNA = "always")

      # output
      out <- list(
        n_fields = nrow(fl),
        field_table = field_table,
        field_list = fl,
        time = self$time,
        n_log = if (!is.null(self$log)) length(self$log) else 0,
        note_names = if (!is.null(self$notes)) names(self$notes) else NULL
      )
      class(out) <- "summary.ABM_Game"
      out
    }
  ),
  private = list(
    #=====================================
    # add_stage
    #=====================================
    .add_state = function(name, x){
      self[[name]] <- x
    },
    #=====================================
    # add_method
    #=====================================
    .add_method = function(name, x) {
      # format the formals
      f <- formals(x)
      f[names(f)=="E"] <- NULL
      formals(x) <- c(alist(E = E), f)
      # input
      environment(x) <- self$.__enclos_env__
      self[[name]] <- x
    },
    #=====================================
    # add_active
    #=====================================
    .add_active = function(name, x) {
      # format the formals
      f <- formals(x)
      f[names(f)=="E"] <- NULL
      formals(x) <- c(alist(E = E), f)
      # input
      environment(x) <- self$.__enclos_env__
      makeActiveBinding(name, x, self)
      self$.__enclos_env__$.__active__[[name]] <- x
    }
  )
)


