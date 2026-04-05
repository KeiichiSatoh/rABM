#' @title ABM_Game (internal R6 class)
#' @name ABM_Game
#' @docType class
#'
#' @description
#' Internal R6 class representing a game object during ABM execution.
#' For user-facing workflows, use [`Game()`] as the constructor.
#'
#' @details
#' Manages fields across the following categories:
#' `"state"`, `"active_state"`, `"act_FUN"`, `"stop_FUN"`,
#' `"report_FUN"`, and `"plot_FUN"`. Field names must be unique across
#' all categories.
#'
#' @section Public fields:
#' \describe{
#'   \item{time}{Integer time step (default: `1`).}
#'   \item{log}{List of saved snapshots (default: `NULL`).}
#'   \item{notes}{List of notes (default: `NULL`).}
#' }
#'
#' @section Public methods:
#' \describe{
#'   \item{`initialize(..., time, log, notes)`}{Initialize an `ABM_Game` object.
#'   `...` accepts `ABM_Field` objects.}
#'   \item{`.add(...)`}{Add `ABM_Field` objects to the game.}
#'   \item{`.remove(...)`}{Remove fields by name.}
#'   \item{`.replace(...)`}{Replace a field value or function.}
#'   \item{`.get_category()`}{Return the named character vector of field categories.}
#'   \item{`.get_flist()`}{Return a `data.frame` of field names and categories.}
#'   \item{`.snapshot(field_names, add_tryCatch)`}{Retrieve a snapshot of selected
#'   fields as a named list, appended with `time`. If `add_tryCatch = TRUE`,
#'   errors during field access are captured as `structure(NULL, class = "error")`.}
#'   \item{`print(max_lines, ...)`}{Print a preview of fields with truncation.}
#'   \item{`.summary()`}{Return a `summary.ABM_Game` object.}
#' }
#'
#' @seealso [`Game()`], [`as.Game()`], [`summary.ABM_Game`]
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
    .replace = function(...){
      # Unzip the input
      x <- Unzip(...)
      x_names <- vapply(x, function(x_i) x_i$name, character(1))

      field_check <- vapply(x, inherits, logical(1), what = "ABM_Field")
      stopifnot("Some elements in the input '...' are not 'ABM_Field' class objects." = all(field_check))

      # retrieve the field category
      fc <- private$field_category

      # match the field category
      matched <- match(x_names, names(fc))
      if(any(is.na(matched))){
        stop("The following field(s) do not exist: ", paste(x_names[is.na(matched)], collapse = ", "))
      }

      # Remove the field
      self$.remove(x_names)

      # add the new ones
      self$.add(...)


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
      # input
      environment(x) <- self$.__enclos_env__
      self[[name]] <- x
    },
    #=====================================
    # add_active
    #=====================================
    .add_active = function(name, x) {
      # input
      environment(x) <- self$.__enclos_env__
      makeActiveBinding(name, x, self)
      self$.__enclos_env__$.__active__[[name]] <- x
    }
  )
)


