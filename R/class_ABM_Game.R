#-------------------------------------------------------------------------------
# ABM_Game (internal R6 class) and its user-facing constructor Game()
#-------------------------------------------------------------------------------

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
#' Dynamically added methods (`act_FUN`, `stop_FUN`, `report_FUN`,
#' `plot_FUN`, `active_state`) keep access to the environment in which
#' they were originally defined (e.g. a calling package's namespace),
#' in addition to `self`/`private`. This allows such methods to call
#' non-exported functions from the package where they were written.
#' Because `R6`'s built-in `clone()` does not preserve this behavior
#' for functions/active bindings, use [`copy_obj()`] (or call
#' `$.rebind_dynamic_fields()` manually after `$clone(deep = TRUE)`)
#' to obtain a fully independent, correctly-bound copy.
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
#'   #'   \item{`.snapshot(field_names, add_tryCatch)`}{Retrieve a snapshot of selected
#'   fields as a named list, appended with `time`. If `add_tryCatch = TRUE`,
#'   errors during field access are captured as the raised condition object
#'   (which inherits from class `"error"`) instead of propagating.}
#'   \item{`print(fields, max_lines, ...)`}{Print a preview of fields with
#'   truncation. By default (`fields = FALSE`), only metadata and a
#'   per-category field-name summary are shown; pass `fields = TRUE` to
#'   also preview each field's contents.}
#'   \item{`.rebind_dynamic_fields()`}{Re-bind all dynamically added methods
#'   and active bindings (`act_FUN`, `stop_FUN`, `report_FUN`, `plot_FUN`,
#'   `active_state`) so that `self`/`private` refer to the current object,
#'   while preserving access to each method's original definition
#'   environment. Intended to be called after `$clone(deep = TRUE)`,
#'   since R6's default clone does not correctly carry over dynamically
#'   added functions and active bindings.}
#' }
#'
#' @seealso [`Game()`], [`copy_obj()`], [`summary.ABM_Game`]
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
      has_fields <- !all(vapply(x, is.null, FUN.VALUE = logical(1)))

      if (has_fields) {
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

        # register the category only after all additions have succeeded
        private$field_category <- x_category
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

      # register the category only after all additions have succeeded
      private$field_category <- c(private$field_category, x_category)

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

        # 3) if act_FUN/stop_FUN/report_FUN/plot_FUN or active_state,
        #    also remove the recorded original environment (used by clone())
        if (!is.null(private$.method_registry[[name]])) {
          private$.method_registry[[name]] <- NULL
        }

        # 4) update field_category
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

      field_check <- vapply(x, inherits, logical(1), what = "ABM_Field")
      stopifnot("Some elements in the input '...' are not 'ABM_Field' class objects." = all(field_check))

      x_names <- vapply(x, function(x_i) x_i$name, character(1))
      stopifnot(
        "All ABM_Field objects must have a non-empty name." =
          all(!is.na(x_names) & nzchar(x_names))
      )

      dup <- unique(x_names[duplicated(x_names)])
      if (length(dup)) stop("Duplicated field names in the input: ", paste(dup, collapse = ", "))

      # retrieve the field category
      fc <- private$field_category

      # match the field category
      matched <- match(x_names, names(fc))
      if(any(is.na(matched))){
        stop("The following field(s) do not exist: ", paste(x_names[is.na(matched)], collapse = ", "))
      }

      # Snapshot the fields being replaced, so we can roll back if `.add()`
      # fails after `.remove()` has already mutated the object.
      backup <- lapply(x_names, function(nm) {
        cat_nm <- unname(fc[nm])
        val <- if (identical(cat_nm, "state")) {
          self[[nm]]
        } else {
          private$.method_registry[[nm]]$fn
        }
        list(name = nm, category = cat_nm, value = val)
      })

      # Remove the old fields
      self$.remove(x_names)

      # Add the new ones; roll back to the pre-remove state on failure.
      #
      # NOTE: this rollback is not fully atomic. If self$.add(...) fails
      # partway through -- i.e. after adding some but not all of the
      # replacement fields -- those partially-added fields remain attached
      # to `self` even though they are not registered in `field_category`
      # (registration only happens after the whole .add() loop succeeds).
      # Such fields become invisible to .get_flist()/print()/etc. but are
      # still reachable via self$<name> directly. This is a known,
      # low-probability limitation: it can only occur if a field addition
      # itself throws partway through .add()'s internal loop (e.g. an
      # unexpected error inside a state/active/method assignment), not from
      # ordinary validation failures, which are all raised before any
      # fields are added.
      add_result <- tryCatch({
        self$.add(...)
        TRUE
      }, error = function(e) e)

      if (!isTRUE(add_result)) {
        for (b in backup) {
          if (identical(b$category, "state")) {
            private$.add_state(name = b$name, x = b$value)
          } else if (identical(b$category, "active_state")) {
            private$.add_active(name = b$name, x = b$value)
          } else {
            private$.add_method(name = b$name, x = b$value)
          }
        }
        backup_categories <- setNames(
          vapply(backup, function(b) b$category, character(1)),
          vapply(backup, function(b) b$name, character(1))
        )
        private$field_category <- c(private$field_category, backup_categories)

        stop(
          "'.replace()' failed while adding the replacement field(s); ",
          "the original field(s) have been restored. Original error: ",
          conditionMessage(add_result),
          call. = FALSE
        )
      }

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
            error = function(e) e
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
    print = function(fields = FALSE, max_lines = 6, ...) {
      stopifnot(
        "'fields' must be a single logical value." =
          is.logical(fields) && length(fields) == 1L && !is.na(fields)
      )
      stopifnot(
        "'max_lines' must be a single non-negative integer" =
          is.numeric(max_lines) &&
          length(max_lines) == 1L &&
          !is.na(max_lines) &&
          max_lines >= 0 &&
          max_lines == floor(max_lines)
      )
      max_lines <- as.integer(max_lines)

      fc <- private$field_category
      truncated_any <- FALSE

      cat("<Game>\n")

      if (isTRUE(fields)) {

        .truncate_lines <- function(lines, max_lines) {
          truncated <- FALSE
          if (max_lines == 0L) {
            return(list(lines = character(0), truncated = length(lines) > 0))
          }
          if (length(lines) > max_lines) {
            truncated <- TRUE
            lines <- c(lines[seq_len(max_lines)], "  ---- (truncated) ----")
          }
          list(lines = lines, truncated = truncated)
        }

        .preview <- function(x, max_lines) {
          if (is.function(x)) {
            out <- .truncate_lines(deparse(x), max_lines)
          } else if ((is.data.frame(x) || is.matrix(x)) && nrow(x) > max_lines) {
            lines <- capture.output(base::print(utils::head(x, max_lines)))
            out <- list(lines = c(lines, "  ---- (truncated) ----"), truncated = TRUE)
          } else if (is.atomic(x) && is.null(dim(x)) && length(x) > max_lines) {
            lines <- capture.output(base::print(utils::head(x, max_lines)))
            out <- list(lines = c(lines, "  ---- (truncated) ----"), truncated = TRUE)
          } else {
            lines <- capture.output(base::print(x))
            if (!length(lines)) lines <- capture.output(utils::str(x))
            out <- .truncate_lines(lines, max_lines)
          }

          if (length(out$lines)) {
            cat(paste(out$lines, collapse = "\n"), "\n", sep = "")
          }
          out$truncated
        }

        for (nm in names(fc)) {
          cat("$", nm, " (", .category_label(fc[[nm]]), ")\n", sep = "")
          if (.preview(self[[nm]], max_lines)) {
            truncated_any <- TRUE
          }
          cat("\n")
        }
      }

      cat("-------------------", "\n")
      cat("time          :", self$time, "\n")
      cat("n of logs     :", length(self$log), "\n")
      cat("n of notes    :", length(self$notes), "\n")
      cat("n of fields   :", length(fc), "\n")

      category_order <- c("state", "active_state", "act_FUN", "stop_FUN", "report_FUN", "plot_FUN")
      labels         <- .category_label(category_order)
      label_width    <- max(nchar(labels))
      avail_width    <- max(getOption("width", 80L) - label_width - 4L, 20L)

      for (i in seq_along(category_order)) {
        category <- category_order[i]
        if (any(fc == category)) {
          label   <- formatC(labels[i], width = -label_width)
          nms     <- names(fc[fc == category])
          wrapped <- strwrap(paste(nms, collapse = ", "), width = avail_width)

          cat("  ", label, ": ", wrapped[1L], "\n", sep = "")
          if (length(wrapped) > 1L) {
            pad <- strrep(" ", label_width + 4L)
            for (w in wrapped[-1L]) cat(pad, w, "\n", sep = "")
          }
        }
      }
      cat("-------------------", "\n")

      if (isTRUE(fields) && isTRUE(truncated_any)) {
        cat("*Some fields are truncated. Increase 'max_lines' to display more.\n")
      }
      if (!isTRUE(fields)) {
        cat("*Field contents are hidden by default. Use print(fields = TRUE) to preview them.\n")
      }

      invisible(NULL)
    },

    #=====================================
    # .rebind_dynamic_fields
    #=====================================
    .rebind_dynamic_fields = function() {
      fc <- private$field_category

      for (nm in names(private$.method_registry)) {
        reg      <- private$.method_registry[[nm]]
        orig_fn  <- reg$fn
        orig_env <- reg$orig_env
        category <- unname(fc[nm])

        new_env  <- new.env(parent = orig_env)
        new_env$self    <- self
        new_env$private <- private

        fn <- orig_fn
        environment(fn) <- new_env

        if (identical(category, "active_state")) {
          if (exists(nm, envir = self, inherits = FALSE)) {
            rm(list = nm, envir = self)
          }
          makeActiveBinding(nm, fn, self)
          self$.__enclos_env__$.__active__[[nm]] <- fn
        } else {
          self[[nm]] <- fn
        }
      }

      invisible(self)
    }
  ),
  private = list(
    #=====================================
    # field_category
    #=====================================
    field_category = character(0),
    #=====================================
    # method_orig_env
    #=====================================
    .method_registry = list(),
    #=====================================
    # add_state
    #=====================================
    .add_state = function(name, x){
      self[[name]] <- x
    },
    #=====================================
    # add_method
    #=====================================
    .add_method = function(name, x) {
      orig_env <- environment(x)
      private$.method_registry[[name]] <- list(fn = x, orig_env = orig_env)

      new_env  <- new.env(parent = orig_env)
      new_env$self    <- self
      new_env$private <- private
      environment(x) <- new_env
      self[[name]] <- x
    },
    #=====================================
    # add_active
    #=====================================
    .add_active = function(name, x) {
      orig_env <- environment(x)
      private$.method_registry[[name]] <- list(fn = x, orig_env = orig_env)

      new_env  <- new.env(parent = orig_env)
      new_env$self    <- self
      new_env$private <- private
      environment(x) <- new_env
      makeActiveBinding(name, x, self)
      self$.__enclos_env__$.__active__[[name]] <- x
    }
  )
)

#-------------------------------------------------------------------------------
# Game(): user-facing constructor
#-------------------------------------------------------------------------------

#' Create an ABM game object
#'
#' `Game()` is the user-facing constructor for an [`ABM_Game`] object,
#' the core object of the **rABM** package.
#' It wraps the internal R6 class [`ABM_Game`] so that users do not need to
#' interact with R6 directly.
#'
#' @details
#' The `ABM_Game` object manages several field categories:
#' - `"state"`: non-function global fields (e.g., parameters, data objects)
#' - `"active_state"`: active bindings (functions evaluated on access)
#' - `"act_FUN"`, `"stop_FUN"`, `"report_FUN"`, `"plot_FUN"`:
#'   functions registered as model-level methods
#'
#' Field names must be unique across all categories.
#'
#' `Game()` passes \code{...} directly to \code{ABM_Game$new()}. Each element
#' of \code{...} must be an [`ABM_Field`] object -- typically created with
#' [`State()`], [`Active()`], [`Act()`], [`Stop()`], [`Report()`], or
#' [`Plot()`] -- or an [`ABM_Zip`] bundle of such objects created with
#' [`Zip()`], which is flattened automatically.
#'
#' @param ... [`ABM_Field`] objects (see [`State()`], [`Active()`],
#'   [`Act()`], [`Stop()`], [`Report()`], [`Plot()`]), or [`Zip()`] bundles
#'   thereof, to be registered on the game.
#' @param time A positive integer time step. If `NULL`, the default (`1`) is used.
#' @param log A list of saved snapshots (default: `NULL`).
#' @param notes A list of notes (default: `NULL`).
#'
#' @return An [`ABM_Game`] object.
#'
#' @seealso [`ABM_Game`], [`Field`], [`Zip`]
#'
#' @export
#' @examples
#' pop <- 100
#' growth_rate <- function(rate = 1.05) { self$pop * rate }
#' reproduce   <- function() { self$pop <- self$pop * 1.1 }
#'
#' G <- Game(
#'   State(pop),
#'   Active(growth_rate),
#'   Act(reproduce)
#' )
#' G
#'
#' # ABM_Field objects can also be bundled with Zip() and passed as one argument
#' common_fields <- Zip(State(pop), Active(growth_rate))
#' G2 <- Game(common_fields, Act(reproduce))
Game <- function(...,
                 time = NULL, log = NULL, notes = NULL){
  ABM_Game$new(...,
               time = time,
               log = log,
               notes = notes)
}

#-------------------------------------------------------------------------------
# Summary
#-------------------------------------------------------------------------------

#' Summarize an ABM game object
#'
#' `ABM_Game` objects do not provide a dedicated statistical summary, since
#' the content and meaningful aggregation of \code{state} fields varies
#' widely across models. This method exists so that calling
#' \code{summary()} on a [`Game()`] object gives a clear, actionable
#' message instead of silently falling back to [`summary.default()`].
#'
#' @param object An [`ABM_Game`] object.
#' @param ... Reserved for future extensions; currently unused.
#'
#' @return Invisibly returns \code{object}.
#'
#' @seealso [`ABM_Game`], [`Game()`]
#'
#' @export
summary.ABM_Game <- function(object, ...) {
  message(
    "summary() is not implemented for 'ABM_Game' objects, since ",
    "meaningful summary statistics depend heavily on what each model ",
    "stores in its 'state' fields. Use print(object) or ",
    "print(object, fields = TRUE) to inspect the game instead."
  )
  invisible(object)
}

#-------------------------------------------------------------------------------
# (internal) Map internal field category strings to display labels
#-------------------------------------------------------------------------------

#' Map internal field category strings to user-facing display labels (internal)
#'
#' @param category Character vector of internal category strings (e.g.
#'   \code{"state"}, \code{"active_state"}, \code{"act_FUN"}, ...).
#' @return Character vector of the same length giving human-readable labels,
#'   for use in \code{print()} output. The underlying category strings
#'   themselves (see \link{Field}) are unaffected; this is purely a
#'   presentation-layer mapping.
#' @keywords internal
.category_label <- function(category) {
  labels <- c(
    state        = "State",
    active_state = "Active State",
    act_FUN      = "Act",
    stop_FUN     = "Stop",
    report_FUN   = "Report",
    plot_FUN     = "Plot"
  )
  unname(labels[category])
}
