#' Run a chunk repeatedly over a set of feed scenarios
#'
#' @param chunk       An object of class \code{ABM_Chunk}. The code evaluated
#'                     on each run.
#' @param feed        A \code{data.frame} or \code{list} supplying the
#'                     scenario-specific variables. If \code{NULL}, the chunk
#'                     is run exactly once with no variable substitution.
#' @param feed_type   Either \code{"data.frame"} or \code{"list"}.
#' @param n_each_run  Number of times each scenario is repeated.
#' @param parallel    If \code{TRUE}, runs in parallel using the \code{future}
#'                     and \code{future.apply} packages (must be installed in
#'                     advance via \code{install.packages(c("future", "future.apply"))}).
#' @param n_cores     Number of cores to use for parallel execution. If
#'                     \code{NULL} (the default), \code{future::availableCores() - 1}
#'                     is used.
#' @param plan_type   One of \code{"auto"} (default), \code{"multicore"}, or
#'                     \code{"multisession"}.
#'                     \code{"multicore"} forks the current process, so loaded
#'                     packages and objects are inherited automatically, but
#'                     it is only available on Unix/Mac (not on Windows, and
#'                     not in an RStudio session where forking is unsafe).
#'                     \code{"multisession"} starts new background R sessions
#'                     and works on every OS. \code{"auto"} selects
#'                     \code{"multicore"} only when
#'                     \code{future::supportsMulticore()} is \code{TRUE}, and
#'                     falls back to \code{"multisession"} otherwise.
#' @param inherit_env  When using \code{"multisession"}, whether to
#'                     automatically pass on the objects in the parent
#'                     environment (the environment given by the \code{parent}
#'                     argument) and the currently attached packages to each
#'                     worker. Defaults to \code{TRUE}.
#'                     (With \code{"multicore"} everything is always inherited
#'                     automatically, so this argument has no effect there.)
#' @param export_objects A character vector of object names to explicitly
#'                     export when using \code{"multisession"}. If \code{NULL}
#'                     (the default) and \code{inherit_env = TRUE},
#'                     \code{ls(parent, all.names = TRUE)} is used
#'                     automatically; if \code{inherit_env = FALSE}, nothing is
#'                     exported.
#' @param export_pkgs  A character vector of package names to load on each
#'                     worker when using \code{"multisession"}. If \code{NULL}
#'                     (the default) and \code{inherit_env = TRUE}, every
#'                     currently attached package (\code{(.packages())}) is
#'                     used automatically; if \code{inherit_env = FALSE},
#'                     nothing is loaded.
#'
#' @details
#' Notes on parallel execution:
#' \itemize{
#'   \item Each run uses its own seed drawn from \code{seed_list}, so results
#'   are reproducible regardless of execution order (sequential or parallel).
#'   \item If the \code{progressr} package is installed, a live progress bar
#'   is shown even during parallel execution. If it is not installed, only
#'   the start/finish messages are shown, as before.
#'   \item With \code{on_error = "stop"}, sequential execution stops
#'   immediately at the first error, whereas parallel execution waits for all
#'   already-dispatched runs to finish and then stops on the first error
#'   found (runs that have already been dispatched in parallel cannot be
#'   individually interrupted).
#' }
#'
#' Accessing feed variables explicitly:
#' By default, the variables supplied through \code{feed} are placed
#' directly into the environment in which \code{chunk} is evaluated, so
#' they can be referenced as plain bare names (e.g. \code{x}) inside
#' \code{chunk}, exactly as if they were ordinary local variables. If you
#' would rather make it explicit, within \code{chunk}, which variables come
#' from \code{feed}, that same evaluation environment is also bound to the
#' name \code{V} inside \code{chunk}, so the same values can equivalently be
#' written as \code{V$x}. Because of this, \code{V} is a reserved name while
#' \code{chunk} runs; avoid assigning your own variable named \code{V}
#' inside \code{chunk}.
#'
#' A practical way to develop the logic that will go inside \code{chunk}
#' while keeping this feed/non-feed distinction visible is to first mock up
#' a single run's worth of the values you intend to place in \code{feed} as
#' an ordinary list, e.g. \code{V <- list(x = 1, rate = 1.05)}, and
#' prototype the intended computation interactively against it (e.g.
#' \code{V$x * V$rate}). Once the logic behaves as expected, wrap it in
#' \code{Chunk({ ... })} (referring to the same values either as \code{x},
#' \code{rate}, or \code{V$x}, \code{V$rate}) and move the swept values into
#' \code{feed}.
#'
#' @return
#' If \code{output_only = TRUE}, only \code{values_out} is returned (a named
#' list of each run's result, named by \code{batch_label}). Otherwise, a list
#' with the following elements:
#' \describe{
#'   \item{values_out}{The result of each run (a named list).}
#'   \item{seed}{The seed used for each run (a named numeric vector).}
#'   \item{batch_label}{The label of each run (\code{"B<scenario>_<run>"}).}
#'   \item{feed}{Only when \code{return_with_feed = TRUE}. The feed value used
#'   for each run.}
#'   \item{error_log}{A named list holding the error raised in each run (or
#'   \code{NULL} if none).}
#'   \item{implementation_took}{Elapsed run time, formatted as
#'   \code{"hh:mm:ss.mmm"}.}
#' }
#'
#' @examples
#' sample_chunk <- Chunk({
#'   x <- 1
#'   y <- x + 1
#' })
#' batch_Chunk(sample_chunk, feed = data.frame(x = 1:3))
#'
#' # Starting a 'future' plan (spawning/forking worker processes) has
#' # noticeable overhead, so this example is skipped by CRAN's routine
#' # checks (\donttest) even though it runs correctly -- see ?tools::Rd2txt
#' # or Writing R Extensions for the distinction between \donttest and
#' # \dontrun.
#' \donttest{
#' if (requireNamespace("future", quietly = TRUE) &&
#'     requireNamespace("future.apply", quietly = TRUE)) {
#'   batch_Chunk(sample_chunk, feed = data.frame(x = 1:3), parallel = TRUE, n_cores = 2)
#' }
#' }
#'
#' # Feed variables can also be accessed explicitly as V$<name>, instead of
#' # relying on them being available as bare names (see Details).
#' chunk_v_style <- Chunk({
#'   y <- V$x + 1
#' })
#' batch_Chunk(chunk_v_style, feed = data.frame(x = 1:3))
#'
#' # A more realistic example: sweep a run_Game() simulation over several
#' # parameter values, with 2 independent repetitions per value.
#' add_money <- function(b = 1) {
#'   self$money[self$selected_agent] <- self$money[self$selected_agent] + b
#' }
#' select_agent <- function() {
#'   self$selected_agent <- sample(1:5, size = 2)
#' }
#' money          <- 1:5
#' selected_agent <- 1:5
#' G <- Game(State(money), State(selected_agent), Act(add_money), Act(select_agent))
#'
#' # 'b' (the amount added at each step) is the feed variable being swept;
#' # it is spliced into the 'plan' string to override add_money()'s default
#' # for that run only (see run_Game()'s call-style plan arguments).
#' sim_chunk <- Chunk({
#'   G_i <- run_Game(G, plan = c("select_agent", paste0("add_money(b = ", b, ")")),
#'                    times = 10, verbose = FALSE)
#'   total_money <- sum(G_i$money)
#' })
#' res <- batch_Chunk(
#'   sim_chunk,
#'   feed = data.frame(b = c(1, 2, 5)),
#'   n_each_run = 2,
#'   keep = "total_money",
#'   return_with_feed = TRUE,
#'   verbose = FALSE
#' )
#' res$values_out
#' res$feed
batch_Chunk <- function(
    chunk,
    feed = NULL,
    feed_type = c("data.frame", "list"),
    n_each_run = 1,
    parallel = FALSE,
    n_cores = NULL,
    plan_type = c("auto", "multicore", "multisession"),
    inherit_env = TRUE,
    export_objects = NULL,
    export_pkgs = NULL,
    return_with_feed = FALSE,
    seed = NULL,
    on_error = c("continue","stop"),
    keep = NULL,
    parent = parent.frame(),
    verbose = TRUE,
    output_only = FALSE,
    return_FUN = NULL
){
  #---- validation ------------
  stopifnot("'chunk' must be a class of 'ABM_Chunk'." = inherits(chunk, "ABM_Chunk"))
  # Check length before checking the value itself
  stopifnot("'n_each_run' must be a positive integer of length 1." = length(n_each_run) == 1)
  stopifnot("'n_each_run' must be a positive integer of length 1." = n_each_run > 0 && n_each_run %% 1 == 0)
  stopifnot("'parallel' must be TRUE or FALSE." = is.logical(parallel) && length(parallel) == 1)
  on_error <- match.arg(on_error)
  feed_type <- match.arg(feed_type)
  plan_type <- match.arg(plan_type)

  if(is.null(feed)){
    # If no feed is supplied, run once with no variable substitution
    feed_type <- "list"
    feed <- list(list())
  }

  #---- reshape feed ----------
  if(feed_type == "data.frame"){
    stopifnot("'feed' must be a class of 'data.frame' when 'feed_type' is 'data.frame'." = is.data.frame(feed))
    feed_list <- vector("list", nrow(feed))
    for(k in seq_len(nrow(feed))){
      feed_list[[k]] <- feed[k, ,drop = FALSE]
    }
  }else{
    # feed_type == "list"
    stopifnot("'feed' must be a class of 'list' when 'feed_type' is 'list'." = is.list(feed))
    feed_list <- feed
  }

  # ------ batch settings --------------------------
  n_scenarios <- length(feed_list)
  n_total_run <- n_scenarios * n_each_run

  # seed
  if (is.null(seed)){
    seed_list <- vapply(seq_len(n_total_run), FUN = function(i) sample.int(.Machine$integer.max, 1L), FUN.VALUE = integer(1))
  }else{
    # Check length before checking the values themselves
    stopifnot("The length of 'seed' must be total number of run to be implemented (i.e., length of feeds * n_each_run)." = length(seed) == n_total_run)
    stopifnot("Each element of 'seed' must be a positive integers." = all(seed > 0 & seed %% 1 == 0))
    seed_list <- seed
  }

  # put batch label
  # Cycle l (run) faster than k (scenario) so that repeated runs within the
  # same scenario are also adjacent in the labels.
  batch_label <- apply(expand.grid(seq_len(n_each_run), seq_len(n_scenarios)), 1,
                       function(x){paste0("B", x[2], "_", x[1])})
  names(seed_list) <- batch_label

  # Index table for (k = scenario, l = run) in the actual loop order
  # (k slow, l fast)
  idx <- expand.grid(l = seq_len(n_each_run), k = seq_len(n_scenarios))
  idx <- idx[order(idx$k, idx$l), , drop = FALSE]
  idx$m <- seq_len(n_total_run)

  # ------ single-run worker (called from both sequential and parallel execution) ------------
  # When 'inherited' is supplied (i.e. during "multisession" execution), a
  # small environment is built on the spot from the values taken from the
  # parent environment (a plain list) and used to evaluate the chunk.
  # When 'inherited' is NULL (sequential or "multicore" execution), the
  # original 'parent' environment is used as-is (its contents can be accessed
  # directly, since the process is either the same one or a forked copy).
  run_one <- function(m, k, l, inherited = NULL){
    base_env <- if (!is.null(inherited)) list2env(inherited, parent = globalenv()) else parent
    V <- list2env(feed_list[[k]], parent = base_env)

    # Bind the evaluation environment to the name 'V' inside itself, so
    # that feed-provided variables can optionally be accessed explicitly
    # as V$<name> inside 'chunk', instead of relying on them being
    # available as bare names. This is removed again below (before values
    # are collected) so it never leaks into the run's output; as a
    # consequence, 'V' is a reserved name inside 'chunk' while it runs.
    assign("V", V, envir = V)

    # set seed (each run uses its own seed, so results are reproducible
    # regardless of execution order or sequential/parallel mode)
    set.seed(seed = seed_list[[m]])

    err <- NULL
    tryCatch(
      eval(chunk, envir = V),
      error = function(e) {
        err <<- e
        if (isTRUE(verbose)) {
          cat("    ERROR: ", conditionMessage(e), "\n", sep = "")
        }
        invisible(NULL)
      }
    )

    # Remove the self-reference again so it never leaks into values_out.
    if (exists("V", envir = V, inherits = FALSE)) rm("V", envir = V)

    # ------ collect values ------
    if (!is.null(keep)) {
      values_temp <- as.list(V)
      missing <- setdiff(keep, names(values_temp))
      keep2 <- intersect(keep, names(values_temp))
      val <- values_temp[keep2]
      attr(val, "missing_keep") <- missing
    }else{
      val <- as.list(V)
    }

    list(value = val, error = err)
  }

  # [optional] print the total number of scenarios
  if(isTRUE(verbose)){
    cat("Total number of scenarios      :", n_scenarios, "\n")
    cat("Number of run for each scenario:", n_each_run, "\n")
    cat("\n")
  }

  # ------ RUN (sequential or parallel) ------------------------------------
  results <- vector("list", n_total_run)

  if (!isTRUE(parallel)) {
    #### ---- Sequential execution ----
    if(isTRUE(verbose)) cat("Ready to run...", "\n")

    # start time
    start_time <- Sys.time()

    # Running
    for (r in seq_len(nrow(idx))) {
      m <- idx$m[r]; k <- idx$k[r]; l <- idx$l[r]

      if (isTRUE(verbose)) {
        cat(paste0("  [", batch_label[m], "] ", "Implementing ", l, "th run of the scenario ", k, ".", "\n"))
      }

      res <- run_one(m, k, l, inherited = NULL)
      results[[m]] <- res

      if (!is.null(res$error) && identical(on_error, "stop")) {
        stop(res$error)
      }
    }

  } else {
    #### ---- Parallel execution (future) ----
    if (!requireNamespace("future", quietly = TRUE) || !requireNamespace("future.apply", quietly = TRUE)) {
      stop("parallel = TRUE requires the 'future' and 'future.apply' packages. Please run: install.packages(c(\"future\", \"future.apply\"))")
    }

    avail_cores <- future::availableCores()
    if (is.na(avail_cores)) avail_cores <- 1L

    if (is.null(n_cores)) {
      n_cores <- max(1L, avail_cores - 1L)
    } else {
      stopifnot("'n_cores' must be a positive integer of length 1." = length(n_cores) == 1 && n_cores > 0 && n_cores %% 1 == 0)
      if (n_cores > avail_cores) {
        warning("'n_cores' (", n_cores, ") exceeds the number of available cores (", avail_cores, "). Using ", avail_cores, " instead.")
        n_cores <- avail_cores
      }
    }

    if (identical(plan_type, "auto")) {
      plan_type <- if (future::supportsMulticore()) "multicore" else "multisession"
    }
    if (identical(plan_type, "multicore") && !future::supportsMulticore()) {
      stop("'plan_type = \"multicore\"' is not supported in this environment (e.g. Windows, or an unsafe RStudio session). Use 'multisession' or 'auto' instead.")
    }

    # Save the current future plan and always restore it when the function exits
    oplan <- future::plan(plan_type, workers = n_cores)
    on.exit(future::plan(oplan), add = TRUE)

    inherited <- NULL
    pkgs_to_export <- NULL
    if (identical(plan_type, "multisession")) {
      # multisession runs in separate processes, so objects from the parent
      # environment are taken out as plain values in advance ('inherited')
      # and reassembled into a small environment on each run.
      objs_to_export <- export_objects
      if (is.null(objs_to_export)) {
        objs_to_export <- if (isTRUE(inherit_env)) ls(parent, all.names = TRUE) else character(0)
      }
      if (length(objs_to_export)) {
        inherited <- mget(objs_to_export, envir = parent, inherits = FALSE)
      }

      pkgs_to_export <- export_pkgs
      if (is.null(pkgs_to_export)) {
        pkgs_to_export <- if (isTRUE(inherit_env)) (.packages()) else character(0)
      }
    }
    # With "multicore", the current process is forked, so packages and
    # objects (including the parent environment) are inherited automatically
    # with no extra work. (inherit_env / export_objects / export_pkgs have no
    # effect with "multicore".)

    use_progress <- isTRUE(verbose) && requireNamespace("progressr", quietly = TRUE)
    if (isTRUE(verbose) && !use_progress) {
      cat("(Tip: install the 'progressr' package to see a live progress bar during parallel runs.)\n")
    }
    if(isTRUE(verbose)){
      cat("Ready to run in parallel (plan_type = ", plan_type, ", n_cores = ", n_cores, ")...\n", sep = "")
    }

    # start time
    start_time <- Sys.time()

    # Running
    run_all_parallel <- function() {
      p <- if (use_progress) progressr::progressor(along = seq_len(n_total_run)) else NULL
      future.apply::future_lapply(
        seq_len(n_total_run),
        function(i) {
          res <- run_one(idx$m[i], idx$k[i], idx$l[i], inherited = inherited)
          if (!is.null(p)) {
            p(sprintf("[%s] scenario %d, run %d", batch_label[idx$m[i]], idx$k[i], idx$l[i]))
          }
          res
        },
        future.seed = FALSE,        # seeding is handled by our own set.seed(seed_list[[m]])
        future.packages = pkgs_to_export
      )
    }

    # Suppress the cautionary warning that future emits when future.seed =
    # FALSE and it detects RNG usage inside the evaluated code (harmless here
    # since we deliberately manage seeding ourselves).
    results <- suppressWarnings(
      if (use_progress) progressr::with_progress(run_all_parallel()) else run_all_parallel()
    )
  }

  # end time
  end_time <- Sys.time()

  # ------ Assemble results ------------------------------------
  values_out <- vector("list", n_total_run)
  error_log <- vector("list", n_total_run)
  missing_all <- character(0)

  for (i in seq_len(n_total_run)) {
    values_out[[i]] <- results[[i]]$value
    # Note: error_log[[i]] <- results[[i]]$error would, for a run with no
    # error (i.e. NULL), delete that i-th element entirely and shrink the
    # list (an R quirk). Wrapping the value in list() with single-bracket
    # assignment stores NULL as an ordinary value instead.
    error_log[i] <- list(results[[i]]$error)
    missing_all <- union(missing_all, attr(results[[i]]$value, "missing_keep"))
    attr(values_out[[i]], "missing_keep") <- NULL
  }
  names(values_out) <- batch_label
  names(error_log) <- batch_label

  if (!is.null(keep) && length(missing_all)) {
    warning("Some names in 'keep' were not found and will be ignored: ",
            paste(missing_all, collapse = ", "))
  }

  # For parallel execution, errors are checked once, after all runs have finished
  if (isTRUE(parallel) && identical(on_error, "stop")) {
    has_error <- !vapply(error_log, is.null, logical(1))
    if (any(has_error)) {
      stop(error_log[[which(has_error)[1]]])
    }
  }

  # [optional] message
  if(isTRUE(verbose)) cat("Finished.", "\n")

  # if output_only
  if(isTRUE(output_only)){
    return(values_out)
  }

  # return with feed
  if(isTRUE(return_with_feed)){
    if(feed_type == "data.frame"){
      m <- 0
      temp_feed <- vector("list", n_total_run)
      for(i in seq_len(n_scenarios)){
        for(j in seq_len(n_each_run)){
          m <- m + 1
          # Without drop = FALSE, a single-column feed would collapse to a
          # plain vector instead of a one-row data.frame, and rbind() would
          # then lose the column name (e.g. "x").
          temp_feed[[m]] <- feed[i, , drop = FALSE]
        }
      }
      feed_to_return <- data.frame(batch = batch_label,
                                   do.call(rbind, temp_feed))
    }else{
      ## feed_type == "list"
      m <- 0
      temp_feed <- vector("list", n_total_run)
      for(i in seq_len(n_scenarios)){
        for(j in seq_len(n_each_run)){
          m <- m + 1
          temp_feed[[m]] <- feed[[i]]
        }
      }
      feed_to_return <- temp_feed
      names(feed_to_return) <- batch_label
    }
  }

  # return
  ## calculate the run-time
  time_taken   <- as.numeric(difftime(end_time, start_time, units = "secs"))
  total_ms     <- floor(time_taken * 1000)
  hours        <- total_ms %/% 3600000L
  minutes      <- (total_ms %% 3600000L) %/% 60000L
  seconds      <- (total_ms %% 60000L) %/% 1000L
  milliseconds <- total_ms %% 1000L
  time_hms     <- sprintf("%02d:%02d:%02d.%03d", hours, minutes, seconds, milliseconds)

  if(isTRUE(verbose)){
    cat(paste("Implementation took", time_hms, "(hh:mm:ss.mmm)\n"))
  }

  if(isTRUE(return_with_feed)){
    list(values_out = values_out,
         seed = seed_list,
         batch_label = batch_label,
         feed = feed_to_return,
         error_log = error_log,
         implementation_took = time_hms)
  }else{
    list(values_out = values_out,
         seed = seed_list,
         batch_label = batch_label,
         error_log = error_log,
         implementation_took = time_hms)
  }
}
