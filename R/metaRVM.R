#' Run a MetaRVM epidemic simulation
#'
#' @description
#' `metaRVM()` is the high-level entry point for running a MetaRVM
#' metapopulation respiratory virus simulation. It parses the configuration,
#' runs one or more stochastic simulation instances, formats the ODIN/MetaRVM
#' output into a tidy long table with calendar dates and demographic attributes,
#' and returns a [`MetaRVMResults`] object for downstream analysis and plotting.
#'
#' @details
#' The configuration input controls:
#'
#' - **Population structure** (user-defined categories and the initial compartment counts from the initialization file)
#' - **Disease parameters** (`ts`, `ve`, `de`, `dp`, `da`, `ds`,
#'   `dh`, `dr`, `pea`, `psr`, `phr`, `dv`, etc.)
#' - **Mixing matrices** (weekday/weekend, day/night contact patterns)
#' - **Vaccination schedule** and immunity waning
#' - **Simulation settings** (start date, length, number of instances,
#'   checkpointing)
#'
#' Internally, `metaRVM()`:
#'
#' 1. Parses the YAML configuration via [parse_config()].
#' 2. Calls the ODIN-based simulation engine [meta_sim()] for each instance.
#' 3. Uses [format_metarvm_output()] to convert time steps to dates and
#'    attach demographic attributes.
#' 4. Wraps the formatted output and metadata in a [`MetaRVMResults`]
#'    object that supports method chaining for subsetting, summarizing,
#'    and plotting.
#'
#' @param config_input Configuration specification in one of three forms:
#'   \itemize{
#'     \item **Character string**: path to a YAML configuration file.
#'     \item **[`MetaRVMConfig`] object**: pre-initialized configuration.
#'     \item **Named list**: output from [parse_config()] with
#'       `return_object = FALSE`.
#'   }
#' @param verbose Optional logical. If `TRUE`, prints meaningful run progress
#'   messages. If `NULL` (default), uses `simulation_config$verbose` from the
#'   parsed config when available; otherwise defaults to `FALSE`.
#' @param suppress_odin_messages Optional logical. If `TRUE`, sets
#'   `options(odin.verbose = FALSE)` for this run to suppress ODIN compilation
#'   chatter. If `NULL` (default), uses
#'   `simulation_config$suppress_odin_messages` from config when available;
#'   otherwise defaults to `FALSE`.
#'
#' @return
#' A [`MetaRVMResults`] R6 object with three key components:
#' \describe{
#'   \item{$results}{A tidy `data.table` with one row per
#'     date–subpopulation–disease state–instance combination. Typical
#'     columns include:
#'     \itemize{
#'       \item `date`: calendar date (`Date`)
#'       \item user-defined demographic category columns (if present
#'             in the initialization file)
#'       \item `disease_state`: compartment or flow label (e.g., `S`, `E`,
#'             `I_symp`, `H`, `R`, `D`, `n_SE`, `n_IsympH`, etc.)
#'       \item `value`: population count or daily flow
#'       \item `instance`: simulation instance index (1, 2, …)
#'     }
#'   }
#'   \item{$config}{The [`MetaRVMConfig`] object used for the run.}
#'   \item{$run_info}{A list with metadata such as `n_instances`,
#'     `date_range`, `delta_t`, and checkpoint information.}
#' }
#'
#' @seealso
#' [parse_config()] for reading YAML configurations,
#' [MetaRVMConfig] for configuration management,
#' [MetaRVMResults] for analysis and plotting,
#' [meta_sim()] for the low-level simulation engine.
#'
#' @examples
#' \donttest{
#' options(odin.verbose = FALSE)
#' example_config <- system.file("extdata", "example_config.yaml",
#'                               package = "MetaRVM")
#'
#' # Run a single-instance simulation from a YAML file
#' results <- metaRVM(example_config)
#'
#' # Print a high-level summary
#' results
#'
#' # Access the tidy results table
#' head(results$results)
#'
#' # Summarize and plot hospitalizations and deaths by user-defined categories
#' results$summarize(
#'   group_by       = c("age", "zone"),
#'   disease_states = c("H", "D"),
#'   stats          = c("median", "quantile"),
#'   quantiles      = c(0.25, 0.75)
#' )$plot()
#'
#' # Using a pre-parsed configuration object
#' cfg <- parse_config(example_config, return_object = TRUE)
#' results2 <- metaRVM(cfg)
#' }
#' @references
#' Fadikar, A., et al. "Developing and deploying a use-inspired metapopulation modeling framework for detailed tracking of stratified health outcomes"
#'
#' @author Arindam Fadikar, Charles Macal, Ignacio Martinez-Moyano, Jonathan Ozik
#'
#' @export
metaRVM <- function(config_input, verbose = NULL, suppress_odin_messages = NULL) {
  # =============================================================================
  # Maintainer Notes: High-level orchestration philosophy
  # -----------------------------------------------------------------------------
  # `metaRVM()` is intentionally a thin orchestrator:
  # - It should not know disease-specific parameter names.
  # - It should not branch by disease in simulation logic.
  # - It should rely on registry hooks for engine selection and arg mapping.
  #
  # Why:
  # - Keeps one stable public run API for all diseases.
  # - Makes adding diseases a registry + builder task, not a core refactor.
  # - Reduces risk of regression in existing diseases when adding new ones.
  #
  # Data flow:
  # config_input -> MetaRVMConfig -> registry resolve -> run loop ->
  # engine output -> merged `out` -> MetaRVMResults.
  # =============================================================================
  disease <- resolve_disease_from_config_input(config_input)
  disease_entry <- get_metarvm_disease_entry(disease)
  sim_engine <- get_metarvm_engine_fn(disease)

  # Handle different input types
  if (is.character(config_input)) {
    # Input is a file path - create MetaRVMConfig object
    config_obj <- MetaRVMConfig$new(config_input)
  } else if (inherits(config_input, "MetaRVMConfig")) {
    # Input is already a MetaRVMConfig object
    config_obj <- config_input
  } else if (is.list(config_input)) {
    # Input is a parsed config list - convert to MetaRVMConfig
    config_obj <- MetaRVMConfig$new(config_input)
  } else {
    stop("config_input must be a file path, MetaRVMConfig object, or parsed config list")
  }

  # Maintainer Note:
  # Runtime verbosity controls are normalized here instead of in every engine.
  # This keeps low-level engines pure (state transitions + output only) and keeps
  # UI/log policy centralized at the orchestration layer.
  verbose_run <- if (!is.null(verbose)) {
    metarvm_as_logical_flag(verbose, "verbose", default = FALSE)
  } else if ("verbose" %in% names(config_obj$config_data)) {
    metarvm_as_logical_flag(config_obj$config_data$verbose, "config_data$verbose", default = FALSE)
  } else {
    FALSE
  }
  suppress_odin_run <- if (!is.null(suppress_odin_messages)) {
    metarvm_as_logical_flag(suppress_odin_messages, "suppress_odin_messages", default = FALSE)
  } else if ("suppress_odin_messages" %in% names(config_obj$config_data)) {
    metarvm_as_logical_flag(config_obj$config_data$suppress_odin_messages,
                            "config_data$suppress_odin_messages", default = FALSE)
  } else {
    FALSE
  }
  vmsg <- function(...) {
    if (isTRUE(verbose_run)) {
      message(...)
    }
  }
  if (isTRUE(suppress_odin_run)) {
    old_odin_verbose <- getOption("odin.verbose")
    options(odin.verbose = FALSE)
    on.exit(options(odin.verbose = old_odin_verbose), add = TRUE)
  }

  # Maintainer Note:
  # nsim = number of parameter draws; nrep = stochastic replicates per draw.
  # Total simulation instances = nsim * nrep.
  # Keep this interpretation stable because summary/quantile workflows rely on it.
  # pass inputs to disease-specific simulation engine
  nsim <- config_obj$config_data$nsim
  nrep <- if ("nrep" %in% names(config_obj$config_data)) {
    as.integer(config_obj$config_data$nrep)
  } else {
    1L
  }
  if (is.na(nrep) || nrep < 1) {
    stop("nrep must be a positive integer")
  }

  is_stoch <- TRUE
  total_instances <- nsim * nrep
  vmsg(sprintf(
    "Starting MetaRVM run: disease=%s, nsim=%d, nrep=%d, total_instances=%d",
    disease, nsim, nrep, total_instances
  ))
  if (isTRUE(suppress_odin_run)) {
    vmsg("ODIN messages suppressed via options(odin.verbose = FALSE) for this run.")
  }
  run_seed_base <- NA_integer_
  if (is_stoch) {
    run_seed <- config_obj$config_data$random_seed
    if (is.null(run_seed)) {
      run_seed <- sample.int(.Machine$integer.max, 1)
    } else {
      run_seed <- suppressWarnings(as.integer(run_seed)[1])
      if (is.na(run_seed)) {
        stop("random_seed must be coercible to a single integer value")
      }
    }
    config_obj$config_data$random_seed <- run_seed
    run_seed_base <- run_seed
    vmsg(sprintf("Using stochastic seed base: %d", run_seed_base))
  }
  nsteps <- floor(config_obj$config_data$sim_length / config_obj$config_data$delta_t)
  day_name <- weekdays(config_obj$config_data$start_date)
  start_day <- dplyr::case_when(
    day_name == "Monday"    ~ 0,
    day_name == "Tuesday"   ~ 1,
    day_name == "Wednesday" ~ 2,
    day_name == "Thursday"  ~ 3,
    day_name == "Friday"    ~ 4,
    day_name == "Saturday"  ~ 5,
    day_name == "Sunday"    ~ 6,
    TRUE ~ NA_integer_
  )

  # Cartesian join gives one row per (parameter-set, replicate) combination.
  # run_idx = (ii - 1) * nrep + rr maps the 2D grid to a 1-D instance index
  # that is contiguous (1..nsim*nrep) and stable regardless of execution order.
  task_grid <- data.table::CJ(ii = seq_len(nsim), rr = seq_len(nrep), sorted = TRUE)
  task_grid[, run_idx := (ii - 1L) * nrep + rr]
  config_data <- config_obj$config_data
  sim_args_list <- lapply(seq_len(nrow(task_grid)), function(task_id) {
    build_metarvm_sim_args(
      disease = disease,
      config_data = config_data,
      ii = task_grid$ii[[task_id]],
      run_idx = task_grid$run_idx[[task_id]],
      nsteps = nsteps,
      start_day = start_day,
      is_stoch = is_stoch
    )
  })
  instance_manifest <- data.table::copy(task_grid[, .(
    instance = run_idx,
    parameter_set = ii,
    replicate = rr
  )])
  instance_manifest[, disease := disease]
  instance_manifest[, seed := run_seed_base + instance - 1L]
  instance_manifest[, sim_args := sim_args_list]

  run_one_instance <- function(run_idx, sim_args) {
    if (is_stoch) {
      set.seed(run_seed_base + run_idx - 1L)
    }

    o <- do.call(sim_engine, sim_args)
    o <- data.table::as.data.table(o)
    o[, instance := run_idx]
    o
  }

  parallel_enabled <- FALSE
  parallel_backend <- "sequential"
  parallel_workers <- 1L
  parallel_reason <- "no registered foreach parallel backend found"
  local_cluster <- NULL
  if (requireNamespace("foreach", quietly = TRUE)) {
    is_registered <- foreach::getDoParRegistered()
    n_workers <- if (is_registered) foreach::getDoParWorkers() else 1L

    if (isTRUE(is_registered) && as.integer(n_workers) > 1L) {
      parallel_enabled <- TRUE
      parallel_workers <- as.integer(n_workers)
      parallel_backend <- foreach::getDoParName()
      parallel_reason <- "using pre-registered foreach backend"
    } else if (requireNamespace("doParallel", quietly = TRUE)) {
      detected_cores <- suppressWarnings(as.integer(parallel::detectCores(logical = TRUE)))
      if (is.na(detected_cores) || detected_cores < 1L) {
        detected_cores <- 1L
      }
      local_workers <- min(total_instances, max(1L, detected_cores - 1L))
      if (local_workers > 1L) {
        local_cluster <- parallel::makeCluster(local_workers)
        doParallel::registerDoParallel(local_cluster)
        on.exit({
          parallel::stopCluster(local_cluster)
          foreach::registerDoSEQ()
        }, add = TRUE)
        parallel_enabled <- TRUE
        parallel_workers <- local_workers
        parallel_backend <- foreach::getDoParName()
        parallel_reason <- "auto-registered local doParallel backend"
      } else {
        parallel_reason <- "single core detected; parallel backend not started"
      }
    } else {
      parallel_reason <- "foreach is installed but doParallel is not installed"
    }
  } else {
    parallel_reason <- "foreach package is not installed"
  }

  if (isTRUE(parallel_enabled)) {
    vmsg(sprintf(
      "Execution mode: parallel foreach backend '%s' with %d workers (%s).",
      parallel_backend, parallel_workers, parallel_reason
    ))
  } else {
    vmsg(sprintf("Execution mode: sequential (%s).", parallel_reason))
  }

  out <- data.table::data.table()
  if (isTRUE(parallel_enabled)) {
    # `%dopar%` is a non-exported infix from foreach.  get() retrieves it from
    # the package namespace at runtime to avoid a hard import dependency.
    dopar <- get("%dopar%", envir = asNamespace("foreach"))
    foreach_result <- tryCatch(
      dopar(
        foreach::foreach(
          task_id = seq_len(nrow(task_grid)),
          .inorder = TRUE,
          .errorhandling = "stop",
          .packages = c("data.table", "dplyr", "tidyr", "odin")
        ),
        {
          run_idx <- task_grid$run_idx[[task_id]]
          sim_args <- sim_args_list[[task_id]]
          run_one_instance(run_idx = run_idx, sim_args = sim_args)
        }
      ),
      error = function(e) e
    )

    if (inherits(foreach_result, "error")) {
      vmsg(
        paste0(
          "Parallel foreach execution failed (", conditionMessage(foreach_result),
          "). Falling back to sequential execution."
        )
      )
      parallel_enabled <- FALSE
      parallel_backend <- "sequential_fallback"
      parallel_workers <- 1L
    } else {
      out <- data.table::rbindlist(foreach_result, use.names = TRUE, fill = TRUE)
    }
  }

  if (!isTRUE(parallel_enabled)) {
    out <- data.table::data.table()
    # Print a progress line roughly every 10% of total instances.
    report_every <- max(1L, as.integer(total_instances %/% 10L))
    for (task_id in seq_len(nrow(task_grid))) {
      ii <- task_grid$ii[[task_id]]
      rr <- task_grid$rr[[task_id]]
      run_idx <- task_grid$run_idx[[task_id]]
      sim_args <- sim_args_list[[task_id]]
      o <- run_one_instance(run_idx = run_idx, sim_args = sim_args)
      out <- rbind(out, o, fill = TRUE)
      if (isTRUE(verbose_run)) {
        if (run_idx == 1L || run_idx == total_instances || (run_idx %% report_every) == 0L) {
          vmsg(sprintf(
            "Completed instance %d/%d (parameter_set=%d, replicate=%d).",
            run_idx, total_instances, ii, rr
          ))
        }
      }
    }
  }

  run_info <- list(
    created_at = Sys.time(),
    N_pop = config_obj$config_data$N_pop,
    nsim = nsim,
    nrep = nrep,
    n_instances = total_instances,
    execution_mode = if (isTRUE(parallel_enabled)) "parallel_foreach" else "sequential",
    parallel_backend = parallel_backend,
    parallel_workers = parallel_workers,
    random_seed = config_obj$config_data$random_seed,
    instance_manifest = instance_manifest,
    verbose = verbose_run,
    suppress_odin_messages = suppress_odin_run,
    delta_t = config_obj$config_data$delta_t,
    date_range = if (nrow(out) > 0) {
      c(config_obj$config_data$start_date + 1,
        config_obj$config_data$start_date + floor(config_obj$config_data$sim_length))
    } else {
      c(NA, NA)
    },
    checkpointing_enabled = if (isTRUE(disease_entry$checkpointing_from_config)) {
      isTRUE(config_obj$config_data$do_chk)
    } else {
      FALSE
    }
  )
  # Maintainer Note:
  # `run_info` intentionally stores both run controls and derived metadata so users
  # can audit reproducibility later (e.g., seed, mode, date window, checkpointing).

  # Create and return MetaRVMResults object
  results_obj <- MetaRVMResults$new(out, config_obj, run_info = run_info)
  vmsg(sprintf("Run complete. Generated %d rows across %d instances.", nrow(out), total_instances))
  return(results_obj)
  # return(out)
}
