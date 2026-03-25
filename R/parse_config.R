#' Parse MetaRVM Configuration File
#'
#' @description
#' Reads and parses a YAML configuration file for MetaRVM simulations, extracting
#' all necessary parameters for epidemic modeling including population data,
#' disease parameters, mixing matrices, vaccination schedules, and simulation settings.
#'
#' @param config_file Character string. Path to a YAML configuration file containing
#'   model parameters and settings.
#' @param return_object Logical. If \code{TRUE}, returns a \code{MetaRVMConfig}
#'   object for method chaining and enhanced functionality. If \code{FALSE}
#'   (default), returns a named list for backward compatibility.
#'
#' @details
#' The function processes a YAML configuration file with the following
#' main sections:
#'
#' \strong{Simulation Configuration:}
#' \itemize{
#'   \item \code{random_seed}: Optional random seed for reproducibility in case of stochastic simulations or stochastic parameters
#'   \item \code{nsim}: Number of simulation instances (default: 1)
#'   \item \code{nrep}: Number of stochastic replicates per parameter set (default: 1)
#'   \item \code{simulation_mode}: Optional simulation mode. Must be one of
#'         \code{"deterministic"} or \code{"stochastic"} (default: \code{"deterministic"}).
#'   \item \code{start_date}: Simulation start date in MM/DD/YYYY format
#'   \item \code{length}: Simulation length in days
#'   \item \code{checkpoint_dir}: Optional checkpoint directory for saving intermediate results
#'   \item \code{checkpoint_dates}: Optional list of dates to save checkpoints.
#'   \item \code{restore_from}: Optional path to restore simulation from checkpoint
#' }
#'
#' \strong{Population Data:}
#' \itemize{
#'   \item \code{initialization}: CSV file with initial population states and optional
#'                        user-defined category columns. The file must contain columns
#'                        \code{population_id}, \code{N}, \code{S0}, \code{I0}, \code{V0}, \code{R0}.
#'                        Any additional columns are treated as demographic categories.
#'   \item \code{vaccination}: CSV file with vaccination schedule over time. The first column must be dates 
#'                          in MM/DD/YYYY format. The rest of the columns must corresponds to respective
#'                          subpopulations in the numeric order of population_id.
#' }
#'
#' \strong{Mixing Matrices:}
#' Contact matrices for different time periods. Each CSV file must have a matrix of order (N_pop x N_pop), where,
#' N_pop is the number of subpopulations. It is assumed that the i-th row and j-th column correspond to i-th and j-th subpopulations. 
#' \itemize{
#'   \item \code{weekday_day}, \code{weekday_night}: Weekday contact patterns
#'   \item \code{weekend_day}, \code{weekend_night}: Weekend contact patterns
#' }
#'
#' \strong{Disease Parameters:}
#' Epidemiological parameters (can be scalars or distributions):
#' \itemize{
#'   \item \code{ts}: Transmission rate for symptomatic individuals
#'   \item \code{ve}: Vaccine effectiveness
#'   \item \code{de, dp, da, ds, dh, dr}: Duration parameters for different disease states
#'   \item \code{pea, psr, phr}: Probability parameters for disease transitions
#' }
#'
#' \strong{Sub-population Parameters:}
#' \code{sub_disease_params} allows specification of different parameter values
#' for specific demographic categories (e.g., age groups, races).
#'
#' The function supports stochastic parameters through distribution specifications
#' with \code{dist}, \code{mu}, \code{sd}, \code{shape}, \code{rate}, etc.
#'
#' @return If \code{return_object = FALSE} (default), returns a named list containing:
#' \describe{
#'   \item{N_pop}{Number of population groups}
#'   \item{pop_map}{Data.table with \code{population_id} and user-defined demographic categories}
#'   \item{S_ini, E_ini, I_asymp_ini, I_presymp_ini, I_symp_ini, H_ini, D_ini, P_ini, V_ini, R_ini}{Initial compartment populations}
#'   \item{vac_time_id, vac_counts, vac_mat}{Vaccination schedule data}
#'   \item{m_wd_d, m_wd_n, m_we_d, m_we_n}{Contact mixing matrices}
#'   \item{ts, ve, dv, de, dp, da, ds, dh, dr, pea, psr, phr}{Disease parameter matrices (nsim × N_pop)}
#'   \item{start_date}{Simulation start date as Date object}
#'   \item{sim_length}{Simulation length in days}
#'   \item{nsim}{Number of simulation instances}
#'   \item{nrep}{Number of stochastic replicates per parameter set}
#'   \item{simulation_mode}{Simulation mode: \code{"deterministic"} or \code{"stochastic"}}
#'   \item{random_seed}{Random seed used (if any)}
#'   \item{delta_t}{Time step size (fixed at 0.5)}
#'   \item{chk_file_names, chk_time_steps, do_chk}{Checkpointing configuration}
#' }
#'
#' If \code{return_object = TRUE}, returns a \code{MetaRVMConfig} object with
#' methods for parameter access and validation.
#'
#' @section Parameter Distributions:
#' Disease parameters can be specified as distributions for stochastic modeling:
#' \itemize{
#'   \item \strong{lognormal}: \code{dist: "lognormal", mu: value, sd: value}
#'   \item \strong{gamma}: \code{dist: "gamma", shape: value, rate: value}
#'   \item \strong{uniform}: \code{dist: "uniform", min: value, max: value}
#'   \item \strong{beta}: \code{dist: "beta", shape1: value, shape2: value}
#'   \item \strong{gaussian}: \code{dist: "gaussian", mean: value, sd: value}
#' }
#'
#' @section File Requirements:
#' \strong{Population initialization file} must contain columns:
#' \itemize{
#'   \item \code{population_id}: Unique identifier for each population group, natural numbers
#'   \item \code{N}: Total population size of the subpopulation
#'   \item \code{S0}, \code{I0}, \code{V0}, \code{R0}: Initial compartment counts
#'   \item Optional user-defined category columns (e.g., \code{age}, \code{race},
#'         \code{zone}, \code{income_level}, \code{occupation})
#' }
#'
#' \strong{Vaccination file} must contain:
#' \code{date} (MM/DD/YYYY format) and vaccination counts for each population group
#'
#' @examples
#' \donttest{
#' options(odin.verbose = FALSE)
#' example_config <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
#' # Parse configuration file and return list (backward compatible)
#' config <- parse_config(example_config)
#' 
#' # Parse and return MetaRVMConfig object for method chaining
#' config_obj <- parse_config(example_config, return_object = TRUE)
#' 
#' # Access parameters from config object
#' config_obj$get("N_pop")
#' config_obj$list_parameters()
#' 
#' # Use with MetaRVM simulation
#' results <- metaRVM(config_obj)
#' }
#'
#' @seealso
#' \code{\link{metaRVM}} for running simulations with parsed configuration
#' \code{\link{MetaRVMConfig}} for the configuration object class
#' \code{\link{process_vac_data}} for vaccination data processing
#'
#' @author Arindam Fadikar
#'
#' @export
parse_config <- function(config_file, return_object = FALSE){

  metarvm_log_init()
  metarvm_log_info("parse_config started: file='{config_file}', return_object={return_object}")

  # read the yaml config file
  yaml_data <- yaml::read_yaml(config_file)

  # provision of relative paths
  yaml_file_path <- dirname(config_file)

  # temporarily set the working directory to the yaml file location
  old_wd <- getwd()
  setwd(yaml_file_path)
  on.exit(setwd(old_wd))

  # =====================================================
  # read mandatory parameters
  is_restore <- !is.null(yaml_data$simulation_config$restore_from)
  metarvm_log_info("Simulation mode: restore={is_restore}")

  delta_t <- 0.5
  if(!is.null(yaml_data$simulation_config$nsim)){
    nsim <- yaml_data$simulation_config$nsim
  }

  vac_time_id <- NULL
  vac_counts <- NULL

  if(!is.null(yaml_data$simulation_config$start_date)) {
    start_date <- as.Date(yaml_data$simulation_config$start_date,
                          tryFormats = c("%m/%d/%Y")) - 1 # we start one day before to treat the first day of simulation as day 1. 
  }
  sim_length <- yaml_data$simulation_config$length
  nsim <- ifelse(!is.null(yaml_data$simulation_config$nsim),
                 yaml_data$simulation_config$nsim, 1)
  nrep <- ifelse(!is.null(yaml_data$simulation_config$nrep),
                 yaml_data$simulation_config$nrep, 1)
  nrep <- suppressWarnings(as.integer(nrep)[1])
  if (is.na(nrep) || nrep < 1) {
    setwd(old_wd)
    stop("nrep must be a positive integer")
  }
  simulation_mode <- "deterministic"
  if (!is.null(yaml_data$simulation_config$simulation_mode)) {
    simulation_mode <- tolower(trimws(as.character(yaml_data$simulation_config$simulation_mode)))
  } else if (!is.null(yaml_data$simulation_config$is_stoch)) {
    is_stoch <- yaml_data$simulation_config$is_stoch
    simulation_mode <- if (isTRUE(is_stoch) || (is.numeric(is_stoch) && is_stoch != 0)) {
      "stochastic"
    } else {
      "deterministic"
    }
  }
  if (!simulation_mode %in% c("deterministic", "stochastic")) {
    setwd(old_wd)
    stop("simulation_mode must be either 'deterministic' or 'stochastic'")
  }
  
  # check random seed
  if(!is.null(yaml_data$simulation_config$random_seed)){
    random_seed <- suppressWarnings(as.integer(yaml_data$simulation_config$random_seed)[1])
    if (is.na(random_seed)) {
      setwd(old_wd)
      stop("random_seed must be coercible to a single integer value")
    }
    set.seed(random_seed)
  } else if (simulation_mode == "stochastic") {
    random_seed <- sample.int(.Machine$integer.max, 1)
    set.seed(random_seed)
  } else {
    random_seed <- NULL
  }

  chk_time_steps <- NULL
  chk_file_names <- NULL
  do_chk <- FALSE

  if(!is.null(yaml_data$simulation_config$checkpoint_dir)){
    checkpoint_dir <- normalizePath(yaml_data$simulation_config$checkpoint_dir, mustWork = FALSE)
    metarvm_log_info("Checkpointing enabled: dir='{checkpoint_dir}'")

    # prepare checkpoint directory
    if(!dir.exists(checkpoint_dir)) dir.create(checkpoint_dir, recursive = TRUE)
    do_chk <- TRUE

    if (!is.null(yaml_data$simulation_config$checkpoint_dates)) {
      # Checkpoint at specified dates
      chk_dates <- as.Date(yaml_data$simulation_config$checkpoint_dates,
                           tryFormats = c("%m/%d/%Y"))

      # Convert dates to time steps
      chk_time_steps <- as.integer((chk_dates - start_date) / delta_t)

      # Generate a matrix of file names: rows for instances, columns for dates
      chk_file_names <- sapply(chk_dates, function(date) {
        date_str <- format(date, "%Y-%m-%d")
        paste0(checkpoint_dir, "/checkpoint_", date_str, "_instance_", 1:(nsim * nrep), ".Rda")
      })
      if (is.vector(chk_file_names)) {
        chk_file_names <- matrix(chk_file_names, ncol = 1)
      }

    } else {
      # Default behavior: checkpoint at the end of the simulation
      end_date <- start_date + sim_length
      date_str <- format(end_date, "%Y-%m-%d")

      # The time step is the last one
      chk_time_steps <- as.integer(sim_length / delta_t)

      # Create a 1-column matrix for the single checkpoint date
      chk_file_names <- matrix(paste0(checkpoint_dir, "/chk_", date_str, "_", 1:(nsim * nrep), ".Rda"), ncol = 1)
    }
  }


  # ======================================================
  # If restore_from is available, initialize
  # meta_sim inputs
  if(is_restore){
    metarvm_log_info("Restoring simulation from '{yaml_data$simulation_config$restore_from}'")

    chk_obj <- readRDS(yaml_data$simulation_config$restore_from)

    ## chk_obj should be of class MetaRVMCheck
    ## verfify if chk_obj is of class MetaRVMCheck
    if(!methods::is(chk_obj, "MetaRVMCheck")){
      metarvm_log_error("Invalid restore object: expected MetaRVMCheck")
      setwd(old_wd)
      stop("The restore_from file does not contain a valid MetaRVMCheck object")
    }

    N_pop <- chk_obj$get("N_pop")
    delta_t <- chk_obj$get("delta_t")

    m_wd_d <- chk_obj$get("m_weekday_day")
    m_wd_n <- chk_obj$get("m_weekday_night")
    m_we_d <- chk_obj$get("m_weekend_day")
    m_we_n <- chk_obj$get("m_weekend_night")

    # read disease parameters
    ts <- chk_obj$get("ts")
    # tv <- chk_obj$get("tv")
    ve <- chk_obj$get("ve")
    dv <- chk_obj$get("dv")
    de <- chk_obj$get("de")
    dp <- chk_obj$get("dp")
    da <- chk_obj$get("da")
    ds <- chk_obj$get("ds")
    dh <- chk_obj$get("dh")
    dr <- chk_obj$get("dr")
    pea <- chk_obj$get("pea")
    psr <- chk_obj$get("psr")
    phr <- chk_obj$get("phr")

    S_ini = chk_obj$get("S")$value
    E_ini = chk_obj$get("E")$value
    I_asymp_ini = chk_obj$get("Ia")$value
    I_presymp_ini = chk_obj$get("Ip")$value
    I_symp_ini = chk_obj$get("Is")$value
    H_ini = chk_obj$get("H")$value
    D_ini = chk_obj$get("D")$value
    P_ini = chk_obj$get("P")$value
    V_ini = chk_obj$get("V")$value
    R_ini = chk_obj$get("R")$value

  }

  ## If other parameters are present, override them
  ## population initialization
  if(!is.null(yaml_data$population_data$initialization)){

    pop_init_file <- yaml_data$population_data$initialization

    # Define reserved columns that are NOT categories
    reserved_cols <- c("population_id", "N", "S0", "I0", "R0", "V0",
                       "E0", "Ia0", "Ip0", "H0", "D0", "P0")

    if (is_restore) {
      # During restore, initialization file can be used as mapping-only metadata
      pop_map_raw <- data.table::fread(pop_init_file)

      if (!"population_id" %in% names(pop_map_raw)) {
        metarvm_log_error("Initialization file missing required column 'population_id'")
        setwd(old_wd)
        stop("Initialization file must contain column: population_id")
      }

      expected_ids <- 1:nrow(pop_map_raw)
      if (!all(sort(pop_map_raw$population_id) == expected_ids)) {
        metarvm_log_error("Invalid population_id sequence in initialization file")
        setwd(old_wd)
        stop("population_id must be sequential natural numbers: 1, 2, ..., ", nrow(pop_map_raw))
      }

      data.table::setorder(pop_map_raw, population_id)

      category_cols <- setdiff(names(pop_map_raw), reserved_cols)
      if (length(category_cols) > 0) {
        pop_map <- pop_map_raw[, c("population_id", category_cols), with = FALSE]
      } else {
        pop_map <- pop_map_raw[, .(population_id)]
      }
      category_names <- category_cols

      if (nrow(pop_map_raw) > 5000) {
        metarvm_log_warn("Large number of populations detected: {nrow(pop_map_raw)}")
        warning(sprintf("Large number of populations (%d). This may affect performance.",
                        nrow(pop_map_raw)))
      }
      if (length(category_names) > 10) {
        metarvm_log_warn("Large number of categories detected: {length(category_names)}")
        warning(sprintf("Large number of categories (%d). Consider reducing for better usability.",
                        length(category_names)))
      }

      if (nrow(pop_map_raw) != N_pop) {
        metarvm_log_error("Initialization/mapping row count does not match checkpoint N_pop")
        setwd(old_wd)
        stop("When restoring from checkpoint, initialization/mapping rows must match checkpoint N_pop")
      }
    } else {
      pop_init <- data.table::fread(pop_init_file)

      # Detect category columns (all columns except reserved ones)
      category_cols <- setdiff(names(pop_init), reserved_cols)

      # Validate required columns are present
      required_cols <- c("population_id", "N", "S0", "I0", "R0", "V0")
      missing <- setdiff(required_cols, names(pop_init))
      if (length(missing) > 0) {
        metarvm_log_error("Initialization missing required columns: {paste(missing, collapse = ', ')}")
        setwd(old_wd)
        stop("Missing required columns in initialization file: ",
             paste(missing, collapse = ", "))
      }

      # Validate population_id is sequential
      expected_ids <- 1:nrow(pop_init)
      if (!all(sort(pop_init$population_id) == expected_ids)) {
        metarvm_log_error("Invalid population_id sequence in initialization file")
        setwd(old_wd)
        stop("population_id must be sequential natural numbers: 1, 2, ..., ", nrow(pop_init))
      }

      # Ensure data is ordered by population_id
      data.table::setorder(pop_init, population_id)

      # Create pop_map from initialization file (contains population_id + category columns)
      if (length(category_cols) > 0) {
        pop_map <- pop_init[, c("population_id", category_cols), with = FALSE]
      } else {
        # No categories - just population_id
        pop_map <- pop_init[, .(population_id)]
      }

      # Store category names for later use
      category_names <- category_cols

      # Warn if too many populations or categories
      if (nrow(pop_init) > 5000) {
        metarvm_log_warn("Large number of populations detected: {nrow(pop_init)}")
        warning(sprintf("Large number of populations (%d). This may affect performance.",
                        nrow(pop_init)))
      }
      if (length(category_names) > 10) {
        metarvm_log_warn("Large number of categories detected: {length(category_names)}")
        warning(sprintf("Large number of categories (%d). Consider reducing for better usability.",
                        length(category_names)))
      }

      # Set up initialization values for non-restore runs
      N_pop <- nrow(pop_init)
      P_ini <- pop_init[, N]
      S_ini <- pop_init[, S0]
      I_symp_ini <- pop_init[, I0]
      V_ini <- pop_init[, V0]
      R_ini <- pop_init[, R0]
      E_ini <- rep(0, N_pop)
      I_asymp_ini <- rep(0, N_pop)
      I_presymp_ini <- rep(0, N_pop)
      H_ini <- rep(0, N_pop)
      D_ini <- rep(0, N_pop)
    }

  }

  # If restoring and no population map was provided, create a minimal one
  if (is_restore && (!exists("pop_map") || is.null(pop_map))) {
    pop_map <- data.table::data.table(population_id = 1:N_pop)
    category_names <- character(0)
  }

  ## check if vac data is present
  if(!is.null(yaml_data$population_data$vaccination)){

    vac_file <- yaml_data$population_data$vaccination
    vac_dt <- data.table::fread(vac_file, header = TRUE)
    processed_vac <- process_vac_data(vac_dt,
                                      sim_start_date = start_date,
                                      sim_length = sim_length,
                                      delta_t = delta_t)
    vac_time_id <- processed_vac[, c("t")]
    vac_counts <- as.matrix(processed_vac[, -1])
  }

  ## check if mixing matrices are present
  if(!is.null(yaml_data$mixing_matrix$weekday_day)){
    m_wd_d_file <- yaml_data$mixing_matrix$weekday_day
    m_wd_d <- as.matrix(utils::read.csv(m_wd_d_file, header = F))
  }
  if(!is.null(yaml_data$mixing_matrix$weekday_night)){
    m_wd_n_file <- yaml_data$mixing_matrix$weekday_night
    m_wd_n <- as.matrix(utils::read.csv(m_wd_n_file, header = F))
  }
  if(!is.null(yaml_data$mixing_matrix$weekend_day)){
    m_we_d_file <- yaml_data$mixing_matrix$weekend_day
    m_we_d <- as.matrix(utils::read.csv(m_we_d_file, header = F))
  }
  
  if(!is.null(yaml_data$mixing_matrix$weekend_night)){
    m_we_n_file <- yaml_data$mixing_matrix$weekend_night
    m_we_n <- as.matrix(utils::read.csv(m_we_n_file, header = F))
  }

  ## check for mixing matrix consistency (rowsum = 1)
  if(any(abs(rowSums(m_wd_d) - 1) > 0.01)) {
    metarvm_log_error("weekday_day mixing matrix rows do not sum to 1")
    setwd(old_wd)
    stop("Rows of weekday day mixing matrix do not sum to 1")
  }
  if(any(abs(rowSums(m_wd_n) - 1) > 0.01)) {
    metarvm_log_error("weekday_night mixing matrix rows do not sum to 1")
    setwd(old_wd)
    stop("Rows of weekday night mixing matrix do not sum to 1")
  }
  if(any(abs(rowSums(m_we_d) - 1) > 0.01)) {
    metarvm_log_error("weekend_day mixing matrix rows do not sum to 1")
    setwd(old_wd)
    stop("Rows of weekend day mixing matrix do not sum to 1")
  }
  if(any(abs(rowSums(m_we_n) - 1) > 0.01)) {
    metarvm_log_error("weekend_night mixing matrix rows do not sum to 1")
    setwd(old_wd)
    stop("Rows of weekend night mixing matrix do not sum to 1")
  }

  ## check if global disease params are present
  if(!is.null(yaml_data$disease_params)){

    # read global disease parameters
    if(!is.null(yaml_data$disease_params$ts)) ts <- yaml_data$disease_params$ts
    # if(!is.null(yaml_data$disease_params$tv)) tv <- yaml_data$disease_params$tv
    if(!is.null(yaml_data$disease_params$ve)) ve <- yaml_data$disease_params$ve
    if(!is.null(yaml_data$disease_params$dv)) dv <- yaml_data$disease_params$dv
    if(!is.null(yaml_data$disease_params$de)) de <- yaml_data$disease_params$de
    if(!is.null(yaml_data$disease_params$dp)) dp <- yaml_data$disease_params$dp
    if(!is.null(yaml_data$disease_params$da)) da <- yaml_data$disease_params$da
    if(!is.null(yaml_data$disease_params$ds)) ds <- yaml_data$disease_params$ds
    if(!is.null(yaml_data$disease_params$dh)) dh <- yaml_data$disease_params$dh
    if(!is.null(yaml_data$disease_params$dr)) dr <- yaml_data$disease_params$dr
    if(!is.null(yaml_data$disease_params$pea)) pea <- yaml_data$disease_params$pea
    if(!is.null(yaml_data$disease_params$psr)) psr <- yaml_data$disease_params$psr
    if(!is.null(yaml_data$disease_params$phr)) phr <- yaml_data$disease_params$phr


    if(length(ts) == 1) ts <- rep(ts, N_pop)
    # if(length(tv) == 1) tv <- rep(tv, N_pop)
    if(length(ve) == 1) ve <- rep(ve, N_pop)
    if(length(dv) == 1) dv <- rep(dv, N_pop)
    if(length(de) == 1) de <- rep(de, N_pop)
    if(length(dp) == 1) dp <- rep(dp, N_pop)
    if(length(da) == 1) da <- rep(da, N_pop)
    if(length(ds) == 1) ds <- rep(ds, N_pop)
    if(length(dh) == 1) dh <- rep(dh, N_pop)
    if(length(dr) == 1) dr <- rep(dr, N_pop)
    if(length(pea) == 1) pea <- rep(pea, N_pop)
    if(length(psr) == 1) psr <- rep(psr, N_pop)
    if(length(phr) == 1) phr <- rep(phr, N_pop)
  }


  ## Stochastic parameters

  ts <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(ts, N_pop))))
  # tv <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(tv, N_pop))))
  ve <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(ve, N_pop))))
  dv <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(dv, N_pop))))
  de <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(de, N_pop))))
  dp <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(dp, N_pop))))
  da <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(da, N_pop))))
  ds <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(ds, N_pop))))
  dh <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(dh, N_pop))))
  dr <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(dr, N_pop))))
  pea <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(pea, N_pop))))
  psr <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(psr, N_pop))))
  phr <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(phr, N_pop))))

  ## check if population specific disease params are present
  if(!is.null(yaml_data$sub_disease_params)){

    sub_disease_params <- yaml_data$sub_disease_params
    cats_to_modify <- names(sub_disease_params)

    # check if population_data$initialization was provided
    if (!exists("category_names") || !exists("pop_map")) {
      metarvm_log_error("sub_disease_params provided without initialization/pop_map")
      setwd(old_wd)
      stop("sub_disease_params requires population_data$initialization to be specified in the config file")
    }

    # check if the subgroup names match with detected categories
    invalid_cats <- setdiff(cats_to_modify, category_names)
    if (length(invalid_cats) > 0) {
      metarvm_log_error("Invalid sub_disease_params categories: {paste(invalid_cats, collapse = ', ')}")
      setwd(old_wd)
      available_cats <- if (length(category_names) > 0) {
        paste(category_names, collapse = ", ")
      } else {
        "none (no category columns found in initialization file)"
      }
      stop(paste("Invalid categories in sub_disease_params:",
                 paste(invalid_cats, collapse = ", "),
                 ". Available categories are:", available_cats))
    }

    for (cat in cats_to_modify){
      cat_vals <- names(sub_disease_params[[cat]])

      # check if the subgroup values are valid
      invalid_cat_vals <- setdiff(cat_vals, unique(pop_map[[cat]]))
      if (length(invalid_cat_vals) > 0) {
        metarvm_log_error("Invalid values for category '{cat}': {paste(invalid_cat_vals, collapse = ', ')}")
        setwd(old_wd)
        valid_cat_vals <- as.character(unique(pop_map[[cat]]))
        stop(sprintf(
          "Invalid values for category '%s' in sub_disease_params: %s. Valid values are: %s",
          cat,
          paste(invalid_cat_vals, collapse = ", "),
          paste(valid_cat_vals, collapse = ", ")
        ))
      }

      for (cat_val in cat_vals){
        row_ids <- which(pop_map[[cat]] == cat_val)
        params_to_modify <- names(sub_disease_params[[cat]][[cat_val]])

        for (param in params_to_modify){
          temp_param <- get(param)
          temp_param[, row_ids] <- sub_disease_params[[cat]][[cat_val]][[param]]
          assign(param, temp_param)
        }
      }
    }

  }

  # Ensure category_names is defined (empty if not set)
  if (!exists("category_names")) {
    category_names <- character(0)
  }

  config_list <- list(N_pop = N_pop,
                      pop_map = pop_map,
                      category_names = category_names,
                      S_ini = S_ini,
                      E_ini = E_ini,
                      I_asymp_ini = I_asymp_ini,
                      I_presymp_ini = I_presymp_ini,
                      I_symp_ini = I_symp_ini,
                      H_ini = H_ini,
                      D_ini = D_ini,
                      P_ini = P_ini,
                      V_ini = V_ini,
                      R_ini = R_ini,
                      vac_time_id = unlist(vac_time_id, use.names = F),
                      vac_counts = vac_counts,
                      vac_mat = cbind(unlist(vac_time_id, use.names = F), vac_counts),
                      m_wd_d = m_wd_d,
                      m_wd_n = m_wd_n,
                      m_we_d = m_we_d,
                      m_we_n = m_we_n,
                      ts = ts,
                      # tv = tv,
                      ve = ve,
                      dv = dv,
                      de = de,
                      dp = dp,
                      da = da,
                      ds = ds,
                      dh = dh,
                      dr = dr,
                      pea = pea,
                      psr = psr,
                      phr = phr,
                      start_date = start_date,
                      sim_length = sim_length,
                      nsim = nsim,
                      nrep = nrep,
                      simulation_mode = simulation_mode,
                      random_seed = random_seed,
                      delta_t = delta_t,
                      chk_file_names = chk_file_names,
                      chk_time_steps = chk_time_steps,
                      do_chk = do_chk)

  setwd(old_wd)  # reset working directory
  metarvm_log_info(
    "parse_config completed: N_pop={N_pop}, nsim={nsim}, categories={length(category_names)}, checkpointing={do_chk}"
  )
  
  if (return_object) {
    return(MetaRVMConfig$new(config_list))
  } else {
    return(config_list)  # Backward compatibility
  }

}

# next_rda <- function() {
#   f <- list.files(path.expand("~/my_data/"), pattern = "^data_\\d+\\.Rda")
#   num <- max(as.numeric(gsub("^data_(\\d)\\.Rda", "\\1", f)) + 1)
#   paste0(path.expand("~/my_data/data_"), num, ".Rda")
# }

#' Read and prepare vaccination data
#'
#' @description
#' This function takes the vaccination schedule given by a data.table and prepares
#' it according to the required structure needed in `meta_sim()` function
#'
#' @param vac_dt A data.table of vaccination schedule
#' @param sim_start_date A calendar date in the format yyyy-mm-dd
#' @param sim_length Number of calendar days that the simulation runs for
#' @param delta_t Step size in the simulation
#'
#' @returns A data.table
#' @keywords internal
process_vac_data <- function(vac_dt, sim_start_date, sim_length, delta_t) {

  # Ensure the date column is of Date type
  vac_dt[[1]] <- as.Date(vac_dt[[1]], tryFormats = c("%m/%d/%Y"))
  
  # Rename the first column to "date" for easier processing
  data.table::setnames(vac_dt, 1, "date")

  date_filtered <- vac_dt %>%
    dplyr::filter(date >= as.Date(sim_start_date)) %>%
    dplyr::mutate(t = (date - as.Date(sim_start_date)) / delta_t) %>%
    dplyr::select(-c(date)) %>%
    dplyr::select(dplyr::last_col(), dplyr::everything())

  ## fill in the missing time in vac data
  complete_time <- data.table::data.table(t = seq(0, sim_length / delta_t))

  ## merge
  vac_all_dates <- merge(complete_time, date_filtered, by = "t", all.x = TRUE)
  vac_all_dates[is.na(vac_all_dates)] <- 0
  
  # since we are starting a day earlier, remove the vaccination counts from that date
  if (nrow(vac_all_dates) > 0 && "t" %in% names(vac_all_dates)) {
    cols_to_update <- setdiff(names(vac_all_dates), "t")
    vac_all_dates[t == 0, (cols_to_update) := 0]
  }

  return(vac_all_dates)
}


# ==============================================================================
#' Function to draw samples for distributional parameters
#'
#' @param config_list A list of configurations
#' @param N_pop Number of subpopulations
#' @param seed
#'
#' @returns A random sample drawn from the distribution specified by the dist component
#' @keywords internal
draw_sample <- function(config_list, N_pop, seed = NULL){

  if(methods::is(config_list, "list")){

    if(!is.null(seed)) set.seed(seed)

    if(config_list$dist == "lognormal")
      x <- stats::rlnorm(1, meanlog = config_list$mu, sdlog = config_list$sd)

    if(config_list$dist == "gamma")
      x <- stats::rgamma(1, shape = config_list$shape, rate = config_list$rate)

    if(config_list$dist == "uniform")
      x <- stats::runif(1, min = config_list$min, max = config_list$max)

    if(config_list$dist == "beta")
      x <- stats::rbeta(1, shape1 = config_list$shape1, shape2 = config_list$shape2)

    if(config_list$dist == "gaussian")
      x <- stats::rnorm(1, mean = config_list$mean, sd = config_list$sd)

    return(rep(x, N_pop))
  } else return(config_list)
}
