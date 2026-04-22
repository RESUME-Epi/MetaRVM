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
  # =============================================================================
  # Maintainer Notes: Parser architecture philosophy
  # -----------------------------------------------------------------------------
  # `parse_config()` is the single public parse entrypoint for all diseases.
  #
  # Design goals:
  # - Keep user-facing API stable over time.
  # - Keep orchestration generic and disease-agnostic.
  # - Route disease-specific parsing to registry-selected builders.
  #
  # Separation of concerns:
  # - This function handles dispatch and shared run-level flags only.
  # - Disease builders implement disease-specific defaults/validation/mapping.
  # - Shared helper utilities at the bottom centralize common validation logic.
  #
  # Extension rule:
  # When adding a disease, avoid adding disease `if/else` blocks here.
  # Instead, register a new `config_builder` in `disease_registry.R`.
  # =============================================================================
  yaml_data <- yaml::read_yaml(config_file)
  disease <- resolve_disease_from_yaml(yaml_data)
  verbose <- metarvm_as_logical_flag(
    yaml_data$simulation_config$verbose,
    option_name = "simulation_config$verbose",
    default = FALSE
  )
  suppress_odin_messages <- metarvm_as_logical_flag(
    yaml_data$simulation_config$suppress_odin_messages,
    option_name = "simulation_config$suppress_odin_messages",
    default = FALSE
  )

  config_builder <- get_metarvm_config_builder(disease)
  config_list <- config_builder(
    yaml_data = yaml_data,
    config_file = config_file,
    disease = disease,
    verbose = verbose,
    suppress_odin_messages = suppress_odin_messages
  )

  if (return_object) {
    return(MetaRVMConfig$new(config_list))
  }
  config_list
}

parse_config_measles_internal <- function(yaml_data, config_file, disease,
                                          verbose = FALSE,
                                          suppress_odin_messages = FALSE) {
  # Maintainer Notes:
  # This builder handles ONLY measles-specific config mapping:
  # - initialization schema for measles compartments,
  # - required measles disease parameters,
  # - per-parameter expansion/sampling to nsim x N_pop matrices.
  #
  # It still reuses shared helpers for:
  # - simulation config parsing
  # - population table validation
  # - mixing matrix loading/validation
  #
  # Expected output:
  # A canonical config list consumed by `metaRVM()` and
  # `build_metarvm_sim_args("measles", ...)`.
  disease_entry <- get_metarvm_disease_entry(disease)

  yaml_file_path <- dirname(config_file)
  old_wd <- getwd()
  setwd(yaml_file_path)
  on.exit(setwd(old_wd), add = TRUE)

  sim_cfg <- metarvm_parse_simulation_config(
    yaml_data = yaml_data,
    default_delta_t = 0.5,
    default_mode = "deterministic",
    force_stochastic = TRUE
  )
  delta_t <- sim_cfg$delta_t
  start_date <- sim_cfg$start_date
  sim_length <- sim_cfg$sim_length
  nsim <- sim_cfg$nsim
  nrep <- sim_cfg$nrep
  simulation_mode <- sim_cfg$simulation_mode
  random_seed <- sim_cfg$random_seed

  if (is.null(yaml_data$population_data$initialization)) {
    stop("population_data$initialization is required")
  }
  pop_init <- data.table::fread(yaml_data$population_data$initialization)

  pop_meta <- metarvm_validate_population_table(
    pop_init = pop_init,
    reserved_cols = disease_entry$init_reserved_cols,
    required_cols = disease_entry$init_required_cols,
    require_any_of = disease_entry$init_require_any_of,
    require_population_id = TRUE
  )
  pop_init <- pop_meta$pop_init
  pop_map <- pop_meta$pop_map
  category_names <- pop_meta$category_names
  N_pop <- pop_meta$N_pop
  S0 <- pop_init[, S0]

  init_defaults <- disease_entry$init_optional_defaults
  init_values <- lapply(names(init_defaults), function(col) {
    if (col %in% names(pop_init)) pop_init[[col]] else rep(init_defaults[[col]], N_pop)
  })
  names(init_values) <- names(init_defaults)

  E10 <- init_values$E10
  E20 <- init_values$E20
  I1_Q0 <- init_values$I1_Q0
  I1_U0 <- init_values$I1_U0
  I2_Q0 <- init_values$I2_Q0
  I2_U0 <- init_values$I2_U0
  R0 <- init_values$R0

  mixing <- metarvm_read_mixing_matrices(yaml_data, require_all = TRUE)
  m_wd_d <- mixing$m_wd_d
  m_wd_n <- mixing$m_wd_n
  m_we_d <- mixing$m_we_d
  m_we_n <- mixing$m_we_n

  if (is.null(yaml_data$disease_params)) {
    stop("disease_params section is required")
  }
  disease_params <- yaml_data$disease_params
  param_names <- disease_entry$disease_param_required
  missing_params <- param_names[!param_names %in% names(disease_params)]
  if (length(missing_params) > 0) {
    stop("Missing required measles disease parameters: ", paste(missing_params, collapse = ", "))
  }

  param_values <- lapply(param_names, function(pnm) {
    val <- disease_params[[pnm]]
    if (length(val) == 1) val <- rep(val, N_pop)
    do.call(rbind, purrr::map(1:nsim, ~ draw_sample(val, N_pop)))
  })
  names(param_values) <- param_names

  config_list <- list(
    N_pop = N_pop,
    disease = disease,
    pop_map = pop_map,
    category_names = category_names,
    S0 = S0,
    E10 = E10,
    E20 = E20,
    I1_Q0 = I1_Q0,
    I1_U0 = I1_U0,
    I2_Q0 = I2_Q0,
    I2_U0 = I2_U0,
    R0 = R0,
    m_wd_d = m_wd_d,
    m_wd_n = m_wd_n,
    m_we_d = m_we_d,
    m_we_n = m_we_n,
    start_date = start_date,
    sim_length = sim_length,
    nsim = nsim,
    nrep = nrep,
    simulation_mode = simulation_mode,
    random_seed = random_seed,
    verbose = verbose,
    suppress_odin_messages = suppress_odin_messages,
    delta_t = delta_t,
    chk_file_names = NULL,
    chk_time_steps = NULL,
    do_chk = FALSE
  )
  for (pnm in names(param_values)) {
    config_list[[pnm]] <- param_values[[pnm]]
  }
  config_list
}

parse_config_rvm_internal <- function(yaml_data, config_file, disease,
                                      verbose = FALSE,
                                      suppress_odin_messages = FALSE) {
  # Maintainer Notes:
  # This builder preserves legacy RVM behavior while fitting the new generic
  # parse architecture. It owns:
  # - checkpoint restore semantics (`restore_from`),
  # - legacy initialization mapping (S_ini, I_symp_ini, etc.),
  # - sub_disease_params category overrides,
  # - conversion of global disease params to nsim x N_pop sampled matrices.
  #
  # Keep this logic here (not in `parse_config()`) so adding new diseases does not
  # increase complexity of the public entrypoint.
  yaml_file_path <- dirname(config_file)
  old_wd <- getwd()
  setwd(yaml_file_path)
  on.exit(setwd(old_wd), add = TRUE)

  is_restore <- !is.null(yaml_data$simulation_config$restore_from)
  sim_cfg <- metarvm_parse_simulation_config(
    yaml_data = yaml_data,
    default_delta_t = 0.5,
    default_mode = "deterministic",
    force_stochastic = FALSE
  )
  delta_t <- sim_cfg$delta_t
  start_date <- sim_cfg$start_date
  sim_length <- sim_cfg$sim_length
  nsim <- sim_cfg$nsim
  nrep <- sim_cfg$nrep
  simulation_mode <- sim_cfg$simulation_mode
  random_seed <- sim_cfg$random_seed

  vac_time_id <- NULL
  vac_counts <- NULL

  chk_time_steps <- NULL
  chk_file_names <- NULL
  do_chk <- FALSE

  if(!is.null(yaml_data$simulation_config$checkpoint_dir)){
    checkpoint_dir <- normalizePath(yaml_data$simulation_config$checkpoint_dir, mustWork = FALSE)
    if(!dir.exists(checkpoint_dir)) dir.create(checkpoint_dir, recursive = TRUE)
    do_chk <- TRUE

    if (!is.null(yaml_data$simulation_config$checkpoint_dates)) {
      chk_dates <- as.Date(yaml_data$simulation_config$checkpoint_dates,
                           tryFormats = c("%m/%d/%Y"))
      chk_time_steps <- as.integer((chk_dates - start_date) / delta_t)

      chk_file_names <- sapply(chk_dates, function(date) {
        date_str <- format(date, "%Y-%m-%d")
        paste0(checkpoint_dir, "/checkpoint_", date_str, "_instance_", 1:(nsim * nrep), ".Rda")
      })
      if (is.vector(chk_file_names)) {
        chk_file_names <- matrix(chk_file_names, ncol = 1)
      }

    } else {
      end_date <- start_date + sim_length
      date_str <- format(end_date, "%Y-%m-%d")
      chk_time_steps <- as.integer(sim_length / delta_t)
      chk_file_names <- matrix(paste0(checkpoint_dir, "/chk_", date_str, "_", 1:(nsim * nrep), ".Rda"), ncol = 1)
    }
  }

  if(is_restore){
    chk_obj <- readRDS(yaml_data$simulation_config$restore_from)
    if(!methods::is(chk_obj, "MetaRVMCheck")){
      stop("The restore_from file does not contain a valid MetaRVMCheck object")
    }

    N_pop <- chk_obj$get("N_pop")
    delta_t <- chk_obj$get("delta_t")

    m_wd_d <- chk_obj$get("m_weekday_day")
    m_wd_n <- chk_obj$get("m_weekday_night")
    m_we_d <- chk_obj$get("m_weekend_day")
    m_we_n <- chk_obj$get("m_weekend_night")

    ts <- chk_obj$get("ts")
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

  if(!is.null(yaml_data$population_data$initialization)){
    pop_init_file <- yaml_data$population_data$initialization
    reserved_cols <- c("population_id", "N", "S0", "I0", "R0", "V0",
                       "E0", "Ia0", "Ip0", "H0", "D0", "P0")

    if (is_restore) {
      pop_map_raw <- data.table::fread(pop_init_file)
      pop_meta <- metarvm_validate_population_table(
        pop_init = pop_map_raw,
        reserved_cols = reserved_cols,
        required_cols = "population_id",
        require_population_id = TRUE
      )
      pop_map <- pop_meta$pop_map
      category_names <- pop_meta$category_names

      if (pop_meta$N_pop != N_pop) {
        stop("When restoring from checkpoint, initialization/mapping rows must match checkpoint N_pop")
      }
    } else {
      pop_init <- data.table::fread(pop_init_file)
      pop_meta <- metarvm_validate_population_table(
        pop_init = pop_init,
        reserved_cols = reserved_cols,
        required_cols = c("population_id", "N", "S0", "I0", "R0", "V0"),
        require_population_id = TRUE
      )
      pop_init <- pop_meta$pop_init
      pop_map <- pop_meta$pop_map
      category_names <- pop_meta$category_names

      N_pop <- pop_meta$N_pop
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

  if (is_restore && (!exists("pop_map") || is.null(pop_map))) {
    pop_map <- data.table::data.table(population_id = 1:N_pop)
    category_names <- character(0)
  }

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

  mixing <- metarvm_read_mixing_matrices(yaml_data, require_all = TRUE)
  m_wd_d <- mixing$m_wd_d
  m_wd_n <- mixing$m_wd_n
  m_we_d <- mixing$m_we_d
  m_we_n <- mixing$m_we_n

  if(!is.null(yaml_data$disease_params)){
    if(!is.null(yaml_data$disease_params$ts)) ts <- yaml_data$disease_params$ts
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

  ts <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(ts, N_pop))))
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

  if(!is.null(yaml_data$sub_disease_params)){
    sub_disease_params <- yaml_data$sub_disease_params
    cats_to_modify <- names(sub_disease_params)

    if (!exists("category_names") || !exists("pop_map")) {
      stop("sub_disease_params requires population_data$initialization to be specified in the config file")
    }

    invalid_cats <- setdiff(cats_to_modify, category_names)
    if (length(invalid_cats) > 0) {
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
      invalid_cat_vals <- setdiff(cat_vals, unique(pop_map[[cat]]))
      if (length(invalid_cat_vals) > 0) {
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

  if (!exists("category_names")) {
    category_names <- character(0)
  }

  list(N_pop = N_pop,
       disease = disease,
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
       vac_time_id = unlist(vac_time_id, use.names = FALSE),
       vac_counts = vac_counts,
       vac_mat = cbind(unlist(vac_time_id, use.names = FALSE), vac_counts),
       m_wd_d = m_wd_d,
       m_wd_n = m_wd_n,
       m_we_d = m_we_d,
       m_we_n = m_we_n,
       ts = ts,
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
       verbose = verbose,
       suppress_odin_messages = suppress_odin_messages,
       delta_t = delta_t,
       chk_file_names = chk_file_names,
       chk_time_steps = chk_time_steps,
       do_chk = do_chk)
}

metarvm_parse_simulation_config <- function(yaml_data,
                                            default_delta_t = 0.5,
                                            default_mode = "deterministic",
                                            force_stochastic = FALSE) {
  # Maintainer Notes:
  # Shared parser for simulation-level controls.
  # This helper defines normalization semantics used across diseases:
  # - `start_date` interpreted as simulation day 0 being one day prior.
  # - `nrep` normalized to positive integer.
  # - `simulation_mode` can be explicit or inferred from legacy `is_stoch`.
  # - seed policy:
  #     * use provided seed when available
  #     * auto-generate seed for stochastic mode if missing
  #     * leave NULL for deterministic mode
  #
  # If you change behavior here, check both deterministic and stochastic disease
  # configs because all builders rely on this helper now.
  sim_cfg <- yaml_data$simulation_config

  delta_t <- ifelse(!is.null(sim_cfg$delta_t), sim_cfg$delta_t, default_delta_t)

  if (!is.null(sim_cfg$start_date)) {
    start_date <- as.Date(sim_cfg$start_date, tryFormats = c("%m/%d/%Y")) - 1
  } else {
    stop("simulation_config$start_date is required")
  }

  sim_length <- sim_cfg$length
  if (is.null(sim_length)) {
    stop("simulation_config$length is required")
  }

  nsim <- ifelse(!is.null(sim_cfg$nsim), sim_cfg$nsim, 1)
  nrep <- ifelse(!is.null(sim_cfg$nrep), sim_cfg$nrep, 1)
  nrep <- suppressWarnings(as.integer(nrep)[1])
  if (is.na(nrep) || nrep < 1) {
    stop("nrep must be a positive integer")
  }

  if (isTRUE(force_stochastic)) {
    simulation_mode <- "stochastic"
  } else {
    simulation_mode <- default_mode
    if (!is.null(sim_cfg$simulation_mode)) {
      simulation_mode <- tolower(trimws(as.character(sim_cfg$simulation_mode)))
    } else if (!is.null(sim_cfg$is_stoch)) {
      is_stoch <- sim_cfg$is_stoch
      simulation_mode <- if (isTRUE(is_stoch) || (is.numeric(is_stoch) && is_stoch != 0)) {
        "stochastic"
      } else {
        "deterministic"
      }
    }
    if (!simulation_mode %in% c("deterministic", "stochastic")) {
      stop("simulation_mode must be either 'deterministic' or 'stochastic'")
    }
  }

  if(!is.null(sim_cfg$random_seed)){
    random_seed <- suppressWarnings(as.integer(sim_cfg$random_seed)[1])
    if (is.na(random_seed)) {
      stop("random_seed must be coercible to a single integer value")
    }
    set.seed(random_seed)
  } else if (simulation_mode == "stochastic") {
    random_seed <- sample.int(.Machine$integer.max, 1)
    set.seed(random_seed)
  } else {
    random_seed <- NULL
  }

  list(
    delta_t = delta_t,
    start_date = start_date,
    sim_length = sim_length,
    nsim = nsim,
    nrep = nrep,
    simulation_mode = simulation_mode,
    random_seed = random_seed
  )
}

metarvm_validate_population_table <- function(pop_init,
                                              reserved_cols,
                                              required_cols = NULL,
                                              require_any_of = NULL,
                                              require_population_id = TRUE) {
  # Maintainer Notes:
  # Shared validator for initialization/map files.
  # Responsibilities:
  # - validate required columns
  # - validate "at least one-of" constraints
  # - enforce sequential population_id convention (1..N)
  # - derive `pop_map` + `category_names`
  #
  # Why sequential IDs?
  # Simulation outputs and matrix indexing assume deterministic ordering.
  # Sequential IDs reduce ambiguity in joins/subsetting and avoid silent mapping
  # errors between config files and engine state vectors.
  if (!is.null(required_cols)) {
    missing <- setdiff(required_cols, names(pop_init))
    if (length(missing) > 0) {
      stop("Missing required columns in initialization file: ",
           paste(missing, collapse = ", "))
    }
  }

  if (!is.null(require_any_of) && length(require_any_of) > 0 &&
      !any(require_any_of %in% names(pop_init))) {
    stop("Initialization file must contain at least one of: ",
         paste(require_any_of, collapse = ", "))
  }

  if (isTRUE(require_population_id) && !"population_id" %in% names(pop_init)) {
    stop("Initialization file must contain column: population_id")
  }

  if (isTRUE(require_population_id)) {
    expected_ids <- 1:nrow(pop_init)
    if (!all(sort(pop_init$population_id) == expected_ids)) {
      stop("population_id must be sequential natural numbers: 1, 2, ..., ", nrow(pop_init))
    }
    data.table::setorder(pop_init, population_id)
  }

  category_cols <- setdiff(names(pop_init), reserved_cols)
  if (length(category_cols) > 0) {
    pop_map <- pop_init[, c("population_id", category_cols), with = FALSE]
  } else {
    pop_map <- pop_init[, .(population_id)]
  }
  category_names <- category_cols

  if (nrow(pop_init) > 5000) {
    warning(sprintf("Large number of populations (%d). This may affect performance.",
                    nrow(pop_init)))
  }
  if (length(category_names) > 10) {
    warning(sprintf("Large number of categories (%d). Consider reducing for better usability.",
                    length(category_names)))
  }

  list(
    pop_init = pop_init,
    pop_map = pop_map,
    category_names = category_names,
    N_pop = nrow(pop_init)
  )
}

metarvm_read_mixing_matrices <- function(yaml_data, require_all = TRUE) {
  # Maintainer Notes:
  # Shared loader/validator for mixing matrices.
  # We enforce row sums ~ 1 to represent contact-allocation proportions.
  # Tolerance (0.01) is intentionally permissive enough for CSV rounding noise.
  #
  # If future disease models require different matrix semantics, add a disease-
  # specific wrapper in the builder rather than weakening this shared invariant.
  mm <- yaml_data$mixing_matrix
  files <- list(
    weekday_day = mm$weekday_day,
    weekday_night = mm$weekday_night,
    weekend_day = mm$weekend_day,
    weekend_night = mm$weekend_night
  )

  if (isTRUE(require_all) && any(vapply(files, is.null, logical(1)))) {
    stop("All four mixing matrices are required: weekday_day, weekday_night, weekend_day, weekend_night")
  }

  m_wd_d <- as.matrix(utils::read.csv(files$weekday_day, header = FALSE))
  m_wd_n <- as.matrix(utils::read.csv(files$weekday_night, header = FALSE))
  m_we_d <- as.matrix(utils::read.csv(files$weekend_day, header = FALSE))
  m_we_n <- as.matrix(utils::read.csv(files$weekend_night, header = FALSE))

  if(any(abs(rowSums(m_wd_d) - 1) > 0.01)) stop("Rows of weekday day mixing matrix do not sum to 1")
  if(any(abs(rowSums(m_wd_n) - 1) > 0.01)) stop("Rows of weekday night mixing matrix do not sum to 1")
  if(any(abs(rowSums(m_we_d) - 1) > 0.01)) stop("Rows of weekend day mixing matrix do not sum to 1")
  if(any(abs(rowSums(m_we_n) - 1) > 0.01)) stop("Rows of weekend night mixing matrix do not sum to 1")

  list(
    m_wd_d = m_wd_d,
    m_wd_n = m_wd_n,
    m_we_d = m_we_d,
    m_we_n = m_we_n
  )
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
  # Maintainer Notes:
  # Vaccination schedules are expanded to full simulation timeline so engines can
  # index by timestep directly without sparse-date checks.
  # Missing dates are filled with zeros.
  # We zero-out t==0 vaccination because simulation convention starts one day early.

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
  # Maintainer Notes:
  # Supports both scalar literals and distribution-spec lists.
  # Distribution specs are sampled once per simulation draw and replicated over
  # N_pop by default; disease builders can layer additional subgroup overrides.
  #
  # If a new distribution is introduced, update here and keep interface backward
  # compatible (`dist` + parameter names).

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

metarvm_as_logical_flag <- function(x, option_name, default = FALSE) {
  # Maintainer Notes:
  # Robust logical coercion for YAML inputs (`true`, `yes`, `1`, etc.).
  # Keep this strict (error on ambiguous values) to avoid silent misconfigurations.
  if (is.null(x)) {
    return(default)
  }

  if (is.logical(x)) {
    return(isTRUE(x[[1]]))
  }

  if (is.numeric(x)) {
    return(!is.na(x[[1]]) && x[[1]] != 0)
  }

  if (is.character(x)) {
    val <- tolower(trimws(x[[1]]))
    if (val %in% c("true", "t", "1", "yes", "y", "on")) {
      return(TRUE)
    }
    if (val %in% c("false", "f", "0", "no", "n", "off")) {
      return(FALSE)
    }
  }

  stop(option_name, " must be logical-like (TRUE/FALSE, yes/no, 1/0)")
}
