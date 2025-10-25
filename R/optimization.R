#' @title MetaRVM Objective Function for Optimization
#' @description
#' This internal function serves as the objective function for optimization routines.
#' It runs a deterministic MetaRVM simulation with a given set of parameters and
#' calculates the Sum of Squared Errors (SSE) between the simulation output and
#' ground truth data.
#'
#' @param params A named vector of parameter values to be evaluated.
#' @param config A `MetaRVMConfig` object or a path to a YAML configuration file.
#' @param ground_truth A `data.table` with observational data.
#' @return The Sum of Squared Errors (SSE) as a single numeric value.
#' @keywords internal
meta_objective_function <- function(params, config, ground_truth) {
  
  # 1. Input handling for config
  if (is.character(config)) {
    config_obj <- MetaRVMConfig$new(config)
  } else if (inherits(config, "MetaRVMConfig")) {
    config_obj <- config
  } else if (is.list(config)) {
    config_obj <- MetaRVMConfig$new(config)
  } else {
    stop("config must be a file path, MetaRVMConfig object, or parsed config list")
  }
  
  # Ensure ground_truth is ordered for correct comparison
  setorder(ground_truth, date, age, disease_state)
  
  # 2. Run simulation with the given parameters
  temp_config_data <- config_obj$config_data
  
  for (p_name in names(params)) {
    temp_config_data[[p_name]] <- rep(params[[p_name]], temp_config_data$N_pop)
  }
  
  param_names <- c("ts", "tv", "dv", "de", "pea", "dp", "da", "ds", "psr", "dh", "phr", "dr", "ve")
  for (p in param_names) {
    if (is.matrix(temp_config_data[[p]])) {
      temp_config_data[[p]] <- temp_config_data[[p]][1, ]
    }
  }
  
  nsteps <- floor(temp_config_data$sim_length / temp_config_data$delta_t)
  
  sim_out <- meta_sim(
    is.stoch = FALSE, nsteps = nsteps, N_pop = temp_config_data$N_pop,
    S0 = temp_config_data$S_ini, I0 = temp_config_data$I_symp_ini,
    P0 = temp_config_data$P_ini, R0 = temp_config_data$R_ini,
    H0 = temp_config_data$H_ini, D0 = temp_config_data$D_ini,
    E0 = temp_config_data$E_ini, Ia0 = temp_config_data$I_asymp_ini,
    Ip0 = temp_config_data$I_presymp_ini,
    m_weekday_day = temp_config_data$m_wd_d, m_weekday_night = temp_config_data$m_wd_n,
    m_weekend_day = temp_config_data$m_we_d, m_weekend_night = temp_config_data$m_we_n,
    delta_t = temp_config_data$delta_t, vac_mat = temp_config_data$vac_mat,
    ts = temp_config_data$ts, tv = temp_config_data$tv, dv = temp_config_data$dv,
    de = temp_config_data$de, pea = temp_config_data$pea, dp = temp_config_data$dp,
    da = temp_config_data$da, ds = temp_config_data$ds, psr = temp_config_data$psr,
    dh = temp_config_data$dh, phr = temp_config_data$phr, dr = temp_config_data$dr,
    ve = temp_config_data$ve
  )
  
  # 3. Format, aggregate, and merge output
  temp_config_obj <- MetaRVMConfig$new(temp_config_data)
  formatted_out <- format_metarvm_output(sim_out, temp_config_obj)
  agg_out <- formatted_out[, .(sim_value = sum(value, na.rm = TRUE)), by = .(date, age, disease_state)]
  merged_data <- merge(ground_truth, agg_out, by = c("date", "age", "disease_state"), all.x = TRUE)
  
  # 4. Calculate and return SSE
  sse <- sum((merged_data$value - merged_data$sim_value)^2, na.rm = TRUE)
  return(sse)
}

#' @title Calibrate MetaRVM using optim
#' @description Wrapper for `optim` to calibrate MetaRVM.
#' @keywords internal
optim_metaRVM <- function(config, ground_truth, params_to_infer, optim_settings) {
  
  # Extract starting values for parameters
  start_params <- sapply(params_to_infer, function(x) x$start)
  
  # Call optim
  optim_results <- optim(
    par = start_params,
    fn = meta_objective_function,
    config = config,
    ground_truth = ground_truth,
    method = optim_settings$method %||% "L-BFGS-B",
    lower = sapply(params_to_infer, function(x) x$lower),
    upper = sapply(params_to_infer, function(x) x$upper),
    control = optim_settings$control %||% list()
  )
  
  return(optim_results)
}

#' @title Calibrate MetaRVM using DEoptim
#' @description Wrapper for `DEoptim` to calibrate MetaRVM.
#' @keywords internal
DEoptim_metaRVM <- function(config, ground_truth, params_to_infer, deoptim_settings) {
  
  # DEoptim requires the `DEoptim` package
  if (!requireNamespace("DEoptim", quietly = TRUE)) {
    stop("Package 'DEoptim' is required for this function. Please install it.", call. = FALSE)
  }
  
  # Extract bounds
  lower_bounds <- sapply(params_to_infer, function(x) x$lower)
  upper_bounds <- sapply(params_to_infer, function(x) x$upper)
  
  # Call DEoptim
  deoptim_results <- DEoptim::DEoptim(
    fn = meta_objective_function,
    lower = lower_bounds,
    upper = upper_bounds,
    config = config,
    ground_truth = ground_truth,
    control = deoptim_settings %||% DEoptim::DEoptim.control()
  )
  
  return(deoptim_results)
}

#' @title Calibrate MetaRVM Parameters
#' @description
#' This master function calibrates MetaRVM model parameters using one of several
#' available optimization methods.
#'
#' @param config A `MetaRVMConfig` object or a path to a YAML configuration file.
#' @param params_to_infer A named list defining the parameters to be estimated,
#'   including their bounds and starting values.
#' @param ground_truth A `data.table` containing observational data.
#' @param method The optimization method to use: "optim" or "DEoptim".
#' @param settings A list of settings for the chosen optimization method.
#'
#' @return The results from the chosen optimization function.
#' @keywords internal
#' @examples
#' \dontrun{
#' # This is a conceptual example.
#' # You would need a valid config object and ground truth data.
#' 
#' # 1. Load or create a configuration object
#' config <- MetaRVMConfig$new("path/to/your/config.yaml")
#' 
#' # 2. Define ground truth data (example structure)
#' ground_truth <- data.table(
#'   date = as.Date("2023-01-10"),
#'   age = "18+",
#'   disease_state = "I_symp",
#'   value = 150
#' )
#' 
#' # 3. Define parameters to infer with bounds and start values
#' params_to_infer <- list(
#'   ts = list(lower = 0.1, upper = 0.9, start = 0.5),
#'   pea = list(lower = 0.2, upper = 0.8, start = 0.4)
#' )
#' 
#' # 4. Calibrate using the 'optim' method
#' optim_results <- calibrate_metaRVM(
#'   config = config,
#'   params_to_infer = params_to_infer,
#'   ground_truth = ground_truth,
#'   method = "optim",
#'   settings = list(method = "L-BFGS-B", control = list(maxit = 100))
#' )
#' print(optim_results)
#' 
#' # 5. Calibrate using the 'DEoptim' method
#' deoptim_results <- calibrate_metaRVM(
#'   config = config,
#'   params_to_infer = params_to_infer,
#'   ground_truth = ground_truth,
#'   method = "DEoptim",
#'   settings = list(itermax = 50)
#' )
#' print(deoptim_results)
#' }
calibrate_metaRVM <- function(config, params_to_infer, ground_truth, method, settings = list()) {
  
  if (method == "optim") {
    return(optim_metaRVM(config, ground_truth, params_to_infer, settings))
  } else if (method == "DEoptim") {
    return(DEoptim_metaRVM(config, ground_truth, params_to_infer, settings))
  } else {
    stop("Invalid method specified. Choose 'optim' or 'DEoptim'.")
  }
}
