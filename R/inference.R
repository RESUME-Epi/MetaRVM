#' @title Perform Bayesian Inference for MetaRVM Parameters
#' @description
#' This function calibrates specified MetaRVM model parameters against ground
#' truth observational data using a Bayesian framework provided by the
#' `RobustCalibration` package.
#'
#' @param config A `MetaRVMConfig` object or a path to a YAML configuration file.
#' @param params_to_infer A named list defining the prior distributions for the
#'   parameters to be estimated.
#' @param ground_truth A `data.table` containing the observational data.
#'   Must include `date`, `age`, `disease_state`, and `value` columns for
#'   age-specific calibration.
#' @param mcmc_settings A list of settings for the MCMC sampler, such as `iter`
#'   and `burnin`.
#'
#' @return An object from `rcalibration` containing the MCMC posterior samples.
#' @export
meta_infer <- function(config, params_to_infer, ground_truth, mcmc_settings) {

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

  # 2. Prepare inputs for rcalibration
  p_ranges <- do.call(rbind, params_to_infer)
  
  # Ensure ground_truth is ordered for correct comparison
  # Calibration is at age-group level, so we include 'age'
  setorder(ground_truth, date, age, disease_state)

  # 3. Define the model wrapper function
  model_wrapper <- function(params) {
    
    temp_config_data <- config_obj$config_data
    
    # Update parameters being inferred
    for (p_name in names(params)) {
      temp_config_data[[p_name]] <- rep(params[[p_name]], temp_config_data$N_pop)
    }
    
    # Ensure all disease parameters are vectors for the deterministic run
    param_names <- c("ts", "tv", "dv", "de", "pea", "dp", "da", "ds", "psr", "dh", "phr", "dr", "ve")
    for (p in param_names) {
      if (is.matrix(temp_config_data[[p]])) {
        temp_config_data[[p]] <- temp_config_data[[p]][1, ]
      }
    }
    
    # Run a single deterministic simulation
    nsteps <- floor(temp_config_data$sim_length / temp_config_data$delta_t)
    
    sim_out <- meta_sim(
      is.stoch = FALSE,
      nsteps = nsteps,
      N_pop = temp_config_data$N_pop,
      S0 = temp_config_data$S_ini,
      I0 = temp_config_data$I_symp_ini,
      P0 = temp_config_data$P_ini,
      R0 = temp_config_data$R_ini,
      H0 = temp_config_data$H_ini,
      D0 = temp_config_data$D_ini,
      E0 = temp_config_data$E_ini,
      Ia0 = temp_config_data$I_asymp_ini,
      Ip0 = temp_config_data$I_presymp_ini,
      m_weekday_day = temp_config_data$m_wd_d,
      m_weekday_night = temp_config_data$m_wd_n,
      m_weekend_day = temp_config_data$m_we_d,
      m_weekend_night = temp_config_data$m_we_n,
      delta_t = temp_config_data$delta_t,
      vac_mat = temp_config_data$vac_mat,
      ts = temp_config_data$ts,
      tv = temp_config_data$tv,
      dv = temp_config_data$dv,
      de = temp_config_data$de,
      pea = temp_config_data$pea,
      dp = temp_config_data$dp,
      da = temp_config_data$da,
      ds = temp_config_data$ds,
      psr = temp_config_data$psr,
      dh = temp_config_data$dh,
      phr = temp_config_data$phr,
      dr = temp_config_data$dr,
      ve = temp_config_data$ve
    )
    
    # Format and aggregate output
    temp_config_obj <- MetaRVMConfig$new(temp_config_data)
    formatted_out <- format_metarvm_output(sim_out, temp_config_obj)
    
    # Aggregate to match ground truth structure (by age group)
    agg_out <- formatted_out[, .(sim_value = sum(value, na.rm = TRUE)), by = .(date, age, disease_state)]
    
    # Merge with ground truth to ensure alignment and proper ordering
    merged_data <- merge(ground_truth, agg_out, by = c("date", "age", "disease_state"), all.x = TRUE)
    
    # Return the simulated values in the same order as ground_truth
    return(merged_data$sim_value)
  }
  
  # 4. Call rcalibration
  rc_args <- list(
    p_ranges = p_ranges,
    obs = ground_truth$value,
    model = model_wrapper,
    n_iterations = mcmc_settings$n_iterations %||% 2000
  )
  
  if (!is.null(mcmc_settings$burn_in)) {
    rc_args$burn_in <- mcmc_settings$burn_in
  }
  
  calibration_results <- do.call(RobustCalibration::rcalibration, rc_args)
  
  return(calibration_results)
}
