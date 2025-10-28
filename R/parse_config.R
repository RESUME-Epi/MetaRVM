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
#' The function processes a comprehensive YAML configuration file with the following
#' main sections:
#'
#' \strong{Simulation Configuration:}
#' \itemize{
#'   \item \code{random_seed}: Optional random seed for reproducibility
#'   \item \code{nsim}: Number of simulation instances (default: 1)
#'   \item \code{start_date}: Simulation start date in MM/DD/YYYY format
#'   \item \code{length}: Simulation length in days
#'   \item \code{checkpoint_dir}: Optional checkpoint directory for saving intermediate results
#'   \item \code{checkpoint_dates}: Optional list of dates to save checkpoints.
#'   \item \code{restore_from}: Optional path to restore simulation from checkpoint
#' }
#'
#' \strong{Population Data:}
#' \itemize{
#'   \item \code{mapping}: CSV file path containing population mapping with demographic categories
#'   \item \code{initialization}: CSV file with initial population states (S0, I0, V0, R0, N)
#'   \item \code{vaccination}: CSV file with vaccination schedule over time
#' }
#'
#' \strong{Mixing Matrices:}
#' Contact matrices for different time periods:
#' \itemize{
#'   \item \code{weekday_day}, \code{weekday_night}: Weekday contact patterns
#'   \item \code{weekend_day}, \code{weekend_night}: Weekend contact patterns
#' }
#'
#' \strong{Disease Parameters:}
#' Epidemiological parameters (can be scalars or distributions):
#' \itemize{
#'   \item \code{ts}: Transmission rate for symptomatic individuals
#'   \item \code{tv}: Transmission rate for vaccinated individuals
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
#'   \item{pop_map}{Data.table with population mapping and demographics}
#'   \item{S_ini, E_ini, I_asymp_ini, I_presymp_ini, I_symp_ini, H_ini, D_ini, P_ini, V_ini, R_ini}{Initial compartment populations}
#'   \item{vac_time_id, vac_counts, vac_mat}{Vaccination schedule data}
#'   \item{m_wd_d, m_wd_n, m_we_d, m_we_n}{Contact mixing matrices}
#'   \item{ts, tv, ve, dv, de, dp, da, ds, dh, dr, pea, psr, phr}{Disease parameter matrices (nsim × N_pop)}
#'   \item{start_date}{Simulation start date as Date object}
#'   \item{sim_length}{Simulation length in days}
#'   \item{nsim}{Number of simulation instances}
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
#' \strong{Population mapping file} must contain columns:
#' \itemize{
#'   \item \code{population_id}: Unique identifier for each population group
#'   \item \code{age}: Age category (e.g., "0-4", "5-11", "12-17", "18-49", "50-64", "65+")
#'   \item \code{race}: Race/ethnicity category
#'   \item \code{hcez}: Geographic zone identifier
#' }
#'
#' \strong{Population initialization file} must contain:
#' \code{N} (total population), \code{S0}, \code{I0}, \code{V0}, \code{R0} (initial compartment counts)
#'
#' \strong{Vaccination file} must contain:
#' \code{date} (MM/DD/YYYY format) and vaccination counts for each population group
#'
#' @examples
#' \dontrun{
#' # Parse configuration file and return list (backward compatible)
#' config <- parse_config("path/to/config.yaml")
#' 
#' # Parse and return MetaRVMConfig object for method chaining
#' config_obj <- parse_config("path/to/config.yaml", return_object = TRUE)
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

  # read the yaml config file
  yaml_data <- yaml::read_yaml(config_file)

  # provision of relative paths
  yaml_file_path <- dirname(config_file)

  # temporarily set the working directory to the yaml file location
  old_wd <- getwd()
  setwd(yaml_file_path)

  # check random seed
  if(!is.null(yaml_data$simulation_config$random_seed)){
    random_seed <- yaml_data$simulation_config$random_seed
    set.seed(random_seed)
  } else {
    random_seed <- NULL
  }

  # =====================================================
  # read mandatory parameters

  delta_t <- 0.5
  if(!is.null(yaml_data$simulation_config$nsim)){
    nsim <- yaml_data$simulation_config$nsim
  }

  pop_map_file <- yaml_data$population_data$mapping
  pop_map <- data.table::fread(pop_map_file, colClasses = "character")

  vac_time_id <- NULL
  vac_counts <- NULL

  if(!is.null(yaml_data$simulation_config$start_date)) {
    start_date <- as.Date(yaml_data$simulation_config$start_date,
                          tryFormats = c("%m/%d/%Y")) - 1 # we start one day before to treat the first day of simulation as day 1. 
  }
  sim_length <- yaml_data$simulation_config$length
  nsim <- ifelse(!is.null(yaml_data$simulation_config$nsim),
                 yaml_data$simulation_config$nsim, 1)

  chk_time_steps <- NULL
  chk_file_names <- NULL
  do_chk <- FALSE

  if(!is.null(yaml_data$simulation_config$checkpoint_dir)){
    checkpoint_dir <- normalizePath(yaml_data$simulation_config$checkpoint_dir, mustWork = FALSE)

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
        paste0(checkpoint_dir, "/checkpoint_", date_str, "_instance_", 1:nsim, ".Rda")
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
      chk_file_names <- matrix(paste0(checkpoint_dir, "/chk_", date_str, "_", 1:nsim, ".Rda"), ncol = 1)
    }
  }


  # ======================================================
  # If restore_from is available, initialize
  # meta_sim inputs
  if(!is.null(yaml_data$simulation_config$restore_from)){

    chk_obj <- readRDS(yaml_data$simulation_config$restore_from)

    ## chk_obj should be of class MetaRVMCheck
    ## verfify if chk_obj is of class MetaRVMCheck
    if(!methods::is(chk_obj, "MetaRVMCheck")){
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
    tv <- chk_obj$get("tv")
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
    pop_init <- data.table::fread(pop_init_file)

    # set up initializtion
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
    setwd(old_wd)
    stop("Rows of weekday day mixing matrix do not sum to 1")
  }
  if(any(abs(rowSums(m_wd_n) - 1) > 0.01)) {
    setwd(old_wd)
    stop("Rows of weekday night mixing matrix do not sum to 1")
  }
  if(any(abs(rowSums(m_we_d) - 1) > 0.01)) {
    setwd(old_wd)
    stop("Rows of weekend day mixing matrix do not sum to 1")
  }
  if(any(abs(rowSums(m_we_n) - 1) > 0.01)) {
    setwd(old_wd)
    stop("Rows of weekend night mixing matrix do not sum to 1")
  }

  ## check if global disease params are present
  if(!is.null(yaml_data$disease_params)){

    # read global disease parameters
    if(!is.null(yaml_data$disease_params$ts)) ts <- yaml_data$disease_params$ts
    if(!is.null(yaml_data$disease_params$tv)) tv <- yaml_data$disease_params$tv
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
    if(length(tv) == 1) tv <- rep(tv, N_pop)
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
  tv <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(tv, N_pop))))
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

    # check if the subgroup names match with mapping
    invalid_cats <- setdiff(cats_to_modify, names(pop_map))
    if (length(invalid_cats) > 0) {
      setwd(old_wd)
      stop(paste("Invalid categories in sub_disease_params:",
                 paste(invalid_cats, collapse = ", "),
                 ". The valid categories are", paste(names(pop_map), collapse = ", ")))
    }

    for (cat in cats_to_modify){
      cat_vals <- names(sub_disease_params[[cat]])

      # check if the subgroup values are valid
      invalid_cat_vals <- setdiff(cat_vals, unique(pop_map[[cat]]))
      if (length(invalid_cat_vals) > 0) {
        setwd(old_wd)
        stop(paste("Invalid values for category", cat, "in sub_disease_params:",
                   paste(invalid_cat_vals, collapse = ", "),
                   ". The valid categories are", paste(unique(pop_map[[cat]]), collapse = ", ")))
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


  config_list <- list(N_pop = N_pop,
                      pop_map = pop_map,
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
                      tv = tv,
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
                      random_seed = random_seed,
                      delta_t = delta_t,
                      chk_file_names = chk_file_names,
                      chk_time_steps = chk_time_steps,
                      do_chk = do_chk)

  setwd(old_wd)  # reset working directory
  
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
  vac_all_dates[t==0, V1:= 0] # since we are starting a day earlier, remove the vaccination counts from that date

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
