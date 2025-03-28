#' Title
#'
#' @param config_file A yaml configuration file
#'
#' @returns a named list of (key, value) pairs of the parameters
#' @export
#'
#' @examples
parse_config <- function(config_file){

  # read the yaml config file
  yaml_data <- yaml::read_yaml(config_file)

  # =====================================================
  # read mandatory parameters

  delta_t <- 0.5
  if(!is.null(yaml_data$simulation_config$nsim)){
    nsim <- yaml_data$simulation_config$nsim
  }

  pop_map_file <- yaml_data$population_data$mapping
  pop_map <- data.table::fread(pop_map_file, colClasses = "character")

  if(!is.null(yaml_data$simulation_config$start_date)) {
    start_date <- as.Date(yaml_data$simulation_config$start_date)
  }
  sim_length <- yaml_data$simulation_config$length
  nsim <- ifelse(!is.null(yaml_data$simulation_config$nsim),
                 yaml_data$simulation_config$nsim, 1)

  if(!is.null(yaml_data$simulation_config$random_seed)){
    random_seed <- yaml_data$simulation_config$randmom_seed
  }
  if(!is.null(yaml_data$simulation_config$chk_dir)){
    chk_dir <- yaml_data$simulation_config$chk_dir

    # prepare checkpoint file names
    if(!dir.exists(chk_dir)) dir.create(chk_dir)

    chk_file_names <- paste0(chk_dir, "/chk_", 1:nsim, ".Rda")

    do_chk <- TRUE
  } else {
    chk_file_names <- NULL
    do_chk <- FALSE
  }


  # ======================================================
  # If restore_from is available, initialize
  # meta_sim inputs
  if(!is.null(yaml_data$simulation_config$restore_from)){

    chk_obj <- readRDS(yaml_data$simulation_config$restore_from)

    N_pop <- chk_obj[["N_pop"]]
    delta_t <- chk_obj[["delta_t"]]

    m_wd_d <- chk_obj[["m_weekday_day"]]
    m_wd_n <- chk_obj[["m_weekday_night"]]
    m_we_d <- chk_obj[["m_weekend_day"]]
    m_we_n <- chk_obj[["m_weekend_night"]]

    # read disease parameters
    ts <- chk_obj[["ts"]]
    tv <- chk_obj[["tv"]]
    ve <- chk_obj[["ve"]]
    dv <- chk_obj[["dv"]]
    de <- chk_obj[["de"]]
    dp <- chk_obj[["dp"]]
    da <- chk_obj[["da"]]
    ds <- chk_obj[["ds"]]
    dh <- chk_obj[["dh"]]
    dr <- chk_obj[["dr"]]
    pea <- chk_obj[["pea"]]
    psr <- chk_obj[["psr"]]
    phr <- chk_obj[["phr"]]

    vac_time_id <- chk_obj[["vac_time_id"]]
    vac_counts <- chk_obj[["vac_counts"]]

    S_ini = chk_obj[["S"]]
    E_ini = chk_obj[["E"]]
    I_asymp_ini = chk_obj[["Ia"]]
    I_presymp_ini = chk_obj[["Ip"]]
    I_symp_ini = chk_obj[["Is"]]
    H_ini = chk_obj[["H"]]
    D_ini = chk_obj[["D"]]
    P_ini = chk_obj[["P"]]
    V_ini = chk_obj[["V"]]
    R_ini = chk_obj[["R"]]

  }

  ## If other parameters are present, override them
  ## population initialization
  if(!is.null(yaml_data$population_data$initialization)){

    pop_init_file <- yaml_data$population_data$initialization
    pop_init <- data.table::fread(pop_init_file)
    # print(pop_init)

    # set up initializtion
    N_pop <- nrow(pop_init)
    P_ini <- pop_init[, c("N")]
    S_ini <- pop_init[, c("S0")]
    I_symp_ini <- pop_init[, c("I0")]
    V_ini <- pop_init[, c("V0")]
    R_ini <- pop_init[, c("R0")]
    E_ini <- rep(0, N_pop)
    I_asymp_ini <- rep(0, N_pop)
    I_presymp_ini <- rep(0, N_pop)
    H_ini <- rep(0, N_pop)
    D_ini <- rep(0, N_pop)

  }

  ## check if vac data is present
  if(!is.null(yaml_data$population_data$vaccination)){

    vac_file <- yaml_data$population_data$vaccination
    vac_dt <- data.table::fread(vac_file)
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
    m_wd_d <- as.matrix(read.csv(m_wd_d_file, header = F))
  }
  if(!is.null(yaml_data$mixing_matrix$weekday_night)){
    m_wd_n_file <- yaml_data$mixing_matrix$weekday_night
    m_wd_n <- as.matrix(read.csv(m_wd_n_file, header = F))
  }
  if(!is.null(yaml_data$mixing_matrix$weekend_day)){
    m_we_d_file <- yaml_data$mixing_matrix$weekend_day
    m_we_d <- as.matrix(read.csv(m_we_d_file, header = F))
  }
  if(!is.null(yaml_data$mixing_matrix$weekend_night)){
    m_we_n_file <- yaml_data$mixing_matrix$weekend_night
    m_we_n <- as.matrix(read.csv(m_we_n_file, header = F))
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
    if(length(dp) == 1) dp <- rep(dp, N_pop)
    if(length(da) == 1) da <- rep(da, N_pop)
    if(length(ds) == 1) ds <- rep(ds, N_pop)
    if(length(dh) == 1) dh <- rep(dh, N_pop)
    if(length(dr) == 1) dr <- rep(dr, N_pop)
    if(length(pea) == 1) pea <- rep(pea, N_pop)
    if(length(psr) == 1) psr <- rep(psr, N_pop)
    if(length(phr) == 1) phr <- rep(phr, N_pop)
  }

  ## check if population specific disease params are present
  if(!is.null(yaml_data$sub_disease_params)){

    sub_disease_params <- yaml_data$sub_disease_params
    cats_to_modify <- names(sub_disease_params)

    for (cat in cats_to_modify){
      cat_vals <- names(sub_disease_params[[cat]])

      for (cat_val in cat_vals){
        row_ids <- which(pop_map[[cat]] == cat_val)
        params_to_modify <- names(sub_disease_params[[cat]][[cat_val]])

        for (param in params_to_modify){
          temp_param <- get(param)
          temp_param[row_ids] <- sub_disease_params[[cat]][[cat_val]][[param]]
          assign(param, temp_param)
        }
      }
    }

  }

  ## Stochastic parameters
  ts <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(ts, N_pop, random_seed))))
  tv <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(tv, N_pop, random_seed))))
  ve <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(ve, N_pop, random_seed))))
  dv <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(dv, N_pop, random_seed))))
  de <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(de, N_pop, random_seed))))
  dp <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(dp, N_pop, random_seed))))
  da <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(da, N_pop, random_seed))))
  ds <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(ds, N_pop, random_seed))))
  dh <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(dh, N_pop, random_seed))))
  dr <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(dr, N_pop, random_seed))))
  pea <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(pea, N_pop, random_seed))))
  psr <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(psr, N_pop, random_seed))))
  phr <- do.call(rbind, (purrr::map(1:nsim, ~ draw_sample(phr, N_pop, random_seed))))



  ## TODO: check for mixing matrix consistency (rowsum = 1)


  return(list(N_pop = N_pop,
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
              vac_time_id = vac_time_id,
              vac_counts = vac_counts,
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
              do_chk = do_chk))

}

# next_rda <- function() {
#   f <- list.files(path.expand("~/my_data/"), pattern = "^data_\\d+\\.Rda")
#   num <- max(as.numeric(gsub("^data_(\\d)\\.Rda", "\\1", f)) + 1)
#   paste0(path.expand("~/my_data/data_"), num, ".Rda")
# }

#' Title
#'
#' @param vac_dt
#' @param sim_start_date
#' @param sim_length
#' @param delta_t
#'
#' @returns
#'
#' @examples
process_vac_data <- function(vac_dt, sim_start_date, sim_length, delta_t) {

  vac_dt$date <- as.Date(vac_dt$date,
                         tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y"))

  date_filtered <- vac_dt %>%
    dplyr::filter(date >= as.Date(sim_start_date)) %>%
    dplyr::mutate(t = (date - as.Date(sim_start_date)) / 0.5) %>%
    dplyr::select(-c(date)) %>%
    dplyr::select(last_col(), everything())

  ## fill in the missing time in vac data
  complete_time <- data.table::data.table(t = seq(0, sim_length / delta_t))

  ## merge
  vac_all_dates <- merge(complete_time, date_filtered, by = "t", all.x = TRUE)
  vac_all_dates[is.na(vac_all_dates)] <- 0

  return(vac_all_dates)
}


# ==============================================================================
#' Function to draw samples for distributional parameters
#'
#' @param config_list A list of configurations
#' @param N_pop Number of subpopulations
#'
#' @returns A random sample drawn from the distribution specified by the dist component
#'
#' @examples
draw_sample <- function(config_list, N_pop, seed = NULL){

  if(is(config_list, "list")){

    if(!is.null(seed)) set.seed(seed)

    if(config_list$dist == "lognormal")
      x <- rlnorm(1, meanlog = config_list$mu, sdlog = config_list$sd)

    if(config_list$dist == "gamma")
      x <- rgamma(1, shape = config_list$shape, rate = config_list$rate)

    if(config_list$dist == "uniform")
      x <- runif(1, min = config_list$min, max = config_list$max)

    if(config_list$dist == "beta")
      x <- rbeta(1, shape1 = config_list$shape1, shape2 = config_list$shape2)

    return(rep(x, N_pop))
  } else return(config_list)
}
