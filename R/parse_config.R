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

  # check if restore_from is available
  if(!is.null(yaml_data$restore_from)){

    chk <- readRDS(yaml_data$restore_from)

    m_wd_d <- chk[["weekday_day"]]
    m_wd_n <- chk[["weekday_night"]]
    m_we_d <- chk[["weekend_day"]]
    m_we_n <- chk[["weekend_night"]]

    # read global disease parameters
    ts <- chk[["ts"]]
    tv <- chk[["tv"]]
    ve <- chk[["ve"]]
    dv <- chk[["dv"]]
    de <- chk[["de"]]
    dp <- chk[["dp"]]
    da <- chk[["da"]]
    ds <- chk[["ds"]]
    dh <- chk[["dh"]]
    dr <- chk[["dr"]]
    pea <- chk[["pea"]]
    psr <- chk[["psr"]]
    phr <- chk[["phr"]]

    # pop_map <- data.table::fread(pop_map_file, colClasses = "character")
    # pop_init <- data.table::fread(pop_init_file)
    vac <- chk[["vac"]]

    S_ini = chk[["S"]]
    E_ini = chk[["E"]]
    I_asymp_ini = chk[["Ia"]]
    I_presymp_ini = chk[["Ip"]]
    I_symp_ini = chk[["Is"]]
    H_ini = chk[["H"]]
    D_ini = chk[["D"]]
    P_ini = chk[["P"]]
    V_ini = chk[["V"]]
    R_ini = chk[["R"]]

  } else {

    pop_init_file <- yaml_data$population_data$initialization
    vac_file <- yaml_data$population_data$vaccination

    m_wd_d_file <- yaml_data$mixing_matrix$weekday_day
    m_wd_n_file <- yaml_data$mixing_matrix$weekday_night
    m_we_d_file <- yaml_data$mixing_matrix$weekend_day
    m_we_n_file <- yaml_data$mixing_matrix$weekend_night

    # read global disease parameters
    ts <- yaml_data$disease_params$ts
    tv <- yaml_data$disease_params$tv
    ve <- yaml_data$disease_params$ve
    dv <- yaml_data$disease_params$dv
    de <- yaml_data$disease_params$de
    dp <- yaml_data$disease_params$dp
    da <- yaml_data$disease_params$da
    ds <- yaml_data$disease_params$ds
    dh <- yaml_data$disease_params$dh
    dr <- yaml_data$disease_params$dr
    pea <- yaml_data$disease_params$pea
    psr <- yaml_data$disease_params$psr
    phr <- yaml_data$disease_params$phr

    pop_map <- data.table::fread(pop_map_file, colClasses = "character")
    pop_init <- data.table::fread(pop_init_file)
    vac <- data.table::fread(vac_file)
    m_wd_d <- read.csv(m_wd_d_file, header = F)
    m_wd_n <- read.csv(m_wd_n_file, header = F)
    m_we_d <- read.csv(m_we_d_file, header = F)
    m_we_n <- read.csv(m_we_n_file, header = F)
  }

  # read mandatory parameters
  pop_map_file <- yaml_data$population_data$mapping

  start_date <- yaml_data$simulation_config$start_date
  sim_length <- yaml_data$simulation_config$length
  nsim <- yaml_data$simulation_config$nsim



  ## TODO: check for mixing matrix consistency (rowsum = 1)

  n_pop <- nrow(pop_map)

  if(is(ts, "numeric")) ts <- rep(ts, n_pop)
  if(is(tv, "numeric")) tv <- rep(tv, n_pop)
  if(is(ve, "numeric")) ve <- rep(ve, n_pop)
  if(is(dv, "integer")) dv <- rep(dv, n_pop)
  if(is(dp, "integer")) dp <- rep(dp, n_pop)
  if(is(da, "integer")) da <- rep(da, n_pop)
  if(is(ds, "integer")) ds <- rep(ds, n_pop)
  if(is(dh, "integer")) dh <- rep(dh, n_pop)
  if(is(dr, "integer")) dr <- rep(dr, n_pop)
  if(is(pea, "numeric")) pea <- rep(pea, n_pop)
  if(is(psr, "numeric")) psr <- rep(psr, n_pop)
  if(is(phr, "numeric")) phr <- rep(phr, n_pop)

  # read sub disease parameters and modify
  # disease parameters
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

  return(list(pop_map = pop_map,
              pop_init = pop_init,
              vac = vac,
              m_wd_d = m_wd_d,
              m_wd_n = m_wd_n,
              m_we_d = m_we_d,
              m_we_n = m_we_n,
              start_date = start_date,
              sim_length = sim_length,
              nsim = nsim,
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
              phr = phr))

}


#' Title
#'
#' @param config_list A list of configurations
#' @param N_pop Number of subpopulations
#'
#' @returns A random sample drawn from the distribution specified by the dist component
#' @export
#'
#' @examples
draw_sample <- function(config_list, N_pop){

  if(is(config_list, "list")){

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
