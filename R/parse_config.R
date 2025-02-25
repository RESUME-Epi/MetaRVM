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
  yaml_data <- read_yaml(config_file)

  # read mandatory parameters
  pop_map_file <- yaml_data$population_data$mapping
  pop_init_file <- yaml_data$population_data$initialization
  vac_file <- yaml_data$population_data$vaccination

  m_wd_d_file <- yaml_data$mixing_matrix$weekday_day
  m_wd_n_file <- yaml_data$mixing_matrix$weekday_night
  m_we_d_file <- yaml_data$mixing_matrix$weekend_day
  m_we_n_file <- yaml_data$mixing_matrix$weekend_night

  start_date <- yaml_data$simulation_config$start_date
  sim_length <- yaml_data$simulation_config$length
  nsim <- yaml_data$simulation_config$nsim

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

  ## TODO: check for mixing matrix consistency (rowsum = 1)

  n_pop <- nrow(pop_map)

  if(class(ts) == "numeric") ts <- rep(ts, n_pop)
  if(class(tv) == "numeric") tv <- rep(tv, n_pop)
  if(class(ve) == "numeric") ve <- rep(ve, n_pop)
  if(class(dv) == "integer") dv <- rep(dv, n_pop)
  if(class(dp) == "integer") dp <- rep(dp, n_pop)
  if(class(da) == "integer") da <- rep(da, n_pop)
  if(class(ds) == "integer") ds <- rep(ds, n_pop)
  if(class(dh) == "integer") dh <- rep(dh, n_pop)
  if(class(dr) == "integer") dr <- rep(dr, n_pop)
  if(class(pea) == "numeric") pea <- rep(pea, n_pop)
  if(class(psr) == "numeric") psr <- rep(psr, n_pop)
  if(class(phr) == "numeric") phr <- rep(phr, n_pop)

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
