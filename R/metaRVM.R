

#' Title
#'
#' @param yaml_config
#'
#' @returns A long format data.table of simulation output
#' @export
#'
metaRVM <- function(yaml_config){

  ## TODO: use Generic Function to call metaRVM on yaml config
  ## or model_config.

  # parse config
  model_config <- parse_config(yaml_config)

  # pass inputs to meta_sim
  nsim <- model_config$nsim
  nsteps <- floor(model_config$sim_length / model_config$delta_t)

  out <- data.table::data.table()
  for (ii in 1:nsim){

    o <- meta_sim(is.stoch = 0,
                  nsteps = nsteps,
                  N_pop = model_config$N_pop,
                  S0 = model_config$S_ini,
                  I0 = model_config$I_symp_ini,
                  P0 = model_config$P_ini,
                  # V0 = model_config$V_ini,
                  R0 = model_config$R_ini,
                  H0 = model_config$H_ini,
                  D0 = model_config$D_ini,
                  E0 = model_config$E_ini,
                  Ia0 = model_config$I_asymp_ini,
                  Ip0 = model_config$I_presymp_ini,
                  m_weekday_day = model_config$m_wd_d,
                  m_weekday_night = model_config$m_wd_n,
                  m_weekend_day = model_config$m_we_d,
                  m_weekend_night = model_config$m_we_n,
                  delta_t = model_config$delta_t,
                  # tvac = model_config$vac_time_id,
                  vac_mat = model_config$vac_mat,
                  ts = model_config$ts[ii, ],
                  tv = model_config$tv[ii, ],
                  dv = model_config$dv[ii, ],
                  de = model_config$de[ii, ],
                  pea = model_config$pea[ii, ],
                  dp = model_config$dp[ii, ],
                  da = model_config$da[ii, ],
                  ds = model_config$ds[ii, ],
                  psr = model_config$psr[ii, ],
                  dh = model_config$dh[ii, ],
                  phr = model_config$phr[ii, ],
                  dr = model_config$dr[ii, ],
                  ve = model_config$ve[ii, ],
                  do_chk = model_config$do_chk,
                  chk_file_name = model_config$chk_file_names[ii])

    o$instance <- ii
    out <- rbind(out, o)
  }

  # convert to daily output
  # long_out_daily <- daily_output(long_out, model_config$start_date)

  # if (requireNamespace("foreach", quietly = TRUE)) {
  #   if(requireNamespace("doParallel", quietly = TRUE)){
  #     if(requireNamespace("parallel", quietly = TRUE)){
  #
  #       ncores <- parallel::detectCores()
  #       doParallel::registerDoParallel(ncores - 2)
  #
  #       res <- foreach::foreach(i = 1:nsim) foreach::`%dopar%`(
  #         sqrt(i)
  #       )
  #     }
  #   }
  # } else {
  #   res <- foreach::foreach(i = 1:nsim) foreach::`%do%`(
  #     sqrt(i)
  #   )
  # }

  return(out)
}
