


#' @title Metapopulation respiratory virus model Simulator
#'
#' @description
#' The main simulator which compiles and run an ODIN model in the background
#'
#'
#' @param N_pop Number of sub-populations
#' @param ts
#' @param tv
#' @param H0
#' @param D0
#' @param Ia0
#' @param Ip0
#' @param E0
#' @param S0 A vector of initial number of susceptible people in N_pop subpopulations
#' @param I0 A vector of initial number of infected people in N_pop subpopulations
#' @param P0 A vector of population sizes for N_pop subpopulations
#' @param V0 A vector of initial number of vaccinated people in N_pop subpopulations
#' @param R0 A vector of initial number of recovered people in N_pop subpopulations
#' @param m_weekday_day mixing matrix for weekday 6 am - 6 pm
#' @param m_weekday_night mixing matrix for weekday 6 pm - 6 am
#' @param m_weekend_day mixing matrix for weekend 6 am - 6 pm
#' @param m_weekend_night mixing matrix for weekend 6 pm - 6 am
#' @param delta_t A positive real number indicating the discrete time increment
#' @param tvac A vector of time indices for vaccination
#' @param vac_mat A matrix of order (length of tvac) x N_pop
#' @param dv mean number of days in Vaccinated state before waning immunity completely
#' @param de mean number of days in Exposed state
#' @param pea Proportion of people becoming infectious asymptomatic from exposed
#' @param dp mean number of days in Infectious presymptomatic state
#' @param da mean number of days in Infectious presymptomatic state
#' @param ds mean number of days in Infectious symptomatic state
#' @param psr Proportion of people recovered from infectious symptomatic
#' @param dh mean number of days in Hospitalized state
#' @param phr Proportion of people Recovered from Hospitalized
#' @param dr mean number of days in Recovered state
#' @param ve Vaccination efficacy, a value between 0 and 1
#' @param nsteps Number of discrete evolution in the simulation
#' @param is.stoch 1 if the simulation is stochastic, 0 otherwise
#' @param seed optional, for reproducibility, only application when is.stoch = 1

#'
#' @return A matrix of model output
#' @export
#'


meta_sim <- function(N_pop, ts, tv,
                     # S0, I0, P0, V0, R0,
                     S0, I0, P0, V0, R0,
                     H0 = 0, D0 = 0, Ia0 = 0, Ip0 = 0, E0 = 0,
                     m_weekday_day, m_weekday_night, m_weekend_day, m_weekend_night,
                     delta_t,
                     tvac, vac_mat,
                     dv, de, pea, dp,
                     da, ds, psr, dh,
                     phr, dr, ve,
                     nsteps,
                     is.stoch = FALSE,
                     seed = NULL,
                     do_chk = FALSE,
                     chk_file_name = NULL){

  metaODIN <- odin::odin({

    stoch <- user(0) # whether the model is run deterministically or not
    dt <- user(1)
    initial(time) <- 0
    update(time) <- (step + 1) * dt

    ## Equations for transitions between compartments by subpopulations

    update(S[])          <- S[i] - n_SE[i] + n_RS[i] + n_VS[i] - n_SV_eff[i]
    update(E[])          <- E[i] + n_SE[i] - n_EI[i] + n_VE[i]
    update(I_presymp[])  <- I_presymp[i] + n_EIpresymp[i] - n_preIsymp[i]
    update(I_asymp[])    <- I_asymp[i] + n_EIasymp[i] - n_IasympR[i]
    update(I_symp[])     <- I_symp[i] + n_preIsymp[i] - n_IsympRH[i]
    update(I_all[])      <- I_presymp[i] + I_asymp[i] + I_symp[i]
    update(R[])          <- R[i] + n_IasympR[i] + n_IsympR[i] +n_HR[i] - n_RS[i]
    update(H[])          <- H[i] + n_IsympH[i] - n_HR[i] - n_HD[i]
    update(D[])          <- D[i] + n_HD[i]
    update(P[])          <- P[i] - n_HD[i]
    update(V[])          <- V[i] - n_VS[i] - n_VE[i] + n_SV_eff[i]
    update(cum_V[])      <- cum_V[i] + n_SV_eff[i]
    update(mob_pop[])    <- S[i] + E[i] + I_all[i] + R[i] + V[i]

    ## =================================================
    ## sub population-based probabilities of transition:
    p_SE[]          <- 1 - exp(-lambda_i[i] * dt)  # S to E
    p_VE[]          <- 1 - exp(-lambda_v[i] * dt)  # V to E
    p_EIpresymp[]   <- 1 - exp(-EtoIpresymp[i] * dt)  # out of E
    p_preIsymp[]    <- 1 - exp(-pretoIsymp[i] * dt)   # I presymp to I symp
    p_IasympR[]     <- 1 - exp(-IasymptoR[i] * dt)    # I asymp to R
    p_IsympRH[]     <- 1 - exp(-IsymptoRH[i] * dt)    # out of I symp
    p_HRD[]         <- 1 - exp(-HtoRD[i] * dt)        # out of H
    p_RS[]          <- 1 - exp(-RtoS[i] * dt)         # R to S
    p_VS[]          <- 1 - exp(-VtoS[i] * dt)         # V to S

    ## =================================================
    ## Integrate vaccination data into model
    n_SV[]      <- interpolate(tt, vac, "constant")
    tt[]        <- user()
    vac[, ]     <- user()
    dim(tt)     <- user()
    dim(vac)    <- user()

    n_SV_eff[] <- n_SV[i] * vac_eff[i]


    ## =================================================
    # time/day specific mobility matrix
    m_weekday_day[, ]    <- user()
    m_weekday_night[, ]  <- user()
    m_weekend_day[, ]    <- user()
    m_weekend_night[, ]  <- user()

    m[, ] <- if((time %% 7 == 0) || (time %% 6 == 0)) (
      if(step %% 2 == 0) m_weekend_day[i,j] else m_weekend_night[i,j]
    ) else (
      if(step %% 2 == 0) m_weekday_day[i,j] else m_weekday_night[i,j]
    )

    ## =================================================
    ## Effective counts in sub-populations
    eff_prod[, ]    <- m[i, j] * mob_pop[i]
    P_eff[]         <- sum(eff_prod[, i]) # colSums

    S_eff_prod[, ]  <- m[i, j] * S[i]
    # S_eff[]         <- sum(S_eff_prod[, i]) # colSums

    V_eff_prod[, ]  <- m[i, j] * V[i]
    # V_eff[]         <- sum(V_eff_prod[, i]) # colSums

    # E_eff_prod[, ]  <- m[i, j] * E[i]
    # E_eff[]         <- sum(E_eff_prod[, i]) # colSums

    I_eff_prod[, ]  <- m[i, j] * I_all[i]
    I_eff[]         <- sum(I_eff_prod[, i]) # colSums

    # R_eff_prod[, ]  <- m[i, j] * R[i]
    # R_eff[]         <- sum(R_eff_prod[, i]) # colSums

    ## =================================================
    ## Force of infection
    lambda_i[] <- beta_i[i] * I_eff[i] / P_eff[i]
    lambda_v[] <- beta_v[i] * I_eff[i] / P_eff[i]

    ## =================================================
    ## Draws from binomial distributions for numbers changing between
    ## compartments:
    n_SE_eff[, ]      <- ceiling(if(S[i] <= 0) 0 else (if(stoch == 1) rbinom(S_eff_prod[j, i], p_SE[i]) else S_eff_prod[j, i] * p_SE[i]))
    n_SE[]            <- sum(n_SE_eff[i, ]) # rowSums
    n_EI[]            <- if(E[i] == 0) 0 else (if(stoch == 1) rbinom(E[i], p_EIpresymp[i]) else E[i] * p_EIpresymp[i])
    n_EI[]            <- ceiling(n_EI[i])
    n_EIpresymp[]     <- ceiling(n_EI[i] * (1 - pea[i]))
    n_EIasymp[]       <- n_EI[i] - n_EIpresymp[i]
    n_preIsymp[]      <- if(I_presymp[i] == 0) 0 else (if(stoch == 1) rbinom(I_presymp[i], p_preIsymp[i]) else I_presymp[i] * p_preIsymp[i])
    n_preIsymp[]      <- ceiling(n_preIsymp[i])
    n_IasympR[]       <- if(I_asymp[i] == 0) 0 else (if(stoch == 1) rbinom(I_asymp[i], p_IasympR[i]) else I_asymp[i] * p_IasympR[i])
    n_IasympR[]       <- ceiling(n_IasympR[i])
    n_IsympRH[]       <- if(I_symp[i] == 0) 0 else (if(stoch == 1) rbinom(I_symp[i], p_IsympRH[i]) else I_symp[i] * p_IsympRH[i])
    n_IsympRH[]       <- ceiling(n_IsympRH[i])
    n_IsympH[]        <- ceiling(n_IsympRH[i] * (1 - psr[i]))
    n_IsympR[]        <- n_IsympRH[i] - n_IsympH[i]
    n_HRD[]           <- if(H[i] == 0) 0 else (if(stoch == 1) rbinom(H[i], p_HRD[i]) else H[i] * p_HRD[i])
    n_HRD[]           <- ceiling(n_HRD[i])
    n_HR[]            <- ceiling(n_HRD[i] * phr[i])
    n_HD[]            <- n_HRD[i] - n_HR[i]
    n_RS[]            <- if(R[i] == 0) 0 else (if(stoch == 1) rbinom(R[i], p_RS[i]) else R[i] * p_RS[i])
    n_RS[]            <- ceiling(n_RS[i])
    n_VE_eff[, ]      <- if(stoch == 1) rbinom(V_eff_prod[j, i], p_VE[i]) else V_eff_prod[j, i] * p_VE[i]
    n_VE[]            <- sum(n_VE_eff[i, ]) # rowSums
    n_VE[]            <- ceiling(n_VE[i])
    n_VS[]            <- if(stoch == 1) rbinom(V[i] - n_VE[i], p_VS[i]) else (V[i] - n_VE[i]) * p_VS[i]
    n_VS[]            <- ceiling(n_VS[i])

    ## =================================================
    ## Initial states:
    initial(S[])            <- S_ini[i]
    initial(E[])            <- E_ini[i]
    initial(I_presymp[])    <- I_presymp_ini[i]
    initial(I_asymp[])      <- I_asymp_ini[i]
    initial(I_symp[])       <- I_symp_ini[i]
    initial(I_all[])        <- I_symp_ini[i]
    initial(R[])            <- R_ini[i]
    initial(H[])            <- H_ini[i]
    initial(D[])            <- D_ini[i]
    initial(V[])            <- V_ini[i]
    initial(cum_V[])        <- V_ini[i]
    initial(P[])            <- P_ini[i]
    initial(mob_pop[])      <- P_ini[i] - H_ini[i] - D_ini[i]

    ## =================================================
    ## additional output for debugging
    output(p_SE)          <- TRUE
    output(p_VE)          <- TRUE
    output(I_eff)         <- TRUE
    output(n_SE)          <- TRUE
    output(n_SV)          <- TRUE
    output(n_VE)          <- TRUE
    output(n_EI)          <- TRUE
    output(n_EIpresymp)   <- TRUE
    output(n_preIsymp)    <- TRUE
    output(n_IsympRH)     <- TRUE
    output(n_IsympH)      <- TRUE
    output(n_IsympR)      <- TRUE
    output(n_HR)          <- TRUE
    output(n_HD)          <- TRUE
    output(n_IasympR)     <- TRUE

    ## =================================================
    ## User defined parameters - default in parentheses:
    S_ini[]         <- user()
    I_symp_ini[]    <- user()
    I_presymp_ini[] <- user()
    I_asymp_ini[]   <- user()
    V_ini[]         <- user()
    P_ini[]         <- user()
    R_ini[]         <- user()
    E_ini[]         <- user()
    H_ini[]         <- user()
    D_ini[]         <- user()

    # beta_e         <- user(0.0165)
    beta_i[]         <- user()
    beta_v[]         <- user()
    pretoIsymp[]     <- user()
    IasymptoR[]      <- user()
    IsymptoRH[]      <- user()
    HtoRD[]          <- user()
    EtoIpresymp[]    <- user()
    RtoS[]           <- user()
    VtoS[]           <- user()
    pea[]            <- user()
    phr[]            <- user()
    psr[]            <- user()
    vac_eff[]        <- user()

    ## =================================================
    # dimensions of arrays
    N_pop           <- user()
    dim(S_ini)         <- N_pop
    dim(E_ini)         <- N_pop
    dim(I_symp_ini)    <- N_pop
    dim(I_asymp_ini)   <- N_pop
    dim(I_presymp_ini) <- N_pop
    dim(V_ini)         <- N_pop
    dim(P_ini)         <- N_pop
    dim(R_ini)         <- N_pop
    dim(H_ini)         <- N_pop
    dim(D_ini)         <- N_pop

    dim(S)           <- N_pop
    dim(E)           <- N_pop
    dim(I_presymp)   <- N_pop
    dim(I_asymp)     <- N_pop
    dim(I_symp)      <- N_pop
    dim(I_all)       <- N_pop
    dim(R)           <- N_pop
    dim(H)           <- N_pop
    dim(D)           <- N_pop
    dim(P)           <- N_pop
    dim(V)           <- N_pop
    dim(cum_V)       <- N_pop
    dim(mob_pop)    <- N_pop

    # dim(beta_e)         <- N_pop
    dim(beta_i)         <- N_pop
    dim(beta_v)         <- N_pop
    dim(EtoIpresymp)    <- N_pop
    dim(pretoIsymp)     <- N_pop
    dim(IasymptoR)      <- N_pop
    dim(IsymptoRH)      <- N_pop
    dim(HtoRD)          <- N_pop
    dim(RtoS)           <- N_pop
    dim(VtoS)           <- N_pop
    dim(pea)            <- N_pop
    dim(phr)            <- N_pop
    dim(psr)            <- N_pop
    dim(vac_eff)        <- N_pop

    dim(p_EIpresymp)    <- N_pop
    dim(p_preIsymp)     <- N_pop
    dim(p_IasympR)      <- N_pop
    dim(p_IsympRH)      <- N_pop
    dim(p_HRD)          <- N_pop
    dim(p_RS)           <- N_pop
    dim(p_VS)           <- N_pop

    dim(n_SE)          <- N_pop
    dim(n_VE)          <- N_pop
    dim(n_EI)          <- N_pop
    dim(n_EIpresymp)   <- N_pop
    dim(n_EIasymp)     <- N_pop
    dim(n_preIsymp)    <- N_pop
    dim(n_IasympR)     <- N_pop
    dim(n_IsympRH)     <- N_pop
    dim(n_IsympH)      <- N_pop
    dim(n_IsympR)      <- N_pop
    dim(n_HRD)         <- N_pop
    dim(n_HR)          <- N_pop
    dim(n_HD)          <- N_pop
    dim(n_RS)          <- N_pop
    dim(n_VS)          <- N_pop
    dim(n_SV)          <- N_pop
    dim(n_SV_eff)      <- N_pop
    dim(n_SE_eff)      <- c(N_pop, N_pop)
    dim(n_VE_eff)      <- c(N_pop, N_pop)

    dim(lambda_i) <- N_pop
    dim(lambda_v) <- N_pop
    dim(p_SE) <- N_pop
    dim(p_VE) <- N_pop
    dim(I_eff) <- N_pop
    dim(P_eff) <- N_pop

    dim(eff_prod) <- c(N_pop, N_pop)
    dim(S_eff_prod) <- c(N_pop, N_pop)
    dim(I_eff_prod) <- c(N_pop, N_pop)
    dim(V_eff_prod) <- c(N_pop, N_pop)
    dim(m) <- c(N_pop, N_pop)
    dim(m_weekday_day) <- c(N_pop, N_pop)
    dim(m_weekday_night) <- c(N_pop, N_pop)
    dim(m_weekend_day) <- c(N_pop, N_pop)
    dim(m_weekend_night) <- c(N_pop, N_pop)

  })

  ## If disease parameters are scalars, create the vector inputs
  if(length(ts) == 1) ts <- rep(ts, N_pop)
  if(length(tv) == 1) tv <- rep(tv, N_pop)
  if(length(ve) == 1) ve <- rep(ve, N_pop)
  if(length(dv) == 1) dv <- rep(dv, N_pop)
  if(length(de) == 1) de <- rep(de, N_pop)
  if(length(de) == 1) de <- rep(de, N_pop)
  if(length(dp) == 1) dp <- rep(dp, N_pop)
  if(length(da) == 1) da <- rep(da, N_pop)
  if(length(ds) == 1) ds <- rep(ds, N_pop)
  if(length(dh) == 1) dh <- rep(dh, N_pop)
  if(length(dr) == 1) dr <- rep(dr, N_pop)
  if(length(pea) == 1) pea <- rep(pea, N_pop)
  if(length(psr) == 1) psr <- rep(psr, N_pop)
  if(length(phr) == 1) phr <- rep(phr, N_pop)




  model <- metaODIN$new(stoch = is.stoch,
                        N_pop = N_pop,
                        beta_i = ts,
                        beta_v = tv,
                        S_ini = S0,
                        E_ini = E0,
                        I_asymp_ini = Ia0,
                        I_presymp_ini = Ip0,
                        I_symp_ini = I0,
                        H_ini = H0,
                        D_ini = D0,
                        P_ini = P0,
                        V_ini = V0,
                        R_ini = R0,
                        m_weekday_day = m_weekday_day,
                        m_weekday_night = m_weekday_night,
                        m_weekend_day = m_weekend_day,
                        m_weekend_night = m_weekend_night,
                        dt = delta_t,
                        tt = tvac,
                        vac = vac_mat,
                        VtoS = 1/dv,
                        EtoIpresymp = 1/de,
                        pea = pea,
                        pretoIsymp = 1/dp,
                        IasymptoR = 1/da,
                        IsymptoRH = 1/ds,
                        psr = psr,
                        HtoRD = 1/dh,
                        phr = phr,
                        RtoS = 1/dr,
                        vac_eff = ve)

  out <- model$run(step = 0:nsteps)
  out_df <- data.frame(out)

  long_out <- out_df %>%
    tidyr::pivot_longer(
      cols = -c("step", "time"),               # Exclude 'time' from being pivoted
      names_to = c("disease_state", "population_id"),  # Create new columns for disease state and subpopulation
      names_pattern = "([A-Za-z_]+)\\.(\\d+)\\.",  # Regex to extract the disease state and subpopulation ID
      values_to = "value")          # Column to store the actual values

  long_out <- data.table::data.table(long_out)

  # Checkpointing
  if(do_chk){
    chk <- list()

    chk[["N_pop"]] <- model_config$N_pop
    chk[["delta_t"]] <- model_config$delta_t

    chk[["m_weekday_day"]] <- model_config$m_wd_d
    chk[["m_weekday_night"]] <- model_config$m_wd_n
    chk[["m_weekend_day"]] <- model_config$m_we_d
    chk[["m_weekend_night"]] <- model_config$m_we_n

    chk[["ts"]] <- model_config$ts
    chk[["tv"]] <- model_config$tv
    chk[["ve"]] <- model_config$ve
    chk[["dv"]] <- model_config$dv
    chk[["de"]] <- model_config$de
    chk[["dp"]] <- model_config$dp
    chk[["da"]] <- model_config$da
    chk[["ds"]] <- model_config$ds
    chk[["dh"]] <- model_config$dh
    chk[["dr"]] <- model_config$dr
    chk[["pea"]] <- model_config$pea
    chk[["psr"]] <- model_config$psr
    chk[["phr"]] <- model_config$phr

    chk[["vac_time_id"]] <- model_config$vac_time_id
    chk[["vac_counts"]] <- model_config$vac_counts

    chk[["S"]] <- long_out[(long_out$step == model_config$sim_length) & (long_out$disease_state == "S"), c("value")]
    chk[["E"]] <- long_out[(long_out$step == model_config$sim_length) & (long_out$disease_state == "E"), c("value")]
    chk[["Ia"]] <- long_out[(long_out$step == model_config$sim_length) & (long_out$disease_state == "I_asymp"), c("value")]
    chk[["Ip"]] <- long_out[(long_out$step == model_config$sim_length) & (long_out$disease_state == "I_presymp"), c("value")]
    chk[["Is"]] <- long_out[(long_out$step == model_config$sim_length) & (long_out$disease_state == "I_symp"), c("value")]
    chk[["H"]] <- long_out[(long_out$step == model_config$sim_length) & (long_out$disease_state == "H"), c("value")]
    chk[["D"]] <- long_out[(long_out$step == model_config$sim_length) & (long_out$disease_state == "D"), c("value")]
    chk[["P"]] <- long_out[(long_out$step == model_config$sim_length) & (long_out$disease_state == "P"), c("value")]
    chk[["V"]] <- long_out[(long_out$step == model_config$sim_length) & (long_out$disease_state == "V"), c("value")]
    chk[["R"]] <- long_out[(long_out$step == model_config$sim_length) & (long_out$disease_state == "R"), c("value")]

    if(!is.null(chk_file_name)) {
      saveRDS(chk, file = chk_file_name)
    }
  }


  return(long_out)
}
