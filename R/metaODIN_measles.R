## Meta population model for measles.

## State transitions:
## S -> E1 -> E2 -> I1_Q or I1_U -> I2_Q or I2_U -> R

#' Metapopulation Measles Simulator (ODIN Core)
#'
#' @description
#' Core ODIN-based simulation engine for a measles metapopulation model with
#' two exposed stages (`E1`, `E2`) and two infectious stages (`I1`, `I2`),
#' each split into quarantined (`_Q`) and unquarantined (`_U`) compartments.
#'
#' @param N_pop Integer. Number of subpopulations.
#' @param beta Numeric vector or scalar. Transmission rate by subpopulation.
#' @param S0 Numeric vector of length `N_pop`. Initial susceptible counts.
#' @param E10 Numeric vector of length `N_pop`. Initial first exposed stage counts.
#' @param E20 Numeric vector of length `N_pop`. Initial second exposed stage counts.
#' @param I1_Q0 Numeric vector of length `N_pop`. Initial `I1_Q` counts.
#' @param I1_U0 Numeric vector of length `N_pop`. Initial `I1_U` counts.
#' @param I2_Q0 Numeric vector of length `N_pop`. Initial `I2_Q` counts.
#' @param I2_U0 Numeric vector of length `N_pop`. Initial `I2_U` counts.
#' @param R0 Numeric vector of length `N_pop`. Initial recovered counts.
#' @param m_weekday_day Numeric matrix (`N_pop x N_pop`). Weekday daytime mixing matrix.
#' @param m_weekday_night Numeric matrix (`N_pop x N_pop`). Weekday nighttime mixing matrix.
#' @param m_weekend_day Numeric matrix (`N_pop x N_pop`). Weekend daytime mixing matrix.
#' @param m_weekend_night Numeric matrix (`N_pop x N_pop`). Weekend nighttime mixing matrix.
#' @param start_day Integer in `0:6`, where 0 is Monday.
#' @param delta_t Numeric. Simulation time step in days (typically `0.5`).
#' @param de1 Numeric vector or scalar. Mean duration (days) in `E1`.
#' @param de2 Numeric vector or scalar. Mean duration (days) in `E2`.
#' @param di1 Numeric vector or scalar. Mean duration (days) in `I1`.
#' @param di2 Numeric vector or scalar. Mean duration (days) in `I2`.
#' @param prop_I1_Q Numeric vector or scalar in `[0, 1]`. Proportion entering `I1_Q` from `E2`.
#' @param prop_I2_Q Numeric vector or scalar in `[0, 1]`. Proportion of `I1_U -> I2` transitions entering `I2_Q`.
#' @param nsteps Integer. Number of simulation steps.
#' @param is.stoch Logical. Included for API compatibility; this model currently
#'   uses stochastic binomial transitions.
#' @param seed Integer or `NULL`. Included for API compatibility.
#' @param do_chk Logical. Included for API compatibility (checkpointing not implemented here).
#' @param chk_time_steps Integer vector or `NULL`. Included for API compatibility.
#' @param chk_file_names Character vector/list or `NULL`. Included for API compatibility.
#'
#' @details
#' Compartments evolve as:
#' \itemize{
#'   \item `S -> E1 -> E2 -> I1_Q/I1_U -> I2_Q/I2_U -> R`
#' }
#'
#' Force of infection is computed from unquarantined infectious individuals
#' (`I1_U + I2_U`) under time-varying day/night and weekday/weekend mixing.
#'
#' @return A `data.table` in long format with columns:
#' \describe{
#'   \item{step}{Time-step index (`0:nsteps`).}
#'   \item{time}{Continuous simulation time (`step * delta_t`).}
#'   \item{disease_state}{Compartment/metric name (for example `S`, `E1`, `I1_U`, `I_all`, `n_E2I1`).}
#'   \item{population_id}{Subpopulation id (integer).}
#'   \item{value}{Compartment value at that time and subpopulation.}
#' }
#'
#' @examples
#' \donttest{
#' options(odin.verbose = FALSE)
#' N_pop <- 2
#' mix <- diag(N_pop)
#' out <- meta_measles_sim(
#'   N_pop = N_pop,
#'   beta = 0.7,
#'   S0 = c(1000, 900),
#'   I1_U0 = c(5, 3),
#'   m_weekday_day = mix,
#'   m_weekday_night = mix,
#'   m_weekend_day = mix,
#'   m_weekend_night = mix,
#'   delta_t = 0.5,
#'   de1 = 3, de2 = 3, di1 = 2, di2 = 3,
#'   prop_I1_Q = 0.2, prop_I2_Q = 0.4,
#'   nsteps = 20
#' )
#' head(out)
#' }
#'
#' @export
meta_measles_sim <- function(N_pop = 1, 
                              beta, 
                              S0 = rep(1000, N_pop),
                              V10 = rep(0, N_pop),
                              V20 = rep(0, N_pop),
                              E10 = rep(0, N_pop), 
                              E20 = rep(0, N_pop),
                              I1_Q0 = rep(0, N_pop),
                              I1_U0 = rep(10, N_pop),
                              I2_Q0 = rep(0, N_pop),
                              I2_U0 = rep(0, N_pop),
                              R0 = rep(0, N_pop),
                              m_weekday_day, m_weekday_night, m_weekend_day, m_weekend_night,
                              start_day = 0,
                              delta_t,
                              #  vac_mat,
                              de1, de2, di1, di2,
                              prop_I1_Q, prop_I2_Q,
                              V1_eff, V2_eff,
                              nsteps,
                              is.stoch = FALSE,
                              seed = NULL,
                              do_chk = FALSE,
                              chk_time_steps = NULL,
                              chk_file_names = NULL){
  
  
  metaODIN_measles <- odin::odin({
    
    dt <- user(1)
    initial(time) <- 0
    update(time) <- (step + 1) * dt

    ## apply vaccine efficacy to initial vaccinated counts
    V1_ini[] <- V10[i] * V1_eff[i]
    V2_ini[] <- V20[i] * V2_eff[i]
    S0_eff[] <- S0[i] - V1_ini[i] - V2_ini[i]


    ## initializations
    initial(S[]) <- S0_eff[i]
    initial(V1[]) <- V1_ini[i]
    initial(V2[]) <- V2_ini[i]
    initial(E1[]) <- E10[i]
    initial(E2[]) <- E20[i]
    initial(I1_Q[]) <- I1_Q0[i]
    initial(I1_U[]) <- I1_U0[i]
    initial(I2_Q[]) <- I2_Q0[i]
    initial(I2_U[]) <- I2_U0[i]
    initial(R[]) <- R0[i]

    ## Equations for transitions between compartments by subpopulations
    update(S[])          <- S[i] - n_SE1[i]
    update(V1[])         <- V1[i] # placeholder for vaccination transitions
    update(V2[])         <- V2[i] # placeholder for vaccination transitions
    update(E1[])         <- E1[i] + n_SE1[i] - n_E1E2[i]
    update(E2[])         <- E2[i] + n_E1E2[i] - n_E2I1[i]
    update(I1_Q[])       <- I1_Q[i] + n_E2I1_Q[i] - n_I1Q_I2Q[i]
    update(I1_U[])       <- I1_U[i] + n_E2I1_U[i] - n_I1U_I2U[i] - n_I1U_I2Q[i]
    update(I2_Q[])       <- I2_Q[i] + n_I1Q_I2Q[i] + n_I1U_I2Q[i] - n_I2Q_R[i]
    update(I2_U[])       <- I2_U[i] + n_I1U_I2U[i] - n_I2U_R[i]
    # update(I1_all)       <- I1_Q[i] + n_E2I1_Q[i] - n_I1Q_I2Q[i] + 
    #                         I1_U[i] + n_E2I1_U[i] - n_I1U_I2U[i] - n_I1U_I2Q[i]
    # update(I2_all)       <- I2_Q[i] + n_I1Q_I2Q[i] - n_I2Q_R[i] + 
    #                         I2_U[i] + n_I1U_I2U[i] - n_I2U_R[i]
    # update(I_all[])      <- I_all[i] + 
    #                         I1_U[i] + n_E2I1_U[i] - n_I1U_I2U[i] - n_I1U_I2Q[i] + 
    #                         I2_U[i] + n_I1U_I2U[i] - n_I2U_R[i]
    update(R[])          <- R[i] + n_I2Q_R[i] + n_I2U_R[i]

    # extra compartments for tracking cumulative counts
    I1_all[]       <- I1_Q[i] + I1_U[i]
    I2_all[]       <- I2_Q[i] + I2_U[i]
    I_all[]        <- I1_all[i] + I2_all[i]
    IU_all[]       <- I1_U[i] + I2_U[i]
    mob_pop[]      <- S[i] + V1[i] + V2[i] + E1[i] + E2[i] + I1_U[i] + I2_U[i] + R[i]


    ## Calculate transitions based on current state and parameters
    p_SE1[]     <- 1 - exp(-lambda[i] * dt) # probability of S->E1
    p_E1E2[]    <- 1 - exp(-E1toE2[i] * dt) # probability of E1->E2
    p_E2_I1[]   <- 1 - exp(-E2toI1[i] * dt) # probability of E2->I1
    p_I1U_I2[]  <- 1 - exp(-I1toI2[i] * dt) # probability of I1_U->I2
    p_I2_R[]    <- 1 - exp(-I2toR[i] * dt) # probability of I2->R

    ## Effective counts in sub-populations
    eff_prod[, ]    <- m[i, j] * mob_pop[i]
    P_eff[]         <- sum(eff_prod[, i]) # colSums

    # S_eff_prod[, ]  <- m[i, j] * S[i]
    I_eff_prod[, ]  <- m[i, j] * IU_all[i]
    I_eff[]         <- sum(I_eff_prod[, i]) # colSums

    ## mixing
    saturday_id <- 5 - start_day
    sunday_id <- 6 - start_day

    m[, ] <- if((time %% 7 == saturday_id) || (time %% 7 == sunday_id)) (
      if(step %% 2 == 0) m_weekend_day[i,j] else m_weekend_night[i,j]
    ) else (
      if(step %% 2 == 0) m_weekday_day[i,j] else m_weekday_night[i,j]
    )

    ## Force of infection
    lambda[] <- beta[i] * I_eff[i] / P_eff[i]

    ## compute the delta counts for each transition using binomial draws

    n_SE1[]     <- rbinom(S[i], p_SE1[i])
    n_E1E2[]  <- rbinom(E1[i], p_E1E2[i])
    n_E2I1[]  <- rbinom(E2[i], p_E2_I1[i])
    n_E2I1_Q[]  <- rbinom(n_E2I1[i], prop_I1_Q[i])
    n_E2I1_U[]  <- n_E2I1[i] - n_E2I1_Q[i]
    n_I1U_I2[]  <- rbinom(I1_U[i], p_I1U_I2[i])
    n_I1U_I2Q[]  <- rbinom(n_I1U_I2[i], prop_I2_Q[i])
    n_I1U_I2U[]  <- n_I1U_I2[i] - n_I1U_I2Q[i]
    n_I1Q_I2Q[]  <- rbinom(I1_Q[i], p_I1U_I2[i])
    n_I2Q_R[]  <- rbinom(I2_Q[i], p_I2_R[i])
    n_I2U_R[]  <- rbinom(I2_U[i], p_I2_R[i])

    ## outputs

    output(I_all) <- TRUE
    output(I1_all) <- TRUE
    output(I2_all) <- TRUE
    output(n_E2I1) <- TRUE

    ## User-defined inputs

    S0[] <- user()
    V10[] <- user()
    V20[] <- user()
    E10[] <- user()
    E20[] <- user()
    I1_Q0[] <- user()
    I1_U0[] <- user()
    I2_Q0[] <- user()
    I2_U0[] <- user()
    R0[] <- user()

    beta[] <- user()
    E1toE2[]  <- user()
    E2toI1[]  <- user()
    I1toI2[] <- user()
    I2toR[] <- user()
    prop_I1_Q[] <- user()
    prop_I2_Q[] <- user()
    V1_eff[] <- user()
    V2_eff[] <- user()

    N_pop                <- user()
    m_weekday_day[, ]    <- user()
    m_weekday_night[, ]  <- user()
    m_weekend_day[, ]    <- user()
    m_weekend_night[, ]  <- user()
    start_day            <- user()

    ## dimensions
    dim(S) <- N_pop
    dim(V1) <- N_pop
    dim(V2) <- N_pop
    dim(V1_ini) <- N_pop
    dim(V2_ini) <- N_pop
    dim(S0_eff) <- N_pop
    dim(E1) <- N_pop
    dim(E2) <- N_pop
    dim(I1_Q) <- N_pop
    dim(I1_U) <- N_pop
    dim(I2_Q) <- N_pop
    dim(I2_U) <- N_pop
    dim(R) <- N_pop
    dim(I1_all) <- N_pop
    dim(I2_all) <- N_pop
    dim(I_all) <- N_pop
    dim(IU_all) <- N_pop
    dim(mob_pop) <- N_pop

    dim(S0) <- N_pop
    dim(E10) <- N_pop
    dim(E20) <- N_pop
    dim(I1_Q0) <- N_pop
    dim(I1_U0) <- N_pop
    dim(I2_Q0) <- N_pop
    dim(I2_U0) <- N_pop
    dim(R0) <- N_pop
    dim(V10) <- N_pop
    dim(V20) <- N_pop

    dim(lambda) <- N_pop
    dim(eff_prod) <- c(N_pop, N_pop)
    # dim(S_eff_prod) <- c(N_pop, N_pop)
    dim(I_eff_prod) <- c(N_pop, N_pop)
    dim(P_eff) <- N_pop
    dim(I_eff) <- N_pop

    dim(n_SE1) <- N_pop
    dim(n_E1E2) <- N_pop
    dim(n_E2I1) <- N_pop
    dim(n_E2I1_Q) <- N_pop
    dim(n_E2I1_U) <- N_pop
    dim(n_I1U_I2) <- N_pop
    dim(n_I1U_I2Q) <- N_pop
    dim(n_I1U_I2U) <- N_pop
    dim(n_I1Q_I2Q) <- N_pop
    dim(n_I2Q_R) <- N_pop
    dim(n_I2U_R) <- N_pop
    
    dim(beta) <- N_pop
    dim(E1toE2) <- N_pop
    dim(E2toI1) <- N_pop
    dim(I1toI2) <- N_pop
    dim(I2toR) <- N_pop
    dim(prop_I1_Q) <- N_pop
    dim(prop_I2_Q) <- N_pop
    dim(V1_eff) <- N_pop
    dim(V2_eff) <- N_pop
    
    dim(p_SE1) <- N_pop
    dim(p_E1E2) <- N_pop
    dim(p_E2_I1) <- N_pop
    dim(p_I1U_I2) <- N_pop
    dim(p_I2_R) <- N_pop

    dim(m) <- c(N_pop, N_pop)
    dim(m_weekday_day) <- c(N_pop, N_pop)
    dim(m_weekday_night) <- c(N_pop, N_pop)
    dim(m_weekend_day) <- c(N_pop, N_pop)
    dim(m_weekend_night) <- c(N_pop, N_pop)
  

  })

  ## If disease parameters are constant across simulations, 
  ## replicate them into matrices with one row

  if(length(beta) == 1) beta <- rep(beta, N_pop)
  if(length(de1) == 1) de1 <- rep(de1, N_pop)
  if(length(de2) == 1) de2 <- rep(de2, N_pop)
  if(length(di1) == 1) di1 <- rep(di1, N_pop)
  if(length(di2) == 1) di2 <- rep(di2, N_pop)
  if(length(prop_I1_Q) == 1) prop_I1_Q <- rep(prop_I1_Q, N_pop)
  if(length(prop_I2_Q) == 1) prop_I2_Q <- rep(prop_I2_Q, N_pop)
  if(length(V1_eff) == 1) V1_eff <- rep(V1_eff, N_pop)
  if(length(V2_eff) == 1) V2_eff <- rep(V2_eff, N_pop)

  ## Run the simulation
  model <- metaODIN_measles$new(N_pop = N_pop,
                               beta = beta,
                               S0 = S0, V10 = V10, V20 = V20, E10 = E10, E20 = E20, I1_Q0 = I1_Q0, I1_U0 = I1_U0, 
                               I2_Q0 = I2_Q0, I2_U0 = I2_U0, R0 = R0,
                               m_weekday_day = m_weekday_day,
                               m_weekday_night = m_weekday_night,
                               m_weekend_day = m_weekend_day,
                               m_weekend_night = m_weekend_night,
                               start_day = start_day,
                               dt = delta_t,
                               E1toE2 = 1/de1, 
                               E2toI1 = 1/de2, 
                               I1toI2 = 1/di1, 
                               I2toR = 1/di2,
                               prop_I1_Q = prop_I1_Q, prop_I2_Q = prop_I2_Q,
                               V1_eff = V1_eff, V2_eff = V2_eff
                               )

  out <- model$run(step = 0:nsteps)
  out_df <- data.frame(out)

  long_out <- out_df %>%
  tidyr::pivot_longer(
    cols = -c(step, time),
    names_to = c("disease_state", "population_id"),
    names_pattern = "^(.*)\\.(\\d+)\\.$",
    values_to = "value"
  ) %>%
  dplyr::mutate(population_id = as.integer(population_id))
  
  long_out <- data.table::as.data.table(long_out)


  return(long_out)
}

