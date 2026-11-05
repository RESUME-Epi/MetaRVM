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
#' @param V10 Numeric vector of length `N_pop`. Initial dose-1 vaccinated counts.
#' @param V20 Numeric vector of length `N_pop`. Initial dose-2 vaccinated counts.
#' @param E10 Numeric vector of length `N_pop`. Initial first exposed stage counts.
#' @param E20 Numeric vector of length `N_pop`. Initial second exposed stage counts.
#' @param I1_Q0 Numeric vector of length `N_pop`. Initial `I1_Q` counts.
#' @param I1_U0 Numeric vector of length `N_pop`. Initial `I1_U` counts.
#' @param I2_Q0 Numeric vector of length `N_pop`. Initial `I2_Q` counts.
#' @param I2_U0 Numeric vector of length `N_pop`. Initial `I2_U` counts.
#' @param R0 Numeric vector of length `N_pop`. Initial recovered counts.
#' @param V1E10 Numeric vector of length `N_pop`. Initial `V1E1` counts (dose-1 breakthrough, first latent stage).
#' @param V1E20 Numeric vector of length `N_pop`. Initial `V1E2` counts (dose-1 breakthrough, second latent stage).
#' @param V1I1_Q0 Numeric vector of length `N_pop`. Initial `V1I1_Q` counts.
#' @param V1I1_U0 Numeric vector of length `N_pop`. Initial `V1I1_U` counts.
#' @param V1I2_Q0 Numeric vector of length `N_pop`. Initial `V1I2_Q` counts.
#' @param V1I2_U0 Numeric vector of length `N_pop`. Initial `V1I2_U` counts.
#' @param V1R0 Numeric vector of length `N_pop`. Initial `V1R` counts.
#' @param V2E10 Numeric vector of length `N_pop`. Initial `V2E1` counts (dose-2 breakthrough, first latent stage).
#' @param V2E20 Numeric vector of length `N_pop`. Initial `V2E2` counts (dose-2 breakthrough, second latent stage).
#' @param V2I1_Q0 Numeric vector of length `N_pop`. Initial `V2I1_Q` counts.
#' @param V2I1_U0 Numeric vector of length `N_pop`. Initial `V2I1_U` counts.
#' @param V2I2_Q0 Numeric vector of length `N_pop`. Initial `V2I2_Q` counts.
#' @param V2I2_U0 Numeric vector of length `N_pop`. Initial `V2I2_U` counts.
#' @param V2R0 Numeric vector of length `N_pop`. Initial `V2R` counts.
#' @param V1_eff Numeric vector or scalar in `[0, 1]`. Vaccine efficacy for dose-1 recipients.
#' @param V2_eff Numeric vector or scalar in `[0, 1]`. Vaccine efficacy for dose-2 recipients.
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
#' @param do_chk Logical. Whether to write checkpoint files at the specified time steps.
#' @param chk_time_steps Integer vector or `NULL`. Simulation steps at which to write checkpoints.
#' @param chk_file_names Character vector/list or `NULL`. File paths for each checkpoint time step.
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
#'   V1_eff = 0.93, V2_eff = 0.97,
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
                              V1E10   = rep(0, N_pop),
                              V1E20   = rep(0, N_pop),
                              V1I1_Q0 = rep(0, N_pop),
                              V1I1_U0 = rep(0, N_pop),
                              V1I2_Q0 = rep(0, N_pop),
                              V1I2_U0 = rep(0, N_pop),
                              V1R0    = rep(0, N_pop),
                              V2E10   = rep(0, N_pop),
                              V2E20   = rep(0, N_pop),
                              V2I1_Q0 = rep(0, N_pop),
                              V2I1_U0 = rep(0, N_pop),
                              V2I2_Q0 = rep(0, N_pop),
                              V2I2_U0 = rep(0, N_pop),
                              V2R0    = rep(0, N_pop),
                              m_weekday_day, m_weekday_night, m_weekend_day, m_weekend_night,
                              start_day = 0,
                              delta_t,
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

    # -------------------------------------------------------------------------
    # Time
    # -------------------------------------------------------------------------
    dt <- user(1)
    initial(time) <- 0
    update(time)  <- (step + 1) * dt

    # -------------------------------------------------------------------------
    # Vaccine efficacy: reduce initial V1/V2 counts to effective susceptible pool
    # V1_ini / V2_ini = protected fraction; S0_eff removes them from S
    # -------------------------------------------------------------------------
    # V1_ini[] <- V10[i] * V1_eff[i]
    # V2_ini[] <- V20[i] * V2_eff[i]
    # S0_eff[] <- S0[i] - V1_ini[i] - V2_ini[i]

    # -------------------------------------------------------------------------
    # Initial conditions
    # -------------------------------------------------------------------------

    ## unvaccinated compartments
    initial(S[])    <- S0[i]
    initial(V1[])   <- V10[i]
    initial(V2[])   <- V20[i]
    initial(E1[])   <- E10[i]
    initial(E2[])   <- E20[i]
    initial(I1_Q[]) <- I1_Q0[i]
    initial(I1_U[]) <- I1_U0[i]
    initial(I2_Q[]) <- I2_Q0[i]
    initial(I2_U[]) <- I2_U0[i]
    initial(R[])    <- R0[i]

    ## V1-vaccinated breakthrough compartments (default 0)
    initial(V1E1[])   <- V1E10[i]
    initial(V1E2[])   <- V1E20[i]
    initial(V1I1_Q[]) <- V1I1_Q0[i]
    initial(V1I1_U[]) <- V1I1_U0[i]
    initial(V1I2_Q[]) <- V1I2_Q0[i]
    initial(V1I2_U[]) <- V1I2_U0[i]
    initial(V1R[])    <- V1R0[i]

    ## V2-vaccinated breakthrough compartments (default 0)
    initial(V2E1[])   <- V2E10[i]
    initial(V2E2[])   <- V2E20[i]
    initial(V2I1_Q[]) <- V2I1_Q0[i]
    initial(V2I1_U[]) <- V2I1_U0[i]
    initial(V2I2_Q[]) <- V2I2_Q0[i]
    initial(V2I2_U[]) <- V2I2_U0[i]
    initial(V2R[])    <- V2R0[i]

    # -------------------------------------------------------------------------
    # Update equations: S -> E1 -> E2 -> I1_Q/I1_U -> I2_Q/I2_U -> R
    # -------------------------------------------------------------------------

    ## unvaccinated
    update(S[])    <- S[i]    - n_SE1[i]
    update(V1[])   <- V1[i]   - n_V1E1[i]
    update(V2[])   <- V2[i]   - n_V2E1[i]
    update(E1[])   <- E1[i]   + n_SE1[i]    - n_E1E2[i]
    update(E2[])   <- E2[i]   + n_E1E2[i]   - n_E2I1[i]
    update(I1_Q[]) <- I1_Q[i] + n_E2I1_Q[i] - n_I1Q_I2Q[i]
    update(I1_U[]) <- I1_U[i] + n_E2I1_U[i] - n_I1U_I2U[i] - n_I1U_I2Q[i]
    update(I2_Q[]) <- I2_Q[i] + n_I1Q_I2Q[i] + n_I1U_I2Q[i] - n_I2Q_R[i]
    update(I2_U[]) <- I2_U[i] + n_I1U_I2U[i] - n_I2U_R[i]
    update(R[])    <- R[i]    + n_I2Q_R[i]  + n_I2U_R[i]

    ## V1-vaccinated breakthrough: V1 -> V1E1 -> V1E2 -> V1I1 -> V1I2 -> V1R
    update(V1E1[])   <- V1E1[i]   + n_V1E1[i]    - n_V1E1E2[i]
    update(V1E2[])   <- V1E2[i]   + n_V1E1E2[i]  - n_V1E2I1[i]
    update(V1I1_Q[]) <- V1I1_Q[i] + n_V1E2I1_Q[i] - n_V1I1Q_I2Q[i]
    update(V1I1_U[]) <- V1I1_U[i] + n_V1E2I1_U[i] - n_V1I1U_I2U[i] - n_V1I1U_I2Q[i]
    update(V1I2_Q[]) <- V1I2_Q[i] + n_V1I1Q_I2Q[i] + n_V1I1U_I2Q[i] - n_V1I2Q_R[i]
    update(V1I2_U[]) <- V1I2_U[i] + n_V1I1U_I2U[i] - n_V1I2U_R[i]
    update(V1R[])    <- V1R[i]    + n_V1I2Q_R[i] + n_V1I2U_R[i]

    ## V2-vaccinated breakthrough: V2 -> V2E1 -> V2E2 -> V2I1 -> V2I2 -> V2R
    update(V2E1[])   <- V2E1[i]   + n_V2E1[i]    - n_V2E1E2[i]
    update(V2E2[])   <- V2E2[i]   + n_V2E1E2[i]  - n_V2E2I1[i]
    update(V2I1_Q[]) <- V2I1_Q[i] + n_V2E2I1_Q[i] - n_V2I1Q_I2Q[i]
    update(V2I1_U[]) <- V2I1_U[i] + n_V2E2I1_U[i] - n_V2I1U_I2U[i] - n_V2I1U_I2Q[i]
    update(V2I2_Q[]) <- V2I2_Q[i] + n_V2I1Q_I2Q[i] + n_V2I1U_I2Q[i] - n_V2I2Q_R[i]
    update(V2I2_U[]) <- V2I2_U[i] + n_V2I1U_I2U[i] - n_V2I2U_R[i]
    update(V2R[])    <- V2R[i]    + n_V2I2Q_R[i] + n_V2I2U_R[i]

    # -------------------------------------------------------------------------
    # Derived / aggregated compartments
    # -------------------------------------------------------------------------

    ## unvaccinated aggregates
    I1_all[]      <- I1_Q[i]  + I1_U[i]
    I2_all[]      <- I2_Q[i]  + I2_U[i]
    I_nvac_all[]  <- I1_all[i] + I2_all[i]   # total infectious, unvaccinated only
    IU_nvac_all[] <- I1_U[i]  + I2_U[i]      # unquarantined infectious, unvaccinated only

    ## V1 breakthrough aggregates
    V1I1_all[]  <- V1I1_Q[i] + V1I1_U[i]
    V1I2_all[]  <- V1I2_Q[i] + V1I2_U[i]
    V1I_all[]   <- V1I1_all[i] + V1I2_all[i]
    V1IU_all[]  <- V1I1_U[i]  + V1I2_U[i]

    ## V2 breakthrough aggregates
    V2I1_all[]  <- V2I1_Q[i] + V2I1_U[i]
    V2I2_all[]  <- V2I2_Q[i] + V2I2_U[i]
    V2I_all[]   <- V2I1_all[i] + V2I2_all[i]
    V2IU_all[]  <- V2I1_U[i]  + V2I2_U[i]

    ## combined totals (unvaccinated + V1 + V2)
    I_all[]   <- I_nvac_all[i]  + V1I_all[i]  + V2I_all[i]
    IU_all[]  <- IU_nvac_all[i] + V1IU_all[i] + V2IU_all[i]

    ## mobile population: excludes quarantined individuals
    mob_pop[] <- S[i]   + V1[i]  + V2[i]  +
                 E1[i]  + E2[i]  +
                 I1_U[i] + I2_U[i] + R[i] +
                 V1E1[i] + V1E2[i] + V1I1_U[i] + V1I2_U[i] + V1R[i] +
                 V2E1[i] + V2E2[i] + V2I1_U[i] + V2I2_U[i] + V2R[i]

    # -------------------------------------------------------------------------
    # Force of infection and time-varying mixing
    # -------------------------------------------------------------------------

    ## effective population and infectious counts via mixing matrix
    eff_prod[, ] <- m[i, j] * mob_pop[i]
    P_eff[]      <- sum(eff_prod[, i])   # colSums: effective total contacts

    I_eff_prod[, ] <- m[i, j] * IU_all[i]
    I_eff[]        <- sum(I_eff_prod[, i])  # colSums: effective infectious contacts

    ## select mixing matrix by day type (weekday/weekend) and time of day (day/night)
    saturday_id <- 5 - start_day
    sunday_id   <- 6 - start_day
    m[, ] <- if ((time %% 7 == saturday_id) || (time %% 7 == sunday_id)) (
      if (step %% 2 == 0) m_weekend_day[i, j] else m_weekend_night[i, j]
    ) else (
      if (step %% 2 == 0) m_weekday_day[i, j] else m_weekday_night[i, j]
    )

    lambda[] <- beta[i] * I_eff[i] / P_eff[i]

    # -------------------------------------------------------------------------
    # Transition probabilities
    # -------------------------------------------------------------------------

    ## infection probabilities (lambda-driven)
    p_SE1[]  <- 1 - exp(-lambda[i] * dt)               # S -> E1
    p_V1E1[] <- p_SE1[i] * (1 - V1_eff[i])            # V1 -> V1E1 (breakthrough)
    p_V2E1[] <- p_SE1[i] * (1 - V2_eff[i])            # V2 -> V2E1 (breakthrough)

    ## disease progression probabilities (shared across all groups)
    p_E1E2[]   <- 1 - exp(-E1toE2[i] * dt)            # E1 -> E2
    p_E2_I1[]  <- 1 - exp(-E2toI1[i] * dt)            # E2 -> I1
    p_I1U_I2[] <- 1 - exp(-I1toI2[i] * dt)            # I1_U -> I2
    p_I2_R[]   <- 1 - exp(-I2toR[i] * dt)             # I2 -> R

    # -------------------------------------------------------------------------
    # Binomial draws: stochastic transition counts
    # -------------------------------------------------------------------------

    ## unvaccinated
    n_SE1[]     <- rbinom(S[i],        p_SE1[i])
    n_E1E2[]    <- rbinom(E1[i],       p_E1E2[i])
    n_E2I1[]    <- rbinom(E2[i],       p_E2_I1[i])
    n_E2I1_Q[]  <- rbinom(n_E2I1[i],  prop_I1_Q[i])   # quarantined fraction of E2->I1
    n_E2I1_U[]  <- n_E2I1[i] - n_E2I1_Q[i]
    n_I1U_I2[]  <- rbinom(I1_U[i],    p_I1U_I2[i])
    n_I1U_I2Q[] <- rbinom(n_I1U_I2[i], prop_I2_Q[i])  # quarantined fraction of I1_U->I2
    n_I1U_I2U[] <- n_I1U_I2[i] - n_I1U_I2Q[i]
    n_I1Q_I2Q[] <- rbinom(I1_Q[i],    p_I1U_I2[i])
    n_I2Q_R[]   <- rbinom(I2_Q[i],    p_I2_R[i])
    n_I2U_R[]   <- rbinom(I2_U[i],    p_I2_R[i])

    ## V1-vaccinated breakthrough
    n_V1E1[]      <- rbinom(V1[i],       p_V1E1[i])
    n_V1E1E2[]    <- rbinom(V1E1[i],     p_E1E2[i])
    n_V1E2I1[]    <- rbinom(V1E2[i],     p_E2_I1[i])
    n_V1E2I1_Q[]  <- rbinom(n_V1E2I1[i], prop_I1_Q[i])
    n_V1E2I1_U[]  <- n_V1E2I1[i] - n_V1E2I1_Q[i]
    n_V1I1U_I2[]  <- rbinom(V1I1_U[i],  p_I1U_I2[i])
    n_V1I1U_I2Q[] <- rbinom(n_V1I1U_I2[i], prop_I2_Q[i])
    n_V1I1U_I2U[] <- n_V1I1U_I2[i] - n_V1I1U_I2Q[i]
    n_V1I1Q_I2Q[] <- rbinom(V1I1_Q[i],  p_I1U_I2[i])
    n_V1I2Q_R[]   <- rbinom(V1I2_Q[i],  p_I2_R[i])
    n_V1I2U_R[]   <- rbinom(V1I2_U[i],  p_I2_R[i])

    ## V2-vaccinated breakthrough
    n_V2E1[]      <- rbinom(V2[i],       p_V2E1[i])
    n_V2E1E2[]    <- rbinom(V2E1[i],     p_E1E2[i])
    n_V2E2I1[]    <- rbinom(V2E2[i],     p_E2_I1[i])
    n_V2E2I1_Q[]  <- rbinom(n_V2E2I1[i], prop_I1_Q[i])
    n_V2E2I1_U[]  <- n_V2E2I1[i] - n_V2E2I1_Q[i]
    n_V2I1U_I2[]  <- rbinom(V2I1_U[i],  p_I1U_I2[i])
    n_V2I1U_I2Q[] <- rbinom(n_V2I1U_I2[i], prop_I2_Q[i])
    n_V2I1U_I2U[] <- n_V2I1U_I2[i] - n_V2I1U_I2Q[i]
    n_V2I1Q_I2Q[] <- rbinom(V2I1_Q[i],  p_I1U_I2[i])
    n_V2I2Q_R[]   <- rbinom(V2I2_Q[i],  p_I2_R[i])
    n_V2I2U_R[]   <- rbinom(V2I2_U[i],  p_I2_R[i])

    # -------------------------------------------------------------------------
    # Outputs
    # -------------------------------------------------------------------------

    ## summary outputs (unvaccinated + combined totals)
    output(I1_all)     <- TRUE
    output(I2_all)     <- TRUE
    output(I_nvac_all) <- TRUE
    output(IU_nvac_all) <- TRUE
    output(I_all)      <- TRUE
    output(n_SE1)     <- TRUE
    # output(S0_eff)     <- TRUE

    ## V1 breakthrough outputs
    output(V1I1_all) <- TRUE
    output(V1I2_all) <- TRUE
    output(V1I_all)  <- TRUE
    output(V1IU_all) <- TRUE
    output(n_V1E1) <- TRUE

    ## V2 breakthrough outputs
    output(V2I1_all) <- TRUE
    output(V2I2_all) <- TRUE
    output(V2I_all)  <- TRUE
    output(V2IU_all) <- TRUE
    output(n_V2E1) <- TRUE

    ## probabilities
    output(p_SE1) <- TRUE
    output(p_V1E1) <- TRUE
    output(p_V2E1) <- TRUE


    # -------------------------------------------------------------------------
    # User-defined inputs
    # -------------------------------------------------------------------------

    ## unvaccinated initial conditions
    S0[]    <- user()
    V10[]   <- user()
    V20[]   <- user()
    E10[]   <- user()
    E20[]   <- user()
    I1_Q0[] <- user()
    I1_U0[] <- user()
    I2_Q0[] <- user()
    I2_U0[] <- user()
    R0[]    <- user()

    ## V1 breakthrough initial conditions (default 0)
    V1E10[]   <- user(0)
    V1E20[]   <- user(0)
    V1I1_Q0[] <- user(0)
    V1I1_U0[] <- user(0)
    V1I2_Q0[] <- user(0)
    V1I2_U0[] <- user(0)
    V1R0[]    <- user(0)

    ## V2 breakthrough initial conditions (default 0)
    V2E10[]   <- user(0)
    V2E20[]   <- user(0)
    V2I1_Q0[] <- user(0)
    V2I1_U0[] <- user(0)
    V2I2_Q0[] <- user(0)
    V2I2_U0[] <- user(0)
    V2R0[]    <- user(0)

    ## disease parameters
    beta[]      <- user()
    E1toE2[]    <- user()
    E2toI1[]    <- user()
    I1toI2[]    <- user()
    I2toR[]     <- user()
    prop_I1_Q[] <- user()
    prop_I2_Q[] <- user()
    V1_eff[]    <- user()
    V2_eff[]    <- user()

    ## mixing matrices and scalar settings
    N_pop               <- user()
    m_weekday_day[, ]   <- user()
    m_weekday_night[, ] <- user()
    m_weekend_day[, ]   <- user()
    m_weekend_night[, ] <- user()
    start_day           <- user()

    # -------------------------------------------------------------------------
    # Dimensions
    # -------------------------------------------------------------------------

    ## unvaccinated state compartments
    dim(S)          <- N_pop
    dim(V1)         <- N_pop
    dim(V2)         <- N_pop
    # dim(V1_ini)     <- N_pop
    # dim(V2_ini)     <- N_pop
    # dim(S0_eff)     <- N_pop
    dim(E1)         <- N_pop
    dim(E2)         <- N_pop
    dim(I1_Q)       <- N_pop
    dim(I1_U)       <- N_pop
    dim(I2_Q)       <- N_pop
    dim(I2_U)       <- N_pop
    dim(R)          <- N_pop
    dim(I1_all)     <- N_pop
    dim(I2_all)     <- N_pop
    dim(I_nvac_all) <- N_pop
    dim(IU_nvac_all) <- N_pop
    dim(I_all)      <- N_pop
    dim(IU_all)     <- N_pop
    dim(mob_pop)    <- N_pop

    ## V1 breakthrough state compartments
    dim(V1E1)     <- N_pop
    dim(V1E2)     <- N_pop
    dim(V1I1_Q)   <- N_pop
    dim(V1I1_U)   <- N_pop
    dim(V1I2_Q)   <- N_pop
    dim(V1I2_U)   <- N_pop
    dim(V1R)      <- N_pop
    dim(V1I1_all) <- N_pop
    dim(V1I2_all) <- N_pop
    dim(V1I_all)  <- N_pop
    dim(V1IU_all) <- N_pop

    ## V2 breakthrough state compartments
    dim(V2E1)     <- N_pop
    dim(V2E2)     <- N_pop
    dim(V2I1_Q)   <- N_pop
    dim(V2I1_U)   <- N_pop
    dim(V2I2_Q)   <- N_pop
    dim(V2I2_U)   <- N_pop
    dim(V2R)      <- N_pop
    dim(V2I1_all) <- N_pop
    dim(V2I2_all) <- N_pop
    dim(V2I_all)  <- N_pop
    dim(V2IU_all) <- N_pop

    ## initial condition arrays
    dim(S0)    <- N_pop
    dim(E10)   <- N_pop
    dim(E20)   <- N_pop
    dim(I1_Q0) <- N_pop
    dim(I1_U0) <- N_pop
    dim(I2_Q0) <- N_pop
    dim(I2_U0) <- N_pop
    dim(R0)    <- N_pop
    dim(V10)   <- N_pop
    dim(V20)   <- N_pop

    dim(V1E10)   <- N_pop
    dim(V1E20)   <- N_pop
    dim(V1I1_Q0) <- N_pop
    dim(V1I1_U0) <- N_pop
    dim(V1I2_Q0) <- N_pop
    dim(V1I2_U0) <- N_pop
    dim(V1R0)    <- N_pop

    dim(V2E10)   <- N_pop
    dim(V2E20)   <- N_pop
    dim(V2I1_Q0) <- N_pop
    dim(V2I1_U0) <- N_pop
    dim(V2I2_Q0) <- N_pop
    dim(V2I2_U0) <- N_pop
    dim(V2R0)    <- N_pop

    ## force of infection intermediates
    dim(lambda)    <- N_pop
    dim(eff_prod)  <- c(N_pop, N_pop)
    dim(I_eff_prod) <- c(N_pop, N_pop)
    dim(P_eff)     <- N_pop
    dim(I_eff)     <- N_pop

    ## unvaccinated flow variables
    dim(n_SE1)     <- N_pop
    dim(n_E1E2)    <- N_pop
    dim(n_E2I1)    <- N_pop
    dim(n_E2I1_Q)  <- N_pop
    dim(n_E2I1_U)  <- N_pop
    dim(n_I1U_I2)  <- N_pop
    dim(n_I1U_I2Q) <- N_pop
    dim(n_I1U_I2U) <- N_pop
    dim(n_I1Q_I2Q) <- N_pop
    dim(n_I2Q_R)   <- N_pop
    dim(n_I2U_R)   <- N_pop

    ## V1 breakthrough flow variables
    dim(n_V1E1)      <- N_pop
    dim(n_V1E1E2)    <- N_pop
    dim(n_V1E2I1)    <- N_pop
    dim(n_V1E2I1_Q)  <- N_pop
    dim(n_V1E2I1_U)  <- N_pop
    dim(n_V1I1U_I2)  <- N_pop
    dim(n_V1I1U_I2Q) <- N_pop
    dim(n_V1I1U_I2U) <- N_pop
    dim(n_V1I1Q_I2Q) <- N_pop
    dim(n_V1I2Q_R)   <- N_pop
    dim(n_V1I2U_R)   <- N_pop

    ## V2 breakthrough flow variables
    dim(n_V2E1)      <- N_pop
    dim(n_V2E1E2)    <- N_pop
    dim(n_V2E2I1)    <- N_pop
    dim(n_V2E2I1_Q)  <- N_pop
    dim(n_V2E2I1_U)  <- N_pop
    dim(n_V2I1U_I2)  <- N_pop
    dim(n_V2I1U_I2Q) <- N_pop
    dim(n_V2I1U_I2U) <- N_pop
    dim(n_V2I1Q_I2Q) <- N_pop
    dim(n_V2I2Q_R)   <- N_pop
    dim(n_V2I2U_R)   <- N_pop

    ## disease parameters
    dim(beta)      <- N_pop
    dim(E1toE2)    <- N_pop
    dim(E2toI1)    <- N_pop
    dim(I1toI2)    <- N_pop
    dim(I2toR)     <- N_pop
    dim(prop_I1_Q) <- N_pop
    dim(prop_I2_Q) <- N_pop
    dim(V1_eff)    <- N_pop
    dim(V2_eff)    <- N_pop

    ## transition probabilities
    dim(p_SE1)    <- N_pop
    dim(p_V1E1)   <- N_pop
    dim(p_V2E1)   <- N_pop
    dim(p_E1E2)   <- N_pop
    dim(p_E2_I1)  <- N_pop
    dim(p_I1U_I2) <- N_pop
    dim(p_I2_R)   <- N_pop

    ## mixing matrices
    dim(m)              <- c(N_pop, N_pop)
    dim(m_weekday_day)  <- c(N_pop, N_pop)
    dim(m_weekday_night) <- c(N_pop, N_pop)
    dim(m_weekend_day)  <- c(N_pop, N_pop)
    dim(m_weekend_night) <- c(N_pop, N_pop)

  })

  # Scalar parameters (shared across all subpopulations) are expanded to length
  # N_pop so the ODIN engine can always index into a per-population array.
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
                               S0 = S0, V10 = V10, V20 = V20,
                               E10 = E10, E20 = E20,
                               I1_Q0 = I1_Q0, I1_U0 = I1_U0,
                               I2_Q0 = I2_Q0, I2_U0 = I2_U0,
                               R0 = R0,
                               V1E10 = V1E10, V1E20 = V1E20,
                               V1I1_Q0 = V1I1_Q0, V1I1_U0 = V1I1_U0,
                               V1I2_Q0 = V1I2_Q0, V1I2_U0 = V1I2_U0,
                               V1R0 = V1R0,
                               V2E10 = V2E10, V2E20 = V2E20,
                               V2I1_Q0 = V2I1_Q0, V2I1_U0 = V2I1_U0,
                               V2I2_Q0 = V2I2_Q0, V2I2_U0 = V2I2_U0,
                               V2R0 = V2R0,
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

  # Same pivot as in meta_sim: ODIN array outputs are named "state.pop_id.",
  # split here into separate disease_state and population_id columns.
  long_out <- out_df %>%
  tidyr::pivot_longer(
    cols = -c(step, time),
    names_to = c("disease_state", "population_id"),
    names_pattern = "^(.*)\\.(\\d+)\\.$",
    values_to = "value"
  ) %>%
  dplyr::mutate(population_id = as.integer(population_id))
  
  long_out <- data.table::as.data.table(long_out)

  if (do_chk && !is.null(chk_time_steps)) {
    for (ci in seq_along(chk_time_steps)) {
      time_step <- chk_time_steps[ci]
      if (time_step %in% out_df$step) {
        chk_file_name_instance <- chk_file_names[[ci]]
        chk <- MetaRVMCheck$new(list(
          disease         = "measles",
          chk_time_step   = time_step,
          N_pop           = N_pop,
          delta_t         = delta_t,
          m_weekday_day   = m_weekday_day,
          m_weekday_night = m_weekday_night,
          m_weekend_day   = m_weekend_day,
          m_weekend_night = m_weekend_night,
          beta      = beta,
          de1       = de1,
          de2       = de2,
          di1       = di1,
          di2       = di2,
          prop_I1_Q = prop_I1_Q,
          prop_I2_Q = prop_I2_Q,
          V1_eff    = V1_eff,
          V2_eff    = V2_eff,
          S      = long_out[(long_out$step == time_step) & (long_out$disease_state == "S"),    c("value")],
          V1     = long_out[(long_out$step == time_step) & (long_out$disease_state == "V1"),   c("value")],
          V2     = long_out[(long_out$step == time_step) & (long_out$disease_state == "V2"),   c("value")],
          E1     = long_out[(long_out$step == time_step) & (long_out$disease_state == "E1"),   c("value")],
          E2     = long_out[(long_out$step == time_step) & (long_out$disease_state == "E2"),   c("value")],
          I1_Q   = long_out[(long_out$step == time_step) & (long_out$disease_state == "I1_Q"), c("value")],
          I1_U   = long_out[(long_out$step == time_step) & (long_out$disease_state == "I1_U"), c("value")],
          I2_Q   = long_out[(long_out$step == time_step) & (long_out$disease_state == "I2_Q"), c("value")],
          I2_U   = long_out[(long_out$step == time_step) & (long_out$disease_state == "I2_U"), c("value")],
          R      = long_out[(long_out$step == time_step) & (long_out$disease_state == "R"),    c("value")],
          V1E1   = long_out[(long_out$step == time_step) & (long_out$disease_state == "V1E1"),   c("value")],
          V1E2   = long_out[(long_out$step == time_step) & (long_out$disease_state == "V1E2"),   c("value")],
          V1I1_Q = long_out[(long_out$step == time_step) & (long_out$disease_state == "V1I1_Q"), c("value")],
          V1I1_U = long_out[(long_out$step == time_step) & (long_out$disease_state == "V1I1_U"), c("value")],
          V1I2_Q = long_out[(long_out$step == time_step) & (long_out$disease_state == "V1I2_Q"), c("value")],
          V1I2_U = long_out[(long_out$step == time_step) & (long_out$disease_state == "V1I2_U"), c("value")],
          V1R    = long_out[(long_out$step == time_step) & (long_out$disease_state == "V1R"),    c("value")],
          V2E1   = long_out[(long_out$step == time_step) & (long_out$disease_state == "V2E1"),   c("value")],
          V2E2   = long_out[(long_out$step == time_step) & (long_out$disease_state == "V2E2"),   c("value")],
          V2I1_Q = long_out[(long_out$step == time_step) & (long_out$disease_state == "V2I1_Q"), c("value")],
          V2I1_U = long_out[(long_out$step == time_step) & (long_out$disease_state == "V2I1_U"), c("value")],
          V2I2_Q = long_out[(long_out$step == time_step) & (long_out$disease_state == "V2I2_Q"), c("value")],
          V2I2_U = long_out[(long_out$step == time_step) & (long_out$disease_state == "V2I2_U"), c("value")],
          V2R    = long_out[(long_out$step == time_step) & (long_out$disease_state == "V2R"),    c("value")]
        ))
        if (!is.null(chk_file_name_instance)) {
          saveRDS(chk, file = chk_file_name_instance)
        }
      }
    }
  }

  return(long_out)
}

