
#' Metapopulation Respiratory Virus Model Simulator
#'
#' @description
#' The core simulation engine that implements a stochastic compartmental SEIRD 
#' (Susceptible-Exposed-Infected-Recovered-Dead) model for respiratory virus 
#' epidemics across multiple demographic subpopulations. The function compiles 
#' and executes an ODIN-based differential equation model with time-varying 
#' contact patterns, vaccination dynamics, and complex disease progression pathways.
#'
#' @param N_pop Integer. Number of demographic subpopulations in the model
#' @param ts Numeric vector or scalar. Transmission rate for symptomatic individuals
#'   in susceptible population. If scalar, applied to all subpopulations
#' @param tv Numeric vector or scalar. Transmission rate for symptomatic individuals
#'   in vaccinated population. If scalar, applied to all subpopulations
#' @param S0 Numeric vector of length N_pop. Initial number of susceptible individuals
#'   in each subpopulation
#' @param I0 Numeric vector of length N_pop. Initial number of symptomatic infected
#'   individuals in each subpopulation
#' @param P0 Numeric vector of length N_pop. Total population sizes for each subpopulation
#' @param R0 Numeric vector of length N_pop. Initial number of recovered individuals
#'   in each subpopulation
#' @param H0 Numeric vector of length N_pop. Initial number of hospitalized individuals
#'   in each subpopulation (default: rep(0, N_pop))
#' @param D0 Numeric vector of length N_pop. Initial number of deceased individuals
#'   in each subpopulation (default: rep(0, N_pop))
#' @param Ia0 Numeric vector of length N_pop. Initial number of asymptomatic infected
#'   individuals in each subpopulation (default: rep(0, N_pop))
#' @param Ip0 Numeric vector of length N_pop. Initial number of presymptomatic infected
#'   individuals in each subpopulation (default: rep(0, N_pop))
#' @param E0 Numeric vector of length N_pop. Initial number of exposed individuals
#'   in each subpopulation (default: rep(0, N_pop))
#' @param m_weekday_day Numeric matrix (N_pop × N_pop). Contact mixing matrix for
#'   weekday daytime (6 AM - 6 PM) interactions
#' @param m_weekday_night Numeric matrix (N_pop × N_pop). Contact mixing matrix for
#'   weekday nighttime (6 PM - 6 AM) interactions
#' @param m_weekend_day Numeric matrix (N_pop × N_pop). Contact mixing matrix for
#'   weekend daytime (6 AM - 6 PM) interactions
#' @param m_weekend_night Numeric matrix (N_pop × N_pop). Contact mixing matrix for
#'   weekend nighttime (6 PM - 6 AM) interactions
#' @param delta_t Positive numeric. Discrete time increment in days (typically 0.5)
#' @param vac_mat Numeric matrix. Vaccination schedule with dimensions (nsteps × (1 + N_pop)).
#'   First column contains time indices, remaining columns contain vaccination counts
#'   for each subpopulation at each time step
#' @param dv Numeric vector or scalar. Mean duration (days) in vaccinated state before
#'   immunity waning. If scalar, applied to all subpopulations
#' @param de Numeric vector or scalar. Mean duration (days) in exposed state.
#'   If scalar, applied to all subpopulations
#' @param pea Numeric vector or scalar. Proportion of exposed individuals becoming
#'   asymptomatic infectious (vs. presymptomatic). Range: [0,1].
#'   If scalar, applied to all subpopulations
#' @param dp Numeric vector or scalar. Mean duration (days) in presymptomatic
#'   infectious state. If scalar, applied to all subpopulations
#' @param da Numeric vector or scalar. Mean duration (days) in asymptomatic
#'   infectious state. If scalar, applied to all subpopulations
#' @param ds Numeric vector or scalar. Mean duration (days) in symptomatic
#'   infectious state. If scalar, applied to all subpopulations
#' @param psr Numeric vector or scalar. Proportion of symptomatic individuals
#'   recovering directly (vs. hospitalization). Range: [0,1].
#'   If scalar, applied to all subpopulations
#' @param dh Numeric vector or scalar. Mean duration (days) in hospitalized state.
#'   If scalar, applied to all subpopulations
#' @param phr Numeric vector or scalar. Proportion of hospitalized individuals
#'   recovering (vs. death). Range: [0,1]. If scalar, applied to all subpopulations
#' @param dr Numeric vector or scalar. Mean duration (days) of immunity in
#'   recovered state. If scalar, applied to all subpopulations
#' @param ve Numeric vector or scalar. Vaccine effectiveness (proportion).
#'   Range: [0,1]. If scalar, applied to all subpopulations
#' @param nsteps Integer. Total number of discrete time evolution steps in simulation
#' @param is.stoch Logical. Whether to run stochastic simulation (TRUE) or
#'   deterministic simulation (FALSE). Default: FALSE
#' @param seed Integer or NULL. Random seed for reproducibility. Only used when
#'   is.stoch = TRUE. Default: NULL
#' @param do_chk Logical. Whether to save model checkpoint at simulation end.
#'   Default: FALSE
#' @param chk_file_name Character string or NULL. Checkpoint file name if do_chk = TRUE.
#'   Default: NULL
#'
#' @details
#' The model implements a complex epidemiological framework with the following features:
#'
#' \strong{Compartmental Structure:}
#' \itemize{
#'   \item \strong{S}: Susceptible individuals
#'   \item \strong{E}: Exposed (incubating) individuals
#'   \item \strong{I_presymp}: Presymptomatic infectious individuals
#'   \item \strong{I_asymp}: Asymptomatic infectious individuals
#'   \item \strong{I_symp}: Symptomatic infectious individuals
#'   \item \strong{H}: Hospitalized individuals
#'   \item \strong{R}: Recovered individuals
#'   \item \strong{D}: Deceased individuals
#'   \item \strong{V}: Vaccinated individuals
#'   \item \strong{P}: Total living population (excludes deaths)
#' }
#'
#' \strong{Disease Progression Pathways:}
#' \enumerate{
#'   \item \strong{S → E}: Exposure through contact with infectious individuals
#'   \item \strong{E → I_asymp/I_presymp}: Progression to infectious states (proportion pea)
#'   \item \strong{I_presymp → I_symp}: Development of symptoms
#'   \item \strong{I_asymp → R}: Direct recovery from asymptomatic state
#'   \item \strong{I_symp → R/H}: Recovery or hospitalization (proportion psr)
#'   \item \strong{H → R/D}: Hospital discharge or death (proportion phr)
#'   \item \strong{R → S}: Loss of immunity
#'   \item \strong{S → V}: Vaccination
#'   \item \strong{V → S/E}: Vaccine waning or breakthrough infection
#' }
#'
#' \strong{Mixing Patterns:}
#' Contact patterns vary by:
#' \itemize{
#'   \item Day of week: Weekday vs. weekend patterns
#'   \item Time of day: Day (6 AM - 6 PM) vs. night (6 PM - 6 AM) patterns
#'   \item Each pattern specified by N_pop × N_pop contact matrix
#' }
#'
#' \strong{Force of Infection:}
#' Transmission occurs through contact between susceptible/vaccinated individuals and
#' all infectious compartments (I_presymp + I_asymp + I_symp), modified by:
#' \itemize{
#'   \item Population-specific transmission rates (ts, tv)
#'   \item Time-varying contact patterns
#'   \item Vaccine effectiveness for breakthrough infections
#' }
#'
#' \strong{Stochastic vs. Deterministic Mode:}
#' \itemize{
#'   \item \strong{Deterministic}: Uses exact differential equations
#'   \item \strong{Stochastic}: Adds demographic stochasticity via binomial draws
#' }
#'
#' \strong{Vaccination Implementation:}
#' Vaccination is implemented as time-varying input with:
#' \itemize{
#'   \item Scheduled vaccination counts per time step and subpopulation
#'   \item Vaccine effectiveness reducing infection probability
#'   \item Waning immunity returning individuals to susceptible state
#' }
#'
#' @return
#' Returns a data.table with the following structure:
#' \describe{
#'   \item{step}{Integer time step index (0 to nsteps)}
#'   \item{time}{Continuous simulation time (step × delta_t)}
#'   \item{disease_state}{Character vector of compartment names}
#'   \item{population_id}{Character vector of subpopulation identifiers}
#'   \item{value}{Numeric values representing population counts in each compartment}
#' }
#'
#' Available disease states in output:
#' \itemize{
#'   \item Core compartments: S, E, I_presymp, I_asymp, I_symp, H, R, D, V, P
#'   \item Derived outputs: I_all (total infectious), cum_V (cumulative vaccinations)
#'   \item Transition flows: n_SE, n_EI, n_HR, n_HD, etc. (new infections, hospitalizations, deaths)
#'   \item Debug outputs: p_SE, p_VE, I_eff (probabilities and effective populations)
#' }
#'
#' @section Parameter Scaling:
#' All duration parameters are automatically converted to rates (1/duration).
#' Scalar parameters are automatically expanded to vectors of length N_pop.
#' This allows flexible specification of homogeneous or heterogeneous parameters.
#'
#' @section Checkpointing:
#' When do_chk = TRUE, the function saves a checkpoint file containing:
#' \itemize{
#'   \item Final compartment states for simulation continuation
#'   \item All model parameters for reproducibility
#'   \item Vaccination schedule data
#'   \item Population structure information
#' }
#'
#' @section Performance Considerations:
#' \itemize{
#'   \item Runtime scales approximately as O(N_pop² × nsteps)
#'   \item Memory usage increases with output frequency and population complexity
#'   \item Stochastic mode adds computational overhead for random number generation
#'   \item Large contact matrices (high N_pop) significantly impact performance
#' }
#'
#' @examples
#' \dontrun{
#' # Basic deterministic simulation
#' N_pop <- 30
#' nsteps <- 400
#' 
#' # Initialize populations
#' S0 <- rep(1000, N_pop)
#' I0 <- rep(10, N_pop)
#' P0 <- S0 + I0
#' R0 <- rep(0, N_pop)
#' 
#' # Contact matrices (simplified - identity matrices)
#' contact_matrix <- diag(N_pop) * 0.1
#' 
#' # Basic vaccination schedule (no vaccination)
#' vac_mat <- matrix(0, nrow = nsteps + 1, ncol = N_pop + 1)
#' vac_mat[, 1] <- 0:nsteps
#' 
#' # Run simulation
#' results <- meta_sim(
#'   N_pop = N_pop,
#'   ts = 0.5,
#'   tv = 0.1,
#'   S0 = S0,
#'   I0 = I0,
#'   P0 = P0,
#'   R0 = R0,
#'   m_weekday_day = contact_matrix,
#'   m_weekday_night = contact_matrix * 0.5,
#'   m_weekend_day = contact_matrix * 0.8,
#'   m_weekend_night = contact_matrix * 0.3,
#'   delta_t = 0.5,
#'   vac_mat = vac_mat,
#'   dv = 365,
#'   de = 3,
#'   pea = 0.3,
#'   dp = 2,
#'   da = 7,
#'   ds = 7,
#'   psr = 0.95,
#'   dh = 10,
#'   phr = 0.9,
#'   dr = 180,
#'   ve = 0.8,
#'   nsteps = nsteps
#' )
#' 
#' # Stochastic simulation with checkpointing
#' results_stoch <- meta_sim(
#'   # ... same parameters as above ...
#'   is.stoch = TRUE,
#'   seed = 12345,
#'   do_chk = TRUE,
#'   chk_file_name = "simulation_checkpoint.rds"
#' )
#' }
#'
#' @seealso
#' \code{\link{metaRVM}} for high-level simulation interface with configuration files
#' \code{\link{parse_config}} for configuration file processing
#' \code{\link{format_metarvm_output}} for output formatting with demographics
#'
#' @references
#' \itemize{
#'   \item ODIN package: \url{https://mrc-ide.github.io/odin/}
#'   \item Fadikar, A., et al. "Developing and deploying a use-inspired metapopulation modeling framework for detailed tracking of stratified health outcomes"
#' }
#' @author Arindam Fadikar, Charles Macal, Ignacio Martinez-Moyano, Jonathan Ozik
#'
#' @export

meta_sim <- function(N_pop, ts, tv,
                     # S0, I0, P0, V0, R0,
                     S0, I0, P0, R0,
                     H0 = rep(0, N_pop),
                     D0 = rep(0, N_pop),
                     Ia0 = rep(0, N_pop),
                     Ip0 = rep(0, N_pop),
                     E0 = rep(0, N_pop),
                     m_weekday_day, m_weekday_night, m_weekend_day, m_weekend_night,
                     delta_t,
                     # tvac,
                     vac_mat,
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

    # first remove vaccinated people from S
    S_eff_prod[, ]  <- m[i, j] * (S[i] - n_SV_eff[i])
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
    n_SE_eff[, ]      <- if(S[i] <= 0) 0 else (if(stoch == 1) rbinom(S_eff_prod[j, i], p_SE[i]) else S_eff_prod[j, i] * p_SE[i])
    n_SE[]            <- sum(n_SE_eff[i, ]) # rowSums
    n_EI[]            <- if(E[i] == 0) 0 else (if(stoch == 1) rbinom(E[i], p_EIpresymp[i]) else E[i] * p_EIpresymp[i])
    # n_EI[]            <- n_EI[i]
    n_EIpresymp[]     <- n_EI[i] * (1 - pea[i])
    n_EIasymp[]       <- n_EI[i] - n_EIpresymp[i]
    n_preIsymp[]      <- if(I_presymp[i] == 0) 0 else (if(stoch == 1) rbinom(I_presymp[i], p_preIsymp[i]) else I_presymp[i] * p_preIsymp[i])
    # n_preIsymp[]      <- ceiling(n_preIsymp[i])
    n_IasympR[]       <- if(I_asymp[i] == 0) 0 else (if(stoch == 1) rbinom(I_asymp[i], p_IasympR[i]) else I_asymp[i] * p_IasympR[i])
    # n_IasympR[]       <- ceiling(n_IasympR[i])
    n_IsympRH[]       <- if(I_symp[i] == 0) 0 else (if(stoch == 1) rbinom(I_symp[i], p_IsympRH[i]) else I_symp[i] * p_IsympRH[i])
    # n_IsympRH[]       <- ceiling(n_IsympRH[i])
    n_IsympH[]        <- n_IsympRH[i] * (1 - psr[i])
    n_IsympR[]        <- n_IsympRH[i] - n_IsympH[i]
    n_HRD[]           <- if(H[i] == 0) 0 else (if(stoch == 1) rbinom(H[i], p_HRD[i]) else H[i] * p_HRD[i])
    # n_HRD[]           <- ceiling(n_HRD[i])
    n_HR[]            <- n_HRD[i] * phr[i]
    n_HD[]            <- n_HRD[i] - n_HR[i]
    n_RS[]            <- if(R[i] == 0) 0 else (if(stoch == 1) rbinom(R[i], p_RS[i]) else R[i] * p_RS[i])
    # n_RS[]            <- ceiling(n_RS[i])
    n_VE_eff[, ]      <- if(stoch == 1) rbinom(V_eff_prod[j, i], p_VE[i]) else V_eff_prod[j, i] * p_VE[i]
    n_VE[]            <- sum(n_VE_eff[i, ]) # rowSums
    # n_VE[]            <- ceiling(n_VE[i])
    n_VS[]            <- if(stoch == 1) rbinom(V[i] - n_VE[i], p_VS[i]) else (V[i] - n_VE[i]) * p_VS[i]
    # n_VS[]            <- ceiling(n_VS[i])

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
    output(p_HRD)         <- TRUE
    output(I_eff)         <- TRUE
    output(n_SE)          <- TRUE
    output(n_SV)          <- TRUE
    output(n_VS)          <- TRUE
    output(n_VE)          <- TRUE
    output(n_EI)          <- TRUE
    output(n_EIpresymp)   <- TRUE
    output(n_preIsymp)    <- TRUE
    output(n_IsympRH)     <- TRUE
    output(n_IsympH)      <- TRUE
    output(n_IsympR)      <- TRUE
    output(n_HRD)         <- TRUE
    output(n_HR)          <- TRUE
    output(n_HD)          <- TRUE
    output(n_IasympR)     <- TRUE
    # output(S_eff_prod)    <- TRUE
    # output(n_SE_eff)      <- TRUE

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


  nsteps <- nsteps - 1

  ## prepare vaccination input
  tvac <- vac_mat[-nsteps, 1]
  vac_mat <- vac_mat[-1, -1]

  V0 <- vac_mat[1, ]


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

    chk[["N_pop"]] <- N_pop
    chk[["delta_t"]] <- delta_t

    chk[["m_weekday_day"]] <- m_weekday_day
    chk[["m_weekday_night"]] <- m_weekday_night
    chk[["m_weekend_day"]] <- m_weekend_day
    chk[["m_weekend_night"]] <- m_weekend_night

    chk[["ts"]] <- ts
    chk[["tv"]] <- tv
    chk[["ve"]] <- ve
    chk[["dv"]] <- dv
    chk[["de"]] <- de
    chk[["dp"]] <- dp
    chk[["da"]] <- da
    chk[["ds"]] <- ds
    chk[["dh"]] <- dh
    chk[["dr"]] <- dr
    chk[["pea"]] <- pea
    chk[["psr"]] <- psr
    chk[["phr"]] <- phr

    chk[["vac_time_id"]] <- tvac
    chk[["vac_counts"]] <- vac_mat

    chk[["S"]] <- long_out[(long_out$step == nsteps) & (long_out$disease_state == "S"), c("value")]
    chk[["E"]] <- long_out[(long_out$step == nsteps) & (long_out$disease_state == "E"), c("value")]
    chk[["Ia"]] <- long_out[(long_out$step == nsteps) & (long_out$disease_state == "I_asymp"), c("value")]
    chk[["Ip"]] <- long_out[(long_out$step == nsteps) & (long_out$disease_state == "I_presymp"), c("value")]
    chk[["Is"]] <- long_out[(long_out$step == nsteps) & (long_out$disease_state == "I_symp"), c("value")]
    chk[["H"]] <- long_out[(long_out$step == nsteps) & (long_out$disease_state == "H"), c("value")]
    chk[["D"]] <- long_out[(long_out$step == nsteps) & (long_out$disease_state == "D"), c("value")]
    chk[["P"]] <- long_out[(long_out$step == nsteps) & (long_out$disease_state == "P"), c("value")]
    chk[["V"]] <- long_out[(long_out$step == nsteps) & (long_out$disease_state == "V"), c("value")]
    chk[["R"]] <- long_out[(long_out$step == nsteps) & (long_out$disease_state == "R"), c("value")]

    if(!is.null(chk_file_name)) {
      saveRDS(chk, file = chk_file_name)
    }
  }


  return(long_out)
  # return(out_df)
}
