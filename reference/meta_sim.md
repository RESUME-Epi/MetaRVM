# Metapopulation Respiratory Virus Model Simulator

The core simulation engine that implements a stochastic compartmental
SEIRD (Susceptible-Exposed-Infected-Recovered-Dead) model for
respiratory virus epidemics across multiple demographic subpopulations.
The function compiles and executes an ODIN-based differential equation
model with time-varying contact patterns, vaccination dynamics, and
complex disease progression pathways.

## Usage

``` r
meta_sim(
  N_pop,
  ts,
  tv,
  S0,
  I0,
  P0,
  R0,
  H0 = rep(0, N_pop),
  D0 = rep(0, N_pop),
  Ia0 = rep(0, N_pop),
  Ip0 = rep(0, N_pop),
  E0 = rep(0, N_pop),
  V0 = rep(0, N_pop),
  m_weekday_day,
  m_weekday_night,
  m_weekend_day,
  m_weekend_night,
  delta_t,
  vac_mat,
  dv,
  de,
  pea,
  dp,
  da,
  ds,
  psr,
  dh,
  phr,
  dr,
  ve,
  nsteps,
  is.stoch = FALSE,
  seed = NULL,
  do_chk = FALSE,
  chk_time_steps = NULL,
  chk_file_names = NULL
)
```

## Arguments

- N_pop:

  Integer. Number of demographic subpopulations in the model

- ts:

  Numeric vector or scalar. Transmission rate for symptomatic
  individuals in susceptible population. If scalar, applied to all
  subpopulations

- tv:

  Numeric vector or scalar. Transmission rate for symptomatic
  individuals in vaccinated population. If scalar, applied to all
  subpopulations

- S0:

  Numeric vector of length N_pop. Initial number of susceptible
  individuals in each subpopulation

- I0:

  Numeric vector of length N_pop. Initial number of symptomatic infected
  individuals in each subpopulation

- P0:

  Numeric vector of length N_pop. Total population sizes for each
  subpopulation

- R0:

  Numeric vector of length N_pop. Initial number of recovered
  individuals in each subpopulation

- H0:

  Numeric vector of length N_pop. Initial number of hospitalized
  individuals in each subpopulation (default: rep(0, N_pop))

- D0:

  Numeric vector of length N_pop. Initial number of deceased individuals
  in each subpopulation (default: rep(0, N_pop))

- Ia0:

  Numeric vector of length N_pop. Initial number of asymptomatic
  infected individuals in each subpopulation (default: rep(0, N_pop))

- Ip0:

  Numeric vector of length N_pop. Initial number of presymptomatic
  infected individuals in each subpopulation (default: rep(0, N_pop))

- E0:

  Numeric vector of length N_pop. Initial number of exposed individuals
  in each subpopulation (default: rep(0, N_pop))

- V0:

  Numeric vector of length N_pop. Initial number of vaccinated
  individuals in each subpopulation

- m_weekday_day:

  Numeric matrix (N_pop × N_pop). Contact mixing matrix for weekday
  daytime (6 AM - 6 PM) interactions

- m_weekday_night:

  Numeric matrix (N_pop × N_pop). Contact mixing matrix for weekday
  nighttime (6 PM - 6 AM) interactions

- m_weekend_day:

  Numeric matrix (N_pop × N_pop). Contact mixing matrix for weekend
  daytime (6 AM - 6 PM) interactions

- m_weekend_night:

  Numeric matrix (N_pop × N_pop). Contact mixing matrix for weekend
  nighttime (6 PM - 6 AM) interactions

- delta_t:

  Positive numeric. Discrete time increment in days (typically 0.5)

- vac_mat:

  Numeric matrix. Vaccination schedule with dimensions (nsteps × (1 +
  N_pop)). First column contains time indices, remaining columns contain
  vaccination counts for each subpopulation at each time step

- dv:

  Numeric vector or scalar. Mean duration (days) in vaccinated state
  before immunity waning. If scalar, applied to all subpopulations

- de:

  Numeric vector or scalar. Mean duration (days) in exposed state. If
  scalar, applied to all subpopulations

- pea:

  Numeric vector or scalar. Proportion of exposed individuals becoming
  asymptomatic infectious (vs. presymptomatic), values between 0 and 1.
  If scalar, applied to all subpopulations. If scalar, applied to all
  subpopulations

- dp:

  Numeric vector or scalar. Mean duration (days) in presymptomatic
  infectious state. If scalar, applied to all subpopulations

- da:

  Numeric vector or scalar. Mean duration (days) in asymptomatic
  infectious state. If scalar, applied to all subpopulations

- ds:

  Numeric vector or scalar. Mean duration (days) in symptomatic
  infectious state. If scalar, applied to all subpopulations

- psr:

  Numeric vector or scalar. Proportion of symptomatic individuals
  recovering directly (vs. hospitalization), values between 0 and 1. If
  scalar, applied to all subpopulations. If scalar, applied to all
  subpopulations

- dh:

  Numeric vector or scalar. Mean duration (days) in hospitalized state.
  If scalar, applied to all subpopulations

- phr:

  Numeric vector or scalar. Proportion of hospitalized individuals
  recovering (vs. death). , values between 0 and 1. If scalar, applied
  to all subpopulations.

- dr:

  Numeric vector or scalar. Mean duration (days) of immunity in
  recovered state. If scalar, applied to all subpopulations

- ve:

  Numeric vector or scalar. Vaccine effectiveness (proportion) , values
  between 0 and 1. If scalar, applied to all subpopulations

- nsteps:

  Integer. Total number of discrete time evolution steps in simulation

- is.stoch:

  Logical. Whether to run stochastic simulation (TRUE) or deterministic
  simulation (FALSE). Default: FALSE

- seed:

  Integer or NULL. Random seed for reproducibility. Only used when
  is.stoch = TRUE. Default: NULL

- do_chk:

  Logical. Whether to save model checkpoint at simulation end. Default:
  FALSE

- chk_time_steps:

  Integer vector or NULL. Time steps at which to save checkpoints.

- chk_file_names:

  List of character vectors or NULL. File names for checkpoints. Each
  element of the list corresponds to a time step in `chk_time_steps`.

## Value

Returns a data.table with the following structure:

- step:

  Integer time step index (0 to nsteps)

- time:

  Continuous simulation time (step × delta_t)

- disease_state:

  Character vector of compartment names

- population_id:

  Character vector of subpopulation identifiers

- value:

  Numeric values representing population counts in each compartment

Available disease states in output:

- Core compartments: S, E, I_presymp, I_asymp, I_symp, H, R, D, V, P

- Derived outputs: I_all (total infectious), cum_V (cumulative
  vaccinations)

- Transition flows: n_SE, n_EI, n_HR, n_HD, etc. (new infections,
  hospitalizations, deaths)

- Debug outputs: p_SE, p_VE, I_eff (probabilities and effective
  populations)

## Details

The model implements a metapopulation epidemiological framework with the
following features:

**Compartmental Structure:**

- **S**: Susceptible individuals

- **E**: Exposed (incubating) individuals

- **I_presymp**: Presymptomatic infectious individuals

- **I_asymp**: Asymptomatic infectious individuals

- **I_symp**: Symptomatic infectious individuals

- **H**: Hospitalized individuals

- **R**: Recovered individuals

- **D**: Deceased individuals

- **V**: Vaccinated individuals

- **P**: Total living population (excludes deaths)

**Disease Progression Pathways:**

1.  **S → E**: Exposure through contact with infectious individuals

2.  **E → I_asymp/I_presymp**: Progression to infectious states
    (proportion pea)

3.  **I_presymp → I_symp**: Development of symptoms

4.  **I_asymp → R**: Direct recovery from asymptomatic state

5.  **I_symp → R/H**: Recovery or hospitalization (proportion psr)

6.  **H → R/D**: Hospital discharge or death (proportion phr)

7.  **R → S**: Loss of immunity

8.  **S → V**: Vaccination

9.  **V → S/E**: Vaccine waning or breakthrough infection

**Mixing Patterns:** Contact patterns vary by:

- Day of week: Weekday vs. weekend patterns

- Time of day: Day (6 AM - 6 PM) vs. night (6 PM - 6 AM) patterns

- Each pattern specified by N_pop × N_pop contact matrix

**Force of Infection:** Transmission occurs through contact between
susceptible/vaccinated individuals and all infectious compartments
(I_presymp + I_asymp + I_symp), modified by:

- Population-specific transmission rates (ts, tv)

- Time-varying contact patterns

- Vaccine effectiveness for breakthrough infections

**Stochastic vs. Deterministic Mode:**

- **Deterministic**: Uses exact differential equations

- **Stochastic**: Adds demographic stochasticity via binomial draws

**Vaccination Implementation:** Vaccination is implemented as
time-varying input with:

- Scheduled vaccination counts per time step and subpopulation

- Vaccine effectiveness reducing infection probability

- Waning immunity returning individuals to susceptible state

## Parameter Scaling

All duration parameters are automatically converted to rates
(1/duration). Scalar parameters are automatically expanded to vectors of
length N_pop. This allows flexible specification of homogeneous or
heterogeneous parameters.

## Checkpointing

When do_chk = TRUE, the function saves a checkpoint file containing:

- Final compartment states for simulation continuation

- All model parameters for reproducibility

- Vaccination schedule data

- Population structure information

## References

- ODIN package: <https://mrc-ide.github.io/odin/>

- Fadikar, A., et al. "Developing and deploying a use-inspired
  metapopulation modeling framework for detailed tracking of stratified
  health outcomes"

## See also

[`metaRVM`](https://RESUME-Epi.github.io/MetaRVM/reference/metaRVM.md)
for high-level simulation interface with configuration files
[`parse_config`](https://RESUME-Epi.github.io/MetaRVM/reference/parse_config.md)
for configuration file processing
[`format_metarvm_output`](https://RESUME-Epi.github.io/MetaRVM/reference/format_metarvm_output.md)
for output formatting with demographics

## Author

Arindam Fadikar, Charles Macal, Ignacio Martinez-Moyano, Jonathan Ozik

## Examples

``` r
options(odin.verbose = FALSE)
# Basic deterministic simulation
N_pop <- 2
nsteps <- 400

# Initialize populations
S0 <- rep(1000, N_pop)
I0 <- rep(10, N_pop)
P0 <- S0 + I0
R0 <- rep(0, N_pop)

# Contact matrices (simplified - identity matrices)
contact_matrix <- diag(N_pop)

# Basic vaccination schedule (10% vaccination)
vac_mat <- matrix(0, nrow = nsteps + 1, ncol = N_pop + 1)
vac_mat[, 1] <- 0:nsteps
vac_mat[1, 1 + (1:N_pop)] <- P0 * 0.1

# Run simulation
results <- meta_sim(
  N_pop = N_pop,
  ts = 0.5,
  tv = 0.1,
  S0 = S0,
  I0 = I0,
  P0 = P0,
  R0 = R0,
  m_weekday_day = contact_matrix,
  m_weekday_night = contact_matrix,
  m_weekend_day = contact_matrix,
  m_weekend_night = contact_matrix,
  delta_t = 0.5,
  vac_mat = vac_mat,
  dv = 365,
  de = 3,
  pea = 0.3,
  dp = 2,
  da = 7,
  ds = 7,
  psr = 0.95,
  dh = 10,
  phr = 0.9,
  dr = 180,
  ve = 0.8,
  nsteps = nsteps,
  is.stoch = FALSE
)


```
