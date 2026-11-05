# Parse MetaRVM Configuration File

Reads and parses a YAML configuration file for MetaRVM simulations,
extracting all necessary parameters for epidemic modeling including
population data, disease parameters, mixing matrices, vaccination
schedules, and simulation settings.

## Usage

``` r
parse_config(config_file, return_object = FALSE)
```

## Arguments

- config_file:

  Character string. Path to a YAML configuration file containing model
  parameters and settings.

- return_object:

  Logical. If `TRUE`, returns a `MetaRVMConfig` object for method
  chaining and enhanced functionality. If `FALSE` (default), returns a
  named list for backward compatibility.

## Value

If `return_object = FALSE` (default), returns a named list containing
entries common to both diseases plus disease-specific initial
compartments and parameter matrices:

- disease:

  The resolved disease name (`"flu"` or `"measles"`)

- N_pop:

  Number of population groups

- pop_map:

  Data.table with `population_id` and user-defined demographic
  categories

- Initial compartments:

  For flu:
  `S_ini, E_ini, I_asymp_ini, I_presymp_ini, I_symp_ini, H_ini, D_ini, V_ini, R_ini`.
  For measles:
  `S_ini, E1_ini, E2_ini, I1_Q_ini, I1_U_ini, I2_Q_ini, I2_U_ini, R_ini, V1_ini, V2_ini`
  plus the vaccinated-breakthrough compartment initials.

- vac_time_id, vac_counts, vac_mat:

  Vaccination schedule data (flu only; not present for measles, which
  instead folds vaccination into the initial `V1_ini`/`V2_ini`
  compartments)

- m_wd_d, m_wd_n, m_we_d, m_we_n:

  Contact mixing matrices

- Disease parameter matrices (nsim × N_pop):

  For flu: `ts, ve, dv, de, dp, da, ds, dh, dr, pea, psr, phr`. For
  measles:
  `beta, de1, de2, di1, di2, prop_I1_Q, prop_I2_Q, V1_eff, V2_eff`.

- start_date:

  Simulation start date as Date object

- sim_length:

  Simulation length in days

- nsim:

  Number of simulation instances

- nrep:

  Number of stochastic replicates per parameter set

- random_seed:

  Random seed used (auto-generated if not supplied)

- delta_t:

  Time step size (fixed at 0.5)

- chk_file_names, chk_time_steps, do_chk:

  Checkpointing configuration

If `return_object = TRUE`, returns a `MetaRVMConfig` object with methods
for parameter access and validation.

## Details

The function processes a YAML configuration file with the following main
sections:

**Model Selection:**

- `model.disease`: Required. Selects the disease to simulate; supported
  values are `"flu"` and `"measles"`. The disease choice determines
  which disease parameters are expected, the population initialization
  CSV column schema, and whether a vaccination schedule file is used
  (flu only; see below).

**Simulation Configuration:**

- `random_seed`: Optional random seed for reproducibility in case of
  stochastic simulations or stochastic parameters

- `nsim`: Number of simulation instances (default: 1)

- `nrep`: Number of stochastic replicates per parameter set (default: 1)
  \#'

- `start_date`: Simulation start date in MM/DD/YYYY format

- `length`: Simulation length in days

- `checkpoint_dir`: Optional checkpoint directory for saving
  intermediate results

- `checkpoint_dates`: Optional list of dates to save checkpoints.

- `restore_from`: Optional path to restore simulation from checkpoint

**Population Data:** The required `initialization` CSV columns are
disease-specific; any additional columns are treated as demographic
categories.

- **Flu**: `population_id`, `N`, `S0`, `I0`, `V0`, `R0` (optional: `E0`,
  `Ia0`, `Ip0`, `H0`, `D0`, defaulted to 0 when absent).

- **Measles**: `population_id`, `N`, `S0`, and at least one of `I1_U0`,
  `I2_U0` (optional: `E10`, `E20`, `I1_Q0`, `I2_Q0`, `R0`, `V10`, `V20`,
  and the vaccinated-breakthrough compartment columns, all defaulted to
  0 when absent). Measles has no `I0` column.

&nbsp;

- `vaccination`: CSV file with a time-varying vaccination schedule.
  **Flu only** — the first column must be dates in MM/DD/YYYY format,
  and the rest of the columns must correspond to respective
  subpopulations in the numeric order of population_id. Measles
  vaccination is specified instead via the initial `V10`/`V20`
  compartment counts in the initialization file; no separate vaccination
  file is used.

**Mixing Matrices:** Contact matrices for different time periods. Each
CSV file must have a matrix of order (N_pop x N_pop), where, N_pop is
the number of subpopulations. It is assumed that the i-th row and j-th
column correspond to i-th and j-th subpopulations.

- `weekday_day`, `weekday_night`: Weekday contact patterns

- `weekend_day`, `weekend_night`: Weekend contact patterns

**Disease Parameters:** Epidemiological parameters (can be scalars or
distributions). The exact parameter names are disease-specific:

- **Flu**: `ts`, `ve`, `dv`, `de`, `dp`, `da`, `ds`, `dh`, `dr`, `pea`,
  `psr`, `phr`

- **Measles**: `beta`, `de1`, `de2`, `di1`, `di2`, `prop_I1_Q`,
  `prop_I2_Q`, `V1_eff`, `V2_eff`

**Sub-population Parameters:** `sub_disease_params` allows specification
of different parameter values for specific demographic categories (e.g.,
age groups, races).

The function supports stochastic parameters through distribution
specifications with `dist`, `mu`, `sd`, `shape`, `rate`, etc.

## Parameter Distributions

Disease parameters can be specified as distributions for stochastic
modeling:

- **lognormal**: `dist: "lognormal", mu: value, sd: value`

- **gamma**: `dist: "gamma", shape: value, rate: value`

- **uniform**: `dist: "uniform", min: value, max: value`

- **beta**: `dist: "beta", shape1: value, shape2: value`

- **gaussian**: `dist: "gaussian", mean: value, sd: value`

## File Requirements

**Population initialization file** must contain columns (see Population
Data above for the full disease-specific list):

- `population_id`: Unique identifier for each population group, natural
  numbers

- `N`: Total population size of the subpopulation

- **Flu**: `S0`, `I0`, `V0`, `R0`

- **Measles**: `S0`, and at least one of `I1_U0`, `I2_U0`

- Optional user-defined category columns (e.g., `age`, `race`, `zone`,
  `income_level`, `occupation`)

**Vaccination file (flu only)** must contain: `date` (MM/DD/YYYY format)
and vaccination counts for each population group. Not used for measles.

## See also

[`metaRVM`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/metaRVM.md)
for running simulations with parsed configuration
[`MetaRVMConfig`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.md)
for the configuration object class
[`process_vac_data`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/process_vac_data.md)
for vaccination data processing

## Author

Arindam Fadikar

## Examples

``` r
# \donttest{
options(odin.verbose = FALSE)
example_config <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
# Parse configuration file and return list (backward compatible)
config <- parse_config(example_config)

# Parse and return MetaRVMConfig object for method chaining
config_obj <- parse_config(example_config, return_object = TRUE)

# Access parameters from config object
config_obj$get("N_pop")
#> [1] 24
config_obj$list_parameters()
#>  [1] "N_pop"                  "disease"                "pop_map"               
#>  [4] "category_names"         "S_ini"                  "E_ini"                 
#>  [7] "I_asymp_ini"            "I_presymp_ini"          "I_symp_ini"            
#> [10] "H_ini"                  "D_ini"                  "P_ini"                 
#> [13] "V_ini"                  "R_ini"                  "vac_time_id"           
#> [16] "vac_counts"             "vac_mat"                "m_wd_d"                
#> [19] "m_wd_n"                 "m_we_d"                 "m_we_n"                
#> [22] "start_date"             "sim_length"             "nsim"                  
#> [25] "nrep"                   "random_seed"            "parallel"              
#> [28] "verbose"                "suppress_odin_messages" "delta_t"               
#> [31] "chk_file_names"         "chk_time_steps"         "do_chk"                
#> [34] "ts"                     "ve"                     "dv"                    
#> [37] "de"                     "dp"                     "da"                    
#> [40] "ds"                     "dh"                     "dr"                    
#> [43] "pea"                    "psr"                    "phr"                   

# Use with MetaRVM simulation
results <- metaRVM(config_obj)
# }
```
