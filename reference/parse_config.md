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

If `return_object = FALSE` (default), returns a named list containing:

- N_pop:

  Number of population groups

- pop_map:

  Data.table with population mapping and demographics

- S_ini, E_ini, I_asymp_ini, I_presymp_ini, I_symp_ini, H_ini, D_ini,
  P_ini, V_ini, R_ini:

  Initial compartment populations

- vac_time_id, vac_counts, vac_mat:

  Vaccination schedule data

- m_wd_d, m_wd_n, m_we_d, m_we_n:

  Contact mixing matrices

- ts, tv, ve, dv, de, dp, da, ds, dh, dr, pea, psr, phr:

  Disease parameter matrices (nsim × N_pop)

- start_date:

  Simulation start date as Date object

- sim_length:

  Simulation length in days

- nsim:

  Number of simulation instances

- random_seed:

  Random seed used (if any)

- delta_t:

  Time step size (fixed at 0.5)

- chk_file_names, chk_time_steps, do_chk:

  Checkpointing configuration

If `return_object = TRUE`, returns a `MetaRVMConfig` object with methods
for parameter access and validation.

## Details

The function processes a comprehensive YAML configuration file with the
following main sections:

**Simulation Configuration:**

- `random_seed`: Optional random seed for reproducibility

- `nsim`: Number of simulation instances (default: 1)

- `start_date`: Simulation start date in MM/DD/YYYY format

- `length`: Simulation length in days

- `checkpoint_dir`: Optional checkpoint directory for saving
  intermediate results

- `checkpoint_dates`: Optional list of dates to save checkpoints.

- `restore_from`: Optional path to restore simulation from checkpoint

**Population Data:**

- `mapping`: CSV file path containing population mapping with
  demographic categories

- `initialization`: CSV file with initial population states (S0, I0, V0,
  R0, N)

- `vaccination`: CSV file with vaccination schedule over time

**Mixing Matrices:** Contact matrices for different time periods:

- `weekday_day`, `weekday_night`: Weekday contact patterns

- `weekend_day`, `weekend_night`: Weekend contact patterns

**Disease Parameters:** Epidemiological parameters (can be scalars or
distributions):

- `ts`: Transmission rate for symptomatic individuals

- `tv`: Transmission rate for vaccinated individuals

- `ve`: Vaccine effectiveness

- `de, dp, da, ds, dh, dr`: Duration parameters for different disease
  states

- `pea, psr, phr`: Probability parameters for disease transitions

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

**Population mapping file** must contain columns:

- `population_id`: Unique identifier for each population group

- `age`: Age category (e.g., "0-4", "5-11", "12-17", "18-49", "50-64",
  "65+")

- `race`: Race/ethnicity category

- `zone`: Geographic zone identifier

**Population initialization file** must contain: `N` (total population),
`S0`, `I0`, `V0`, `R0` (initial compartment counts)

**Vaccination file** must contain: `date` (MM/DD/YYYY format) and
vaccination counts for each population group

## See also

[`metaRVM`](https://RESUME-Epi.github.io/MetaRVM/reference/metaRVM.md)
for running simulations with parsed configuration
[`MetaRVMConfig`](https://RESUME-Epi.github.io/MetaRVM/reference/MetaRVMConfig.md)
for the configuration object class
[`process_vac_data`](https://RESUME-Epi.github.io/MetaRVM/reference/process_vac_data.md)
for vaccination data processing

## Author

Arindam Fadikar

## Examples

``` r
if (FALSE) { # \dontrun{
# Parse configuration file and return list (backward compatible)
config <- parse_config("path/to/config.yaml")

# Parse and return MetaRVMConfig object for method chaining
config_obj <- parse_config("path/to/config.yaml", return_object = TRUE)

# Access parameters from config object
config_obj$get("N_pop")
config_obj$list_parameters()

# Use with MetaRVM simulation
results <- metaRVM(config_obj)
} # }
```
