# YAML Configuration for metaRVM

## Introduction

The `metaRVM` package uses a YAML file to configure the model
parameters. This vignette describes the structure of the YAML
configuration file, starting with a simple example and progressively
introducing more advanced features.

## Basic Configuration

A minimal configuration file specifies the data sources, simulation
settings, and disease parameters with fixed scalar values.

``` yaml
run_id: SimpleRun
population_data:
  initialization: data/population_init.csv
  vaccination: data/vaccination.csv
mixing_matrix:
  weekday_day: data/m_weekday_day.csv
  weekday_night: data/m_weekday_night.csv
  weekend_day: data/m_weekend_day.csv
  weekend_night: data/m_weekend_night.csv
disease_params:
  ts: 0.5
  ve: 0.4
  dv: 180
  dp: 1
  de: 3
  da: 5
  ds: 6
  dh: 8
  dr: 180
  pea: 0.3
  psr: 0.95
  phr: 0.97
simulation_config:
  start_date: 01/01/2025 # m/d/Y
  length: 90
  nsim: 1
  nrep: 1
  simulation_mode: deterministic
  random_seed: 42
```

### Configuration Sections

- **`run_id`**: A unique name for the simulation.
- **`population_data`**: Paths to CSV files for population demographics,
  initial state, and vaccination schedules.
- **`mixing_matrix`**: Paths to CSV files defining contact patterns for
  different times of the week.
- **`disease_params`**: Disease characteristics. In this example, all
  parameters are single, fixed values.
- **`simulation_config`**: Settings for the simulation run, such as
  start date, duration, and number of simulations.

### Input File Structures

The `metaRVM` package requires several CSV files to be structured in a
specific way. Below are the descriptions for each of the required input
files, along with examples of what they should look like.

#### Population Data Files

- **`initialization`**: This file specifies both the demographic
  structure and initial state of the population for the simulation. It
  must contain the following **required columns**:
  - `population_id`: A unique identifier for each subpopulation
    (sequential natural numbers: 1, 2, 3, …)
  - `N`: The total number of individuals in each subpopulation
  - `S0`: The initial number of susceptible individuals
  - `I0`: The initial number of symptomatic infected individuals
  - `V0`: The initial number of vaccinated individuals
  - `R0`: The initial number of recovered individuals

  **User-defined demographic category columns**: Any additional columns
  beyond the required ones are automatically detected as demographic
  categories. These can be used for stratification and in
  `sub_disease_params` for category-specific parameters. Common examples
  include:
  - `age`: Age groups (e.g., “0-17”, “18-64”, “65+”)
  - `race`: Race or ethnicity categories
  - `zone`: Healthcare zones or geographic regions
  - Or any custom categories like `income_level`, `occupation`,
    `risk_group`, etc.

**Example of a population initialization file:**

    #> First 10 rows of population_init_n24.csv:
    #>    population_id   age race zone      N     S0  I0 V0 R0
    #> 1              1  0-17    A   11  30742  30711  31  0  7
    #> 2              2 18-64    A   11  41429  41388  41  0  9
    #> 3              3   65+    A   11   3321   3318   3  0  0
    #> 4              4  0-17    B   11  70138  70068  70  0  6
    #> 5              5 18-64    B   11  12298  12286  12  0 32
    #> 6              6   65+    B   11  11549  11537  12  0  0
    #> 7              7  0-17    C   11  84178  84094  84  0 13
    #> 8              8 18-64    C   11 113521 113407 114  0 15
    #> 9              9   65+    C   11  11924  11912  12  0  0
    #> 10            10  0-17    D   11 199686 199486 200  0 14
    #> 
    #> ... (24 total rows)

- **`vaccination`**: The vaccination schedule file contains the number
  of vaccinations administered over time. The first column must be
  `date` in `MM/DD/YYYY` format, followed by columns for each
  subpopulation in the same order as `population_id` in the
  initialization file.

**Example of a vaccination schedule file:**

    #> First 10 rows of vaccination_n24.csv:
    #>          date v1 v2 v3  v4 v5 v6  v7  v8  v9 v10 v11 v12 v13 v14 v15  v16 v17
    #> 1  09/16/2023  0  0  0   0  0  0  11   5   4   8   6   4   2   5   1    8  11
    #> 2  09/23/2023  5 12  1  11  7  2 180 187  99 362 293  72 311 163  24  519 203
    #> 3  09/30/2023  3  5  2  19 10  3 198 235  88 424 364  89 445 255  36  625 251
    #> 4  10/07/2023 20 14 15  46 19  8 403 385 124 846 580 187 675 389  45 1242 347
    #> 5  10/14/2023 28 29 19  81 50  7 471 425 105 997 511 162 727 400  42 1351 313
    #> 6  10/21/2023 37 55 24 110 43  8 378 499 115 980 483 160 623 511  39 1212 317
    #> 7  10/28/2023 28 43 19  86 48 16 339 464  88 809 438 126 548 411  52 1090 287
    #> 8  11/04/2023 22 53 19  91 49 28 329 391  70 765 431 147 522 391  22  956 259
    #> 9  11/11/2023 36 62 21 110 60 10 337 396  87 769 414 148 575 425  39  966 199
    #> 10 11/18/2023 40 65 14 102 55 14 340 431  97 875 392 118 513 417  28 1041 231
    #>    v18 v19 v20 v21 v22 v23 v24
    #> 1    5   0   0   0   0   0   3
    #> 2  113   2   3  26   4  13  10
    #> 3  119   4  22  27   2  18  27
    #> 4  193   8  41  50   4  70  44
    #> 5  161  10  98 217  36 123  98
    #> 6  202  18 160 214  51 149 168
    #> 7  152  32 199 273  60 153 267
    #> 8  126  26 168 239  33 149 225
    #> 9  149  26 197 255  52 142 275
    #> 10 141  26 212 242  50 153 191
    #> 
    #> ... (51 total rows)
    #> 
    #> Note: Columns represent vaccination counts for each population_id (1-24)

#### Mixing Matrix Files

The mixing matrix files define the contact patterns between different
subpopulations. Each file should be a CSV without a header, where the
rows and columns correspond to the subpopulations in the same order as
`population_id` in the initialization file. The values in the matrix
represent the proportion of time that individuals from one subpopulation
spend with individuals from another. The sum of each row must equal 1.

**Example of a mixing matrix file (weekday day):**

    #> First 10 rows and 10 columns of m_weekday_day.csv:
    #>             V1           V2          V3          V4           V5          V6
    #> 1  0.860000000 0.0010475811 0.003377249 0.001710217 0.0011920273 0.006571116
    #> 2  0.003611954 0.9100000000 0.001328864 0.003596329 0.0027309903 0.004173784
    #> 3  0.001344397 0.0072452422 0.870000000 0.002513688 0.0050834111 0.009789632
    #> 4  0.006689671 0.0005482346 0.004749909 0.920000000 0.0070678313 0.001806128
    #> 5  0.007740689 0.0039632387 0.004643154 0.001091787 0.8700000000 0.009791262
    #> 6  0.006379144 0.0032865609 0.003432134 0.001761560 0.0071883914 0.890000000
    #> 7  0.001444749 0.0064237517 0.002656119 0.001968115 0.0043013059 0.001206245
    #> 8  0.002179355 0.0070411463 0.001587234 0.003439760 0.0033650992 0.006414028
    #> 9  0.005390480 0.0066530217 0.005482305 0.003007641 0.0006105881 0.003145261
    #> 10 0.001666809 0.0029011017 0.008534104 0.001698315 0.0098720801 0.004586367
    #>              V7           V8           V9          V10
    #> 1  1.464237e-02 0.0143653234 0.0118876071 0.0080742019
    #> 2  6.528305e-03 0.0041375710 0.0057636782 0.0054420004
    #> 3  8.763376e-03 0.0005057071 0.0007488488 0.0073198670
    #> 4  2.822754e-04 0.0070548564 0.0009509153 0.0012611329
    #> 5  1.004983e-02 0.0041073157 0.0088236656 0.0006445009
    #> 6  3.144937e-05 0.0085528740 0.0063335264 0.0023219191
    #> 7  9.200000e-01 0.0035656434 0.0070237063 0.0042256901
    #> 8  4.098307e-03 0.9100000000 0.0067478716 0.0100518811
    #> 9  9.597300e-04 0.0002268853 0.9200000000 0.0009418937
    #> 10 5.236045e-03 0.0065183293 0.0012107981 0.8700000000
    #> 
    #> Matrix dimensions: 24 x 24
    #> Row sums (should all equal 1):
    #>  [1] 1 1 1 1 1 1 1 1 1 1

### Disease Parameter Descriptions

Below is a list of the disease parameters used in `metaRVM`:

- `ts`: Transmission rate for symptomatic individuals in the susceptible
  population.
- `ve`: Vaccine effectiveness (proportion, range: \[0, 1\]).
- `dv`: Mean duration (in days) in the vaccinated state before immunity
  wanes.
- `dp`: Mean duration (in days) in the presymptomatic infectious state.
- `de`: Mean duration (in days) in the exposed state.
- `da`: Mean duration (in days) in the asymptomatic infectious state.
- `ds`: Mean duration (in days) in the symptomatic infectious state.
- `dh`: Mean duration (in days) in the hospitalized state.
- `dr`: Mean duration (in days) of immunity in the recovered state.
- `pea`: Proportion of exposed individuals who become asymptomatic
  (vs. presymptomatic) (range: 0-1).
- `psr`: Proportion of symptomatic individuals who recover directly
  (vs. requiring hospitalization) (range: 0-1).
- `phr`: Proportion of hospitalized individuals who recover (vs. die)
  (range: 0-1).

## Defining Parameters with Distributions

Instead of fixed values, disease parameters can be defined using
statistical distributions. This is useful for capturing uncertainty in
the parameters. `metaRVM` supports `uniform` and `lognormal`
distributions.

Here is an example of defining `ve`, `da`, `ds`, and `dh` with
distributions:

``` yaml
disease_params:
  ts: 0.5
  ve:
    dist: uniform
    min: 0.29
    max: 0.53
  dv: 158
  dp: 1
  de: 3
  da:
    dist: uniform
    min: 3
    max: 7
  ds:
    dist: uniform
    min: 5
    max: 7
  dh:
    dist: lognormal
    mu: 8
    sd: 8.9
  dr: 187
  pea: 0.333
  psr: 0.95
  phr: 0.97
```

- For a `uniform` distribution, `min` and `max` values must be
  specified.
- For a `lognormal` distribution, `mu` and `sd` (mean and standard
  deviation on the log scale) must be specified.

## Specifying Subgroup Parameters

`metaRVM` allows different disease parameters to be specified for
demographic subgroups using the `sub_disease_params` section. These
subgroup-specific parameters override the global parameters defined in
`disease_params`.

The demographic categories used in this section must match the
user-defined category column names in the initialization CSV file
specified under `population_data`. For example, if the initialization
file has columns named `age`, `income_level`, and `occupation`, any of
these categories can be used in `sub_disease_params`. The specific
values (e.g., `"0-4"`, `"low"`, `"healthcare"`) must exactly match the
values in those columns.

The following example defines different parameters for different age
groups:

``` yaml
sub_disease_params:
    age:
      0-17:
        dh: 4
        pea: 0.08
        psr: 0.9303
        phr: 0.9920
      18-64:
        dh: 4
        pea: 0.08
        psr: 0.9726
        phr: 0.9920
      65+:
        dh: 7
        pea: 0.05
        psr: 0.9091
        phr: 0.9227
```

In this configuration, individuals in the “0-4” age group will have a
`dh` (duration of hospitalization) of 4, overriding any global `dh`
value. Similarly, the transmission rate `ts` for the “18-49” group is
set to 0.01.

## Stochastic Simulation with Distributional Parameters

When both parameter uncertainty and stochastic disease transitions are
represented, set `simulation_mode: stochastic` and define one or more
disease parameters as distributions.

- `nsim`: number of sampled parameter sets
- `nrep`: number of stochastic replicates per parameter set
- total runs = `nsim * nrep`

Example:

``` yaml
run_id: StochasticDistRun
population_data:
  initialization: data/population_init.csv
  vaccination: data/vaccination.csv
mixing_matrix:
  weekday_day: data/m_weekday_day.csv
  weekday_night: data/m_weekday_night.csv
  weekend_day: data/m_weekend_day.csv
  weekend_night: data/m_weekend_night.csv
disease_params:
  ts: 0.5
  ve:
    dist: uniform
    min: 0.29
    max: 0.53
  dv: 158
  dp: 1
  de: 3
  da:
    dist: uniform
    min: 3
    max: 7
  ds:
    dist: uniform
    min: 5
    max: 7
  dh:
    dist: lognormal
    mu: 2.0
    sd: 0.5
  dr: 187
  pea: 0.333
  psr: 0.95
  phr: 0.97
simulation_config:
  start_date: 01/01/2025
  length: 90
  nsim: 20
  nrep: 5
  simulation_mode: stochastic
  random_seed: 42
```

For reproducibility, provide `random_seed`. This seed is used to
reproduce both the parameter draws (for distributional parameters) and
the stochastic model replicates.

## Checkpointing and Restoring Simulations

For long-running simulations, it is useful to save the state of the
model at intermediate points. This is known as checkpointing. `metaRVM`
allows checkpoints to be saved and simulations to be restored from a
saved state.

### Enabling Checkpointing

To enable checkpointing, `checkpoint_dir` and optionally
`checkpoint_dates` need to be added to the `simulation_config` section
of the YAML file.

- `checkpoint_dir`: The directory where checkpoint files will be saved.
- `checkpoint_dates`: A list of dates (in `MM/DD/YYYY` format) on which
  to save a checkpoint. If this is not provided, a single checkpoint
  will be saved at the end of the simulation.

Here is an example of how to configure checkpointing:

``` yaml
simulation_config:
  start_date: 01/01/2025
  length: 90
  nsim: 10
  nrep: 1
  simulation_mode: deterministic
  random_seed: 42
  checkpoint_dir: "path/to/checkpoints"
  checkpoint_dates: ["01/15/2025", "01/30/2025"]
```

### Restoring from a Checkpoint

To restore a simulation from a checkpoint file, the `restore_from`
parameter is used in the `simulation_config` section. The model is
initialized with the state saved in the specified checkpoint file.

``` yaml
simulation_config:
  start_date: 01/30/2025 # Should be the next date of the checkpoint date
  length: 60 # Remaining simulation length
  nsim: 10
  nrep: 1
  simulation_mode: deterministic
  random_seed: 42
  restore_from: "path/to/checkpoints/checkpoint_2025-01-30_instance_1.Rda"
```

When restoring, the `start_date` should correspond to the next date of
the checkpoint, and the `length` should be the remaining duration of the
simulation. Note that each instance of a simulation must be restored
individually.
