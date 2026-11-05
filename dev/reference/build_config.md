# Build a MetaRVM configuration

Constructs a MetaRVM configuration programmatically. The argument
structure mirrors the YAML configuration file format. The result can be
returned as a raw list, written to a YAML file, or parsed directly into
a
[MetaRVMConfig](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.md)
object — or any combination of the three.

## Usage

``` r
build_config(
  disease,
  population_init,
  mixing_matrices,
  disease_params,
  start_date,
  sim_length,
  nsim = 1L,
  nrep = 1L,
  run_id = NULL,
  vaccination = NULL,
  sub_disease_params = NULL,
  random_seed = NULL,
  delta_t = NULL,
  parallel = FALSE,
  checkpoint_dir = NULL,
  checkpoint_dates = NULL,
  restore_from = NULL,
  output_file = NULL,
  return_object = FALSE,
  verbose = FALSE,
  suppress_odin_messages = FALSE
)
```

## Arguments

- disease:

  Character. Disease to simulate. Supported values: `"flu"`,
  `"measles"`. The vaccination mechanism differs by disease: flu reads a
  time-varying schedule from the `vaccination` CSV argument; measles
  encodes vaccination as initial compartment counts (`V10`, `V20`)
  directly in the population CSV — no separate vaccination file is used.

- population_init:

  Character. Path to the population initialization CSV.

- mixing_matrices:

  Named list of paths to mixing-matrix CSV files. Required keys:
  `weekday_day`, `weekday_night`, `weekend_day`, `weekend_night`.

- disease_params:

  Named list of disease parameters. Each element may be a scalar, a
  per-population vector, or a distribution-spec list such as
  `list(dist = "lognormal", mu = -0.7, sd = 0.1)`.

- start_date:

  Character (`"MM/DD/YYYY"`) or
  [Date](https://rdrr.io/r/base/Dates.html). Simulation start date.

- sim_length:

  Positive integer. Number of days to simulate.

- nsim:

  Positive integer. Number of parameter draws (default `1`).

- nrep:

  Positive integer. Replicates per parameter draw (default `1`).

- run_id:

  Optional character identifier stored in run metadata. Auto-generated
  from the current timestamp when `NULL`.

- vaccination:

  Optional character. Path to the vaccination schedule CSV. Used by flu
  only; ignored for measles (which encodes vaccination as `V10`/`V20`
  columns in the population CSV).

- sub_disease_params:

  Optional named list of per-category parameter overrides. Structure
  mirrors the `sub_disease_params:` YAML section, e.g.
  `list(age = list(child = list(ts = 0.6)))`. Values can also be
  distribution specs, e.g. `list(dist = "lognormal", mu = 2, sd = 0.5)`,
  drawn independently per simulation replicate the same way
  `disease_params` distributions are.

- random_seed:

  Optional integer seed for reproducibility. Auto-generated when `NULL`.

- delta_t:

  Numeric time-step in days. Defaults to `0.5` when `NULL`.

- parallel:

  Logical. Whether
  [`metaRVM()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/metaRVM.md)
  may use a parallel foreach backend. Default `FALSE`. Set to `TRUE`
  only at the top-level call; leave `FALSE` when the config will be used
  inside a parallel worker to prevent nested-parallel conflicts.

- checkpoint_dir:

  Optional character. Directory for checkpoint files. Supported for both
  flu and measles.

- checkpoint_dates:

  Optional character vector of dates (`"MM/DD/YYYY"`) at which to write
  checkpoints. Supported for both flu and measles.

- restore_from:

  Optional character. Path to a checkpoint `.Rda` file to restore from.
  Supported for both flu and measles.

- output_file:

  Optional character. When provided the configuration is written to this
  path as a YAML file. Paths inside the config are stored as absolute
  paths so the file is self-contained regardless of working directory.

- return_object:

  Logical. When `TRUE` the configuration is parsed and returned as a
  [MetaRVMConfig](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.md)
  object. When `FALSE` (default) the raw configuration list is returned
  (invisibly when `output_file` is also given).

- verbose:

  Logical. Passed through to
  [`parse_config()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/parse_config.md)
  when `return_object = TRUE`.

- suppress_odin_messages:

  Logical. Passed through to
  [`parse_config()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/parse_config.md)
  when `return_object = TRUE`.

## Value

A
[MetaRVMConfig](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.md)
object when `return_object = TRUE`; otherwise the raw configuration list
(invisibly if `output_file` was also written).

## See also

[`parse_config()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/parse_config.md),
[`metaRVM()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/metaRVM.md)

## Examples

``` r
ext <- function(x) system.file("extdata", x, package = "MetaRVM")

# Return the raw config list for inspection / modification
cfg <- build_config(
  disease         = "flu",
  population_init = ext("population_init_n24.csv"),
  mixing_matrices = list(
    weekday_day   = ext("m_weekday_day.csv"),
    weekday_night = ext("m_weekday_night.csv"),
    weekend_day   = ext("m_weekend_day.csv"),
    weekend_night = ext("m_weekend_night.csv")
  ),
  disease_params  = list(
    ts = 0.5, ve = 0.4, dv = 180, dp = 1, de = 3,
    da = 5,   ds = 6,   dh = 8,   dr = 180,
    pea = 0.3, psr = 0.95, phr = 0.97
  ),
  vaccination     = ext("vaccination_n24.csv"),
  start_date      = "10/01/2023",
  sim_length      = 30L,
  nsim            = 2L
)

# Parse directly to a MetaRVMConfig object
cfg_obj <- build_config(
  disease         = "flu",
  population_init = ext("population_init_n24.csv"),
  mixing_matrices = list(
    weekday_day   = ext("m_weekday_day.csv"),
    weekday_night = ext("m_weekday_night.csv"),
    weekend_day   = ext("m_weekend_day.csv"),
    weekend_night = ext("m_weekend_night.csv")
  ),
  disease_params  = list(
    ts = 0.5, ve = 0.4, dv = 180, dp = 1, de = 3,
    da = 5,   ds = 6,   dh = 8,   dr = 180,
    pea = 0.3, psr = 0.95, phr = 0.97
  ),
  vaccination     = ext("vaccination_n24.csv"),
  start_date      = "10/01/2023",
  sim_length      = 30L,
  return_object   = TRUE
)
```
