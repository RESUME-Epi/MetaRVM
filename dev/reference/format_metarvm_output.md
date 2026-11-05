# Format raw MetaRVM simulation output

Converts raw step-indexed output from
[`meta_sim()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/meta_sim.md)
or
[`meta_measles_sim()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/meta_measles_sim.md)
into a tidy long-format `data.table` with calendar dates and
user-defined demographic attributes.

The transformation differs by state type:

- **Compartment states** (S, E, I, H, R, D, V, …): only integer
  time-step values are retained.

- **Flow states** (`n_` prefix): within-day pairs are summed to daily
  counts.

## Usage

``` r
format_metarvm_output(sim_output, config)
```

## Arguments

- sim_output:

  `data.table` of raw simulation output from
  [`meta_sim()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/meta_sim.md)
  or
  [`meta_measles_sim()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/meta_measles_sim.md).
  Must contain columns `time`, `disease_state`, `population_id`,
  `value`, and `instance`.

- config:

  A
  [`MetaRVMConfig`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.md)
  object or a named list. Must supply `start_date`, `pop_map` (with a
  `population_id` column), and `category_names`.

## Value

A `data.table` in long format with one row per (date, demographic
combination, disease state, simulation instance). Columns:

- `date`:

  `Date`. Calendar date derived from `start_date + time`.

- User category columns:

  One column per demographic category detected in the population CSV
  (e.g., `age`, `zone`, `race`).

- `disease_state`:

  `character`. Compartment or flow-count state label.

- `value`:

  `numeric`. Compartment count or summed daily flow count.

- `instance`:

  `integer`. Simulation instance index.

## See also

[`metaRVM()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/metaRVM.md),
[`meta_sim()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/meta_sim.md),
[`MetaRVMResults`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMResults.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Typically called internally by metaRVM(); exposed for custom pipelines.
cfg <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
config <- parse_config(cfg)
raw <- meta_sim(...)  # low-level engine output
formatted <- format_metarvm_output(raw, config)
} # }
```
