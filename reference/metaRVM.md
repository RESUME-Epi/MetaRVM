# Run MetaRVM Epidemic Simulation

Executes a meta-population compartmental epidemic model simulation using
specified configuration parameters. The function runs multiple
simulation instances with stochastic parameter variations and returns
formatted results with calendar dates and demographic attributes for
comprehensive analysis and visualization.

## Usage

``` r
metaRVM(config_input)
```

## Arguments

- config_input:

  Configuration input in one of three formats:

  - Character string: File path to YAML configuration file

  - MetaRVMConfig object: Pre-initialized configuration object

  - Named list: Parsed configuration data from
    [`parse_config`](https://RESUME-Epi.github.io/MetaRVM/reference/parse_config.md)

## Value

Returns a
[`MetaRVMResults`](https://RESUME-Epi.github.io/MetaRVM/reference/MetaRVMResults.md)
object containing:

results

:   Formatted data.table with columns:

config

:   Original MetaRVMConfig object

run_info

:   Simulation metadata including date range, instance count

The returned object supports method chaining for analysis:

- `subset_data()`: Filter by demographics, disease states, dates

- `summarize()`: Aggregate across demographic categories with statistics

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html): Create time
  series visualizations (via method chaining)

## Details

The MetaRVM simulation implements a meta-population SEIRD
(Susceptible-Exposed-Infected-Recovered-Dead) compartmental model with
additional complexity for asymptomatic and presymptomatic infections,
hospitalization states, and vaccination dynamics. The model accounts
for:

**Compartmental Structure:**

- **S**: Susceptible individuals

- **E**: Exposed (incubating) individuals

- **I_asymp**: Asymptomatic infectious individuals

- **I_presymp**: Presymptomatic infectious individuals

- **I_symp**: Symptomatic infectious individuals

- **H**: Hospitalized individuals

- **R**: Recovered individuals

- **D**: Dead individuals

- **P**: Protected/vaccinated individuals

**Model Features:**

- Multiple population groups with demographic stratification (age, race,
  geography)

- Time-varying contact patterns (weekday/weekend, day/night mixing
  matrices)

- Stochastic parameter variations across simulation instances

- Vaccination schedules with time-varying administration

- Checkpointing capability for long simulations

**Disease Parameters:** The model uses the following key parameters (can
be stochastic across instances):

- `ts`: Transmission rate for symptomatic individuals

- `tv`: Transmission rate for vaccinated individuals

- `ve`: Vaccine effectiveness

- `de, dp, da, ds, dh, dr`: Duration parameters for disease states

- `pea, psr, phr`: Probability parameters for state transitions

**Simulation Process:** For each simulation instance, the function:

1.  Initializes population compartments from configuration

2.  Applies instance-specific stochastic parameter values

3.  Runs the ODE solver (`meta_sim`) with specified time steps

4.  Collects output for all time points and populations

5.  Combines results across all instances

6.  Formats output with calendar dates and demographic information

7.  Returns structured MetaRVMResults object for analysis

## Configuration Requirements

The configuration must include:

- **Population data**: Initial compartment values, demographic mapping

- **Disease parameters**: Transmission rates, durations, probabilities

- **Contact matrices**: Weekday/weekend and day/night mixing patterns

- **Simulation settings**: Start date, length, number of instances

- **Vaccination schedule**: Time-varying vaccination rates (optional)

## Performance Notes

- Simulation time scales with `nsim × nsteps × N_pop`

- Large population numbers or long time periods may require substantial
  memory

- Consider checkpointing (`do_chk = TRUE`) for long simulations

- Output formatting adds overhead but provides analysis-ready data
  structure

## Error Handling

The function validates input configuration and will stop with
informative messages for:

- Invalid `config_input` type or format

- Missing required configuration parameters

- Inconsistent population or parameter dimensions

- File access issues for configuration or checkpoint files

## References

Fadikar, A., et al. "Developing and deploying a use-inspired
metapopulation modeling framework for detailed tracking of stratified
health outcomes"

## See also

[`parse_config`](https://RESUME-Epi.github.io/MetaRVM/reference/parse_config.md)
for configuration file parsing
[`MetaRVMConfig`](https://RESUME-Epi.github.io/MetaRVM/reference/MetaRVMConfig.md)
for configuration object class
[`MetaRVMResults`](https://RESUME-Epi.github.io/MetaRVM/reference/MetaRVMResults.md)
for results object and analysis methods
[`format_metarvm_output`](https://RESUME-Epi.github.io/MetaRVM/reference/format_metarvm_output.md)
for output formatting details
[`meta_sim`](https://RESUME-Epi.github.io/MetaRVM/reference/meta_sim.md)
for the underlying ODE simulation engine

## Author

Arindam Fadikar, Charles Macal, Ignacio Martinez-Moyano, Jonathan Ozik

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with YAML configuration file
results <- metaRVM("path/to/config.yaml")

# Print summary
results

# Access formatted data directly
head(results$results)

# Method chaining for analysis and visualization
results$summarize(
  group_by = c("age", "race"),
  stats = c("median", "quantile"),
  disease_states = c("H", "D")
)$plot()

# Subset and analyze specific populations
subset_results <- results$subset_data(
  age = c("65+"),
  disease_states = c("H", "D"),
  date_range = c(as.Date("2024-01-01"), as.Date("2024-03-01"))
)

# Using with pre-parsed configuration
config <- parse_config("config.yaml")
config_obj <- MetaRVMConfig$new(config)
results <- metaRVM(config_obj)

# Accessing run metadata
results$run_info$n_instances
results$run_info$date_range
} # }
```
