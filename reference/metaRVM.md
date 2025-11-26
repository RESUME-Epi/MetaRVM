# Run MetaRVM Epidemic Simulation

Executes a meta-population compartmental epidemic model simulation using
specified configuration parameters. The function runs multiple
simulation instances with stochastic parameter variations and returns
formatted results with calendar dates and demographic attributes for
comprehensive analysis and visualization. Under the hood, it calls
[`meta_sim`](https://RESUME-Epi.github.io/MetaRVM/reference/meta_sim.md)
function after parsing the inputs.

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

**Disease Parameters:** The model uses the following key parameters (can
be stochastic across instances):

- `ts`: Transmission rate for symptomatic individuals

- `tv`: Transmission rate for vaccinated individuals

- `ve`: Vaccine effectiveness

- `de, dp, da, ds, dh, dr`: Duration parameters for disease states

- `pea, psr, phr`: Proportion parameters for state transitions

**Simulation Process:** For each simulation instance, the function:

1.  Initializes population compartments from configuration

2.  Applies instance-specific stochastic parameter values

3.  Runs the ODE solver
    ([`meta_sim`](https://RESUME-Epi.github.io/MetaRVM/reference/meta_sim.md))
    with specified time steps

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

- **Vaccination schedule**: Time-varying vaccination rates

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
example_config <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
# Basic usage with YAML configuration file
results <- metaRVM(example_config)
#> Generating model in c
#> Using cached model

# Print summary
results
#> MetaRVM Results Object
#> =====================
#> Instances: 1 
#> Populations: 24 
#> Date range: 2023-10-01 to 2024-02-27 
#> Total observations: 111600 
#> Disease states: D, E, H, I_all, I_asymp, I_eff, I_presymp, I_symp, P, R, S, V, cum_V, mob_pop, n_EI, n_EIpresymp, n_HD, n_HR, n_HRD, n_IasympR, n_IsympH, n_IsympR, n_IsympRH, n_SE, n_SV, n_VE, n_VS, n_preIsymp, p_HRD, p_SE, p_VE 

# Access formatted data directly
head(results$results)
#>          date    age   race   zone disease_state        value instance
#>        <Date> <char> <char> <char>        <char>        <num>    <int>
#> 1: 2023-10-01   0-17      A     11             D 2.252583e-04        1
#> 2: 2023-10-01   0-17      A     11             E 1.305178e+01        1
#> 3: 2023-10-01   0-17      A     11             H 2.304447e-01        1
#> 4: 2023-10-01   0-17      A     11         I_all 2.731688e+01        1
#> 5: 2023-10-01   0-17      A     11       I_asymp 3.227854e-01        1
#> 6: 2023-10-01   0-17      A     11         I_eff 2.647304e+01        1

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
#> 19723 
#> 19783 

# Using with pre-parsed configuration
config_obj <- parse_config(example_config, return_object = TRUE)
results <- metaRVM(config_obj)
#> Generating model in c
#> Using cached model

# Accessing run metadata
results$run_info$n_instances
#> [1] 1
results$run_info$date_range
#> [1] "2023-10-01" "2024-02-27"


```
