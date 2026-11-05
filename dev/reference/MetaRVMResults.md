# MetaRVM Results Class

R6 class to handle MetaRVM simulation results with comprehensive
analysis and visualization methods. This class stores formatted
simulation results and provides methods for data summarization,
subsetting, and visualization with flexible demographic groupings.

## Details

The MetaRVMResults class automatically formats raw simulation output
upon initialization, converting time steps to calendar dates and adding
demographic attributes. It provides methods for flexible data
summarization across any user-defined demographic categories, plus
method chaining for streamlined analysis workflows.

## Author

Arindam Fadikar

## Public fields

- `config`:

  MetaRVMConfig object used to generate these results

- `results`:

  data.table containing formatted simulation results

- `run_info`:

  List containing run metadata

## Methods

### Public methods

- [`MetaRVMResults$new()`](#method-MetaRVMResults-new)

- [`MetaRVMResults$print()`](#method-MetaRVMResults-print)

- [`MetaRVMResults$plot()`](#method-MetaRVMResults-plot)

- [`MetaRVMResults$subset_data()`](#method-MetaRVMResults-subset_data)

- [`MetaRVMResults$summarize()`](#method-MetaRVMResults-summarize)

- [`MetaRVMResults$get_instance_manifest()`](#method-MetaRVMResults-get_instance_manifest)

- [`MetaRVMResults$get_instance_params()`](#method-MetaRVMResults-get_instance_params)

- [`MetaRVMResults$clone()`](#method-MetaRVMResults-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new MetaRVMResults object

#### Usage

    MetaRVMResults$new(
      raw_results,
      config,
      run_info = NULL,
      formatted_results = NULL
    )

#### Arguments

- `raw_results`:

  Raw simulation results data.table

- `config`:

  MetaRVMConfig object used for the simulation

- `run_info`:

  Optional metadata about the run

- `formatted_results`:

  formatted simulation results data.table

#### Returns

New MetaRVMResults object (invisible)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print summary of results

#### Usage

    MetaRVMResults$print()

#### Returns

Self (invisible)

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Plot simulation trajectories directly from results

#### Usage

    MetaRVMResults$plot(
      group_by = character(0),
      disease_states = NULL,
      date_range = NULL,
      instances = NULL,
      stats = NULL,
      quantiles = c(0.25, 0.75),
      exclude_p_columns = TRUE,
      ci_level = 0.95,
      theme = theme_minimal(),
      title = NULL
    )

#### Arguments

- `group_by`:

  Vector of demographic category names to group/facet by

- `disease_states`:

  Optional disease states to include

- `date_range`:

  Optional date range for filtering

- `instances`:

  Optional instance IDs to include

- `stats`:

  Statistics for summary plotting. If NULL, plots raw trajectories.

- `quantiles`:

  Quantiles for uncertainty bands when summary plotting

- `exclude_p_columns`:

  Logical, whether to exclude p\_ columns (default: TRUE)

- `ci_level`:

  Confidence level label used in summary plot title

- `theme`:

  ggplot2 theme function (default: theme_minimal())

- `title`:

  Optional custom title

#### Returns

ggplot object

------------------------------------------------------------------------

### Method `subset_data()`

Subset the data based on any combination of parameters

#### Usage

    MetaRVMResults$subset_data(
      ...,
      disease_states = NULL,
      date_range = NULL,
      instances = NULL,
      exclude_p_columns = TRUE
    )

#### Arguments

- `...`:

  Named arguments for category filters (e.g., age = c("0-17"), income =
  c("low", "high"))

- `disease_states`:

  Vector of disease states to include (default: all, excludes p\_
  columns)

- `date_range`:

  Vector of two dates start_date, and end_date for filtering (default:
  all)

- `instances`:

  Vector of instance numbers to include (default: all)

- `exclude_p_columns`:

  Logical, whether to exclude p\_ columns (default: TRUE)

#### Returns

MetaRVMResults object with subset of results

------------------------------------------------------------------------

### Method `summarize()`

Summarize results across specified demographic characteristics

#### Usage

    MetaRVMResults$summarize(
      group_by,
      disease_states = NULL,
      date_range = NULL,
      stats = c("mean", "median", "sd"),
      quantiles = c(0.25, 0.75),
      exclude_p_columns = TRUE
    )

#### Arguments

- `group_by`:

  Vector of demographic category names to group by. Must be valid
  category names from the configuration (e.g., c("age", "zone"),
  c("income_level", "occupation")). Use config\$get_category_names() to
  see available categories.

- `disease_states`:

  Vector of disease states to include (default: all, excludes p\_
  columns)

- `date_range`:

  Optional date range for filtering

- `stats`:

  Vector of statistics to calculate: c("mean", "median", "sd", "min",
  "max", "sum", "quantile"). If NULL, returns all instances

- `quantiles`:

  Vector of quantiles to calculate if "quantile" is in stats (default:
  c(0.25, 0.75))

- `exclude_p_columns`:

  Logical, whether to exclude p\_ columns (default: TRUE)

#### Returns

data.table with summarized time series data or all instances if stats =
NULL

------------------------------------------------------------------------

### Method `get_instance_manifest()`

Get per-instance simulation manifest (parameter-set id, replicate id,
seed, and settings)

#### Usage

    MetaRVMResults$get_instance_manifest(instances = NULL, include_sim_args = TRUE)

#### Arguments

- `instances`:

  Optional vector of instance ids to filter

- `include_sim_args`:

  Logical. If FALSE, omits the `sim_args` list-column for a lighter
  table

#### Returns

data.table with one row per simulation instance

------------------------------------------------------------------------

### Method `get_instance_params()`

Get per-instance settings table with parameters list-column (`params`)

#### Usage

    MetaRVMResults$get_instance_params(instances = NULL)

#### Arguments

- `instances`:

  Optional vector of instance ids to filter

#### Returns

data.table with instance mapping and exact parameter values used in
simulation

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MetaRVMResults$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \donttest{
options(odin.verbose = FALSE)
example_config <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
# Run simulation
results_obj <- metaRVM(example_config)
#> Loading required namespace: pkgbuild
# Access formatted results
head(results_obj$results)
#>          date    age   race  zone disease_state    value instance
#>        <Date> <char> <char> <int>        <char>    <num>    <int>
#> 1: 2023-10-01   0-17      A    11             D  0.00000        1
#> 2: 2023-10-01   0-17      A    11             E 19.00000        1
#> 3: 2023-10-01   0-17      A    11             H  1.00000        1
#> 4: 2023-10-01   0-17      A    11         I_all 24.00000        1
#> 5: 2023-10-01   0-17      A    11       I_asymp  1.00000        1
#> 6: 2023-10-01   0-17      A    11         I_eff 22.50998        1

# Subset data with multiple filters
subset_data <- results_obj$subset_data(
  age = c("18-64", "65+"), 
  disease_states = c("H", "D"),
  date_range = c(as.Date("2024-01-01"), as.Date("2024-02-01"))
)

# Method chaining for analysis and visualization
results_obj$subset_data(disease_states = "H")$summarize(
  group_by = c("age", "zone"), 
  stats = c("median", "quantile"),
  quantiles = c(0.25, 0.75)
)$plot()

# }
```
