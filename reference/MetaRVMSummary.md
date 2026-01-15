# MetaRVM Summary Class

R6 class for summarized MetaRVM results with plotting capabilities and
method chaining support. This class stores summarized simulation data
and provides visualization methods that automatically adapt based on the
data structure and grouping variables.

## Details

The MetaRVMSummary class is designed to work seamlessly with method
chaining from MetaRVMResults. It stores either summary statistics (mean,
median, quantiles, etc.) or individual instance data, and provides
intelligent plotting methods that automatically determine appropriate
visualizations based on the data structure and demographic groupings.

The class supports two data types:

- **Summary data**: Contains aggregated statistics across simulation
  instances

- **Instance data**: Contains individual trajectory data for each
  simulation instance

Plotting behavior adapts automatically:

- Single grouping variable: Facets by demographic category, colors by
  disease state

- Two grouping variables: Grid layout with both demographics as facet
  dimensions

- Three grouping variables: Grid layout with first two as facets, third
  as color

## Public Fields

- `data`:

  data.table containing summarized results

- `config`:

  MetaRVMConfig object from original simulation

- `type`:

  Character string indicating data type ("summary" or "instances")

## Author

Arindam Fadikar

## Public fields

- `data`:

  Summarized data

- `config`:

  Original MetaRVMConfig object

- `type`:

  Type of summary ("instances" or "summary")

## Methods

### Public methods

- [`MetaRVMSummary$new()`](#method-MetaRVMSummary-new)

- [`MetaRVMSummary$print()`](#method-MetaRVMSummary-print)

- [`MetaRVMSummary$plot()`](#method-MetaRVMSummary-plot)

- [`MetaRVMSummary$clone()`](#method-MetaRVMSummary-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize MetaRVMSummary object

#### Usage

    MetaRVMSummary$new(data, config, type)

#### Arguments

- `data`:

  data.table containing summarized or instance data

- `config`:

  MetaRVMConfig object from original simulation

- `type`:

  Character string indicating data type ("summary" or "instances")

#### Returns

New MetaRVMSummary object (invisible)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print summary of the data object

#### Usage

    MetaRVMSummary$print()

#### Returns

Self (invisible)

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Plot method that shows median with quantile bands

#### Usage

    MetaRVMSummary$plot(ci_level = 0.95, theme = theme_minimal(), title = NULL)

#### Arguments

- `ci_level`:

  Confidence level for empirical quantiles (default: 0.95). Only used if
  quantile columns are not pre-specified

- `theme`:

  ggplot2 theme function (default: theme_minimal())

- `title`:

  Optional custom plot title

#### Details

This method creates time series plots with automatic layout adaptation
based on grouping variables:

- For summary data: Shows median lines with quantile confidence bands

- Automatically determines faceting strategy based on number of grouping
  variables

- Uses disease states for color differentiation when appropriate

The method requires specific data structure:

- Summary data must contain 'median_value' and quantile columns (e.g.,
  'q25', 'q75')

- Instance data must contain 'instance' column for individual trajectory
  grouping

#### Returns

ggplot object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MetaRVMSummary$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \donttest{
options(odin.verbose = FALSE)
example_config <- system.file("extdata", "example_config_dist.yaml", package = "MetaRVM")
# Run simulation
results <- metaRVM(example_config)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
#> Unused equations: beta_v, dim_beta_v
#>  beta_v[] <- user() # (line 108)
#>  dim(beta_v) <- N_pop # (line 145)
# Typically created through method chaining
summary_obj <- results$subset_data(disease_state = "H")$summarize(
  group_by = c("age", "race"), 
  stats = c("median", "quantile"),
  quantiles = c(0.25, 0.75)
)

# Direct plotting
summary_obj$plot()


# Plot with custom ggplot theme and confidence level
summary_obj$plot(theme = ggplot2::theme_bw())

# }
```
