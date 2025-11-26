# MetaRVM Results Class

R6 class to handle MetaRVM simulation results with comprehensive
analysis and visualization methods. This class stores formatted
simulation results and provides methods for data summarization,
subsetting, and visualization with flexible demographic groupings.

## Details

The MetaRVMResults class automatically formats raw simulation output
upon initialization, converting time steps to calendar dates and adding
demographic attributes. It provides methods for flexible data
summarization across any combination of age, race, and geographic zone
categories, plus method chaining for streamlined analysis workflows.

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

- [`MetaRVMResults$subset_data()`](#method-MetaRVMResults-subset_data)

- [`MetaRVMResults$summarize()`](#method-MetaRVMResults-summarize)

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

### Method `subset_data()`

Subset the data based on any combination of parameters

#### Usage

    MetaRVMResults$subset_data(
      ages = NULL,
      races = NULL,
      zones = NULL,
      disease_states = NULL,
      date_range = NULL,
      instances = NULL,
      exclude_p_columns = TRUE
    )

#### Arguments

- `ages`:

  Vector of age categories to include (default: all)

- `races`:

  Vector of race categories to include (default: all)

- `zones`:

  Vector of zones to include (default: all)

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

  Vector of demographic variables to group by: c("age", "race", "zone")

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

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MetaRVMResults$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
example_config <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
# Run simulation
results_obj <- metaRVM(example_config)
#> Loading required namespace: pkgbuild
#> Generating model in c
#> ℹ Re-compiling odin838871ae (debug build)
#> ── R CMD INSTALL ───────────────────────────────────────────────────────────────
#> * installing *source* package ‘odin838871ae’ ...
#> ** this is package ‘odin838871ae’ version ‘0.0.1’
#> ** using staged installation
#> ** libs
#> using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
#> gcc -std=gnu2x -I"/opt/R/4.5.2/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -UNDEBUG -Wall -pedantic -g -O0 -fdiagnostics-color=always -c odin.c -o odin.o
#> odin.c: In function ‘user_get_scalar_int’:
#> odin.c:1770:47: warning: format ‘%d’ expects argument of type ‘int’, but argument 2 has type ‘const char *’ [-Wformat=]
#>  1770 |       Rf_error("Expected scalar integer for '%d'", name);
#>       |                                              ~^    ~~~~
#>       |                                               |    |
#>       |                                               int  const char *
#>       |                                              %s
#> odin.c: In function ‘user_get_array’:
#> odin.c:1928:48: warning: format ‘%d’ expects argument of type ‘int’, but argument 2 has type ‘size_t’ {aka ‘long unsigned int’} [-Wformat=]
#>  1928 |         Rf_error("Incorrect size of dimension %d of %s (expected %d)",
#>       |                                               ~^
#>       |                                                |
#>       |                                                int
#>       |                                               %ld
#>  1929 |                  i + 1, name, dim_expected);
#>       |                  ~~~~~                          
#>       |                    |
#>       |                    size_t {aka long unsigned int}
#> odin.c: In function ‘interpolate_check_y’:
#> odin.c:2007:45: warning: format ‘%d’ expects argument of type ‘int’, but argument 3 has type ‘size_t’ {aka ‘long unsigned int’} [-Wformat=]
#>  2007 |       Rf_error("Expected %s to have length %d (for '%s')",
#>       |                                            ~^
#>       |                                             |
#>       |                                             int
#>       |                                            %ld
#>  2008 |                name_arg, nx, name_target);
#>       |                          ~~                  
#>       |                          |
#>       |                          size_t {aka long unsigned int}
#> odin.c:2011:37: warning: format ‘%d’ expects argument of type ‘int’, but argument 2 has type ‘size_t’ {aka ‘long unsigned int’} [-Wformat=]
#>  2011 |       Rf_error("Expected dimension %d of %s to have size %d (for '%s')",
#>       |                                    ~^
#>       |                                     |
#>       |                                     int
#>       |                                    %ld
#>  2012 |                i, name_arg, nx, name_target);
#>       |                ~                     
#>       |                |
#>       |                size_t {aka long unsigned int}
#> odin.c:2011:59: warning: format ‘%d’ expects argument of type ‘int’, but argument 4 has type ‘size_t’ {aka ‘long unsigned int’} [-Wformat=]
#>  2011 |       Rf_error("Expected dimension %d of %s to have size %d (for '%s')",
#>       |                                                          ~^
#>       |                                                           |
#>       |                                                           int
#>       |                                                          %ld
#>  2012 |                i, name_arg, nx, name_target);
#>       |                             ~~                             
#>       |                             |
#>       |                             size_t {aka long unsigned int}
#> gcc -std=gnu2x -I"/opt/R/4.5.2/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -UNDEBUG -Wall -pedantic -g -O0 -fdiagnostics-color=always -c registration.c -o registration.o
#> gcc -std=gnu2x -shared -L/opt/R/4.5.2/lib/R/lib -L/usr/local/lib -o odin838871ae.so odin.o registration.o -L/opt/R/4.5.2/lib/R/lib -lR
#> installing to /tmp/RtmpKBILgz/devtools_install_183b5f4348fa/00LOCK-file183b457f3ae7/00new/odin838871ae/libs
#> ** checking absolute paths in shared objects and dynamic libraries
#> * DONE (odin838871ae)
#> ℹ Loading odin838871ae
# Access formatted results
head(results_obj$results)
#>          date    age   race   zone disease_state        value instance
#>        <Date> <char> <char> <char>        <char>        <num>    <int>
#> 1: 2023-10-01   0-17      A     11             D 2.252583e-04        1
#> 2: 2023-10-01   0-17      A     11             E 1.305178e+01        1
#> 3: 2023-10-01   0-17      A     11             H 2.304447e-01        1
#> 4: 2023-10-01   0-17      A     11         I_all 2.731688e+01        1
#> 5: 2023-10-01   0-17      A     11       I_asymp 3.227854e-01        1
#> 6: 2023-10-01   0-17      A     11         I_eff 2.647304e+01        1

# Subset data with multiple filters
subset_data <- results_obj$subset_data(
  age = c("18-49", "50-64"), 
  disease_states = c("H", "D"),
  date_range = c(as.Date("2024-01-01"), as.Date("2024-02-01"))
)
#> 19723 
#> 19754 

# Method chaining for analysis and visualization
results_obj$summarize(
  group_by = c("age", "race"), 
  stats = c("median", "quantile"),
  quantiles = c(0.25, 0.75)
)$plot()
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the MetaRVM package.
#>   Please report the issue at <https://github.com/RESUME-Epi/MetaRVM/issues>.


```
