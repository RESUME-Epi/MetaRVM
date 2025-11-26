# MetaRVM Configuration Class

R6 class to handle MetaRVM configuration data with validation and
methods. This class encapsulates all configuration parameters needed for
MetaRVM simulations, providing methods for parameter access, validation,
and introspection.

## Details

The MetaRVMConfig class stores parsed configuration data from YAML files
and provides structured access to simulation parameters. It
automatically validates configuration completeness and provides
convenient methods for accessing demographic categories, population
mappings, and other simulation settings.

## Author

Arindam Fadikar

## Public fields

- `config_file`:

  Path to the original YAML config file (if applicable)

- `config_data`:

  List containing all parsed configuration parameters

## Methods

### Public methods

- [`MetaRVMConfig$new()`](#method-MetaRVMConfig-new)

- [`MetaRVMConfig$get()`](#method-MetaRVMConfig-get)

- [`MetaRVMConfig$get_all()`](#method-MetaRVMConfig-get_all)

- [`MetaRVMConfig$list_parameters()`](#method-MetaRVMConfig-list_parameters)

- [`MetaRVMConfig$parameter_summary()`](#method-MetaRVMConfig-parameter_summary)

- [`MetaRVMConfig$set()`](#method-MetaRVMConfig-set)

- [`MetaRVMConfig$print()`](#method-MetaRVMConfig-print)

- [`MetaRVMConfig$get_pop_map()`](#method-MetaRVMConfig-get_pop_map)

- [`MetaRVMConfig$get_age_categories()`](#method-MetaRVMConfig-get_age_categories)

- [`MetaRVMConfig$get_race_categories()`](#method-MetaRVMConfig-get_race_categories)

- [`MetaRVMConfig$get_zones()`](#method-MetaRVMConfig-get_zones)

- [`MetaRVMConfig$clone()`](#method-MetaRVMConfig-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new MetaRVMConfig object

#### Usage

    MetaRVMConfig$new(input)

#### Arguments

- `input`:

  Either a file path (character) or parsed config list

#### Returns

New MetaRVMConfig object (invisible)

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Get a configuration parameter

#### Usage

    MetaRVMConfig$get(param)

#### Arguments

- `param`:

  Parameter name

#### Returns

The requested parameter value

------------------------------------------------------------------------

### Method `get_all()`

Get all configuration parameters as a list

#### Usage

    MetaRVMConfig$get_all()

#### Returns

Named list of all configuration parameters

------------------------------------------------------------------------

### Method `list_parameters()`

List all available parameter names

#### Usage

    MetaRVMConfig$list_parameters()

#### Returns

Character vector of parameter names

------------------------------------------------------------------------

### Method `parameter_summary()`

Show summary of parameter types and sizes

#### Usage

    MetaRVMConfig$parameter_summary()

#### Returns

Data frame with parameter information

------------------------------------------------------------------------

### Method `set()`

Set a configuration parameter

#### Usage

    MetaRVMConfig$set(param, value)

#### Arguments

- `param`:

  Character string. Parameter name to set

- `value`:

  The value to assign to the parameter

#### Returns

Self (invisible) for method chaining

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print summary of configuration

#### Usage

    MetaRVMConfig$print()

#### Returns

Self (invisible)

------------------------------------------------------------------------

### Method `get_pop_map()`

Get population mapping data

#### Usage

    MetaRVMConfig$get_pop_map()

#### Returns

data.table containing population mapping with demographic categories

------------------------------------------------------------------------

### Method `get_age_categories()`

Get available age categories

#### Usage

    MetaRVMConfig$get_age_categories()

#### Returns

Character vector of unique age categories, or NULL if no population
mapping available

------------------------------------------------------------------------

### Method `get_race_categories()`

Get available race categories

#### Usage

    MetaRVMConfig$get_race_categories()

#### Returns

Character vector of unique race categories, or NULL if no population
mapping available

------------------------------------------------------------------------

### Method `get_zones()`

Get available zones

#### Usage

    MetaRVMConfig$get_zones()

#### Returns

Character vector of unique zone identifiers, or NULL if no population
mapping available

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MetaRVMConfig$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Initialize from YAML file
example_config <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
config <- MetaRVMConfig$new(example_config)

# Access parameters
config$get("N_pop")
#> [1] 24
config$get("start_date")
#> [1] "2023-09-30"

# Get demographic categories
ages <- config$get_age_categories()
races <- config$get_race_categories()
zones <- config$get_zones()
```
