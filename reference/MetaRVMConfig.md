# MetaRVM Configuration Class

R6 class to handle MetaRVM configuration data with validation and
methods. This class encapsulates all configuration parameters needed for
MetaRVM simulations, providing methods for parameter access, validation,
and introspection.

## Details

The MetaRVMConfig class stores parsed configuration data from YAML files
and provides structured access to simulation parameters. It
automatically validates configuration completeness and provides
convenient methods for accessing demographic categories,
initialization-derived population metadata, and other simulation
settings.

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

- [`MetaRVMConfig$get_category_names()`](#method-MetaRVMConfig-get_category_names)

- [`MetaRVMConfig$get_category_values()`](#method-MetaRVMConfig-get_category_values)

- [`MetaRVMConfig$get_all_categories()`](#method-MetaRVMConfig-get_all_categories)

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

data.table containing population_id and user-defined demographic
category columns

------------------------------------------------------------------------

### Method `get_category_names()`

Get names of all category columns

#### Usage

    MetaRVMConfig$get_category_names()

#### Details

Category columns are automatically detected from the initialization CSV
file. Any column that is not a reserved column (population_id, N, S0,
I0, R0, V0, etc.) is treated as a demographic category (e.g., age, zone,
income_level, occupation).

#### Returns

Character vector of category column names, or empty vector if no
categories

#### Examples

    \dontrun{
    config <- MetaRVMConfig$new("config.yaml")
    category_names <- config$get_category_names()  # e.g., c("age", "zone", "risk_group")
    }

------------------------------------------------------------------------

### Method `get_category_values()`

Get unique values for a specific category

#### Usage

    MetaRVMConfig$get_category_values(category_name)

#### Arguments

- `category_name`:

  Character string specifying the category name

#### Returns

Character/numeric vector of unique values for the specified category

#### Examples

    \dontrun{
    config <- MetaRVMConfig$new("config.yaml")
    ages <- config$get_category_values("age")  # if age is defined
    income_levels <- config$get_category_values("income_level")  # if defined
    }

------------------------------------------------------------------------

### Method `get_all_categories()`

Get all categories as a named list

#### Usage

    MetaRVMConfig$get_all_categories()

#### Returns

Named list where names are category column names and values are vectors
of unique values for each category. Returns empty list if no categories.

#### Examples

    \dontrun{
    config <- MetaRVMConfig$new("config.yaml")
    all_cats <- config$get_all_categories()
    # Returns: list(age = c("0-17", "18-64", "65+"), risk_group = c("low", "high"), ...)
    }

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

# Get demographic category names (user-defined)
category_names <- config$get_category_names()  # e.g., c("age", "zone", "risk_group")

# Get values for specific categories
ages <- config$get_category_values("age")

# Get all categories as a named list
all_categories <- config$get_all_categories()


## ------------------------------------------------
## Method `MetaRVMConfig$get_category_names`
## ------------------------------------------------

if (FALSE) { # \dontrun{
config <- MetaRVMConfig$new("config.yaml")
category_names <- config$get_category_names()  # e.g., c("age", "zone", "risk_group")
} # }

## ------------------------------------------------
## Method `MetaRVMConfig$get_category_values`
## ------------------------------------------------

if (FALSE) { # \dontrun{
config <- MetaRVMConfig$new("config.yaml")
ages <- config$get_category_values("age")  # if age is defined
income_levels <- config$get_category_values("income_level")  # if defined
} # }

## ------------------------------------------------
## Method `MetaRVMConfig$get_all_categories`
## ------------------------------------------------

if (FALSE) { # \dontrun{
config <- MetaRVMConfig$new("config.yaml")
all_cats <- config$get_all_categories()
# Returns: list(age = c("0-17", "18-64", "65+"), risk_group = c("low", "high"), ...)
} # }
```
