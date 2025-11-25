# MetaRVM Checkpoint Class

R6 class to handle MetaRVM checkpoint data. This class is a simplified
version of
[MetaRVMConfig](https://RESUME-Epi.github.io/MetaRVM/reference/MetaRVMConfig.md)
tailored for storing and accessing simulation checkpoints.

## Details

The `MetaRVMCheck` class is designed to hold the state of a simulation
at a specific time point, allowing for continuation or analysis. It
stores all necessary parameters and population states.

## Author

Arindam Fadikar

## Super class

[`MetaRVM::MetaRVMConfig`](https://RESUME-Epi.github.io/MetaRVM/reference/MetaRVMConfig.md)
-\> `MetaRVMCheck`

## Public fields

- `check_data`:

  List containing all parsed checkpoint data

## Methods

### Public methods

- [`MetaRVMCheck$new()`](#method-MetaRVMCheck-new)

- [`MetaRVMCheck$clone()`](#method-MetaRVMCheck-clone)

Inherited methods

- [`MetaRVM::MetaRVMConfig$get()`](https://RESUME-Epi.github.io/MetaRVM/reference/MetaRVMConfig.html#method-get)
- [`MetaRVM::MetaRVMConfig$get_age_categories()`](https://RESUME-Epi.github.io/MetaRVM/reference/MetaRVMConfig.html#method-get_age_categories)
- [`MetaRVM::MetaRVMConfig$get_all()`](https://RESUME-Epi.github.io/MetaRVM/reference/MetaRVMConfig.html#method-get_all)
- [`MetaRVM::MetaRVMConfig$get_pop_map()`](https://RESUME-Epi.github.io/MetaRVM/reference/MetaRVMConfig.html#method-get_pop_map)
- [`MetaRVM::MetaRVMConfig$get_race_categories()`](https://RESUME-Epi.github.io/MetaRVM/reference/MetaRVMConfig.html#method-get_race_categories)
- [`MetaRVM::MetaRVMConfig$get_zones()`](https://RESUME-Epi.github.io/MetaRVM/reference/MetaRVMConfig.html#method-get_zones)
- [`MetaRVM::MetaRVMConfig$list_parameters()`](https://RESUME-Epi.github.io/MetaRVM/reference/MetaRVMConfig.html#method-list_parameters)
- [`MetaRVM::MetaRVMConfig$parameter_summary()`](https://RESUME-Epi.github.io/MetaRVM/reference/MetaRVMConfig.html#method-parameter_summary)
- [`MetaRVM::MetaRVMConfig$print()`](https://RESUME-Epi.github.io/MetaRVM/reference/MetaRVMConfig.html#method-print)
- [`MetaRVM::MetaRVMConfig$set()`](https://RESUME-Epi.github.io/MetaRVM/reference/MetaRVMConfig.html#method-set)

------------------------------------------------------------------------

### Method `new()`

Initialize a new MetaRVMCheck object

#### Usage

    MetaRVMCheck$new(input)

#### Arguments

- `input`:

  A list containing checkpoint data.

#### Returns

A new `MetaRVMCheck` object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MetaRVMCheck$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
