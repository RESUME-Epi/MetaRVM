# MetaRVM Checkpoint Class

R6 class that stores a saved simulation state produced by
[`meta_sim()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/meta_sim.md).
Used to resume or branch a simulation from an intermediate time point.

## Details

Checkpoint objects are written automatically during a simulation run
when `do_chk = TRUE` is set in the simulation config. Each checkpoint
captures the full compartment state (S, E, I, H, R, D, V, …) and all
disease parameters at the saved time step, enabling a resumed run to
pick up exactly where it left off.

`MetaRVMCheck` extends
[`MetaRVMConfig`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.md):
all `$get()`, `$get_category_names()`, and related accessor methods are
inherited and work identically. Required checkpoint fields are
disease-specific and validated against the disease registry at
construction time.

## See also

[`metaRVM()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/metaRVM.md)
for enabling checkpointing via the `simulation_config` section;
[`MetaRVMConfig`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.md)
for the parent class and its accessor methods.

## Author

Arindam Fadikar

## Super class

[`MetaRVM::MetaRVMConfig`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.md)
-\> `MetaRVMCheck`

## Public fields

- `check_data`:

  List containing all parsed checkpoint data

## Methods

### Public methods

- [`MetaRVMCheck$new()`](#method-MetaRVMCheck-new)

- [`MetaRVMCheck$clone()`](#method-MetaRVMCheck-clone)

Inherited methods

- [`MetaRVM::MetaRVMConfig$get()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.html#method-get)
- [`MetaRVM::MetaRVMConfig$get_all()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.html#method-get_all)
- [`MetaRVM::MetaRVMConfig$get_all_categories()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.html#method-get_all_categories)
- [`MetaRVM::MetaRVMConfig$get_category_names()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.html#method-get_category_names)
- [`MetaRVM::MetaRVMConfig$get_category_values()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.html#method-get_category_values)
- [`MetaRVM::MetaRVMConfig$get_pop_map()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.html#method-get_pop_map)
- [`MetaRVM::MetaRVMConfig$list_parameters()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.html#method-list_parameters)
- [`MetaRVM::MetaRVMConfig$parameter_summary()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.html#method-parameter_summary)
- [`MetaRVM::MetaRVMConfig$print()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.html#method-print)
- [`MetaRVM::MetaRVMConfig$set()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.html#method-set)
- [`MetaRVM::MetaRVMConfig$set_delta_t()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.html#method-set_delta_t)
- [`MetaRVM::MetaRVMConfig$set_nrep()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.html#method-set_nrep)
- [`MetaRVM::MetaRVMConfig$set_parallel()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.html#method-set_parallel)
- [`MetaRVM::MetaRVMConfig$set_random_seed()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.html#method-set_random_seed)
- [`MetaRVM::MetaRVMConfig$set_run_id()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.html#method-set_run_id)
- [`MetaRVM::MetaRVMConfig$set_sim_length()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.html#method-set_sim_length)
- [`MetaRVM::MetaRVMConfig$set_start_date()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/MetaRVMConfig.html#method-set_start_date)

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

## Examples

``` r
if (FALSE) { # \dontrun{
# Checkpoints are created automatically when do_chk = TRUE in the config.
# To inspect a saved checkpoint:
chk_data <- readRDS("my_checkpoint.rds")
chk <- MetaRVMCheck$new(chk_data)
chk$get("chk_time_step")
chk$get("N_pop")
} # }
```
