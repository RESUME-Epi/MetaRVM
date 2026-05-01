# MetaRVM 2.2.0

* Added measles metapopulation model (`meta_measles_sim()`, `metaRVM_measles()`,
  `parse_config_measles()`). The measles model uses a two-stage latent /
  two-stage infectious compartmental structure (E1, E2, I1_Q, I1_U, I2_Q, I2_U)
  with quarantined and unquarantined infectious tracks.

* Introduced disease registry architecture (`disease_registry.R`). Disease-specific
  parsing, engine selection, and output specification are now declared in a single
  registry. Adding a new disease requires only a registry entry and builder
  functions, with no changes to the core orchestration code.

* `model.disease` is now a required field in all configuration YAML files (nested
  under the `model:` section). Supported values: `"flu"`, `"measles"`. Configs
  without this field produce an informative error listing valid options.

* Renamed the internal disease identifier from `"rvm"` to `"flu"`. Existing YAML
  files must add `model:\n  disease: flu` to remain valid.

* Removed `simulation_mode`. All simulations run stochastically. The field is
  silently ignored if present in YAML for backward compatibility.

* Parallel execution falls back to sequential automatically when no registered
  `foreach` backend is found or when parallel execution fails. Execution mode and
  worker count are reported in `run_info`.

# MetaRVM 2.1.0

* Added `simulation_mode` (`deterministic`/`stochastic`) and `nrep` support in
  configuration parsing and run metadata.

* Improved stochastic reproducibility: provided `random_seed` is honored, and a
  seed is generated/stored when missing.

* Updated stochastic transition splitting to use binomial draws for integer
  compartment flows (`n_EIpresymp`, `n_IsympH`, `n_HR`).

* Improved metapopulation infection-flow indexing consistency for destination-
  specific force of infection in `n_SE_eff` and `n_VE_eff`.

* Added bounded vaccination and transition safeguards to reduce overdraw risks
  (for example, capping applied vaccination by available susceptible count and
  guarding zero effective population in force-of-infection calculations).

* Added integer row allocation for stochastic mixing flows (`S` and `V`) using
  floor-plus-residual adjustment so row totals are conserved.

* Added regression tests for stochastic seed behavior, `nsim * nrep` instance
  counts, and row-allocation conservation checks for `S`/`V` mixing.

* Enhanced results helper APIs:
  - `MetaRVMResults$plot()` now provides direct trajectory plotting before
    summarization.
  - `MetaRVMSummary$plot()` now supports instance-trajectory plotting.
  - `summarize()` uses a more efficient single-pass grouped aggregation.

# MetaRVM 2.0.0

* Subpopulation categories are now user-defined from
  `population_data$initialization`; non-reserved columns are detected
  automatically.

* Improved checkpoint restore behavior in `parse_config()`: when
  `restore_from` is provided, `population_data$initialization` can be used as
  mapping-only metadata (without `N`, `S0`, `I0`, etc.).

* Improved validation messages for category filters: invalid category names now
  list available categories, and invalid category values now list valid values
  for the selected category.

* Improved `format_metarvm_output()` handling of `population_id` and dynamic
  column selection for user-defined categories.

* Updated documentation and vignettes to reflect user-defined subpopulation
  categories and checkpoint restore behavior.

# MetaRVM 1.0.1

# MetaRVM 1.0.0

* Initial CRAN submission.
