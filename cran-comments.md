## Submission

This is a new submission of MetaRVM (2.1.0 -> 2.2.0, last accepted version on CRAN was 2.1.0).

### What changed

- Added a measles metapopulation model (`meta_measles_sim()`, `metaRVM_measles()`,
  `parse_config_measles()`) using a two-stage latent/infectious compartmental
  structure with quarantined and unquarantined infectious tracks.
- Introduced a disease registry architecture (`disease_registry.R`) so
  disease-specific parsing, engine selection, and output specification are
  declared in a single registry; adding a new disease requires no changes to
  core orchestration code.
- `model.disease` is now a required field in configuration YAML files
  (supported values: `"flu"`, `"measles"`); configs without it produce an
  informative error.
- Renamed the internal disease identifier from `"rvm"` to `"flu"` (existing
  YAML files must add `model:\n  disease: flu`).
- Removed `simulation_mode`; all simulations now run stochastically. The field
  is silently ignored if present in YAML for backward compatibility.
- Parallel execution now falls back to sequential automatically when no
  registered `foreach` backend is found or parallel execution fails.
- Added support for distribution specs in `sub_disease_params`.
- Updated documentation, vignettes, and pkgdown site to match current
  behavior.

### Checks

R CMD check was run with:

- `devtools::check()`

Results:
- 0 errors | 0 warnings | 0 notes