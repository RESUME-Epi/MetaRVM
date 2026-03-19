## Resubmission

This is a resubmission of MetaRVM.

### What changed

- Updated subpopulation handling so demographic categories are user-defined from `population_data$initialization`.
- Improved checkpoint restore parsing:
  - restore mode now accepts mapping-only initialization files (no `N`, `S0`, `I0`, etc. required).
- Improved input validation/error messages:
  - invalid category names now report available categories;
  - invalid category values now report valid values.
- Updated documentation and vignettes to match current behavior.
- Updated CITATION format for proper rendering.
- Added `.Rbuildignore` rules to exclude `.orig` and temporary YAML files in `inst/extdata`.

### Checks

R CMD check was run with:

- `devtools::check()`

Results:
- 0 errors | 0 warnings | 0 notes
