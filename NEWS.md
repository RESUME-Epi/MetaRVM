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
