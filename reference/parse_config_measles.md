# Type-safe wrapper that validates `model.disease = "measles"` before delegating to [`parse_config()`](https://RESUME-Epi.github.io/MetaRVM/reference/parse_config.md). Use [`parse_config()`](https://RESUME-Epi.github.io/MetaRVM/reference/parse_config.md) directly instead; it dispatches on disease automatically via the disease registry.

Type-safe wrapper that validates `model.disease = "measles"` before
delegating to
[`parse_config()`](https://RESUME-Epi.github.io/MetaRVM/reference/parse_config.md).
Use
[`parse_config()`](https://RESUME-Epi.github.io/MetaRVM/reference/parse_config.md)
directly instead; it dispatches on disease automatically via the disease
registry.

## Usage

``` r
parse_config_measles(config_file, return_object = FALSE)
```
