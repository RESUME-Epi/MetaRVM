# Type-safe wrapper that validates `model.disease = "measles"` before delegating to [`metaRVM()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/metaRVM.md). Use [`metaRVM()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/metaRVM.md) directly instead; it dispatches on disease automatically via the disease registry.

Type-safe wrapper that validates `model.disease = "measles"` before
delegating to
[`metaRVM()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/metaRVM.md).
Use
[`metaRVM()`](https://RESUME-Epi.github.io/MetaRVM/dev/reference/metaRVM.md)
directly instead; it dispatches on disease automatically via the disease
registry.

## Usage

``` r
metaRVM_measles(config_input)
```
