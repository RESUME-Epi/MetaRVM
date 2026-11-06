# Copy a MetaRVMConfig object

Returns an independent deep copy of a `MetaRVMConfig` object. Because R6
objects use reference semantics, assignment (`b <- a`) creates a second
handle to the *same* object. Use `copy_config()` when you need a truly
independent copy that can be modified without affecting the original.

## Usage

``` r
copy_config(config)
```

## Arguments

- config:

  A `MetaRVMConfig` object.

## Value

A new `MetaRVMConfig` object with the same configuration data.

## Examples

``` r
if (FALSE) { # \dontrun{
config  <- parse_config("my_config.yaml")
config2 <- copy_config(config)
config2$set("nsim", 50L)   # does not affect config
} # }
```
