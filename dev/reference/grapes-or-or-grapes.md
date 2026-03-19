# NULL Coalescing Operator

Returns the left-hand side if it's not NULL, otherwise returns the
right-hand side. This is a utility function used internally by MetaRVM
classes.

## Usage

``` r
x %||% y
```

## Arguments

- x:

  Left-hand side value

- y:

  Right-hand side value (default/fallback)

## Value

x if x is not NULL, otherwise y

## Examples

``` r
if (FALSE) { # \dontrun{
user_title <- "User Title"
# Internal usage in classes
title <- user_title %||% "Default Title"
} # }
```
