# Format MetaRVM simulation output

This function formats raw MetaRVM simulation output by:

1.  Converting time steps to calendar dates

2.  Adding demographic attributes from population mapping

3.  Handling different disease states appropriately:

    - Regular states (S, E, I, etc.): Keep values at integer time points

    - New count states (n\_ prefix): Sum pairs to get daily counts

## Usage

``` r
format_metarvm_output(sim_output, config)
```

## Arguments

- sim_output:

  data.table containing raw simulation output

- config:

  MetaRVMConfig object or config list containing parameters

## Value

data.table with formatted output including calendar dates and
demographics
