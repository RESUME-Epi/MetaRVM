# Read and prepare vaccination data

This function takes the vaccination schedule given by a data.table and
prepares it according to the required structure needed in
[`meta_sim()`](https://RESUME-Epi.github.io/MetaRVM/reference/meta_sim.md)
function

## Usage

``` r
process_vac_data(vac_dt, sim_start_date, sim_length, delta_t)
```

## Arguments

- vac_dt:

  A data.table of vaccination schedule

- sim_start_date:

  A calendar date in the format yyyy-mm-dd

- sim_length:

  Number of calendar days that the simulation runs for

- delta_t:

  Step size in the simulation

## Value

A data.table
