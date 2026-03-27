# Getting Started with MetaRVM

## Introduction

MetaRVM is a comprehensive R package for simulating respiratory virus
epidemics using meta-population compartmental models. This vignette
describes the basic usage of the package.

## Installation

The development version of MetaRVM can be installed from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("RESUME-Epi/MetaRVM")
```

## Loading the Package

``` r
library(MetaRVM)
library(ggplot2)
options(odin.verbose = FALSE)
```

## Basic Example

This example shows how to run a basic meta-population simulation.

The `metaRVM` package includes a set of example files in its `extdata`
directory. To run the example, these files must first be located. The
[`system.file()`](https://rdrr.io/r/base/system.file.html) function in R
is the recommended way to do this, as it finds the files wherever the
package is installed.

``` r
# Locate the example YAML configuration file
yaml_file <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
print(yaml_file)
#> [1] "/home/runner/work/_temp/Library/MetaRVM/extdata/example_config.yaml"
```

The `yaml_file` variable now holds the full path to the example
configuration file. This file is set up to use the other example data
files (also in the `extdata` directory) with relative paths. Below is
the content of the yaml file.

``` yaml
run_id: ExampleRun
population_data:
  initialization: population_init_n24.csv
  vaccination: vaccination_n24.csv
mixing_matrix:
  weekday_day: m_weekday_day.csv
  weekday_night: m_weekday_night.csv
  weekend_day: m_weekend_day.csv
  weekend_night: m_weekend_night.csv
disease_params:
  ts: 0.5
  ve: 0.4
  dv: 180
  dp: 1
  de: 3
  da: 5
  ds: 6
  dh: 8
  dr: 180
  pea: 0.3
  psr: 0.95
  phr: 0.97
simulation_config:
  start_date: 01/01/2023 # m/d/Y
  length: 150
  nsim: 1
  nrep: 1
  simulation_mode: deterministic
  random_seed: 42
```

For a detailed explanation of all the configuration options, please see
the `yaml-configuration.html` vignette.

## Running the Simulation

Once the path to the configuration file is available, the simulation can
be run using the
[`metaRVM()`](https://RESUME-Epi.github.io/MetaRVM/reference/metaRVM.md)
function.

``` r
# Load the metaRVM library
library(MetaRVM)

# Run the simulation
sim_out <- metaRVM(yaml_file)
#> Loading required namespace: pkgbuild
#> Generating model in c
#> ℹ Re-compiling odinf64d486c (debug build)
#> ℹ Loading odinf64d486c
```

``` r
print(sim_out)
#> MetaRVM Results Object
#> =====================
#> Instances: 1 
#> Populations: 
#> Date range: 2023-10-01 to 2024-02-27 
#> Parameter sets (nsim): 1 
#> Replicates per set (nrep): 1 
#> Simulation mode: deterministic 
#> Total observations: 388800 
#> Disease states: D, E, H, I_all, I_asymp, I_eff, I_presymp, I_symp, P, R, S, S_alloc, S_eff_prod, S_src_int, V, V_alloc, V_src_int, cum_V, mob_pop, n_EI, n_EIpresymp, n_HD, n_HR, n_HRD, n_IasympR, n_IsympH, n_IsympR, n_IsympRH, n_RS, n_SE, n_SE_eff, n_SV, n_VE, n_VS, n_preIsymp, p_HRD, p_RS, p_SE, p_VE
head(sim_out$results)
#>          date    age   race  zone disease_state        value instance
#>        <Date> <char> <char> <int>        <char>        <num>    <int>
#> 1: 2023-10-01   0-17      A    11             D 2.252583e-04        1
#> 2: 2023-10-01   0-17      A    11             E 1.365434e+01        1
#> 3: 2023-10-01   0-17      A    11             H 2.304447e-01        1
#> 4: 2023-10-01   0-17      A    11         I_all 2.742619e+01        1
#> 5: 2023-10-01   0-17      A    11       I_asymp 3.555784e-01        1
#> 6: 2023-10-01   0-17      A    11         I_eff 2.483657e+01        1
```

For more details on running `metaRVM`, refer to the
`running-a-simulation.html` vignette.
