
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MetaRVM <img src="man/figures/logo.png" width="170px" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/NSF-RESUME/MetaRVM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NSF-RESUME/MetaRVM/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This is a compartmental model simulation code for generic respiratory
virus diseases.

## Background

`MetaRVM` is an open-source R package for modeling the spread of
infectious diseases in subpopulations, which can be flexibly defined by
geography, demographics, or other stratifications. It is designed to
support real-time public health decision-making. `MetaRVM` is a
metapopulation model, which extends the classic
Susceptible-Infected-Recovered (SIR) framework by propagating infection
across interacting subpopulations (e.g., age groups, neighborhoods),
whose interactions are governed by realistic mixing patterns.

The `MetaRVM` model builds upon the SEIR framework by introducing
additional compartments to capture more detailed dynamics of disease
progression, while allowing for heterogeneous mixing among different
demographic stratum. These generalizations allow the model to account
for factors such as vaccinations, hospitalizations, and fatalities.

For more details, please refer to the paper: [Developing and deploying a
use-inspired metapopulation modeling framework for detailed tracking of
stratified health
outcomes](https://www.medrxiv.org/content/10.1101/2025.05.05.25327021v1.full-text)

## Documentation

Full documentation is available at:
<https://RESUME-Epi.github.io/MetaRVM/>

<figure>
<img src="man/figures/GIRD-Vensim.svg" alt="Model schematics" />
<figcaption aria-hidden="true">Model schematics</figcaption>
</figure>

## Installation

You can install the development version of MetaRVM from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("NSF-RESUME/MetaRVM")
```
