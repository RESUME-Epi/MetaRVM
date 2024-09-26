
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MetaRVM

<!-- badges: start -->
<!-- badges: end -->

The goal of MetaRVM is to …

## Installation

You can install the development version of MetaRVM from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("NSF-RESUME/MetaRVM")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(MetaRVM)
## basic example code
```

``` r
## Model initialization
N_pop <- 6
pop_init <- data.frame(N = rep(NA, N_pop),
                       S0 = rep(NA, N_pop),
                       I0 = rep(NA, N_pop),
                       V0 = rep(NA, N_pop))

pop_init$N <- sample(1000:1500, N_pop)
pop_init$I0 <- sample(10:100, N_pop)
pop_init$V0 <- sample(1:10, N_pop)
pop_init$S0 <- pop_init$N - pop_init$I0 - pop_init$V0

pop_zones <- pop_init

## Vaccine profile
tt <- seq(0, 100, by = 14)
nt <- length(tt)
vol <- sample(1:10, (nt-1)*N_pop, replace = TRUE)

vac_df <- cbind(tt, rbind(pop_init$V0, matrix(vol, nrow = nt-1)))
colnames(vac_df) <- c("t", paste0("v", 1:N_pop))

vac_zones <- vac_df

## mixing matrix

m1 <- m2 <- m3 <- m4 <- matrix(NA, nrow = N_pop, ncol = N_pop)
diag(m1) <- round(runif(N_pop, min = 0.85, max = 0.92), 2)
diag(m2) <- 1
diag(m3) <- round(runif(N_pop, min = 0.7, max = 0.9), 2)
diag(m4) <- 1

for(ii in 1:N_pop){
  tmp <- runif(N_pop - 1)
  m1[ii, -ii] <- round(tmp * (1 - m1[ii, ii]) / sum(tmp), 2)
  m2[ii, -ii] <- 0
  m3[ii, -ii] <- round(tmp * (1 - m3[ii, ii]) / sum(tmp), 2)
  m4[ii, -ii] <- 0
}

m_weekday_day <- m1
m_weekday_night <- m2
m_weekend_day <- m3
m_weekend_night <- m4
```

Then run the simulation:

``` r
out <- meta_sim(N_pop = N_pop,
                beta_i = 0.7,
                beta_v = 0.2,
                S0 = pop_zones$S0,
                I0 = pop_zones$I0,
                P0 = pop_zones$N,
                V0 = pop_zones$V0,
                m_weekday_day = m_weekday_day,
                m_weekend_day = m_weekend_day,
                m_weekday_night = m_weekday_night,
                m_weekend_night = m_weekend_night,
                delta_t = 0.5,
                tvac = vac_zones[, 1],
                vac_mat = as.matrix(vac_zones[, 2:7]),
                VtoS = 0.02,
                 EtoIpresymp = 0.33,
                 etopa = 0.5,
                 pretoIsymp = 0.5,
                 IasymptoR = 0.2,
                 IsymptoRH = 0.5,
                 istohr = 0.5,
                 HtoRD = 0.33,
                 htor = 0.5,
                 RtoS = 0.02,
                 vac_eff = 0.5,
                 nsteps = 100,
                 is.stoch = 0,
                 seed = NULL)
#> Loading required namespace: pkgbuild
#> Generating model in c
#> ℹ Re-compiling odin0d0f68f3 (debug build)
#> ── R CMD INSTALL ───────────────────────────────────────────────────────────────
#> * installing *source* package ‘odin0d0f68f3’ ...
#> ** using staged installation
#> ** libs
#> gcc -I"/usr/share/R/include" -DNDEBUG      -fpic  -g -O2 -ffile-prefix-map=/build/r-base-4A2Reg/r-base-4.1.2=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -UNDEBUG -Wall -pedantic -g -O0 -c odin.c -o odin.o
#> gcc -I"/usr/share/R/include" -DNDEBUG      -fpic  -g -O2 -ffile-prefix-map=/build/r-base-4A2Reg/r-base-4.1.2=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -UNDEBUG -Wall -pedantic -g -O0 -c registration.c -o registration.o
#> gcc -shared -L/usr/lib/R/lib -Wl,-Bsymbolic-functions -flto=auto -ffat-lto-objects -flto=auto -Wl,-z,relro -o odin0d0f68f3.so odin.o registration.o -L/usr/lib/R/lib -lR
#> installing to /tmp/RtmpOvThtK/devtools_install_3d47f64447ad0/00LOCK-file3d47f2349f1ab/00new/odin0d0f68f3/libs
#> ** checking absolute paths in shared objects and dynamic libraries
#> * DONE (odin0d0f68f3)
#> ℹ Loading odin0d0f68f3
```
