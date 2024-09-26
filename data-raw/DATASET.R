## code to prepare sample datasets goes here


## =============================================================================
## only zone based sub-population

set.seed(2)

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
t <- seq(0, 100, by = 14)
nt <- length(t)
vol <- sample(1:10, (nt-1)*N_pop, replace = TRUE)

vac_df <- cbind(t, rbind(pop_init$V0, matrix(vol, nrow = nt-1)))
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

usethis::use_data(pop_zones, overwrite = TRUE)
usethis::use_data(vac_zones, overwrite = TRUE)
usethis::use_data(m_weekday_day, overwrite = TRUE)
usethis::use_data(m_weekday_night, overwrite = TRUE)
usethis::use_data(m_weekend_day, overwrite = TRUE)
usethis::use_data(m_weekend_night, overwrite = TRUE)
