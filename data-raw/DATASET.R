## code to prepare sample datasets goes here


## =============================================================================
## Three way demographics

## This script generates a sample input data for meta population model
## The stratification is based on age, race, HCEZ

## Model initialization
N_total <-  2719822

ages <- data.frame(cat = c("0-4", "5-19", "20-49", "50-79", "80-"), prop = c(0.0005,	0.1826,	0.3505,	0.4453,	0.0211))
races <- data.frame(cat = c("Hispanic", "Asian", "Black", "White", "Other"),
                    prop = c(0.2299, 0.1261, 0.2632, 0.2877, 0.0981))
zones <- data.frame(cat = c("North/Central", "Near South", "Northwest", "Southwest",
                            "West", "Far South"),
                    prop = c(0.1703,	0.1254,	0.1928,	0.2286,	0.0687,	0.2143))

all_groups <- expand.grid(ages$cat, races$cat, zones$cat)
all_groups$prop <- apply(expand.grid(ages$prop, races$prop, zones$prop), 1, prod)
all_groups$prop <- all_groups$prop / sum(all_groups$prop)
sum(all_groups$prop)

N_pop <- nrow(all_groups)
pop_init <- data.frame(population_id = 1:N_pop,
                       N = rep(NA, N_pop),
                       S0 = rep(NA, N_pop),
                       I0 = rep(NA, N_pop),
                       V0 = rep(NA, N_pop))
pop_init$N <- floor(N_total * all_groups$prop)
pop_init$I0 <- floor(pop_init$N * runif(N_pop, 0, 0.01))
pop_init$V0 <- floor(pop_init$N * runif(N_pop, 0, 0.05))
pop_init$S0 <- pop_init$N - pop_init$I0 - pop_init$V0

pop_zones <- pop_init
# write.csv(pop_zones, file = "~/Documents/temp/pop_init_150.csv", row.names = F)

## Vaccine profile
# t <- seq(0, 100, by = 14)
tt <- seq(0, 30, by = 2)
nt <- length(tt)
vol <- sample(1:10, (nt-1)*N_pop, replace = TRUE)

vac_df <- cbind(tt, rbind(pop_init$V0, matrix(vol, nrow = nt-1, ncol = N_pop)))
colnames(vac_df) <- c("t", paste0("pop_", 1:N_pop))

vac_zones <- vac_df
# write.csv(vac_zones, file = "~/Documents/temp/vac_150.csv", row.names = F)

## mixing matrix

m1 <- m2 <- m3 <- m4 <- matrix(NA, nrow = N_pop, ncol = N_pop)
diag(m1) <- round(runif(N_pop, min = 0.95, max = 0.98), 2)
# diag(m1) <- 1
diag(m2) <- 1
diag(m3) <- round(runif(N_pop, min = 0.95, max = 0.98), 2)
# diag(m3) <- 1
diag(m4) <- 1

for(ii in 1:N_pop){
  tmp <- runif(N_pop - 1)
  m1[ii, -ii] <- round(tmp * (1 - m1[ii, ii]) / sum(tmp), 10)
  m2[ii, -ii] <- 0
  m3[ii, -ii] <- round(tmp * (1 - m3[ii, ii]) / sum(tmp), 10)
  m4[ii, -ii] <- 0
}

# check constraints
# rowSums(m1)
# rowSums(m2)
# rowSums(m3)
# rowSums(m4)


m_weekday_day <- m1
m_weekday_night <- m2
m_weekend_day <- m3
m_weekend_night <- m4



# set.seed(2)
#
# ## Model initialization
# N_pop <- 6
# pop_init <- data.frame(N = rep(NA, N_pop),
#                        S0 = rep(NA, N_pop),
#                        I0 = rep(NA, N_pop),
#                        V0 = rep(NA, N_pop))
#
# pop_init$N <- sample(1000:1500, N_pop)
# pop_init$I0 <- sample(10:100, N_pop)
# pop_init$V0 <- sample(1:10, N_pop)
# pop_init$S0 <- pop_init$N - pop_init$I0 - pop_init$V0
#
# pop_zones <- pop_init
#
# ## Vaccine profile
# t <- seq(0, 100, by = 14)
# nt <- length(t)
# vol <- sample(1:10, (nt-1)*N_pop, replace = TRUE)
#
# vac_df <- cbind(t, rbind(pop_init$V0, matrix(vol, nrow = nt-1)))
# colnames(vac_df) <- c("t", paste0("v", 1:N_pop))
#
# vac_zones <- vac_df
#
# ## mixing matrix
#
# m1 <- m2 <- m3 <- m4 <- matrix(NA, nrow = N_pop, ncol = N_pop)
# diag(m1) <- round(runif(N_pop, min = 0.85, max = 0.92), 2)
# diag(m2) <- 1
# diag(m3) <- round(runif(N_pop, min = 0.7, max = 0.9), 2)
# diag(m4) <- 1
#
# for(ii in 1:N_pop){
#   tmp <- runif(N_pop - 1)
#   m1[ii, -ii] <- round(tmp * (1 - m1[ii, ii]) / sum(tmp), 2)
#   m2[ii, -ii] <- 0
#   m3[ii, -ii] <- round(tmp * (1 - m3[ii, ii]) / sum(tmp), 2)
#   m4[ii, -ii] <- 0
# }
#
# m_weekday_day <- m1
# m_weekday_night <- m2
# m_weekend_day <- m3
# m_weekend_night <- m4

usethis::use_data(pop_zones, overwrite = TRUE)
usethis::use_data(vac_zones, overwrite = TRUE)
usethis::use_data(m_weekday_day, overwrite = TRUE)
usethis::use_data(m_weekday_night, overwrite = TRUE)
usethis::use_data(m_weekend_day, overwrite = TRUE)
usethis::use_data(m_weekend_night, overwrite = TRUE)
