test_that("meta_sim conserves population (no death) and produces no negative values", {
  # skip_on_cran()   # odin compilation can slow down CRAN

  # ---- Minimal 1-pop deterministic model with no deaths ----
  N_pop   <- 1
  nsteps  <- 20
  delta_t <- 0.5

  # Initial conditions
  S0 <- 900
  I0 <- 100
  R0 <- 0
  P0 <- S0 + I0 + R0
  H0 <- 0
  D0 <- 0
  Ia0 <- 0
  Ip0 <- 0
  E0  <- 0
  V0  <- 0

  # Mixing = identity (single population)
  M <- matrix(1, 1, 1)

  # No vaccination over time
  vac_mat <- matrix(0, nrow = nsteps + 1, ncol = 1 + N_pop)
  vac_mat[, 1] <- 0:nsteps   # required time index column

  # Run model (deterministic)
  res <- meta_sim(
    N_pop = N_pop,

    ts = 0.5,
    tv = 0.0,

    S0 = S0,
    I0 = I0,
    R0 = R0,
    P0 = P0,
    H0 = H0,
    D0 = D0,
    Ia0 = Ia0,
    Ip0 = Ip0,
    E0 = E0,
    V0 = V0,

    m_weekday_day   = M,
    m_weekday_night = M,
    m_weekend_day   = M,
    m_weekend_night = M,

    delta_t = delta_t,
    vac_mat = vac_mat,

    # disease parameters (deterministic)
    dv  = 365,
    de  = 3,
    pea = 0.3,
    dp  = 2,
    da  = 7,
    ds  = 7,

    psr = 1.0,   # symptomatic → recovery (no symptomatic death)
    dh  = 10,
    phr = 1.0,   # hospitalized → recovery (no hospital death)

    dr = 180,
    ve = 0.0,

    nsteps   = nsteps,
    is.stoch = FALSE
  )

  # ---------------------------------------------------------
  # 1. Structure check
  # ---------------------------------------------------------
  expect_s3_class(res, "data.table")
  expect_true(all(c("step", "time", "disease_state", "population_id", "value") %in% names(res)))

  # ---------------------------------------------------------
  # 2. No negative values in any compartment
  # ---------------------------------------------------------
  expect_true(all(res$value >= 0),
              info = "meta_sim output contains negative compartment values")

  # ---------------------------------------------------------
  # 3. No deaths occur (D compartment stays zero)
  # ---------------------------------------------------------
  d_vals <- res[disease_state == "D", value]
  if (length(d_vals) > 0) {
    expect_true(all(d_vals == 0))
  }

  # ---------------------------------------------------------
  # 4. Total living population (P) stays constant at P0
  # ---------------------------------------------------------
  p_vals <- res[disease_state == "P", value]
  expect_gt(length(p_vals), 0)
  expect_true(all(abs(p_vals - P0) < 1e-6),
              info = "Total population P does not remain constant under no-death scenario")
})


test_that("meta_sim handles multi-pop mixing and conserves P per population (no death)", {
  skip_on_cran()

  N_pop   <- 3
  nsteps  <- 20
  delta_t <- 0.5

  # Initial conditions per population
  S0 <- c(900, 800, 700)
  I0 <- c(100, 50,  25)
  R0 <- c(0,   0,   0)
  P0 <- S0 + I0 + R0

  H0 <- rep(0, N_pop)
  D0 <- rep(0, N_pop)
  Ia0 <- rep(0, N_pop)
  Ip0 <- rep(0, N_pop)
  E0  <- rep(0, N_pop)
  V0  <- rep(0, N_pop)

  # Nontrivial mixing matrix
  M <- matrix(c(
    0.7, 0.2, 0.1,
    0.2, 0.6, 0.2,
    0.1, 0.3, 0.6
  ), nrow = N_pop, byrow = TRUE)

  # No vaccination over time
  vac_mat <- matrix(0, nrow = nsteps + 1, ncol = 1 + N_pop)
  vac_mat[, 1] <- 0:nsteps

  res <- meta_sim(
    N_pop = N_pop,

    ts = rep(0.5, N_pop),
    tv = rep(0.0, N_pop),

    S0 = S0,
    I0 = I0,
    R0 = R0,
    P0 = P0,
    H0 = H0,
    D0 = D0,
    Ia0 = Ia0,
    Ip0 = Ip0,
    E0  = E0,
    V0  = V0,

    m_weekday_day   = M,
    m_weekday_night = M,
    m_weekend_day   = M,
    m_weekend_night = M,

    delta_t = delta_t,
    vac_mat = vac_mat,

    dv  = 365,
    de  = 3,
    pea = 0.3,
    dp  = 2,
    da  = 7,
    ds  = 7,

    psr = 1.0,   # no death via symptomatic
    dh  = 10,
    phr = 1.0,   # no death via hospital

    dr = 180,
    ve = 0.0,

    nsteps   = nsteps,
    is.stoch = FALSE
  )

  expect_s3_class(res, "data.table")
  expect_true(all(c("step", "time", "disease_state", "population_id", "value") %in% names(res)))

  # For each population, total P should remain equal to its initial P0
  P_vals <- res[disease_state == "P",
                .(P = unique(value)),
                by = population_id]

  expect_equal(
    P_vals[order(population_id)]$P,
    P0[order(seq_len(N_pop))],
    tolerance = 1e-6
  )
})

test_that("meta_sim compartment sums equal total living population P (no death)", {
  skip_on_cran()

  N_pop   <- 3
  nsteps  <- 20
  delta_t <- 0.5

  S0 <- c(900, 800, 700)
  I0 <- c(100, 50,  25)
  R0 <- c(0,   0,   0)
  P0 <- S0 + I0 + R0

  H0 <- rep(0, N_pop)
  D0 <- rep(0, N_pop)
  Ia0 <- rep(0, N_pop)
  Ip0 <- rep(0, N_pop)
  E0  <- rep(0, N_pop)
  V0  <- rep(0, N_pop)

  M <- matrix(c(
    0.7, 0.2, 0.1,
    0.2, 0.6, 0.2,
    0.1, 0.3, 0.6
  ), nrow = N_pop, byrow = TRUE)

  vac_mat <- matrix(0, nrow = nsteps + 1, ncol = 1 + N_pop)
  vac_mat[, 1] <- 0:nsteps

  res <- meta_sim(
    N_pop = N_pop,

    ts = rep(0.5, N_pop),
    tv = rep(0.0, N_pop),

    S0 = S0,
    I0 = I0,
    R0 = R0,
    P0 = P0,
    H0 = H0,
    D0 = D0,
    Ia0 = Ia0,
    Ip0 = Ip0,
    E0  = E0,
    V0  = V0,

    m_weekday_day   = M,
    m_weekday_night = M,
    m_weekend_day   = M,
    m_weekend_night = M,

    delta_t = delta_t,
    vac_mat = vac_mat,

    dv  = 365,
    de  = 3,
    pea = 0.3,
    dp  = 2,
    da  = 7,
    ds  = 7,

    psr = 1.0,
    dh  = 10,
    phr = 1.0,

    dr = 180,
    ve = 0.0,

    nsteps   = nsteps,
    is.stoch = FALSE
  )

  # Sum all *living* compartments: everything except P and D
  living_sum <- res[disease_state %in% c("S", "V", "E", "I_asymp", "I_presymp", "I_symp", "H", "R"),
                    .(sum_living = sum(value)),
                    by = .(time, population_id)]

  P_vals <- res[disease_state == "P",
                .(P = unique(value)),
                by = .(time, population_id)]

  merged <- merge(living_sum, P_vals,
                  by = c("time", "population_id"),
                  all = FALSE)

  
  # sums must match P for each time, pop
  expect_true(all(abs(merged$sum_living - merged$P) < 1e-6))
})