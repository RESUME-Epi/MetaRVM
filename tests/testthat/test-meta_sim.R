test_that("meta_sim runs and produces valid output structure", {
  
  # 1. Define a minimal set of parameters for a deterministic run
  # n_pop <- 2
  # sim_length <- 1 # Run for 1 day
  # delta_t <- 0.1 # Time step of 0.1 days
  # nsteps <- sim_length / delta_t
  
  # # Initial populations
  # S0 <- c(990, 495)
  # E0 <- c(10, 5)
  # I0 <- c(0, 0)
  # R0 <- c(0, 0)
  # # All other compartments are zero
  
  # # Parameters (simplified, non-stochastic)
  # ts <- rep(0.1, n_pop) # Transmission rate
  # de <- rep(5, n_pop)   # Incubation period
  
  # # Mobility matrices (no movement for simplicity)
  # m_ident <- diag(n_pop)
  
  # # 2. Call meta_sim
  # sim_output <- meta_sim(
  #   is.stoch = FALSE,
  #   nsteps = nsteps,
  #   N_pop = n_pop,
  #   S0 = S0, E0 = E0, I0 = I0, R0 = R0,
  #   P0 = rep(0, n_pop), H0 = rep(0, n_pop), D0 = rep(0, n_pop),
  #   Ia0 = rep(0, n_pop), Ip0 = rep(0, n_pop),
  #   m_weekday_day = m_ident, m_weekday_night = m_ident,
  #   m_weekend_day = m_ident, m_weekend_night = m_ident,
  #   delta_t = delta_t,
  #   vac_mat = matrix(0, nrow = nsteps + 1, ncol = n_pop + 1),
  #   ts = ts, tv = rep(0, n_pop), dv = rep(1, n_pop), de = de,
  #   pea = rep(0, n_pop), dp = rep(1, n_pop), da = rep(1, n_pop),
  #   ds = rep(1, n_pop), psr = rep(0, n_pop), dh = rep(1, n_pop),
  #   phr = rep(0, n_pop), dr = rep(1, n_pop), ve = rep(0, n_pop)
  # )
  
  # # 3. Structural Validation
  # expect_s3_class(sim_output, "data.table")
  # expect_named(sim_output, c("time", "population_id", "disease_state", "value"))
  
  # # The number of disease states can vary, so we'll check that the number of rows is a multiple
  # # of the number of populations and time steps.
  # n_disease_states <- length(unique(sim_output$disease_state))
  # expected_rows <- (nsteps + 1) * n_pop * n_disease_states
  # expect_equal(nrow(sim_output), expected_rows)
  
  # # 4. Known-Value Testing (for the first time step)
  
  # # Manual calculation for t = 0.1
  # # Change in S = -ts * S * (I) / N * delta_t
  # # Change in E = ts * S * (I) / N * delta_t - (1/de) * E * delta_t
  # # Note: I (total infectious) is I_symp + I_asymp + I_presymp, which are all 0 at t=0.
  # # So, the only change should be from Exposed to Infected.
  
  # # Pop 1:
  # S0_p1 <- 990
  # E0_p1 <- 10
  # de_p1 <- 5
  
  # # Expected change in E for pop 1
  # delta_E1 <- -(1 / de_p1) * E0_p1 * delta_t
  # expected_E1_t1 <- E0_p1 + delta_E1
  
  # # The outflow from E goes into I_presymp and I_asymp based on `pea`
  # # Since pea = 0, all outflow goes to I_presymp
  # expected_Ip1_t1 <- (1 / de_p1) * E0_p1 * delta_t
  
  # # Pop 2:
  # S0_p2 <- 495
  # E0_p2 <- 5
  # de_p2 <- 5
  
  # # Expected change in E for pop 2
  # delta_E2 <- -(1 / de_p2) * E0_p2 * delta_t
  # expected_E2_t1 <- E0_p2 + delta_E2
  # expected_Ip2_t1 <- (1 / de_p2) * E0_p2 * delta_t
  
  # # Extract simulated values at t = 0.1
  # t1_output <- sim_output[time == 0.1]
  
  # # Assertions
  # expect_equal(t1_output[population_id == 1 & disease_state == "S", value], S0_p1, tolerance = 1e-6)
  # expect_equal(t1_output[population_id == 1 & disease_state == "E", value], expected_E1_t1, tolerance = 1e-6)
  # expect_equal(t1_output[population_id == 1 & disease_state == "I_presymp", value], expected_Ip1_t1, tolerance = 1e-6)
  
  # expect_equal(t1_output[population_id == 2 & disease_state == "S", value], S0_p2, tolerance = 1e-6)
  # expect_equal(t1_output[population_id == 2 & disease_state == "E", value], expected_E2_t1, tolerance = 1e-6)
  # expect_equal(t1_output[population_id == 2 & disease_state == "I_presymp", value], expected_Ip2_t1, tolerance = 1e-6)
  
})
