ext_m <- function(x) system.file("extdata", x, package = "MetaRVM")

# ============================================================
# 1. meta_measles_sim() — structural + conservation
# ============================================================

test_that("meta_measles_sim returns expected structure and no negative values", {
  M <- matrix(1, 1, 1)

  res <- meta_measles_sim(
    N_pop           = 1,
    beta            = 0.5,
    S0              = 990,
    I1_U0           = 10,
    m_weekday_day   = M, m_weekday_night = M,
    m_weekend_day   = M, m_weekend_night = M,
    delta_t         = 0.5,
    de1 = 3, de2 = 3, di1 = 2, di2 = 3,
    prop_I1_Q = 0.2, prop_I2_Q = 0.5,
    V1_eff = 0.93, V2_eff = 0.97,
    nsteps = 20,
    seed   = 42L
  )

  expect_s3_class(res, "data.table")
  expect_true(all(c("step", "time", "disease_state", "population_id", "value") %in% names(res)))
  expect_true(all(res$value >= 0), info = "meta_measles_sim output contains negative values")
})

test_that("meta_measles_sim conserves total population (no death compartment)", {
  N_total <- 1000
  M       <- matrix(1, 1, 1)

  res <- meta_measles_sim(
    N_pop           = 1,
    beta            = 0.5,
    S0              = N_total - 10,
    I1_U0           = 10,
    m_weekday_day   = M, m_weekday_night = M,
    m_weekend_day   = M, m_weekend_night = M,
    delta_t         = 0.5,
    de1 = 3, de2 = 3, di1 = 2, di2 = 3,
    prop_I1_Q = 0.2, prop_I2_Q = 0.5,
    V1_eff = 0.93, V2_eff = 0.97,
    nsteps = 30,
    seed   = 123L
  )

  stock_states <- c("S", "E1", "E2", "I1_Q", "I1_U", "I2_Q", "I2_U", "R")
  stocks <- res[disease_state %in% stock_states,
                .(total = sum(value)), by = .(step, population_id)]

  expect_true(
    all(abs(stocks$total - N_total) < 1e-8),
    info = "Total population not conserved across time steps"
  )
})

test_that("meta_measles_sim handles multi-pop mixing with no negative values", {
  skip_on_cran()

  N_pop <- 3
  M <- matrix(c(0.7, 0.2, 0.1,
                0.2, 0.6, 0.2,
                0.1, 0.3, 0.6), nrow = 3, byrow = TRUE)

  res <- meta_measles_sim(
    N_pop           = N_pop,
    beta            = c(0.5, 0.4, 0.6),
    S0              = c(900, 800, 700),
    I1_U0           = c(10, 5, 3),
    m_weekday_day   = M, m_weekday_night = M,
    m_weekend_day   = M, m_weekend_night = M,
    delta_t         = 0.5,
    de1 = 3, de2 = 3, di1 = 2, di2 = 3,
    prop_I1_Q = 0.2, prop_I2_Q = 0.5,
    V1_eff = 0.93, V2_eff = 0.97,
    nsteps = 20,
    seed   = 99L
  )

  expect_s3_class(res, "data.table")
  expect_setequal(unique(res$population_id), 1:3)
  expect_true(all(res$value >= 0))
})

test_that("meta_measles_sim conserves population per subpopulation with multi-pop (no vaccination)", {
  skip_on_cran()

  N_pop   <- 3
  N0      <- c(1000, 800, 600)
  M <- diag(N_pop)  # diagonal: no cross-stratum spread

  res <- meta_measles_sim(
    N_pop           = N_pop,
    beta            = 0.5,
    S0              = N0 - c(10, 5, 3),
    I1_U0           = c(10, 5, 3),
    m_weekday_day   = M, m_weekday_night = M,
    m_weekend_day   = M, m_weekend_night = M,
    delta_t         = 0.5,
    de1 = 3, de2 = 3, di1 = 2, di2 = 3,
    prop_I1_Q = 0.2, prop_I2_Q = 0.5,
    V1_eff = 0.93, V2_eff = 0.97,
    nsteps = 20,
    seed   = 7L
  )

  stock_states <- c("S", "E1", "E2", "I1_Q", "I1_U", "I2_Q", "I2_U", "R")
  stocks <- res[disease_state %in% stock_states,
                .(total = sum(value)), by = .(step, population_id)]

  for (pop in 1:N_pop) {
    pop_totals <- stocks[population_id == pop, total]
    expect_true(
      all(abs(pop_totals - N0[pop]) < 1e-8),
      info = sprintf("Population %d not conserved", pop)
    )
  }
})

# ============================================================
# 2. metaRVM() end-to-end for measles
# ============================================================

test_that("metaRVM runs end-to-end for measles and returns expected structure", {
  cfg_path <- ext_m("example_config_measles_min.yaml")
  expect_true(file.exists(cfg_path))

  res <- metaRVM(cfg_path)

  expect_s3_class(res, "MetaRVMResults")
  expect_true(is.list(res$run_info))

  dt <- res$results
  expect_s3_class(dt, "data.table")
  expect_true(all(c("date", "disease_state", "value", "instance") %in% names(dt)))

  # population_init_measles_min.csv has age_group and zone as demographic categories
  expect_true(all(c("age_group", "zone") %in% names(dt)))

  expect_s3_class(dt$date, "Date")
  expect_type(dt$value, "double")
  expect_true(is.numeric(dt$instance))

  dr <- as.Date(res$run_info$date_range)
  expect_equal(range(dt$date), dr)
})

test_that("metaRVM measles accepts config path, list, and MetaRVMConfig consistently", {
  cfg_path <- ext_m("example_config_measles_min.yaml")

  cfg_list <- parse_config(cfg_path)
  cfg_obj  <- parse_config(cfg_path, return_object = TRUE)

  res_path <- metaRVM(cfg_path)
  res_list <- metaRVM(cfg_list)
  res_obj  <- metaRVM(cfg_obj)

  for (r in list(res_list, res_obj)) {
    expect_s3_class(r, "MetaRVMResults")
    expect_equal(r$run_info$date_range, res_path$run_info$date_range)
    expect_equal(r$run_info$N_pop,      res_path$run_info$N_pop)
    expect_equal(r$run_info$nsim,       res_path$run_info$nsim)
  }
})

test_that("metaRVM measles uses provided random_seed for reproducibility", {
  cfg_path <- tempfile(fileext = ".yaml")
  cfg <- list(
    run_id = "MeaslesSeedTest",
    model  = list(disease = "measles"),
    population_data = list(
      initialization = ext_m("population_init_measles_min.csv")
    ),
    mixing_matrix = list(
      weekday_day   = ext_m("m_weekday_day_measles_min.csv"),
      weekday_night = ext_m("m_weekday_night_measles_min.csv"),
      weekend_day   = ext_m("m_weekend_day_measles_min.csv"),
      weekend_night = ext_m("m_weekend_night_measles_min.csv")
    ),
    disease_params = list(
      beta = 0.65, de1 = 3, de2 = 3, di1 = 2, di2 = 3,
      prop_I1_Q = 0.2, prop_I2_Q = 0.5, V1_eff = 0.93, V2_eff = 0.97
    ),
    simulation_config = list(
      start_date = "10/01/2023", length = 10, nsim = 1, nrep = 5,
      random_seed = 77777
    )
  )
  yaml::write_yaml(cfg, cfg_path)

  res1 <- metaRVM(cfg_path)
  res2 <- metaRVM(cfg_path)

  expect_equal(res1$config$get("random_seed"), 77777L)
  expect_equal(res1$results$value, res2$results$value)
})

test_that("metaRVM measles generates and stores random_seed when omitted", {
  cfg_path <- tempfile(fileext = ".yaml")
  cfg <- list(
    run_id = "MeaslesSeedGenerated",
    model  = list(disease = "measles"),
    population_data = list(
      initialization = ext_m("population_init_measles_min.csv")
    ),
    mixing_matrix = list(
      weekday_day   = ext_m("m_weekday_day_measles_min.csv"),
      weekday_night = ext_m("m_weekday_night_measles_min.csv"),
      weekend_day   = ext_m("m_weekend_day_measles_min.csv"),
      weekend_night = ext_m("m_weekend_night_measles_min.csv")
    ),
    disease_params = list(
      beta = 0.65, de1 = 3, de2 = 3, di1 = 2, di2 = 3,
      prop_I1_Q = 0.2, prop_I2_Q = 0.5, V1_eff = 0.93, V2_eff = 0.97
    ),
    simulation_config = list(
      start_date = "10/01/2023", length = 10, nsim = 1, nrep = 5
    )
  )
  yaml::write_yaml(cfg, cfg_path)

  res <- metaRVM(cfg_path)
  seed <- res$config$get("random_seed")

  expect_false(is.null(seed))
  expect_true(is.numeric(seed))
  expect_length(seed, 1)
})

test_that("metaRVM measles runs nsim * nrep instances", {
  cfg_path <- tempfile(fileext = ".yaml")
  cfg <- list(
    run_id = "MeaslesReplicates",
    model  = list(disease = "measles"),
    population_data = list(
      initialization = ext_m("population_init_measles_min.csv")
    ),
    mixing_matrix = list(
      weekday_day   = ext_m("m_weekday_day_measles_min.csv"),
      weekday_night = ext_m("m_weekday_night_measles_min.csv"),
      weekend_day   = ext_m("m_weekend_day_measles_min.csv"),
      weekend_night = ext_m("m_weekend_night_measles_min.csv")
    ),
    disease_params = list(
      beta = 0.65, de1 = 3, de2 = 3, di1 = 2, di2 = 3,
      prop_I1_Q = 0.2, prop_I2_Q = 0.5, V1_eff = 0.93, V2_eff = 0.97
    ),
    simulation_config = list(
      start_date = "10/01/2023", length = 10, nsim = 2, nrep = 3,
      random_seed = 42
    )
  )
  yaml::write_yaml(cfg, cfg_path)

  res <- metaRVM(cfg_path)
  expect_equal(length(unique(res$results$instance)), 6L)
})

# ============================================================
# 3. subset_data — literal states and group aliases
# ============================================================

test_that("measles subset_data accepts literal state name n_SE1", {
  res <- metaRVM(ext_m("example_config_measles_min.yaml"))

  sub <- res$subset_data(disease_states = "n_SE1")
  expect_s3_class(sub, "MetaRVMResults")
  expect_true(all(sub$results$disease_state == "n_SE1"))
})

test_that("measles subset_data expands 'incidence' alias to n_E2I1", {
  res <- metaRVM(ext_m("example_config_measles_min.yaml"))

  inc <- res$subset_data(disease_states = "incidence")
  expect_s3_class(inc, "MetaRVMResults")
  expect_true(all(inc$results$disease_state %in% c("n_SE1", "n_V1E1", "n_V2E1")))
})

test_that("measles subset_data typo gives informative error listing valid names and aliases", {
  res <- metaRVM(ext_m("example_config_measles_min.yaml"))

  expect_error(
    res$subset_data(disease_states = "incidnce"),
    regexp = "Unknown disease_states:.*Valid state names:.*Valid group aliases:"
  )
})

test_that("measles subset_data rejects invalid demographic category value", {
  res <- metaRVM(ext_m("example_config_measles_min.yaml"))

  expect_error(
    res$subset_data(age_group = "NOT_AN_AGE"),
    regexp = "Invalid values for category 'age_group': NOT_AN_AGE\\. Valid values are:"
  )
})

# ============================================================
# 4. summarize
# ============================================================

test_that("metaRVM measles summarize returns MetaRVMSummary", {
  res <- metaRVM(ext_m("example_config_measles_min.yaml"))

  summ <- res$summarize(
    group_by = c("age_group", "zone"),
    stats    = c("median", "quantile")
  )
  expect_s3_class(summ, "MetaRVMSummary")
})
