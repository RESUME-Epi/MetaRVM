test_that("parse_config returns expected list structure for example config", {
  cfg_path <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
  expect_true(file.exists(cfg_path))

  cfg <- parse_config(cfg_path)

  # Basic type/names
  expect_type(cfg, "list")

  expected_top_names <- c(
    "N_pop", "pop_map",
    "S_ini", "E_ini", "I_asymp_ini", "I_presymp_ini", "I_symp_ini",
    "H_ini", "D_ini", "P_ini", "V_ini", "R_ini",
    "vac_time_id", "vac_counts", "vac_mat",
    "m_wd_d", "m_wd_n", "m_we_d", "m_we_n",
    "ts", "ve", "dv", "de", "dp", "da", "ds", "dh", "dr",
    "pea", "psr", "phr",
    "start_date", "sim_length", "nsim", "nrep", "random_seed",
    "delta_t", "chk_file_names", "chk_time_steps", "do_chk"
  )

  # We don't require exact equality, just that all documented fields exist
  expect_true(all(expected_top_names %in% names(cfg)))

  # N_pop and pop_map consistency
  expect_true(is.numeric(cfg$N_pop))
  expect_true(cfg$N_pop > 0)
  expect_true(data.table::is.data.table(cfg$pop_map))
  expect_equal(nrow(cfg$pop_map), cfg$N_pop)

  # Initialization vectors should be length N_pop
  ini_names <- c("S_ini", "E_ini", "I_asymp_ini", "I_presymp_ini",
                 "I_symp_ini", "H_ini", "D_ini", "P_ini", "V_ini", "R_ini")

  for (nm in ini_names) {
    expect_equal(length(cfg[[nm]]), cfg$N_pop, info = paste("Wrong length for", nm))
    expect_true(all(cfg[[nm]] >= 0), info = paste("Negative values in", nm))
  }

  # Vaccination matrix: rows = time points, cols = 1 (time) + N_pop
  expect_true(is.matrix(cfg$vac_mat))
  expect_equal(ncol(cfg$vac_mat), cfg$N_pop + 1)
  expect_equal(length(cfg$vac_time_id), nrow(cfg$vac_mat))
  
  # Mixing matrices should be N_pop x N_pop
  mix_names <- c("m_wd_d", "m_wd_n", "m_we_d", "m_we_n")
  for (nm in mix_names) {
    expect_true(is.matrix(cfg[[nm]]), info = paste(nm, "is not a matrix"))
    expect_equal(dim(cfg[[nm]]), c(cfg$N_pop, cfg$N_pop),
                 info = paste("Wrong dimension for", nm))
  }

  # start_date & sim_length & delta_t
  expect_s3_class(cfg$start_date, "Date")
  expect_true(is.numeric(cfg$sim_length))
  expect_true(cfg$sim_length > 0)
  expect_true(is.numeric(cfg$delta_t))
  expect_equal(cfg$delta_t, 0.5)
  expect_equal(cfg$nrep, 1L)
})

test_that("parse_config expands disease parameters with correct dimensions and ranges", {
  cfg_path <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
  cfg <- parse_config(cfg_path)

  N_pop <- cfg$N_pop
  nsim  <- cfg$nsim

  # All disease params are documented as matrices (nsim x N_pop)
  mat_params <- c("ts", "ve", "dv", "de", "dp", "da", "ds", "dh", "dr",
                  "pea", "psr", "phr")

  for (nm in mat_params) {
    m <- cfg[[nm]]
    expect_true(is.matrix(m), info = paste(nm, "is not a matrix"))
    expect_equal(dim(m), c(nsim, N_pop),
                 info = paste("Wrong dimension for", nm))
  }

  # Durations should be positive
  dur_params <- c("dv", "de", "dp", "da", "ds", "dh", "dr")
  for (nm in dur_params) {
    m <- cfg[[nm]]
    expect_true(all(m > 0), info = paste("Non-positive duration in", nm))
  }

  # Probabilities should be in [0, 1]
  prob_params <- c("pea", "psr", "phr", "ve")
  for (nm in prob_params) {
    m <- cfg[[nm]]
    expect_true(all(m >= 0 & m <= 1),
                info = paste("Probability out of [0,1] range in", nm))
  }
})

test_that("parse_config with return_object = TRUE returns a MetaRVMConfig with expected methods", {
  cfg_path <- system.file("extdata", "example_config.yaml", package = "MetaRVM")

  cfg_obj <- parse_config(cfg_path, return_object = TRUE)

  # Class and basic methods
  expect_s3_class(cfg_obj, "MetaRVMConfig")
  expect_true(is.function(cfg_obj$get))
  expect_true(is.function(cfg_obj$set))
  expect_true(is.function(cfg_obj$list_parameters))

  # N_pop via object equals N_pop via list
  cfg_list <- parse_config(cfg_path, return_object = FALSE)
  expect_equal(cfg_obj$get("N_pop"), cfg_list$N_pop)

  # Parameter list contains key entries
  params <- cfg_obj$list_parameters()
  expect_true(all(c("N_pop", "pop_map", "ts", "start_date", "sim_length") %in% params))

  # pop_map through helper method (if defined) or get()
  # (uncomment the first line if you have get_pop_map(); otherwise use get("pop_map")):
  # pop_map <- cfg_obj$get_pop_map()
  pop_map <- cfg_obj$get("pop_map")
  expect_true(data.table::is.data.table(pop_map))
  expect_equal(nrow(pop_map), cfg_obj$get("N_pop"))
})

test_that("parse_config reports valid category values when sub_disease_params value is invalid", {
  ext <- function(x) system.file("extdata", x, package = "MetaRVM")
  cfg_path <- tempfile(fileext = ".yaml")

  cfg <- list(
    run_id = "InvalidSubgroupValue",
    model = list(disease = "flu"),
    population_data = list(
      initialization = ext("population_init_n24.csv"),
      vaccination = ext("vaccination_n24.csv")
    ),
    mixing_matrix = list(
      weekday_day = ext("m_weekday_day.csv"),
      weekday_night = ext("m_weekday_night.csv"),
      weekend_day = ext("m_weekend_day.csv"),
      weekend_night = ext("m_weekend_night.csv")
    ),
    disease_params = list(
      ts = 0.5, tv = 0.25, ve = 0.4, dv = 180, dp = 1, de = 3,
      da = 5, ds = 6, dh = 8, dr = 180, pea = 0.3, psr = 0.95, phr = 0.97
    ),
    sub_disease_params = list(
      age = list(
        INVALID_AGE_GROUP = list(ts = 0.6)
      )
    ),
    simulation_config = list(
      start_date = "10/01/2023",
      length = 30,
      nsim = 1
    )
  )

  yaml::write_yaml(cfg, cfg_path)

  expect_error(
    parse_config(cfg_path),
    regexp = "Invalid values for category 'age' in sub_disease_params: INVALID_AGE_GROUP\\. Valid values are:"
  )
})

test_that("parse_config always produces a random_seed", {
  ext <- function(x) system.file("extdata", x, package = "MetaRVM")
  cfg_path <- tempfile(fileext = ".yaml")

  cfg <- list(
    run_id = "SimulationMode",
    model = list(disease = "flu"),
    population_data = list(
      initialization = ext("population_init_n24.csv"),
      vaccination = ext("vaccination_n24.csv")
    ),
    mixing_matrix = list(
      weekday_day = ext("m_weekday_day.csv"),
      weekday_night = ext("m_weekday_night.csv"),
      weekend_day = ext("m_weekend_day.csv"),
      weekend_night = ext("m_weekend_night.csv")
    ),
    disease_params = list(
      ts = 0.5, ve = 0.4, dv = 180, dp = 1, de = 3,
      da = 5, ds = 6, dh = 8, dr = 180, pea = 0.3, psr = 0.95, phr = 0.97
    ),
    simulation_config = list(
      start_date = "10/01/2023",
      length = 30,
      nsim = 1
    )
  )

  yaml::write_yaml(cfg, cfg_path)
  parsed <- parse_config(cfg_path)
  expect_false(is.null(parsed$random_seed))
  expect_true(is.integer(parsed$random_seed))
})

test_that("parse_config accepts nrep and validates it", {
  ext <- function(x) system.file("extdata", x, package = "MetaRVM")
  cfg_ok_path <- tempfile(fileext = ".yaml")
  cfg_bad_path <- tempfile(fileext = ".yaml")

  base_cfg <- list(
    run_id = "Replicates",
    model = list(disease = "flu"),
    population_data = list(
      initialization = ext("population_init_n24.csv"),
      vaccination = ext("vaccination_n24.csv")
    ),
    mixing_matrix = list(
      weekday_day = ext("m_weekday_day.csv"),
      weekday_night = ext("m_weekday_night.csv"),
      weekend_day = ext("m_weekend_day.csv"),
      weekend_night = ext("m_weekend_night.csv")
    ),
    disease_params = list(
      ts = 0.5, ve = 0.4, dv = 180, dp = 1, de = 3,
      da = 5, ds = 6, dh = 8, dr = 180, pea = 0.3, psr = 0.95, phr = 0.97
    ),
    simulation_config = list(
      start_date = "10/01/2023",
      length = 30,
      nsim = 2,
      nrep = 3
    )
  )

  yaml::write_yaml(base_cfg, cfg_ok_path)
  expect_equal(parse_config(cfg_ok_path)$nrep, 3L)

  bad_cfg <- base_cfg
  bad_cfg$simulation_config$nrep <- 0
  yaml::write_yaml(bad_cfg, cfg_bad_path)
  expect_error(
    parse_config(cfg_bad_path),
    regexp = "nrep must be a positive integer"
  )
})

test_that("parse_config errors when model.disease is missing", {
  ext <- function(x) system.file("extdata", x, package = "MetaRVM")
  cfg_path <- tempfile(fileext = ".yaml")

  cfg <- list(
    run_id = "MissingDisease",
    population_data = list(
      initialization = ext("population_init_n24.csv"),
      vaccination = ext("vaccination_n24.csv")
    ),
    mixing_matrix = list(
      weekday_day = ext("m_weekday_day.csv"),
      weekday_night = ext("m_weekday_night.csv"),
      weekend_day = ext("m_weekend_day.csv"),
      weekend_night = ext("m_weekend_night.csv")
    ),
    disease_params = list(
      ts = 0.5, ve = 0.4, dv = 180, dp = 1, de = 3,
      da = 5, ds = 6, dh = 8, dr = 180, pea = 0.3, psr = 0.95, phr = 0.97
    ),
    simulation_config = list(
      start_date = "10/01/2023",
      length = 10,
      nsim = 1
    )
  )

  yaml::write_yaml(cfg, cfg_path)
  expect_error(
    parse_config(cfg_path),
    regexp = "'disease' field is required"
  )
})

test_that("parse_config_measles applies sub_disease_params overrides to param_values columns", {
  ext <- function(x) system.file("extdata", x, package = "MetaRVM")
  cfg_path <- tempfile(fileext = ".yaml")

  cfg <- list(
    run_id = "MeaslesSubDisease",
    model = list(disease = "measles"),
    population_data = list(
      initialization = ext("population_init_measles_min.csv")
    ),
    mixing_matrix = list(
      weekday_day = ext("m_weekday_day_measles_min.csv"),
      weekday_night = ext("m_weekday_night_measles_min.csv"),
      weekend_day = ext("m_weekend_day_measles_min.csv"),
      weekend_night = ext("m_weekend_night_measles_min.csv")
    ),
    disease_params = list(
      beta = 0.65, de1 = 3, de2 = 3, di1 = 2, di2 = 3,
      prop_I1_Q = 0.2, prop_I2_Q = 0.5, V1_eff = 0.93, V2_eff = 0.97
    ),
    sub_disease_params = list(
      age_group = list(
        child = list(beta = 0.8)
      )
    ),
    simulation_config = list(
      start_date = "10/01/2023",
      length = 10,
      nsim = 2
    )
  )

  yaml::write_yaml(cfg, cfg_path)
  parsed <- parse_config(cfg_path)

  # population_init_measles_min.csv: pop 1 = child/urban, pop 2 = adult/urban, pop 3 = child/rural
  # child columns: 1 and 3; adult column: 2
  expect_true(is.matrix(parsed$beta))
  expect_equal(dim(parsed$beta), c(2L, 3L))

  # child subpopulations (cols 1 and 3) should have overridden beta = 0.8
  expect_true(all(parsed$beta[, c(1, 3)] == 0.8))
  # adult subpopulation (col 2) should retain base beta = 0.65
  expect_true(all(parsed$beta[, 2] == 0.65))
})

test_that("parse_config_measles errors on invalid sub_disease_params category value", {
  ext <- function(x) system.file("extdata", x, package = "MetaRVM")
  cfg_path <- tempfile(fileext = ".yaml")

  cfg <- list(
    run_id = "MeaslesInvalidSubVal",
    model = list(disease = "measles"),
    population_data = list(
      initialization = ext("population_init_measles_min.csv")
    ),
    mixing_matrix = list(
      weekday_day = ext("m_weekday_day_measles_min.csv"),
      weekday_night = ext("m_weekday_night_measles_min.csv"),
      weekend_day = ext("m_weekend_day_measles_min.csv"),
      weekend_night = ext("m_weekend_night_measles_min.csv")
    ),
    disease_params = list(
      beta = 0.65, de1 = 3, de2 = 3, di1 = 2, di2 = 3,
      prop_I1_Q = 0.2, prop_I2_Q = 0.5, V1_eff = 0.93, V2_eff = 0.97
    ),
    sub_disease_params = list(
      age_group = list(
        INVALID_GROUP = list(beta = 0.9)
      )
    ),
    simulation_config = list(
      start_date = "10/01/2023",
      length = 10,
      nsim = 1
    )
  )

  yaml::write_yaml(cfg, cfg_path)
  expect_error(
    parse_config(cfg_path),
    regexp = "Invalid values for category 'age_group' in sub_disease_params: INVALID_GROUP\\. Valid values are:"
  )
})
