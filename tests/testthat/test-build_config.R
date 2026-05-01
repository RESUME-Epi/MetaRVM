ext <- function(x) system.file("extdata", x, package = "MetaRVM")

flu_mix <- function() {
  list(
    weekday_day   = ext("m_weekday_day.csv"),
    weekday_night = ext("m_weekday_night.csv"),
    weekend_day   = ext("m_weekend_day.csv"),
    weekend_night = ext("m_weekend_night.csv")
  )
}
flu_params <- function() {
  list(ts = 0.5, ve = 0.4, dv = 180, dp = 1, de = 3,
       da = 5, ds = 6, dh = 8, dr = 180, pea = 0.3, psr = 0.95, phr = 0.97)
}

test_that("build_config returns a raw list with correct YAML structure", {
  cfg <- build_config(
    disease         = "flu",
    population_init = ext("population_init_n24.csv"),
    mixing_matrices = flu_mix(),
    disease_params  = flu_params(),
    vaccination     = ext("vaccination_n24.csv"),
    start_date      = "10/01/2023",
    sim_length      = 30L,
    nsim            = 2L,
    nrep            = 3L,
    random_seed     = 42L
  )

  expect_type(cfg, "list")
  expect_equal(cfg$model$disease, "flu")
  expect_equal(cfg$simulation_config$length, 30L)
  expect_equal(cfg$simulation_config$nsim,   2L)
  expect_equal(cfg$simulation_config$nrep,   3L)
  expect_equal(cfg$simulation_config$random_seed, 42L)
  expect_true(!is.null(cfg$population_data$initialization))
  expect_true(!is.null(cfg$population_data$vaccination))
  expect_true(all(c("weekday_day", "weekday_night", "weekend_day", "weekend_night")
                  %in% names(cfg$mixing_matrix)))
  expect_equal(cfg$disease_params$ts, 0.5)
  # Paths are normalised to absolute
  expect_true(startsWith(cfg$population_data$initialization, "/"))
})

test_that("build_config with return_object = TRUE returns a MetaRVMConfig", {
  cfg_obj <- build_config(
    disease         = "flu",
    population_init = ext("population_init_n24.csv"),
    mixing_matrices = flu_mix(),
    disease_params  = flu_params(),
    vaccination     = ext("vaccination_n24.csv"),
    start_date      = "10/01/2023",
    sim_length      = 20L,
    nsim            = 1L,
    return_object   = TRUE
  )

  expect_s3_class(cfg_obj, "MetaRVMConfig")
  expect_equal(cfg_obj$get("disease"), "flu")
  expect_equal(cfg_obj$get("sim_length"), 20L)
})

test_that("build_config writes a parseable YAML file", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))

  build_config(
    disease         = "flu",
    population_init = ext("population_init_n24.csv"),
    mixing_matrices = flu_mix(),
    disease_params  = flu_params(),
    vaccination     = ext("vaccination_n24.csv"),
    start_date      = "10/01/2023",
    sim_length      = 15L,
    output_file     = tmp
  )

  expect_true(file.exists(tmp))
  parsed <- parse_config(tmp)
  expect_type(parsed, "list")
  expect_equal(parsed$sim_length, 15L)
})

test_that("build_config can write YAML and return object simultaneously", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))

  cfg_obj <- build_config(
    disease         = "flu",
    population_init = ext("population_init_n24.csv"),
    mixing_matrices = flu_mix(),
    disease_params  = flu_params(),
    vaccination     = ext("vaccination_n24.csv"),
    start_date      = "10/01/2023",
    sim_length      = 10L,
    output_file     = tmp,
    return_object   = TRUE
  )

  expect_true(file.exists(tmp))
  expect_s3_class(cfg_obj, "MetaRVMConfig")
})

test_that("build_config accepts Date object for start_date", {
  cfg <- build_config(
    disease         = "flu",
    population_init = ext("population_init_n24.csv"),
    mixing_matrices = flu_mix(),
    disease_params  = flu_params(),
    vaccination     = ext("vaccination_n24.csv"),
    start_date      = as.Date("2023-10-01"),
    sim_length      = 10L
  )
  expect_equal(cfg$simulation_config$start_date, "10/01/2023")
})

test_that("build_config passes sub_disease_params into the list", {
  cfg <- build_config(
    disease         = "flu",
    population_init = ext("population_init_n24.csv"),
    mixing_matrices = flu_mix(),
    disease_params  = flu_params(),
    vaccination     = ext("vaccination_n24.csv"),
    start_date      = "10/01/2023",
    sim_length      = 10L,
    sub_disease_params = list(age = list(`65+` = list(ts = 0.6)))
  )
  expect_equal(cfg$sub_disease_params$age$`65+`$ts, 0.6)
})

test_that("build_config errors on missing required arguments", {
  expect_error(
    build_config(
      disease         = "flu",
      mixing_matrices = flu_mix(),
      disease_params  = flu_params(),
      start_date      = "10/01/2023",
      sim_length      = 10L
    ),
    regexp = "population_init is required"
  )

  expect_error(
    build_config(
      disease         = "flu",
      population_init = ext("population_init_n24.csv"),
      disease_params  = flu_params(),
      start_date      = "10/01/2023",
      sim_length      = 10L
    ),
    regexp = "mixing_matrices is required"
  )

  expect_error(
    build_config(
      disease         = "flu",
      population_init = ext("population_init_n24.csv"),
      mixing_matrices = flu_mix(),
      disease_params  = flu_params(),
      start_date      = "10/01/2023",
      sim_length      = -1L
    ),
    regexp = "sim_length must be a positive integer"
  )
})

test_that("build_config errors when mixing_matrices keys are incomplete", {
  bad_mix <- flu_mix()
  bad_mix$weekend_night <- NULL

  expect_error(
    build_config(
      disease         = "flu",
      population_init = ext("population_init_n24.csv"),
      mixing_matrices = bad_mix,
      disease_params  = flu_params(),
      start_date      = "10/01/2023",
      sim_length      = 10L
    ),
    regexp = "mixing_matrices is missing required keys: weekend_night"
  )
})

test_that("build_config works for measles disease", {
  cfg <- build_config(
    disease         = "measles",
    population_init = ext("population_init_measles_min.csv"),
    mixing_matrices = list(
      weekday_day   = ext("m_weekday_day_measles_min.csv"),
      weekday_night = ext("m_weekday_night_measles_min.csv"),
      weekend_day   = ext("m_weekend_day_measles_min.csv"),
      weekend_night = ext("m_weekend_night_measles_min.csv")
    ),
    disease_params  = list(
      beta = 0.65, de1 = 3, de2 = 3, di1 = 2, di2 = 3,
      prop_I1_Q = 0.2, prop_I2_Q = 0.5, V1_eff = 0.93, V2_eff = 0.97
    ),
    start_date = "10/01/2023",
    sim_length = 20L,
    nsim       = 1L
  )

  expect_equal(cfg$model$disease, "measles")
  expect_equal(cfg$disease_params$beta, 0.65)
})
