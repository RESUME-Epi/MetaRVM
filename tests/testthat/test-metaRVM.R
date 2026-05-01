test_that("metaRVM runs end-to-end for example_config and returns expected structure", {
  # skip_on_cran()  # odin compilation can be slow on CRAN

  cfg_path <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
  expect_true(file.exists(cfg_path))

  res <- metaRVM(cfg_path)

  # Top-level object
  expect_s3_class(res, "MetaRVMResults")
  expect_true(is.list(res$run_info))

  # Results table
  dt <- res$results
  expect_s3_class(dt, "data.table")

  # Column names MUST be exactly these:
  expected_cols <- c("date", "age", "race", "zone",
                     "disease_state", "value", "instance")
  expect_setequal(names(dt), expected_cols)

  # Types: date as Date, value numeric, instance integer-ish
  expect_s3_class(dt$date, "Date")
  expect_type(dt$value, "double")
  expect_true(is.numeric(dt$instance))

  # Date range should match run_info$date_range (coerce if needed)
  expect_true("date_range" %in% names(res$run_info))
  dr <- res$run_info$date_range
  dr <- as.Date(dr)
  expect_equal(range(dt$date), dr)
})


test_that("metaRVM accepts config path, list, and MetaRVMConfig and produces consistent run_info", {
  # skip_on_cran()

  cfg_path <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
  expect_true(file.exists(cfg_path))

  # three input types
  cfg_list <- parse_config(cfg_path)                       # list
  cfg_obj  <- parse_config(cfg_path, return_object = TRUE) # MetaRVMConfig

  res_path <- metaRVM(cfg_path)
  res_list <- metaRVM(cfg_list)
  res_obj  <- metaRVM(cfg_obj)

  # All are MetaRVMResults
  expect_s3_class(res_path, "MetaRVMResults")
  expect_s3_class(res_list, "MetaRVMResults")
  expect_s3_class(res_obj,  "MetaRVMResults")

  # run_info should be consistent across input types
  expect_equal(res_path$run_info$date_range, res_list$run_info$date_range)
  expect_equal(res_path$run_info$date_range, res_obj$run_info$date_range)

  # A couple of key fields match
  for (r in list(res_list, res_obj)) {
    expect_equal(r$run_info$N_pop, res_path$run_info$N_pop)
    expect_equal(r$run_info$nsim,  res_path$run_info$nsim)
  }
})


test_that("MetaRVMResults subset_data and summarize behave as documented", {
  # skip_on_cran()

  cfg_path <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
  res <- metaRVM(cfg_path)
  dt  <- res$results

  # sanity
  expect_s3_class(res, "MetaRVMResults")
  expect_s3_class(dt, "data.table")

  # ---- subset_data: filter by disease_state ----
  # pick a state that should exist; "H" is typical, but you can change if needed
  hosp <- res$subset_data(disease_states = "H")

  expect_s3_class(hosp, "MetaRVMResults")
  expect_s3_class(hosp$results, "data.table")
  if (nrow(hosp$results) > 0) {
    expect_true(all(hosp$results$disease_state == "H"))
  }

  # also try filtering by multiple fields if you like (age/race/zone)
  # e.g. elderly_hosp <- res$subset_data(disease_states = "H", ages = "65+")

  # ---- summarize: basic check it returns a MetaRVMSummary with grouped columns ----
  summ <- res$summarize(
    group_by = c("age", "race"),
    stats    = c("median", "quantile")
  )

  expect_s3_class(summ, "MetaRVMSummary")

 
})

test_that("subset_data reports valid values when category value is invalid", {
  cfg_path <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
  res <- metaRVM(cfg_path)

  expect_error(
    res$subset_data(age = "NOT_A_VALID_AGE"),
    regexp = "Invalid values for category 'age': NOT_A_VALID_AGE\\. Valid values are:"
  )
})

test_that("subset_data accepts literal state names and named group aliases", {
  cfg_path <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
  res <- metaRVM(cfg_path)

  # Literal compartment name works
  hosp <- res$subset_data(disease_states = "H")
  expect_true(all(hosp$results$disease_state == "H"))

  # Named group alias expands correctly
  inc <- res$subset_data(disease_states = "incidence")
  expect_true(all(inc$results$disease_state %in% c("n_SE", "n_VE")))

  # Mix of literal and alias works
  mixed <- res$subset_data(disease_states = c("incidence", "H"))
  expect_true(all(mixed$results$disease_state %in% c("n_SE", "n_VE", "H")))

  # Typo in literal state name gives informative error
  expect_error(
    res$subset_data(disease_states = "hospitalzations"),
    regexp = "Unknown disease_states:.*Valid state names:.*Valid group aliases:"
  )

  # Typo in group alias also caught
  expect_error(
    res$subset_data(disease_states = "incidnce"),
    regexp = "Unknown disease_states:.*Valid state names:.*Valid group aliases:"
  )
})

test_that("metaRVM uses provided random_seed for stochastic runs", {
  ext <- function(x) system.file("extdata", x, package = "MetaRVM")
  cfg_path <- tempfile(fileext = ".yaml")

  cfg <- list(
    run_id = "StochasticSeedProvided",
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
      length = 20,
      nsim = 1,
      random_seed = 12345
    )
  )

  yaml::write_yaml(cfg, cfg_path)

  res1 <- metaRVM(cfg_path)
  res2 <- metaRVM(cfg_path)

  expect_equal(res1$config$get("random_seed"), 12345L)
  expect_equal(res2$config$get("random_seed"), 12345L)
  expect_equal(res1$results$value, res2$results$value)
})

test_that("metaRVM generates and stores random_seed for stochastic runs when missing", {
  ext <- function(x) system.file("extdata", x, package = "MetaRVM")
  cfg_path <- tempfile(fileext = ".yaml")

  cfg <- list(
    run_id = "StochasticSeedGenerated",
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
      length = 20,
      nsim = 1
    )
  )

  yaml::write_yaml(cfg, cfg_path)

  res <- metaRVM(cfg_path)
  generated_seed <- res$config$get("random_seed")

  expect_true(!is.null(generated_seed))
  expect_true(is.numeric(generated_seed))
  expect_length(generated_seed, 1)
})

test_that("metaRVM runs nsim * nrep instances", {
  ext <- function(x) system.file("extdata", x, package = "MetaRVM")
  cfg_path <- tempfile(fileext = ".yaml")

  cfg <- list(
    run_id = "ReplicateCount",
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
      length = 20,
      nsim = 2,
      nrep = 3
    )
  )

  yaml::write_yaml(cfg, cfg_path)

  res <- metaRVM(cfg_path)
  expect_equal(length(unique(res$results$instance)), 6L)
})

test_that("stochastic integer row allocation conserves S and V row totals", {
  ext <- function(x) system.file("extdata", x, package = "MetaRVM")
  cfg_path <- tempfile(fileext = ".yaml")

  cfg <- list(
    run_id = "RowAllocationConservation",
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
      length = 10,
      nsim = 1,
      random_seed = 4242
    )
  )

  yaml::write_yaml(cfg, cfg_path)
  res <- metaRVM(cfg_path)
  dt <- data.table::as.data.table(res$results)

  id_cols <- setdiff(names(dt), c("date", "disease_state", "value", "instance"))
  by_cols <- c("date", "instance", id_cols)

  s_src <- dt[disease_state == "S_src_int", .(S_src_int = value), by = by_cols]
  s_alloc <- dt[disease_state == "S_alloc", .(S_alloc_sum = sum(value, na.rm = TRUE)), by = by_cols]
  s_chk <- merge(s_src, s_alloc, by = by_cols)
  expect_gt(nrow(s_chk), 0)
  expect_true(all(abs(s_chk$S_src_int - s_chk$S_alloc_sum) < 1e-8))

  v_src <- dt[disease_state == "V_src_int", .(V_src_int = value), by = by_cols]
  v_alloc <- dt[disease_state == "V_alloc", .(V_alloc_sum = sum(value, na.rm = TRUE)), by = by_cols]
  v_chk <- merge(v_src, v_alloc, by = by_cols)
  expect_gt(nrow(v_chk), 0)
  expect_true(all(abs(v_chk$V_src_int - v_chk$V_alloc_sum) < 1e-8))
})
