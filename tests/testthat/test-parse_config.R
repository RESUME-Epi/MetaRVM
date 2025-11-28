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
    "ts", "tv", "ve", "dv", "de", "dp", "da", "ds", "dh", "dr",
    "pea", "psr", "phr",
    "start_date", "sim_length", "nsim", "random_seed",
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
})

test_that("parse_config expands disease parameters with correct dimensions and ranges", {
  cfg_path <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
  cfg <- parse_config(cfg_path)

  N_pop <- cfg$N_pop
  nsim  <- cfg$nsim

  # All disease params are documented as matrices (nsim x N_pop)
  mat_params <- c("ts", "tv", "ve", "dv", "de", "dp", "da", "ds", "dh", "dr",
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

