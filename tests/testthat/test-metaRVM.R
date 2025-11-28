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