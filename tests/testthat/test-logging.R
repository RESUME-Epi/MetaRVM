test_that("metaRVM writes logs to configured file when logging is enabled", {
  cfg_path <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
  expect_true(file.exists(cfg_path))

  log_file <- tempfile(fileext = ".log")
  old_opts <- options(
    MetaRVM.log_enabled = TRUE,
    MetaRVM.log_level = "INFO",
    MetaRVM.log_file = log_file
  )
  on.exit(options(old_opts), add = TRUE)

  metaRVM(cfg_path)

  expect_true(file.exists(log_file))
  log_lines <- readLines(log_file, warn = FALSE)
  expect_true(any(grepl("metaRVM started", log_lines, fixed = TRUE)))
  expect_true(any(grepl("metaRVM completed", log_lines, fixed = TRUE)))
})

test_that("invalid log level falls back to INFO with warning", {
  cfg_path <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
  expect_true(file.exists(cfg_path))

  log_file <- tempfile(fileext = ".log")
  old_opts <- options(
    MetaRVM.log_enabled = TRUE,
    MetaRVM.log_level = "NOT_A_LEVEL",
    MetaRVM.log_file = log_file
  )
  on.exit(options(old_opts), add = TRUE)

  expect_warning(
    parse_config(cfg_path),
    regexp = "Invalid MetaRVM.log_level"
  )
})
