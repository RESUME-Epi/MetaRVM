# Internal logging helpers for MetaRVM.
# Logging is opt-in via options(MetaRVM.log_enabled = TRUE).

metarvm_log_enabled <- function() {
  isTRUE(getOption("MetaRVM.log_enabled", FALSE))
}

metarvm_log_init <- function() {
  if (!metarvm_log_enabled() || !requireNamespace("logger", quietly = TRUE)) {
    return(invisible(FALSE))
  }

  threshold <- getOption("MetaRVM.log_level", "INFO")
  logger::log_threshold(threshold)

  log_file <- getOption("MetaRVM.log_file", NULL)
  if (is.null(log_file)) {
    logger::log_appender(logger::appender_stdout)
  } else {
    logger::log_appender(logger::appender_file(log_file))
  }

  invisible(TRUE)
}

metarvm_log_debug <- function(...) {
  if (!metarvm_log_enabled() || !requireNamespace("logger", quietly = TRUE)) {
    return(invisible(NULL))
  }
  logger::log_debug(...)
  invisible(NULL)
}

metarvm_log_info <- function(...) {
  if (!metarvm_log_enabled() || !requireNamespace("logger", quietly = TRUE)) {
    return(invisible(NULL))
  }
  logger::log_info(...)
  invisible(NULL)
}

metarvm_log_warn <- function(...) {
  if (!metarvm_log_enabled() || !requireNamespace("logger", quietly = TRUE)) {
    return(invisible(NULL))
  }
  logger::log_warn(...)
  invisible(NULL)
}

metarvm_log_error <- function(...) {
  if (!metarvm_log_enabled() || !requireNamespace("logger", quietly = TRUE)) {
    return(invisible(NULL))
  }
  logger::log_error(...)
  invisible(NULL)
}
