# Internal logging helpers for MetaRVM.
# Logging is opt-in via options(MetaRVM.log_enabled = TRUE).

.metarvm_logger_state <- new.env(parent = emptyenv())
.metarvm_logger_state$signature <- NULL

metarvm_log_enabled <- function() {
  isTRUE(getOption("MetaRVM.log_enabled", FALSE))
}

metarvm_logger_available <- function() {
  requireNamespace("logger", quietly = TRUE)
}

metarvm_log_level <- function() {
  level_raw <- getOption("MetaRVM.log_level", "INFO")
  level <- toupper(trimws(as.character(level_raw)[1]))
  valid_levels <- c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
  if (!level %in% valid_levels) {
    warning(
      sprintf(
        "Invalid MetaRVM.log_level '%s'; using 'INFO'. Valid levels: %s",
        level_raw,
        paste(valid_levels, collapse = ", ")
      ),
      call. = FALSE
    )
    level <- "INFO"
  }
  level
}

metarvm_log_init <- function() {
  if (!metarvm_log_enabled() || !metarvm_logger_available()) {
    return(invisible(FALSE))
  }

  threshold <- metarvm_log_level()
  log_file <- getOption("MetaRVM.log_file", NULL)
  log_target <- if (is.null(log_file)) "<stdout>" else as.character(log_file)[1]
  signature <- paste(threshold, log_target, sep = "::")
  if (identical(.metarvm_logger_state$signature, signature)) {
    return(invisible(TRUE))
  }

  logger::log_threshold(threshold)
  if (is.null(log_file)) {
    logger::log_appender(logger::appender_stdout)
  } else {
    log_dir <- dirname(log_file)
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    }
    logger::log_appender(logger::appender_file(log_file))
  }

  .metarvm_logger_state$signature <- signature
  invisible(TRUE)
}

metarvm_log_debug <- function(...) {
  if (!metarvm_log_enabled() || !metarvm_logger_available()) {
    return(invisible(NULL))
  }
  logger::log_debug(..., .topenv = parent.frame())
  invisible(NULL)
}

metarvm_log_info <- function(...) {
  if (!metarvm_log_enabled() || !metarvm_logger_available()) {
    return(invisible(NULL))
  }
  logger::log_info(..., .topenv = parent.frame())
  invisible(NULL)
}

metarvm_log_warn <- function(...) {
  if (!metarvm_log_enabled() || !metarvm_logger_available()) {
    return(invisible(NULL))
  }
  logger::log_warn(..., .topenv = parent.frame())
  invisible(NULL)
}

metarvm_log_error <- function(...) {
  if (!metarvm_log_enabled() || !metarvm_logger_available()) {
    return(invisible(NULL))
  }
  logger::log_error(..., .topenv = parent.frame())
  invisible(NULL)
}
