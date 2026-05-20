# Recursively deep-copies a list, using data.table::copy() for data.table leaves.
.deep_copy_list <- function(x) {
  if (inherits(x, "data.table")) {
    data.table::copy(x)
  } else if (is.list(x)) {
    lapply(x, .deep_copy_list)
  } else {
    x
  }
}

#' Copy a MetaRVMConfig object
#'
#' Returns an independent deep copy of a `MetaRVMConfig` object. Because R6
#' objects use reference semantics, assignment (`b <- a`) creates a second
#' handle to the *same* object. Use `copy_config()` when you need a truly
#' independent copy that can be modified without affecting the original.
#'
#' @param config A `MetaRVMConfig` object.
#' @return A new `MetaRVMConfig` object with the same configuration data.
#'
#' @examples
#' \dontrun{
#' config  <- parse_config("my_config.yaml")
#' config2 <- copy_config(config)
#' config2$set("nsim", 50L)   # does not affect config
#' }
#'
#' @export
copy_config <- function(config) {
  if (!inherits(config, "MetaRVMConfig")) {
    stop("'config' must be a MetaRVMConfig object")
  }
  new_obj <- MetaRVMConfig$new(.deep_copy_list(config$config_data))
  new_obj$config_file <- config$config_file
  new_obj
}
