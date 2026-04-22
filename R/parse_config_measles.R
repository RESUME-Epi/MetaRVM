#' Parse MetaMeasles Configuration File
#'
#' @description
#' Compatibility wrapper for measles configuration parsing.
#' Use [parse_config()] as the primary parser entrypoint.
#'
#' @param config_file Character string. Path to a YAML configuration file.
#' @param return_object Logical. If `TRUE`, returns a `MetaRVMConfig` object.
#'   If `FALSE` (default), returns a named list.
#'
#' @return Parsed configuration as a named list or `MetaRVMConfig` object.
#' @export
parse_config_measles <- function(config_file, return_object = FALSE) {
  yaml_data <- yaml::read_yaml(config_file)
  disease <- resolve_disease_from_yaml(yaml_data)
  if (!identical(disease, "measles")) {
    stop("parse_config_measles() requires model.disease = 'measles' in config")
  }
  parse_config(config_file = config_file, return_object = return_object)
}
