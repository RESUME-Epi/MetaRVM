#' Parse a Measles Configuration File
#'
#' @description
#' Type-safe wrapper for measles configuration parsing. Validates that the
#' supplied YAML specifies `model.disease = "measles"` before delegating to
#' [parse_config()]. Use [parse_config()] directly when disease-type validation
#' is not required.
#'
#' @param config_file Character string. Path to a YAML configuration file.
#'   The file must include `model.disease = "measles"`.
#' @param return_object Logical. If `TRUE`, returns a [`MetaRVMConfig`] object.
#'   If `FALSE` (default), returns a named list.
#'
#' @return A named list or [`MetaRVMConfig`] object with all parsed measles
#'   simulation parameters. See [parse_config()] for a full description of the
#'   returned fields.
#'
#' @seealso [parse_config()] for the general-purpose parser and full field
#'   documentation; [metaRVM_measles()] for running the parsed config.
#'
#' @examples
#' \donttest{
#' cfg <- system.file("extdata", "example_config_measles_min.yaml", package = "MetaRVM")
#' parsed <- parse_config_measles(cfg)
#' parsed$disease  # "measles"
#' parsed$N_pop
#' }
#'
#' @export
parse_config_measles <- function(config_file, return_object = FALSE) {
  yaml_data <- yaml::read_yaml(config_file)
  disease <- resolve_disease_from_yaml(yaml_data)
  if (!identical(disease, "measles")) {
    stop("parse_config_measles() requires model.disease = 'measles' in config")
  }
  parse_config(config_file = config_file, return_object = return_object)
}
