#' Run a MetaRVM Measles Simulation
#'
#' @description
#' Type-safe wrapper for measles simulations. Validates that the supplied
#' configuration specifies `model.disease = "measles"` before delegating to
#' [metaRVM()]. Use [metaRVM()] directly when disease-type validation is not
#' required.
#'
#' @param config_input Configuration specification as one of: a character path
#'   to a YAML file, a [`MetaRVMConfig`] object, or a parsed config list.
#'   The config must include `model.disease = "measles"` or the function will
#'   stop with an informative error.
#'
#' @return A [`MetaRVMResults`] object. See [metaRVM()] for a full description
#'   of all fields, `run_info` metadata, and the results API.
#'
#' @seealso [metaRVM()] for the primary simulation entry point; [parse_config_measles()]
#'   for measles-specific config parsing.
#'
#' @examples
#' \donttest{
#' options(odin.verbose = FALSE)
#' cfg <- system.file("extdata", "example_config_measles_min.yaml", package = "MetaRVM")
#' results <- metaRVM_measles(cfg)
#' head(results$results)
#' }
#'
#' @export
metaRVM_measles <- function(config_input) {
  disease <- resolve_disease_from_config_input(config_input)
  if (!identical(disease, "measles")) {
    stop("metaRVM_measles() requires model.disease = 'measles' in config")
  }
  metaRVM(config_input)
}
