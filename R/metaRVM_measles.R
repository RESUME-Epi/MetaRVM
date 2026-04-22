#' Run a MetaRVM Measles Simulation
#'
#' @description
#' Compatibility wrapper for measles simulation runs.
#' Use [metaRVM()] as the primary run entrypoint.
#'
#' @param config_input Configuration specification as one of:
#'   character path to YAML, `MetaRVMConfig` object, or parsed config list.
#'
#' @return A `MetaRVMResults` object.
#' @export
metaRVM_measles <- function(config_input) {
  disease <- resolve_disease_from_config_input(config_input)
  if (!identical(disease, "measles")) {
    stop("metaRVM_measles() requires model.disease = 'measles' in config")
  }
  metaRVM(config_input)
}
