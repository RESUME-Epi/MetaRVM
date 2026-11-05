#' Type-safe wrapper that validates `model.disease = "measles"` before
#' delegating to [metaRVM()]. Use [metaRVM()] directly instead; it dispatches
#' on disease automatically via the disease registry.
#'
#' @keywords internal
metaRVM_measles <- function(config_input) {
  disease <- resolve_disease_from_config_input(config_input)
  if (!identical(disease, "measles")) {
    stop("metaRVM_measles() requires model.disease = 'measles' in config")
  }
  metaRVM(config_input)
}
