#' Type-safe wrapper that validates `model.disease = "measles"` before
#' delegating to [parse_config()]. Use [parse_config()] directly instead; it
#' dispatches on disease automatically via the disease registry.
#'
#' @keywords internal
parse_config_measles <- function(config_file, return_object = FALSE) {
  yaml_data <- yaml::read_yaml(config_file)
  disease <- resolve_disease_from_yaml(yaml_data)
  if (!identical(disease, "measles")) {
    stop("parse_config_measles() requires model.disease = 'measles' in config")
  }
  parse_config(config_file = config_file, return_object = return_object)
}
