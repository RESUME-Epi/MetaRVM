#' Extract disease states
#'
#' @param out A matrix of output from `meta_sim` function.
#' @param disease_states A character scalar or vector of disease states. The
#' disease state names should match with `c("S", "E", "H", "D", "I_presymp", "I_asymp", "I_symp", "R", "V")`
#'
#' @return A long tibble
#' @export
#'
#' @examples
#' \dontrun{
#' get_disease_state(out, disease_states = c("I_presymp", "I_asymp", "I_symp"))
#' }

get_disease_state <- function(out,
                              disease_states = c("S", "E", "H", "D", "I_presymp", "I_asymp", "I_symp", "R", "V")){

  # convert to long format
  long_out <- out %>%
    tidyr::pivot_longer(
      cols = -c("step", "time"),               # Exclude 'time' from being pivoted
      names_to = c("disease_state", "population_id"),  # Create new columns for disease state and subpopulation
      names_pattern = "([A-Za-z_]+)\\.(\\d+)\\.",  # Regex to extract the disease state and subpopulation ID
      values_to = "value"          # Column to store the actual values
    )

  # subset the data
  out_sub <- long_out %>%
    dplyr::filter(disease_state %in% disease_states) %>%
    dplyr::group_by(step, disease_state)

  return(out_sub)
}

# ==============================================================================
#' Extract disease states by HCE zone
#'
#' @param out matrix of output from `meta_sim` function.
#' @param HCEZ_id A scalar or vector of HCE zone ids.
#' @param disease_states A character scalar or vector of disease states. The
#' disease state names should match with `c("S", "E", "H", "D", "I_presymp", "I_asymp", "I_symp", "R", "V")`
#'
#' @return A long tibble
#' @export
#'
#' @examples
#' \dontrun{
#' get_HCEZ(out, disease_states = c("I_presymp", "I_asymp", "I_symp"), HCEZ_id = c(2, 4))
#' }
get_HCEZ <- function(out, HCEZ_id = 1:6,
                     disease_states = c("S", "E", "H", "D", "I_presymp", "I_asymp", "I_symp", "R", "V")){

  # convert to long format
  long_out <- out %>%
    tidyr::pivot_longer(
      cols = -c("step", "time"),               # Exclude 'time' from being pivoted
      names_to = c("disease_state", "population_id"),  # Create new columns for disease state and subpopulation
      names_pattern = "([A-Za-z_]+)\\.(\\d+)\\.",  # Regex to extract the disease state and subpopulation ID
      values_to = "value"          # Column to store the actual values
    )

  # subset the data
  out_sub <- long_out %>%
    dplyr::filter(population_id %in% HCEZ_id) %>%
    dplyr::filter(disease_state %in% disease_states) %>%
    dplyr::group_by(step, disease_state)

  return(out_sub)
}


