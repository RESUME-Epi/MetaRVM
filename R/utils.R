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

# ==============================================================================
#' Format simulation output from wide to tall
#'
#' @param out output from the meta_sim function with added "rep" column
#'
#' @return a tall output table
#' @export
#'
#' @examples
#'
format_output <- function(out){
  long_out <- out %>%
    tidyr::pivot_longer(
      cols = -c("step", "time", "rep"),               # Exclude 'time' from being pivoted
      names_to = c("disease_state", "population_id"),  # Create new columns for disease state and subpopulation
      names_pattern = "([A-Za-z_]+)\\.(\\d+)\\.",  # Regex to extract the disease state and subpopulation ID
      values_to = "value"          # Column to store the actual values
    )
}

# ==============================================================================
#' Title
#'
#' @param long_out
#' @param start_date
#'
#' @return
#' @export
#'
#' @examples
daily_output <- function(long_out, start_date){

  long_out_daily <- long_out %>%
    dplyr::filter(time %% 1 == 0) %>%
    dplyr::filter(disease_state %in% c("S", "E", "H", "D",
                                       "I_presymp", "I_asymp",
                                       "I_symp", "R", "V")) %>%
    dplyr::mutate(disease_state = factor(disease_state,
                                         levels = c("S", "E", "H", "D",
                                                    "I_presymp", "I_asymp",
                                                    "I_symp", "R", "V"),
                                         labels = c("Susceptible",
                                                    "Exposed",
                                                    "Hospitalized",
                                                    "Dead",
                                                    "Presymptomatic",
                                                    "Asymptomatic",
                                                    "Symptomatic",
                                                    "Recovered",
                                                    "Vaccinated"))) %>%
    dplyr::group_by(time, disease_state, rep) %>%
    dplyr::summarize(total_value = sum(value), .groups = "drop") %>%
    dplyr::mutate(date = start_date + time)

  return(long_out_daily)
}

# ==============================================================================
#' Title
#'
#' @param long_out
#' @param start_date
#'
#' @return
#' @export
#'
#' @examples
daily_out_rates_sums <- function(long_out, start_date, disease_states){

  # group_map <- c(
  #   n_SE = "E_sum",
  #   n_VE = "E_sum",
  #   n_IsympH = "H_sum",
  #   n_HD = "D_sum",
  #   n_SV = "V_sum",
  # )

  df_states <- long_out %>%
    dplyr::filter(time %% 1 == 0) %>%
    dplyr::filter(disease_state %in% disease_states) %>%
    # dplyr::mutate(group = group_map[disease_state]) %>%
    dplyr::group_by(time, rep) %>%
    dplyr::summarise(d_sum = sum(value), .groups = "drop") %>% ungroup()

  df_pop <- long_out %>%
    dplyr::filter(time %% 1 == 0) %>%
    dplyr::filter(disease_state == "P") %>%    # Filter for P compartment (total population)
    dplyr::group_by(time, rep) %>%
    dplyr::select(time, rep, P = value) %>%
    dplyr::summarise(P_sum = sum(P), .groups = "drop") %>% ungroup()

  df_combined <- df_states %>%
    dplyr::left_join(df_pop, by = c("time", "rep")) %>%
    dplyr::mutate(d_rate = d_sum / P_sum) %>%
    dplyr::mutate(date = start_date + time)

  return(df_combined)
}


#' Title
#'
#' @param long_out
#' @param start_date
#' @param pop_map_df
#' @param agecats
#' @param racecats
#' @param zones
#' @param disease_states
#'
#' @return
#' @export
#'
#' @examples
subset_simout <- function(long_out, start_date, pop_map_df, agecats, racecats, zones, disease_states){

  # merge with population mapping
  df_long <- merge(long_out, pop_map_df, by = "population_id")

  long_out_sub <- df_long %>%
    dplyr::filter(time %% 1 == 0) %>%
    dplyr::filter(disease_state %in% c("S", "E", "H", "D",
                                       "I_presymp", "I_asymp",
                                       "I_symp", "R", "V")) %>%
    dplyr::mutate(disease_state = factor(disease_state,
                                         levels = c("S", "E", "H", "D",
                                                    "I_presymp", "I_asymp",
                                                    "I_symp", "R", "V"),
                                         labels = c("Susceptible",
                                                    "Exposed",
                                                    "Hospitalized",
                                                    "Dead",
                                                    "Presymptomatic",
                                                    "Asymptomatic",
                                                    "Symptomatic",
                                                    "Recovered",
                                                    "Vaccinated"))) %>%
    dplyr::filter(age %in% agecats) %>%
    dplyr::filter(race %in% racecats) %>%
    dplyr::filter(hcez %in% zones) %>%
    dplyr::filter(disease_state %in% disease_states) %>%
    dplyr::mutate(date = start_date + time) %>%
    dplyr::select(c(population_id, date, disease_state, value, age, race, hcez))
}

#' Title
#'
#' @param long_out
#' @param pop_map_df
#' @param start_date
#'
#' @return
#' @export
#'
#' @examples
zone_summary <- function(long_out, pop_map_df, start_date){
  # merge with population mapping
  df_long <- merge(long_out, pop_map_df, by = "population_id")

  zone_data <- df_long %>%
    # dplyr::filter(population_id == zone_id) %>%
    dplyr::filter(time %% 1 == 0) %>%
    dplyr::filter(disease_state %in% c("S", "E", "H", "D",
                                       "I_presymp", "I_asymp",
                                       "I_symp", "R", "V")) %>%
    dplyr::mutate(disease_state = factor(disease_state,
                                         levels = c("S", "E", "H", "D",
                                                    "I_presymp", "I_asymp",
                                                    "I_symp", "R", "V"),
                                         labels = c("Susceptible",
                                                    "Exposed",
                                                    "Hospitalized",
                                                    "Dead",
                                                    "Presymptomatic",
                                                    "Asymptomatic",
                                                    "Symptomatic",
                                                    "Recovered",
                                                    "Vaccinated"))) %>%
    dplyr::group_by(time, disease_state, hcez, rep) %>%
    dplyr::summarize(total_value = sum(value), .groups = "drop") %>%
    dplyr::mutate(date = start_date + time)

  return(zone_data)
}


