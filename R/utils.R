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
#'
format_output <- function(out){
  long_out <- out %>%
    tidyr::pivot_longer(
      cols = -c("step", "time", "instance"),               # Exclude 'time' from being pivoted
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
#' @export
#'
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
    dplyr::group_by(time, disease_state, instance) %>%
    dplyr::summarize(total_value = sum(value), .groups = "drop")


  long_out_new_counts_daily <- long_out %>%
    dplyr::filter(disease_state %in% c("n_SE", "n_SV", "n_VE", "n_EI",
                                       "n_EIpresymp", "n_preIsymp", "n_IsympRH",
                                       "n_IsympH", "n_IsympR", "n_HR", "n_HD",
                                       "n_IasympR")) %>%
    dplyr::mutate(disease_state = factor(disease_state,
                                         levels = c("n_SE", "n_SV", "n_VE", "n_EI",
                                                    "n_EIpresymp", "n_preIsymp", "n_IsympRH",
                                                    "n_IsympH", "n_IsympR", "n_HR", "n_HD",
                                                    "n_IasympR"),
                                         # labels = c("n_SE", "n_SV", "n_VE", "n_EI",
                                         #            "n_EIpresymp", "n_preIsymp", "n_IsympRH",
                                         #            "n_IsympH", "n_IsympR", "n_HR", "n_HD",
                                         #            "n_IasympR")
                                         labels = c("New Exposed (S)",
                                                    "New Vaccinated",
                                                    "New Exposed (V)",
                                                    "New Infectious",
                                                    "New Infectious presymptomatic",
                                                    "New Infectious symptomatic",
                                                    "New Recovered/Hospitalized (symp)",
                                                    "New Hospitalized (symp)",
                                                    "New Recovered (symp)",
                                                    "New Recovered (H)",
                                                    "New Dead",
                                                    "New Recovered (asymp)")
                                         ),
                  time_integer = floor(time)) %>%
    dplyr::group_by(time_integer, disease_state, instance) %>%
    dplyr::summarize(total_value = sum(value), .groups = "drop") %>%
    dplyr::rename(time = time_integer)


  long_out_daily <- rbind(long_out_daily, long_out_new_counts_daily)


  if (!is.null(start_date)) {
    long_out_daily <- long_out_daily %>%
      dplyr::mutate(date = start_date + time)
  }

  return(long_out_daily)
}

# ==============================================================================

#' Title
#'
#' @param long_out_daily
#' @param conf_level
#' @param value_column
#'
#' @export
#'
summarize_out <- function(long_out_daily, conf_level, value_column){

  lower_prob <- (100 - conf_level) / 200
  upper_prob <- 1 - lower_prob

  summary_out <- long_out_daily %>%
    dplyr::group_by(date, disease_state) %>%
    dplyr::summarize(
      median = stats::median(.data[[value_column]], na.rm = TRUE),
      lower_bound = stats::quantile(.data[[value_column]], probs = lower_prob, na.rm = TRUE),
      upper_bound = stats::quantile(.data[[value_column]], probs = upper_prob, na.rm = TRUE),
      .groups = "drop"
    )

  return(summary_out)

}



# ==============================================================================
#' Title
#'
#' @param long_out
#' @param start_date
#' @param disease_states
#'
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
    # dplyr::filter(time %% 1 == 0) %>%
    dplyr::filter(disease_state %in% disease_states) %>%
    dplyr::mutate(time_integer = floor(time)) %>%
    dplyr::group_by(time_integer, instance) %>%
    dplyr::summarise(d_sum = sum(value), .groups = "drop") %>%
    dplyr::rename(time = time_integer) %>% dplyr::ungroup()

  df_pop <- long_out %>%
    # dplyr::filter(time %% 1 == 0) %>%
    dplyr::filter(disease_state == "P") %>%    # Filter for P compartment (total population)
    dplyr::mutate(time_integer = floor(time)) %>%
    dplyr::group_by(time_integer, instance) %>%
    dplyr::select(time_integer, instance, P = value) %>%
    dplyr::summarise(P_sum = sum(P), .groups = "drop") %>%
    dplyr::rename(time = time_integer) %>% dplyr::ungroup()

  df_combined <- df_states %>%
    dplyr::left_join(df_pop, by = c("time", "instance")) %>%
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
#' @export
#'
subset_simout <- function(long_out, start_date, pop_map_df, agecats, racecats,
                          zones, disease_states){

  # merge with population mapping
  df_long <- merge(long_out, pop_map_df, by = "population_id")

  # process census counts

  long_out_daily <- df_long %>%
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
    dplyr::select(c(instance, date, disease_state,
                    value, age, race, hcez))

  # process new counts, these needed to be sumed up to daily values

  long_out_new_counts_daily <- df_long %>%
    dplyr::filter(disease_state %in% c("n_SE", "n_SV", "n_VE", "n_EI",
                                       "n_EIpresymp", "n_preIsymp", "n_IsympRH",
                                       "n_IsympH", "n_IsympR", "n_HR", "n_HD",
                                       "n_IasympR")) %>%
    dplyr::mutate(disease_state = factor(disease_state,
                                         levels = c("n_SE", "n_SV", "n_VE", "n_EI",
                                                    "n_EIpresymp", "n_preIsymp", "n_IsympRH",
                                                    "n_IsympH", "n_IsympR", "n_HR", "n_HD",
                                                    "n_IasympR"),
                                         # labels = c("n_SE", "n_SV", "n_VE", "n_EI",
                                         #            "n_EIpresymp", "n_preIsymp", "n_IsympRH",
                                         #            "n_IsympH", "n_IsympR", "n_HR", "n_HD",
                                         #            "n_IasympR")
                                         labels = c("New Exposed (S)",
                                                    "New Vaccinated",
                                                    "New Exposed (V)",
                                                    "New Infectious",
                                                    "New Infectious presymptomatic",
                                                    "New Infectious symptomatic",
                                                    "New Recovered/Hospitalized (Is)",
                                                    "New Hospitalized",
                                                    "New Recovered (Is)",
                                                    "New Recovered (H)",
                                                    "New Dead",
                                                    "New Recovered (Ia)")
                                         ),
                  time_integer = floor(time)) %>%
    dplyr::filter(age %in% agecats) %>%
    dplyr::filter(race %in% racecats) %>%
    dplyr::filter(hcez %in% zones) %>%
    dplyr::filter(disease_state %in% disease_states) %>%
    dplyr::group_by(time_integer, disease_state, instance, age,
                    race, hcez) %>%
    dplyr::summarize(total_value = sum(value), .groups = "drop") %>%
    dplyr::rename(time = time_integer, value = total_value) %>%
    dplyr::mutate(date = start_date + time) %>%
    dplyr::select(c(instance, date, disease_state,
                    value, age, race, hcez))


  long_out_sub <- rbind(long_out_daily, long_out_new_counts_daily)

  return(long_out_sub)

}

#' Title
#'
#' @param long_out
#' @param pop_map_df
#' @param start_date
#' @param category
#' @param conf_level
#'
#' @export
#'
cat_summary <- function(long_out, pop_map_df, start_date, category,
                        conf_level){

  # merge with population mapping
  df_long <- merge(long_out, pop_map_df, by = "population_id")

  cat_data <- df_long %>%
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
    dplyr::group_by(time, disease_state, instance, !!dplyr::sym(category)) %>%
    dplyr::summarize(total_value = sum(value), .groups = "drop") %>%
    dplyr::mutate(date = start_date + time)


  return(cat_data)
}


