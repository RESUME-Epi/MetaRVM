#' Format MetaRVM simulation output
#' 
#' This function formats raw MetaRVM simulation output by:
#' 1. Converting time steps to calendar dates
#' 2. Adding demographic attributes from population mapping
#' 3. Handling different disease states appropriately:
#'    - Regular states (S, E, I, etc.): Keep values at integer time points
#'    - New count states (n_ prefix): Sum pairs to get daily counts
#'
#' @param sim_output data.table containing raw simulation output
#' @param config MetaRVMConfig object or config list containing parameters
#' @return data.table with formatted output including calendar dates and demographics
#' 
#' @section Note: 
#' This function is used for formatting the \code{\link{meta_sim}} output when \code{\link{MetaRVM}}
#' function is called.
#' @export
format_metarvm_output <- function(sim_output, config) {
  
  # Handle both MetaRVMConfig objects and config lists
  if (inherits(config, "MetaRVMConfig")) {
    start_date <- config$get("start_date")
    pop_map <- config$get_pop_map()
  } else if (is.list(config)) {
    start_date <- config$start_date
    pop_map <- config$pop_map
  } else {
    stop("config must be either a MetaRVMConfig object or a config list")
  }
  
  # Make a copy to avoid modifying the original
  formatted_results <- data.table::copy(sim_output)
  
  # Join with population mapping to get demographics
  # population_id is character in both tables, so no conversion needed
  formatted_results <- formatted_results[pop_map, on = .(population_id)]
  
  # Handle different disease states
  # For regular states (S, E, I, etc.) - keep only integer time points
  regular_states <- formatted_results[!grepl("^n_", disease_state) & time %% 1 == 0]
  
  # For n_ states (new counts) - sum pairs of time points to get daily counts
  n_states <- formatted_results[grepl("^n_", disease_state)]
  
  if (nrow(n_states) > 0) {
    # Create day groupings: 0.5,1 -> day 1; 1.5,2 -> day 2, etc.
    n_states[, day := ceiling(time)]
    
    # Sum values within each day for n_ states
    n_states_daily <- n_states[, .(
      value = sum(value, na.rm = TRUE)
    ), by = .(day, disease_state, population_id, age, race, zone, instance)]
    
    # Create time column to match regular states (use integer days)
    n_states_daily[, time := day]
    n_states_daily[, day := NULL]
  } else {
    n_states_daily <- data.table::data.table()
  }
  
  # Combine regular states and aggregated n_ states
  if (nrow(n_states_daily) > 0) {
    combined_results <- rbind(
      regular_states[, .(time, disease_state, population_id, age, race, zone, value, instance)],
      n_states_daily[, .(time, disease_state, population_id, age, race, zone, value, instance)]
    )
  } else {
    combined_results <- regular_states[, .(time, disease_state, population_id, age, race, zone, value, instance)]
  }
  
  # Convert time to calendar date
  # time 0 = start_date, time 1 = start_date + 1 day, etc.
  combined_results[, date := start_date + time]
  
  # only keep time > 0
  # Select and reorder final columns
  final_results <- combined_results[time > 0, .(
    date,
    age, 
    race, 
    zone,
    disease_state, 
    value, 
    instance
  )]
  
  # Sort by date, instance, and demographics for better readability
  data.table::setorder(final_results, date, instance, zone, age, race, disease_state)
  
  return(final_results)
}