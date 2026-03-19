#' Format MetaRVM simulation output
#' 
#' This function formats raw MetaRVM simulation output by:
#' 1. Converting time steps to calendar dates
#' 2. Adding user-defined demographic attributes from the initialization-derived population metadata
#' 3. Handling different disease states appropriately:
#'    - Regular states (S, E, I, etc.): Keep values at integer time points
#'    - New count states (n_ prefix): Sum pairs to get daily counts
#'
#' @param sim_output data.table containing raw simulation output from \code{\link{meta_sim}}
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
    category_cols <- config$get_category_names()
  } else if (is.list(config)) {
    start_date <- config$start_date
    pop_map <- config$pop_map
    category_cols <- if ("category_names" %in% names(config)) {
      config$category_names
    } else {
      character(0)
    }
  } else {
    stop("config must be either a MetaRVMConfig object or a config list")
  }

  # Derive categories dynamically if they are not explicitly available
  if (length(category_cols) == 0 && !is.null(pop_map)) {
    category_cols <- setdiff(names(pop_map), "population_id")
  }
  
  # Make a copy to avoid modifying the original
  formatted_results <- data.table::copy(sim_output)

  if (!"population_id" %in% names(formatted_results)) {
    stop("sim_output must contain a 'population_id' column")
  }
  if (is.null(pop_map) || !"population_id" %in% names(pop_map)) {
    stop("config must provide pop_map with a 'population_id' column")
  }

  # Ensure compatible join keys for dynamically defined subpopulations
  formatted_results[, population_id := as.character(population_id)]
  pop_map <- data.table::copy(pop_map)
  pop_map[, population_id := as.character(population_id)]
  
  # Join with population mapping to get demographics
  # Keep all simulation rows and append user-defined category columns
  formatted_results <- pop_map[formatted_results, on = .(population_id)]
  
  # Handle different disease states
  # For regular states (S, E, I, etc.) - keep only integer time points
  regular_states <- formatted_results[!grepl("^n_", disease_state) & time %% 1 == 0]
  
  # For n_ states (new counts) - sum pairs of time points to get daily counts
  n_states <- formatted_results[grepl("^n_", disease_state)]
  
  if (nrow(n_states) > 0) {
    # Create day groupings: 0.5,1 -> day 1; 1.5,2 -> day 2, etc.
    n_states[, day := ceiling(time)]

    # Sum values within each day for n_ states
    # Build grouping columns dynamically
    group_cols <- c("day", "disease_state", "population_id", category_cols, "instance")
    n_states_daily <- n_states[, .(
      value = sum(value, na.rm = TRUE)
    ), by = group_cols]
    
    # Create time column to match regular states (use integer days)
    n_states_daily[, time := day]
    n_states_daily[, day := NULL]
  } else {
    n_states_daily <- data.table::data.table()
  }
  
  # Combine regular states and aggregated n_ states
  # Build column selection dynamically
  select_cols <- c("time", "disease_state", "population_id", category_cols, "value", "instance")

  if (nrow(n_states_daily) > 0) {
    combined_results <- rbind(
      regular_states[, select_cols, with = FALSE],
      n_states_daily[, select_cols, with = FALSE]
    )
  } else {
    combined_results <- regular_states[, select_cols, with = FALSE]
  }
  
  # Convert time to calendar date
  # time 0 = start_date, time 1 = start_date + 1 day, etc.
  combined_results[, date := start_date + time]
  
  # only keep time > 0
  # Select and reorder final columns dynamically
  final_cols <- c("date", category_cols, "disease_state", "value", "instance")
  final_results <- combined_results[time > 0, final_cols, with = FALSE]

  # Sort by date, instance, and demographics for better readability
  sort_cols <- c("date", "instance", category_cols, "disease_state")
  data.table::setorderv(final_results, sort_cols)
  
  return(final_results)
}
