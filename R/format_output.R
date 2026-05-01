#' Format raw MetaRVM simulation output
#'
#' @description
#' Converts raw step-indexed output from [meta_sim()] or [meta_measles_sim()]
#' into a tidy long-format `data.table` with calendar dates and user-defined
#' demographic attributes.
#'
#' The transformation differs by state type:
#' - **Compartment states** (S, E, I, H, R, D, V, …): only integer time-step
#'   values are retained.
#' - **Flow states** (`n_` prefix): within-day pairs are summed to daily
#'   counts.
#'
#' @param sim_output `data.table` of raw simulation output from [meta_sim()] or
#'   [meta_measles_sim()]. Must contain columns `time`, `disease_state`,
#'   `population_id`, `value`, and `instance`.
#' @param config A [`MetaRVMConfig`] object or a named list. Must supply
#'   `start_date`, `pop_map` (with a `population_id` column), and
#'   `category_names`.
#'
#' @return A `data.table` in long format with one row per (date, demographic
#'   combination, disease state, simulation instance). Columns:
#'   \describe{
#'     \item{`date`}{`Date`. Calendar date derived from `start_date + time`.}
#'     \item{User category columns}{One column per demographic category
#'       detected in the population CSV (e.g., `age`, `zone`, `race`).}
#'     \item{`disease_state`}{`character`. Compartment or flow-count state label.}
#'     \item{`value`}{`numeric`. Compartment count or summed daily flow count.}
#'     \item{`instance`}{`integer`. Simulation instance index.}
#'   }
#'
#' @seealso [metaRVM()], [meta_sim()], [`MetaRVMResults`]
#'
#' @examples
#' \dontrun{
#' # Typically called internally by metaRVM(); exposed for custom pipelines.
#' cfg <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
#' config <- parse_config(cfg)
#' raw <- meta_sim(...)  # low-level engine output
#' formatted <- format_metarvm_output(raw, config)
#' }
#'
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
  
  # Right-join: pop_map[formatted_results, on = ...] keeps every row from
  # formatted_results and appends matching category columns from pop_map.
  # This is data.table's equivalent of dplyr::left_join(formatted_results, pop_map).
  formatted_results <- pop_map[formatted_results, on = .(population_id)]

  # Compartment states (S, E, I, ...) are recorded at every half-step; only
  # integer time steps represent end-of-day snapshots we want to report.
  regular_states <- formatted_results[!grepl("^n_", disease_state) & time %% 1 == 0]

  # Flow states (n_ prefix) capture events within each half-step.  Summing both
  # half-steps (e.g., t=0.5 and t=1) gives the full-day count.
  # ceiling(time) maps 0.5→1, 1→1, 1.5→2, 2→2, etc., grouping each pair.
  n_states <- formatted_results[grepl("^n_", disease_state)]

  if (nrow(n_states) > 0) {
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
  
  # start_date is already one day before the user's requested start (the -1
  # offset in parse_config), so adding integer time aligns to the correct dates.
  combined_results[, date := start_date + time]

  # Exclude time == 0 (the synthetic "day before" initial state) from output.
  final_cols <- c("date", category_cols, "disease_state", "value", "instance")
  final_results <- combined_results[time > 0, final_cols, with = FALSE]

  # Sort by date, instance, and demographics for better readability
  sort_cols <- c("date", "instance", category_cols, "disease_state")
  data.table::setorderv(final_results, sort_cols)
  
  return(final_results)
}
