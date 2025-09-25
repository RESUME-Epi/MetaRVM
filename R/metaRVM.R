#' Run MetaRVM Epidemic Simulation
#'
#' @description
#' Executes a meta-population compartmental epidemic model simulation using specified
#' configuration parameters. The function runs multiple simulation instances with
#' stochastic parameter variations and returns formatted results with calendar dates
#' and demographic attributes for comprehensive analysis and visualization.
#'
#' @param config_input Configuration input in one of three formats:
#'   \itemize{
#'     \item Character string: File path to YAML configuration file
#'     \item MetaRVMConfig object: Pre-initialized configuration object
#'     \item Named list: Parsed configuration data from \code{\link{parse_config}}
#'   }
#'
#' @details
#' The MetaRVM simulation implements a meta-population SEIRD (Susceptible-Exposed-Infected-Recovered-Dead)
#' compartmental model with additional complexity for asymptomatic and presymptomatic infections,
#' hospitalization states, and vaccination dynamics. The model accounts for:
#'
#' \strong{Compartmental Structure:}
#' \itemize{
#'   \item \strong{S}: Susceptible individuals
#'   \item \strong{E}: Exposed (incubating) individuals  
#'   \item \strong{I_asymp}: Asymptomatic infectious individuals
#'   \item \strong{I_presymp}: Presymptomatic infectious individuals
#'   \item \strong{I_symp}: Symptomatic infectious individuals
#'   \item \strong{H}: Hospitalized individuals
#'   \item \strong{R}: Recovered individuals
#'   \item \strong{D}: Dead individuals
#'   \item \strong{P}: Protected/vaccinated individuals
#' }
#'
#' \strong{Model Features:}
#' \itemize{
#'   \item Multiple population groups with demographic stratification (age, race, geography)
#'   \item Time-varying contact patterns (weekday/weekend, day/night mixing matrices)
#'   \item Stochastic parameter variations across simulation instances
#'   \item Vaccination schedules with time-varying administration
#'   \item Checkpointing capability for long simulations
#' }
#'
#' \strong{Disease Parameters:}
#' The model uses the following key parameters (can be stochastic across instances):
#' \itemize{
#'   \item \code{ts}: Transmission rate for symptomatic individuals
#'   \item \code{tv}: Transmission rate for vaccinated individuals
#'   \item \code{ve}: Vaccine effectiveness
#'   \item \code{de, dp, da, ds, dh, dr}: Duration parameters for disease states
#'   \item \code{pea, psr, phr}: Probability parameters for state transitions
#' }
#'
#' \strong{Simulation Process:}
#' For each simulation instance, the function:
#' \enumerate{
#'   \item Initializes population compartments from configuration
#'   \item Applies instance-specific stochastic parameter values
#'   \item Runs the ODE solver (\code{meta_sim}) with specified time steps
#'   \item Collects output for all time points and populations
#'   \item Combines results across all instances
#'   \item Formats output with calendar dates and demographic information
#'   \item Returns structured MetaRVMResults object for analysis
#' }
#'
#' @return
#' Returns a \code{\link{MetaRVMResults}} object containing:
#' \describe{
#'   \item{results}{Formatted data.table with columns:}
#'     \itemize{
#'       \item \code{date}: Calendar date (Date class)
#'       \item \code{age}: Age category (e.g., "0-4", "5-11", "18-49")
#'       \item \code{race}: Race/ethnicity category
#'       \item \code{zone}: Geographic zone identifier
#'       \item \code{disease_state}: Compartment name (S, E, I_asymp, H, etc.)
#'       \item \code{value}: Population count in compartment
#'       \item \code{instance}: Simulation instance number
#'     }
#'   \item{config}{Original MetaRVMConfig object}
#'   \item{run_info}{Simulation metadata including date range, instance count}
#' }
#'
#' The returned object supports method chaining for analysis:
#' \itemize{
#'   \item \code{subset_data()}: Filter by demographics, disease states, dates
#'   \item \code{summarize()}: Aggregate across demographic categories with statistics
#'   \item \code{plot()}: Create time series visualizations (via method chaining)
#' }
#'
#' @section Configuration Requirements:
#' The configuration must include:
#' \itemize{
#'   \item \strong{Population data}: Initial compartment values, demographic mapping
#'   \item \strong{Disease parameters}: Transmission rates, durations, probabilities
#'   \item \strong{Contact matrices}: Weekday/weekend and day/night mixing patterns
#'   \item \strong{Simulation settings}: Start date, length, number of instances
#'   \item \strong{Vaccination schedule}: Time-varying vaccination rates (optional)
#' }
#'
#' @section Performance Notes:
#' \itemize{
#'   \item Simulation time scales with \code{nsim × nsteps × N_pop}
#'   \item Large population numbers or long time periods may require substantial memory
#'   \item Consider checkpointing (\code{do_chk = TRUE}) for long simulations
#'   \item Output formatting adds overhead but provides analysis-ready data structure
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with YAML configuration file
#' results <- metaRVM("path/to/config.yaml")
#' 
#' # Print summary
#' results
#' 
#' # Access formatted data directly
#' head(results$results)
#' 
#' # Method chaining for analysis and visualization
#' results$summarize(
#'   group_by = c("age", "race"),
#'   stats = c("median", "quantile"),
#'   disease_states = c("H", "D")
#' )$plot()
#' 
#' # Subset and analyze specific populations
#' subset_results <- results$subset_data(
#'   age = c("65+"),
#'   disease_states = c("H", "D"),
#'   date_range = c(as.Date("2024-01-01"), as.Date("2024-03-01"))
#' )
#' 
#' # Using with pre-parsed configuration
#' config <- parse_config("config.yaml")
#' config_obj <- MetaRVMConfig$new(config)
#' results <- metaRVM(config_obj)
#' 
#' # Accessing run metadata
#' results$run_info$n_instances
#' results$run_info$date_range
#' }
#'
#' @section Error Handling:
#' The function validates input configuration and will stop with informative messages for:
#' \itemize{
#'   \item Invalid \code{config_input} type or format
#'   \item Missing required configuration parameters
#'   \item Inconsistent population or parameter dimensions
#'   \item File access issues for configuration or checkpoint files
#' }
#'
#' @seealso
#' \code{\link{parse_config}} for configuration file parsing
#' \code{\link{MetaRVMConfig}} for configuration object class
#' \code{\link{MetaRVMResults}} for results object and analysis methods
#' \code{\link{format_metarvm_output}} for output formatting details
#' \code{\link{meta_sim}} for the underlying ODE simulation engine
#'
#' @references
#' Fadikar, A., et al. "Developing and deploying a use-inspired metapopulation modeling framework for detailed tracking of stratified health outcomes"
#'
#' @author Arindam Fadikar, Charles Macal, Ignacio Martinez-Moyano, Jonathan Ozik
#'
#' @export
metaRVM <- function(config_input) {
  # Handle different input types
  if (is.character(config_input)) {
    # Input is a file path - create MetaRVMConfig object
    config_obj <- MetaRVMConfig$new(config_input)
  } else if (inherits(config_input, "MetaRVMConfig")) {
    # Input is already a MetaRVMConfig object
    config_obj <- config_input
  } else if (is.list(config_input)) {
    # Input is a parsed config list - convert to MetaRVMConfig
    config_obj <- MetaRVMConfig$new(config_input)
  } else {
    stop("config_input must be a file path, MetaRVMConfig object, or parsed config list")
  }
  
  # pass inputs to meta_sim
  nsim <- config_obj$config_data$nsim
  nsteps <- floor(config_obj$config_data$sim_length / config_obj$config_data$delta_t)

  out <- data.table::data.table()
  for (ii in 1:nsim){

    o <- meta_sim(is.stoch = 0,
                  nsteps = nsteps,
                  N_pop = config_obj$config_data$N_pop,
                  S0 = config_obj$config_data$S_ini,
                  I0 = config_obj$config_data$I_symp_ini,
                  P0 = config_obj$config_data$P_ini,
                  # V0 = config_obj$config_data$V_ini,
                  R0 = config_obj$config_data$R_ini,
                  H0 = config_obj$config_data$H_ini,
                  D0 = config_obj$config_data$D_ini,
                  E0 = config_obj$config_data$E_ini,
                  Ia0 = config_obj$config_data$I_asymp_ini,
                  Ip0 = config_obj$config_data$I_presymp_ini,
                  m_weekday_day = config_obj$config_data$m_wd_d,
                  m_weekday_night = config_obj$config_data$m_wd_n,
                  m_weekend_day = config_obj$config_data$m_we_d,
                  m_weekend_night = config_obj$config_data$m_we_n,
                  delta_t = config_obj$config_data$delta_t,
                  # tvac = config_obj$config_data$vac_time_id,
                  vac_mat = config_obj$config_data$vac_mat,
                  ts = config_obj$config_data$ts[ii, ],
                  tv = config_obj$config_data$tv[ii, ],
                  dv = config_obj$config_data$dv[ii, ],
                  de = config_obj$config_data$de[ii, ],
                  pea = config_obj$config_data$pea[ii, ],
                  dp = config_obj$config_data$dp[ii, ],
                  da = config_obj$config_data$da[ii, ],
                  ds = config_obj$config_data$ds[ii, ],
                  psr = config_obj$config_data$psr[ii, ],
                  dh = config_obj$config_data$dh[ii, ],
                  phr = config_obj$config_data$phr[ii, ],
                  dr = config_obj$config_data$dr[ii, ],
                  ve = config_obj$config_data$ve[ii, ],
                  do_chk = config_obj$config_data$do_chk,
                  chk_file_name = config_obj$config_data$chk_file_names[ii])

    o$instance <- ii
    out <- rbind(out, o)
  }

  
  # Create and return MetaRVMResults object
  results_obj <- MetaRVMResults$new(out, config_obj)
  return(results_obj)
  # return(out)
}