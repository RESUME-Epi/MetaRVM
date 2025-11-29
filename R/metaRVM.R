#' Run a MetaRVM epidemic simulation
#'
#' @description
#' `metaRVM()` is the high-level entry point for running a MetaRVM
#' metapopulation respiratory virus simulation. It parses the configuration,
#' runs one or more simulation instances (deterministic or stochastic),
#' formats the ODIN/MetaRVM output into a tidy long table with calendar
#' dates and demographic attributes, and returns a [`MetaRVMResults`]
#' object for downstream analysis and plotting.
#'
#' @details
#' The configuration input controls:
#'
#' - **Population structure** (e.g., age, race, zone)
#' - **Disease parameters** (`ts`, `tv`, `ve`, `de`, `dp`, `da`, `ds`,
#'   `dh`, `dr`, `pea`, `psr`, `phr`, `dv`, etc.)
#' - **Mixing matrices** (weekday/weekend, day/night contact patterns)
#' - **Vaccination schedule** and immunity waning
#' - **Simulation settings** (start date, length, number of instances,
#'   stochastic vs. deterministic mode, checkpointing)
#'
#' Internally, `metaRVM()`:
#'
#' 1. Parses the YAML configuration via [parse_config()].
#' 2. Calls the ODIN-based simulation engine [meta_sim()] for each instance.
#' 3. Uses [format_metarvm_output()] to convert time steps to dates and
#'    attach demographic attributes.
#' 4. Wraps the formatted output and metadata in a [`MetaRVMResults`]
#'    object that supports method chaining for subsetting, summarizing,
#'    and plotting.
#'
#' @param config_input Configuration specification in one of three forms:
#'   \itemize{
#'     \item **Character string**: path to a YAML configuration file.
#'     \item **[`MetaRVMConfig`] object**: pre-initialized configuration.
#'     \item **Named list**: output from [parse_config()] with
#'       `return_object = FALSE`.
#'   }
#'
#' @return
#' A [`MetaRVMResults`] R6 object with three key components:
#' \describe{
#'   \item{$results}{A tidy `data.table` with one row per
#'     date–subpopulation–disease state–instance combination. Typical
#'     columns include:
#'     \itemize{
#'       \item `date`: calendar date (`Date`)
#'       \item `age`, `race`, `zone`: demographic categories (if present
#'             in the population mapping)
#'       \item `disease_state`: compartment or flow label (e.g., `S`, `E`,
#'             `I_symp`, `H`, `R`, `D`, `n_SE`, `n_IsympH`, etc.)
#'       \item `value`: population count or daily flow
#'       \item `instance`: simulation instance index (1, 2, …)
#'     }
#'   }
#'   \item{$config}{The [`MetaRVMConfig`] object used for the run.}
#'   \item{$run_info}{A list with metadata such as `n_instances`,
#'     `date_range`, `delta_t`, and checkpoint information.}
#' }
#'
#' @seealso
#' [parse_config()] for reading YAML configurations,
#' [MetaRVMConfig] for configuration management,
#' [MetaRVMResults] for analysis and plotting,
#' [meta_sim()] for the low-level simulation engine.
#'
#' @examples
#' options(odin.verbose = FALSE)
#' example_config <- system.file("extdata", "example_config.yaml",
#'                               package = "MetaRVM")
#'
#' # Run a single-instance simulation from a YAML file
#' results <- metaRVM(example_config)
#'
#' # Print a high-level summary
#' results
#'
#' # Access the tidy results table
#' head(results$results)
#'
#' # Summarize and plot hospitalizations and deaths by age and race
#' results$summarize(
#'   group_by       = c("age", "race"),
#'   disease_states = c("H", "D"),
#'   stats          = c("median", "quantile"),
#'   quantiles      = c(0.25, 0.75)
#' )$plot()
#'
#' # Using a pre-parsed configuration object
#' cfg <- parse_config(example_config, return_object = TRUE)
#' results2 <- metaRVM(cfg)
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
                  V0 = config_obj$config_data$V_ini,
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
                  chk_time_steps = config_obj$config_data$chk_time_steps,
                  chk_file_names = config_obj$config_data$chk_file_names[ii, ])

    o$instance <- ii
    out <- rbind(out, o)
  }

  
  # Create and return MetaRVMResults object
  results_obj <- MetaRVMResults$new(out, config_obj)
  return(results_obj)
  # return(out)
}
