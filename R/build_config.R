#' Build a MetaRVM configuration
#'
#' Constructs a MetaRVM configuration programmatically. The argument structure
#' mirrors the YAML configuration file format. The result can be returned as a
#' raw list, written to a YAML file, or parsed directly into a
#' [MetaRVMConfig] object — or any combination of the three.
#'
#' @param disease Character. Disease to simulate. Supported values:
#'   `"flu"`, `"measles"`.
#' @param population_init Character. Path to the population initialization CSV.
#' @param mixing_matrices Named list of paths to mixing-matrix CSV files.
#'   Required keys: `weekday_day`, `weekday_night`, `weekend_day`,
#'   `weekend_night`.
#' @param disease_params Named list of disease parameters. Each element may be
#'   a scalar, a per-population vector, or a distribution-spec list such as
#'   `list(dist = "lognormal", mu = -0.7, sd = 0.1)`.
#' @param start_date Character (`"MM/DD/YYYY"`) or [Date]. Simulation start
#'   date.
#' @param sim_length Positive integer. Number of days to simulate.
#' @param nsim Positive integer. Number of parameter draws (default `1`).
#' @param nrep Positive integer. Replicates per parameter draw (default `1`).
#' @param run_id Optional character identifier stored in run metadata.
#'   Auto-generated from the current timestamp when `NULL`.
#' @param vaccination Optional character. Path to the vaccination schedule CSV
#'   (flu only).
#' @param sub_disease_params Optional named list of per-category parameter
#'   overrides. Structure mirrors the `sub_disease_params:` YAML section, e.g.
#'   `list(age = list(child = list(ts = 0.6)))`.
#' @param random_seed Optional integer seed for reproducibility. Auto-generated
#'   when `NULL`.
#' @param delta_t Numeric time-step in days. Defaults to `0.5` when `NULL`.
#' @param checkpoint_dir Optional character. Directory for checkpoint files
#'   (flu only).
#' @param checkpoint_dates Optional character vector of dates
#'   (`"MM/DD/YYYY"`) at which to write checkpoints (flu only).
#' @param restore_from Optional character. Path to a checkpoint `.Rda` file to
#'   restore from (flu only).
#' @param output_file Optional character. When provided the configuration is
#'   written to this path as a YAML file. Paths inside the config are stored
#'   as absolute paths so the file is self-contained regardless of working
#'   directory.
#' @param return_object Logical. When `TRUE` the configuration is parsed and
#'   returned as a [MetaRVMConfig] object. When `FALSE` (default) the raw
#'   configuration list is returned (invisibly when `output_file` is also
#'   given).
#' @param verbose Logical. Passed through to [parse_config()] when
#'   `return_object = TRUE`.
#' @param suppress_odin_messages Logical. Passed through to [parse_config()]
#'   when `return_object = TRUE`.
#'
#' @return A [MetaRVMConfig] object when `return_object = TRUE`; otherwise the
#'   raw configuration list (invisibly if `output_file` was also written).
#'
#' @seealso [parse_config()], [metaRVM()]
#'
#' @examples
#' ext <- function(x) system.file("extdata", x, package = "MetaRVM")
#'
#' # Return the raw config list for inspection / modification
#' cfg <- build_config(
#'   disease         = "flu",
#'   population_init = ext("population_init_n24.csv"),
#'   mixing_matrices = list(
#'     weekday_day   = ext("m_weekday_day.csv"),
#'     weekday_night = ext("m_weekday_night.csv"),
#'     weekend_day   = ext("m_weekend_day.csv"),
#'     weekend_night = ext("m_weekend_night.csv")
#'   ),
#'   disease_params  = list(
#'     ts = 0.5, ve = 0.4, dv = 180, dp = 1, de = 3,
#'     da = 5,   ds = 6,   dh = 8,   dr = 180,
#'     pea = 0.3, psr = 0.95, phr = 0.97
#'   ),
#'   vaccination     = ext("vaccination_n24.csv"),
#'   start_date      = "10/01/2023",
#'   sim_length      = 30L,
#'   nsim            = 2L
#' )
#'
#' # Parse directly to a MetaRVMConfig object
#' cfg_obj <- build_config(
#'   disease         = "flu",
#'   population_init = ext("population_init_n24.csv"),
#'   mixing_matrices = list(
#'     weekday_day   = ext("m_weekday_day.csv"),
#'     weekday_night = ext("m_weekday_night.csv"),
#'     weekend_day   = ext("m_weekend_day.csv"),
#'     weekend_night = ext("m_weekend_night.csv")
#'   ),
#'   disease_params  = list(
#'     ts = 0.5, ve = 0.4, dv = 180, dp = 1, de = 3,
#'     da = 5,   ds = 6,   dh = 8,   dr = 180,
#'     pea = 0.3, psr = 0.95, phr = 0.97
#'   ),
#'   vaccination     = ext("vaccination_n24.csv"),
#'   start_date      = "10/01/2023",
#'   sim_length      = 30L,
#'   return_object   = TRUE
#' )
#'
#' @export
build_config <- function(
    disease,
    population_init,
    mixing_matrices,
    disease_params,
    start_date,
    sim_length,
    nsim                   = 1L,
    nrep                   = 1L,
    run_id                 = NULL,
    vaccination            = NULL,
    sub_disease_params     = NULL,
    random_seed            = NULL,
    delta_t                = NULL,
    checkpoint_dir         = NULL,
    checkpoint_dates       = NULL,
    restore_from           = NULL,
    output_file            = NULL,
    return_object          = FALSE,
    verbose                = FALSE,
    suppress_odin_messages = FALSE
) {
  # ---- validate disease -------------------------------------------------
  disease <- normalize_disease_name(disease)

  # ---- validate required scalar arguments --------------------------------
  if (missing(population_init) || is.null(population_init))
    stop("population_init is required")
  if (missing(mixing_matrices) || is.null(mixing_matrices))
    stop("mixing_matrices is required")
  if (missing(disease_params) || is.null(disease_params))
    stop("disease_params is required")
  if (missing(start_date) || is.null(start_date))
    stop("start_date is required")
  if (missing(sim_length) || is.null(sim_length))
    stop("sim_length is required")

  if (!is.list(disease_params))
    stop("disease_params must be a named list")

  required_mix_keys <- c("weekday_day", "weekday_night", "weekend_day", "weekend_night")
  missing_mix <- setdiff(required_mix_keys, names(mixing_matrices))
  if (length(missing_mix) > 0)
    stop("mixing_matrices is missing required keys: ", paste(missing_mix, collapse = ", "))

  sim_length <- as.integer(sim_length)
  if (is.na(sim_length) || sim_length <= 0L)
    stop("sim_length must be a positive integer")
  nsim <- as.integer(nsim)
  if (is.na(nsim) || nsim <= 0L) stop("nsim must be a positive integer")
  nrep <- as.integer(nrep)
  if (is.na(nrep) || nrep <= 0L) stop("nrep must be a positive integer")

  # ---- normalise start_date to string ------------------------------------
  if (inherits(start_date, "Date")) {
    start_date <- format(start_date, "%m/%d/%Y")
  } else {
    start_date <- as.character(start_date)
  }

  # ---- resolve all file paths to absolute --------------------------------
  # Absolute paths make the config self-contained when written to YAML or
  # passed through a temp file for parsing.
  population_init <- normalizePath(population_init, mustWork = TRUE)
  for (key in required_mix_keys) {
    mixing_matrices[[key]] <- normalizePath(mixing_matrices[[key]], mustWork = TRUE)
  }
  if (!is.null(vaccination))   vaccination   <- normalizePath(vaccination,   mustWork = TRUE)
  if (!is.null(restore_from))  restore_from  <- normalizePath(restore_from,  mustWork = TRUE)
  if (!is.null(checkpoint_dir)) checkpoint_dir <- normalizePath(checkpoint_dir, mustWork = FALSE)
  if (!is.null(output_file))   output_file   <- normalizePath(output_file,   mustWork = FALSE)

  # ---- auto-generate run_id if not provided ------------------------------
  if (is.null(run_id)) {
    run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }

  # ---- build the YAML-equivalent list ------------------------------------
  simulation_config <- list(
    start_date = start_date,
    length     = sim_length,
    nsim       = nsim,
    nrep       = nrep
  )
  if (!is.null(random_seed))      simulation_config$random_seed           <- as.integer(random_seed)
  if (!is.null(delta_t))          simulation_config$delta_t               <- delta_t
  if (!is.null(checkpoint_dir))   simulation_config$checkpoint_dir        <- checkpoint_dir
  if (!is.null(checkpoint_dates)) simulation_config$checkpoint_dates      <- as.list(checkpoint_dates)
  if (!is.null(restore_from))     simulation_config$restore_from          <- restore_from
  if (isTRUE(verbose))            simulation_config$verbose               <- TRUE
  if (isTRUE(suppress_odin_messages)) simulation_config$suppress_odin_messages <- TRUE

  population_data <- list(initialization = population_init)
  if (!is.null(vaccination)) population_data$vaccination <- vaccination

  cfg <- list(
    run_id      = run_id,
    model       = list(disease = disease),
    population_data = population_data,
    mixing_matrix = list(
      weekday_day   = mixing_matrices$weekday_day,
      weekday_night = mixing_matrices$weekday_night,
      weekend_day   = mixing_matrices$weekend_day,
      weekend_night = mixing_matrices$weekend_night
    ),
    disease_params    = disease_params,
    simulation_config = simulation_config
  )
  if (!is.null(sub_disease_params)) cfg$sub_disease_params <- sub_disease_params

  # ---- write YAML if requested -------------------------------------------
  if (!is.null(output_file)) {
    yaml::write_yaml(cfg, output_file)
  }

  # ---- parse and return object if requested ------------------------------
  if (return_object) {
    if (!is.null(output_file)) {
      # Re-use the file we just wrote so there's no second temp copy
      return(parse_config(output_file, return_object = TRUE))
    }
    tmp <- tempfile(fileext = ".yaml")
    on.exit(unlink(tmp), add = TRUE)
    yaml::write_yaml(cfg, tmp)
    return(parse_config(tmp, return_object = TRUE))
  }

  if (!is.null(output_file)) invisible(cfg) else cfg
}
