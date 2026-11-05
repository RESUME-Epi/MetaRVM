# Internal helpers for disease-aware parsing/running.
#
# ==============================================================================
# Maintainer Notes: Why this registry exists
# ------------------------------------------------------------------------------
# This file is the package's "disease plug-in contract". The core philosophy is:
# 1) Keep public entry points stable (`parse_config()`, `metaRVM()`).
# 2) Move disease-specific variability into declarative registry entries.
# 3) Avoid spreading `if (disease == "...")` checks across many files.
#
# In practice:
# - `parse_config()` asks this registry which config builder to use.
# - `metaRVM()` asks this registry which simulation engine to call and how to
#   build that engine's argument list.
# - downstream helpers (plot/summary/subset) ask this registry for output specs.
#
# This pattern scales better than branching logic because adding a disease becomes
# mostly "add one entry here + implement referenced functions", without changing
# high-level orchestration code.
#
# ==============================================================================
# Maintainer Notes: Registry entry contract
# ------------------------------------------------------------------------------
# Each disease entry should define (at minimum):
# - `parser`: currently kept for compatibility/clarity.
# - `config_builder`: function name used by `parse_config()`.
# - `runner`: usually "metaRVM" (single run entrypoint).
# - `engine_fn`: low-level ODIN engine function called inside run loop.
# - `always_stochastic`: TRUE to override any legacy simulation_mode in config.
# - `checkpointing_from_config`: whether run_info should reflect do_chk.
# - `build_sim_args(config_data, ii, run_idx, nsteps, start_day, is_stoch)`.
#
# Optional but strongly recommended:
# - `output_spec`: naming conventions and state groups for user APIs.
# - init schema fields (`init_required_cols`, `init_optional_defaults`, etc.).
# - disease param schema fields (`disease_param_required`, ...).
#
# IMPORTANT invariants for `build_sim_args`:
# - Return names must exactly match formal args of `engine_fn`.
# - `run_idx` is instance index across `nsim * nrep` and should be used for
#   checkpoint filename lookup (not `ii`), otherwise replicates can overwrite or
#   load wrong files.
# - Any optional values should be defaulted here, so `metaRVM()` remains generic.
#
# ==============================================================================
# Maintainer Notes: How to add a new disease
# ------------------------------------------------------------------------------
# 1) Implement a parser builder (e.g., `parse_config_newdisease_internal`).
# 2) Implement or expose a simulation engine (e.g., `meta_newdisease_sim`).
# 3) Add a new registry entry with fields above.
# 4) Add example config/init/mixing files and smoke tests.
# 5) Verify:
#    - `parse_config(config)$disease` resolves correctly,
#    - `metaRVM(config)` returns `MetaRVMResults`,
#    - summary/plot state mapping works via `output_spec`.
# ==============================================================================

normalize_disease_name <- function(disease) {
  if (is.null(disease) || length(disease) == 0) {
    stop("'disease' field is required in config but was not provided. Supported diseases: ",
         paste(names(metarvm_disease_registry()), collapse = ", "))
  }
  # [1] silently takes the first element if a vector is accidentally passed;
  # as.character handles factors and numeric inputs from YAML parsers.
  disease_chr <- tolower(trimws(as.character(disease)[1]))
  if (identical(disease_chr, "")) {
    stop("'disease' field is required in config but was empty. Supported diseases: ",
         paste(names(metarvm_disease_registry()), collapse = ", "))
  }
  disease_chr
}

metarvm_disease_registry <- function() {
  # -----------------------------------------------------------------------
  # Registry contract — every disease entry must define these fields:
  #
  #   parser / config_builder / runner / engine_fn
  #     Function names for the parse, build, orchestrate, and simulate steps.
  #
  #   always_stochastic
  #     TRUE  → engine ignores `is_stoch` and always runs stochastic.
  #     FALSE → caller may request deterministic mode.
  #
  #   checkpointing_from_config
  #     TRUE  → `do_chk` / checkpoint params come from the YAML config.
  #     FALSE → checkpointing not supported; `do_chk` is always FALSE.
  #
  #   vaccination_mechanism
  #     "file"    → vaccination is a time-varying schedule loaded from a CSV
  #                 specified under population_data$vaccination in the YAML.
  #                 metarvm_parse_vaccination() reads and processes this file.
  #     "initial" → vaccination state is encoded directly as initial compartment
  #                 counts in the population CSV (e.g. V10, V20 for measles).
  #                 No separate vaccination file is read or needed.
  #
  #   Population CSV init column contract (enforced by metarvm_validate_population_table):
  #
  #     init_reserved_cols    All column names that have a special meaning
  #                           (not treated as demographic categories).
  #     init_required_cols    Columns that MUST be present in a fresh run.
  #     init_require_any_of   At least one of these must be present (or NULL).
  #     init_optional_defaults Named list of columns defaulted to 0 when absent.
  #
  #   Disease-specific init schema:
  #
  #     flu     Required: population_id, N, S0, I0, R0, V0
  #             Optional: E0, Ia0, Ip0, H0, D0 (default 0)
  #             Vaccination: separate time-series file (vaccination_mechanism = "file")
  #
  #     measles Required: population_id, N, S0
  #             Require any of: I1_U0, I2_U0 (must seed at least one infectious compartment)
  #             Optional: all other compartments default to 0
  #             Vaccination: V10 / V20 initial counts in the population CSV
  #                          (vaccination_mechanism = "initial"); no separate file
  # -----------------------------------------------------------------------
  list(
    flu = list(
      parser = "parse_config",
      config_builder = "parse_config_flu_internal",
      runner = "metaRVM",
      engine_fn = "meta_sim",
      always_stochastic = FALSE,
      checkpointing_from_config = TRUE,
      vaccination_mechanism = "file",
      build_sim_args = function(config_data, ii, run_idx, nsteps, start_day, is_stoch) {
        # ii       = parameter-set index (1..nsim); selects which row of each
        #            nsim × N_pop disease-parameter matrix to use for this draw.
        # run_idx  = global instance index (1..nsim*nrep); used for checkpoint
        #            filenames so parallel replicates don't overwrite each other.
        list(
          is.stoch = is_stoch,
          nsteps = nsteps,
          N_pop = config_data$N_pop,
          # Initial compartment counts (vectors of length N_pop)
          S0 = config_data$S_ini,
          I0 = config_data$I_symp_ini,
          P0 = config_data$P_ini,
          V0 = config_data$V_ini,
          R0 = config_data$R_ini,
          H0 = config_data$H_ini,
          D0 = config_data$D_ini,
          E0 = config_data$E_ini,
          Ia0 = config_data$I_asymp_ini,
          Ip0 = config_data$I_presymp_ini,
          # Time-varying contact matrices (N_pop × N_pop each)
          m_weekday_day = config_data$m_wd_d,
          m_weekday_night = config_data$m_wd_n,
          m_weekend_day = config_data$m_we_d,
          m_weekend_night = config_data$m_we_n,
          start_day = start_day,
          delta_t = config_data$delta_t,
          vac_mat = config_data$vac_mat,
          # Disease parameters: row ii of nsim × N_pop matrix → vector for this draw
          ts = config_data$ts[ii, ],
          dv = config_data$dv[ii, ],
          de = config_data$de[ii, ],
          pea = config_data$pea[ii, ],
          dp = config_data$dp[ii, ],
          da = config_data$da[ii, ],
          ds = config_data$ds[ii, ],
          psr = config_data$psr[ii, ],
          dh = config_data$dh[ii, ],
          phr = config_data$phr[ii, ],
          dr = config_data$dr[ii, ],
          ve = config_data$ve[ii, ],
          # Checkpointing: run_idx (not ii) keeps replicate files distinct
          do_chk = config_data$do_chk,
          chk_time_steps = config_data$chk_time_steps,
          chk_file_names = config_data$chk_file_names[run_idx, ]
        )
      },
      output_spec = list(
        state_col = "disease_state",
        value_col = "value",
        time_col = "date",
        default_exclude_patterns = c("^p_"),
        state_groups = list(
          incidence      = c("n_SE", "n_VE"),   # new exposures from S and V
          hospitalizations = c("n_IsympH"),      # symptomatic → hospital
          deaths         = c("n_HD"),            # hospital → dead
          recoveries     = c("n_IsympR", "n_IasympR", "n_HR")  # all recovery flows
        )
      ),
      required_config_fields = c(
        "disease", "N_pop", "pop_map", "start_date",
        "sim_length", "delta_t", "nsim", "nrep"
      ),
      required_checkpoint_fields = c(
        "disease", "N_pop", "delta_t",
        "m_weekday_day", "m_weekday_night", "m_weekend_day", "m_weekend_night",
        "ts", "ve", "dv", "de", "dp", "da", "ds", "dh", "dr", "pea", "psr", "phr",
        "S", "E", "Ia", "Ip", "Is", "H", "D", "P", "V", "R",
        "chk_time_step"
      ),
      disease_param_required = c(
        # Transmission / vaccine
        "ts",   # transmission probability per contact
        "ve",   # vaccine efficacy (fraction of V that can't be infected)
        "dv",   # duration of vaccine-conferred immunity (days)
        # Progression durations
        "de",   # mean duration in exposed (E) compartment (days)
        "dp",   # mean duration in pre-symptomatic (Ip) compartment (days)
        "da",   # mean duration in asymptomatic (Ia) compartment (days)
        "ds",   # mean duration in symptomatic (Is) compartment (days)
        "dh",   # mean duration in hospital (H) compartment (days)
        "dr",   # mean duration in recovered (R) before returning to S (days)
        # Branching probabilities
        "pea",  # proportion of exposed → asymptomatic (vs pre-symptomatic)
        "psr",  # proportion of symptomatic → recovery (vs hospital)
        "phr"   # proportion of hospitalized → recovery (vs death)
      ),
      init_reserved_cols = c(
        "population_id", "N", "S0", "I0", "R0", "V0",
        "E0", "Ia0", "Ip0", "H0", "D0", "P0"
      ),
      init_required_cols = c("population_id", "N", "S0", "I0", "R0", "V0"),
      init_require_any_of = NULL,
      init_optional_defaults = list(E0 = 0, Ia0 = 0, Ip0 = 0, H0 = 0, D0 = 0)
    ),
    measles = list(
      parser = "parse_config",
      config_builder = "parse_config_measles_internal",
      runner = "metaRVM",
      engine_fn = "meta_measles_sim",
      always_stochastic = TRUE,
      checkpointing_from_config = TRUE,
      vaccination_mechanism = "initial",
      build_sim_args = function(config_data, ii, run_idx, nsteps, start_day, is_stoch) {
        # ii = parameter-set index; selects the row of each nsim × N_pop matrix.
        # Measles has no deterministic mode, so is.stoch is hard-coded TRUE
        # regardless of what the caller passes.
        args <- list(
          is.stoch = TRUE,
          nsteps = nsteps,
          N_pop = config_data$N_pop,
          # Unvaccinated initial compartment counts
          S0 = config_data$S0,
          E10 = config_data$E10,
          E20 = config_data$E20,
          I1_Q0 = config_data$I1_Q0,
          I1_U0 = config_data$I1_U0,
          I2_Q0 = config_data$I2_Q0,
          I2_U0 = config_data$I2_U0,
          R0 = config_data$R0,
          # Vaccinated initial compartment counts
          V10 = config_data$V10,
          V20 = config_data$V20,
          # V1 breakthrough compartment initials
          V1E10   = config_data$V1E10,
          V1E20   = config_data$V1E20,
          V1I1_Q0 = config_data$V1I1_Q0,
          V1I1_U0 = config_data$V1I1_U0,
          V1I2_Q0 = config_data$V1I2_Q0,
          V1I2_U0 = config_data$V1I2_U0,
          V1R0    = config_data$V1R0,
          # V2 breakthrough compartment initials
          V2E10   = config_data$V2E10,
          V2E20   = config_data$V2E20,
          V2I1_Q0 = config_data$V2I1_Q0,
          V2I1_U0 = config_data$V2I1_U0,
          V2I2_Q0 = config_data$V2I2_Q0,
          V2I2_U0 = config_data$V2I2_U0,
          V2R0    = config_data$V2R0,
          # Time-varying contact matrices (N_pop × N_pop each)
          m_weekday_day = config_data$m_wd_d,
          m_weekday_night = config_data$m_wd_n,
          m_weekend_day = config_data$m_we_d,
          m_weekend_night = config_data$m_we_n,
          start_day = start_day,
          delta_t = config_data$delta_t,
          # Disease parameters: row ii of nsim × N_pop matrix → vector for this draw
          beta = config_data$beta[ii, ],
          de1 = config_data$de1[ii, ],
          de2 = config_data$de2[ii, ],
          di1 = config_data$di1[ii, ],
          di2 = config_data$di2[ii, ],
          prop_I1_Q = config_data$prop_I1_Q[ii, ],
          prop_I2_Q = config_data$prop_I2_Q[ii, ]
        )

        # V1_eff/V2_eff can be a scalar vector or nsim × N_pop matrix when sampled.
        # Default to 1 (no vaccine effect) when absent from config.
        if ("V1_eff" %in% names(config_data)) {
          args$V1_eff <- if (is.matrix(config_data$V1_eff)) config_data$V1_eff[ii, ] else config_data$V1_eff
        } else {
          args$V1_eff <- rep(1, config_data$N_pop)
        }
        if ("V2_eff" %in% names(config_data)) {
          args$V2_eff <- if (is.matrix(config_data$V2_eff)) config_data$V2_eff[ii, ] else config_data$V2_eff
        } else {
          args$V2_eff <- rep(1, config_data$N_pop)
        }
        # Checkpointing: run_idx (not ii) keeps replicate files distinct
        args$do_chk         <- config_data$do_chk
        args$chk_time_steps <- config_data$chk_time_steps
        args$chk_file_names <- config_data$chk_file_names[run_idx, ]
        args
      },
      output_spec = list(
        state_col = "disease_state",
        value_col = "value",
        time_col = "date",
        default_exclude_patterns = c("^p_"),
        state_groups = list(
          incidence = c("n_SE1", "n_V1E1", "n_V2E1")
        )
      ),
      required_config_fields = c(
        "disease", "N_pop", "pop_map", "start_date",
        "sim_length", "delta_t", "nsim", "nrep",
        "S0", "E10", "E20", "I1_Q0", "I1_U0", "I2_Q0", "I2_U0", "R0",
        "V10", "V20",
        "V1E10", "V1E20", "V1I1_Q0", "V1I1_U0", "V1I2_Q0", "V1I2_U0", "V1R0",
        "V2E10", "V2E20", "V2I1_Q0", "V2I1_U0", "V2I2_Q0", "V2I2_U0", "V2R0",
        "beta", "de1", "de2", "di1", "di2", "prop_I1_Q", "prop_I2_Q", "V1_eff", "V2_eff"
      ),
      required_checkpoint_fields = c(
        "disease", "N_pop", "delta_t",
        "m_weekday_day", "m_weekday_night", "m_weekend_day", "m_weekend_night",
        "beta", "de1", "de2", "di1", "di2", "prop_I1_Q", "prop_I2_Q", "V1_eff", "V2_eff",
        "S", "V1", "V2", "E1", "E2", "I1_Q", "I1_U", "I2_Q", "I2_U", "R",
        "V1E1", "V1E2", "V1I1_Q", "V1I1_U", "V1I2_Q", "V1I2_U", "V1R",
        "V2E1", "V2E2", "V2I1_Q", "V2I1_U", "V2I2_Q", "V2I2_U", "V2R",
        "chk_time_step"
      ),
      init_reserved_cols = c(
        "population_id", "N", "S0", "E10", "E20",
        "I1_Q0", "I1_U0", "I2_Q0", "I2_U0", "R0",
        "V10", "V20",
        "V1E10", "V1E20", "V1I1_Q0", "V1I1_U0", "V1I2_Q0", "V1I2_U0", "V1R0",
        "V2E10", "V2E20", "V2I1_Q0", "V2I1_U0", "V2I2_Q0", "V2I2_U0", "V2R0"
      ),
      init_required_cols = c("population_id", "N", "S0"),
      init_require_any_of = c("I1_U0", "I2_U0"),
      init_optional_defaults = list(
        E10 = 0, E20 = 0,
        I1_Q0 = 0, I1_U0 = 0, I2_Q0 = 0, I2_U0 = 0, R0 = 0,
        V10 = 0, V20 = 0,
        V1E10 = 0, V1E20 = 0, V1I1_Q0 = 0, V1I1_U0 = 0, V1I2_Q0 = 0, V1I2_U0 = 0, V1R0 = 0,
        V2E10 = 0, V2E20 = 0, V2I1_Q0 = 0, V2I1_U0 = 0, V2I2_Q0 = 0, V2I2_U0 = 0, V2R0 = 0
      ),
      disease_param_required = c(
        # Transmission
        "beta",      # per-contact transmission rate (S → E1 force of infection)
        # Exposed stage durations (two-stage latent period)
        "de1",       # mean duration in E1 (first exposed stage, days)
        "de2",       # mean duration in E2 (second exposed stage, days)
        # Infectious stage durations (two-stage infectious period)
        "di1",       # mean duration in I1 (first infectious stage, days)
        "di2",       # mean duration in I2 (second infectious stage, days)
        # Quarantine fractions
        "prop_I1_Q", # proportion of E2→I1 transitions entering quarantined I1_Q
        "prop_I2_Q", # proportion of I1_U→I2 transitions entering quarantined I2_Q
        # Vaccine efficacy (reduce force of infection on V1/V2 compartments)
        "V1_eff",    # efficacy of one-dose vaccine (fraction reduction in susceptibility)
        "V2_eff"     # efficacy of two-dose vaccine (fraction reduction in susceptibility)
      )
    )
  )
}

get_metarvm_disease_entry <- function(disease) {
  # Normalize first so callers can pass raw user input (e.g. "Flu", "FLU").
  disease_id <- normalize_disease_name(disease)
  registry <- metarvm_disease_registry()
  if (!disease_id %in% names(registry)) {
    stop(
      "Unsupported disease: ", disease_id,
      ". Supported diseases are: ",
      paste(names(registry), collapse = ", ")
    )
  }
  registry[[disease_id]]
}

get_metarvm_required_config_fields <- function(disease) {
  entry <- get_metarvm_disease_entry(disease)
  entry$required_config_fields
}

get_metarvm_required_checkpoint_fields <- function(disease) {
  entry <- get_metarvm_disease_entry(disease)
  entry$required_checkpoint_fields
}

get_metarvm_engine_fn <- function(disease) {
  entry <- get_metarvm_disease_entry(disease)
  fn_name <- entry$engine_fn
  if (is.null(fn_name)) {
    stop("No engine_fn defined for disease: ", disease)
  }
  # get(..., mode = "function") resolves the string name to the actual function
  # object in the package namespace at call time, avoiding a hard import.
  get(fn_name, mode = "function")
}

get_metarvm_config_builder <- function(disease) {
  entry <- get_metarvm_disease_entry(disease)
  fn_name <- entry$config_builder
  if (is.null(fn_name)) {
    stop("No config_builder defined for disease: ", disease)
  }
  # Same late-binding pattern as get_metarvm_engine_fn: string → function.
  get(fn_name, mode = "function")
}

build_metarvm_sim_args <- function(disease, config_data, ii, run_idx, nsteps, start_day, is_stoch) {
  entry <- get_metarvm_disease_entry(disease)
  builder <- entry$build_sim_args
  if (is.null(builder) || !is.function(builder)) {
    stop("No build_sim_args function defined for disease: ", disease)
  }
  # The returned list's names must exactly match the formal argument names of
  # engine_fn; metaRVM() passes it via do.call(engine_fn, sim_args).
  builder(
    config_data = config_data,
    ii = ii,
    run_idx = run_idx,
    nsteps = nsteps,
    start_day = start_day,
    is_stoch = is_stoch
  )
}

get_metarvm_output_spec <- function(disease) {
  entry <- get_metarvm_disease_entry(disease)
  spec <- entry$output_spec
  if (is.null(spec)) {
    spec <- list()
  }
  # Fill in package-wide defaults so callers always receive a complete spec,
  # even when a disease entry omits optional output fields.
  if (is.null(spec$state_col)) spec$state_col <- "disease_state"
  if (is.null(spec$value_col)) spec$value_col <- "value"
  if (is.null(spec$time_col)) spec$time_col <- "date"
  if (is.null(spec$default_exclude_patterns)) spec$default_exclude_patterns <- c("^p_")
  if (is.null(spec$state_groups)) spec$state_groups <- list()
  spec
}

resolve_metarvm_state_selection <- function(disease, states) {
  if (is.null(states)) {
    return(NULL)
  }
  spec <- get_metarvm_output_spec(disease)
  state_groups <- spec$state_groups
  # Expand named groups (e.g. flu "incidence" → c("n_SE", "n_VE")) while passing through
  # literal state names unchanged.  unlist flattens so the result is a plain
  # character vector regardless of group size.
  resolved <- unlist(lapply(states, function(x) {
    if (!is.null(state_groups[[x]])) {
      state_groups[[x]]
    } else {
      x
    }
  }), use.names = FALSE)
  unique(as.character(resolved))
}

resolve_disease_from_yaml <- function(yaml_data) {
  disease <- NULL
  # Two-tier lookup: canonical location is model$disease (nested), but a bare
  # top-level `disease:` key is accepted as a fallback for hand-written configs.
  model_section <- yaml_data[["model"]]
  if (!is.null(model_section) && !is.null(model_section[["disease"]])) {
    disease <- model_section[["disease"]]
  } else if (!is.null(yaml_data[["disease"]])) {
    disease <- yaml_data[["disease"]]
  }
  if (is.null(disease)) {
    stop("'disease' field is required in config YAML. Supported diseases: ",
         paste(names(metarvm_disease_registry()), collapse = ", "))
  }
  normalize_disease_name(disease)
}

resolve_disease_from_config_input <- function(config_input) {
  # Three-arm dispatch based on input type:
  # 1) file path string  → parse YAML, use resolve_disease_from_yaml
  # 2) MetaRVMConfig R6  → read already-parsed config_data field
  # 3) plain list        → check top-level `disease` first, then nested
  #                        `config_data$disease` for MetaRVMConfig-like lists

  if (is.character(config_input)) {
    yaml_data <- yaml::read_yaml(config_input)
    return(resolve_disease_from_yaml(yaml_data))
  }

  if (inherits(config_input, "MetaRVMConfig")) {
    return(normalize_disease_name(config_input$config_data[["disease"]]))
  }

  if (is.list(config_input)) {
    if ("disease" %in% names(config_input)) {
      return(normalize_disease_name(config_input$disease))
    }
    # Handle a list that mimics MetaRVMConfig internal structure (e.g. from
    # serialization/deserialization where the R6 class is stripped).
    if ("config_data" %in% names(config_input) &&
        is.list(config_input$config_data) &&
        "disease" %in% names(config_input$config_data)) {
      return(normalize_disease_name(config_input$config_data$disease))
    }
    stop("'disease' field not found in config list. Supported diseases: ",
         paste(names(metarvm_disease_registry()), collapse = ", "))
  }

  stop("config_input must be a file path, MetaRVMConfig object, or parsed config list")
}
