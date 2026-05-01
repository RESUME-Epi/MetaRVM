#' @title MetaRVM Configuration Class
#' @description 
#' R6 class to handle MetaRVM configuration data with validation and methods.
#' This class encapsulates all configuration parameters needed for MetaRVM simulations,
#' providing methods for parameter access, validation, and introspection.
#' 
#' @details
#' The MetaRVMConfig class stores parsed configuration data from YAML files and provides
#' structured access to simulation parameters. It automatically validates configuration
#' completeness and provides convenient methods for accessing demographic categories,
#' initialization-derived population metadata, and other simulation settings.
#' 
#' @examples
#' # Initialize from YAML file
#' example_config <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
#' config <- MetaRVMConfig$new(example_config)
#'
#' # Access parameters
#' config$get("N_pop")
#' config$get("start_date")
#'
#' # Get demographic category names (user-defined)
#' category_names <- config$get_category_names()  # e.g., c("age", "zone", "risk_group")
#'
#' # Get values for specific categories
#' ages <- config$get_category_values("age")
#'
#' # Get all categories as a named list
#' all_categories <- config$get_all_categories()
#' 
#' @import R6
#' @import data.table
#' @author Arindam Fadikar
#' @export
MetaRVMConfig <- R6::R6Class(
  "MetaRVMConfig",
  public = list(
    #' @field config_file Path to the original YAML config file (if applicable)
    config_file = NULL,
    
    #' @field config_data List containing all parsed configuration parameters
    config_data = NULL,
    
    #' @description Initialize a new MetaRVMConfig object
    #' @param input Either a file path (character) or parsed config list
    #' @return New MetaRVMConfig object (invisible)
    initialize = function(input) {
      if (is.character(input)) {
        self$config_file <- input
        self$config_data <- parse_config(input)
      } else if (is.list(input)) {
        self$config_data <- input
        self$config_file <- NULL
      } else {
        stop("Input must be either a file path (character) or parsed config list")
      }
      
      # Validate the configuration
      private$validate_config()
      invisible(self)
    },
    
    #' @description Get a configuration parameter
    #' @param param Parameter name
    #' @return The requested parameter value
    get = function(param) {
      if (!param %in% names(self$config_data)) {
        stop(sprintf("Parameter '%s' not found in configuration", param))
      }
      self$config_data[[param]]
    },
    
    #' @description Get all configuration parameters as a list
    #' @return Named list of all configuration parameters
    get_all = function() {
      return(self$config_data)
    },
    
    #' @description List all available parameter names
    #' @return Character vector of parameter names
    list_parameters = function() {
      return(names(self$config_data))
    },
    
    #' @description Show summary of parameter types and sizes
    #' @return Data frame with parameter information
    parameter_summary = function() {
      param_info <- lapply(self$config_data, function(x) {
        list(
          type = class(x)[1],
          length = length(x),
          size = if(is.matrix(x)) paste(dim(x), collapse = " x ") else length(x)
        )
      })
      
      data.frame(
        parameter = names(param_info),
        type = sapply(param_info, `[[`, "type"),
        length = sapply(param_info, `[[`, "length"),
        size = sapply(param_info, `[[`, "size"),
        stringsAsFactors = FALSE
      )
    },
    
    #' @description Set a configuration parameter
    #' @param param Character string. Parameter name to set
    #' @param value The value to assign to the parameter
    #' @return Self (invisible) for method chaining
    set = function(param, value) {
      self$config_data[[param]] <- value
      private$validate_config()
      invisible(self)
    },
    
    #' @description Print summary of configuration
    #' @return Self (invisible)
    print = function() {
      cat("MetaRVM Configuration Object\n")
      cat("============================\n")
      if (!is.null(self$config_file)) {
        cat("Config file:", self$config_file, "\n")
      }
      cat("Parameters:", length(self$config_data), "\n")
      
      # Show parameter names
      param_names <- names(self$config_data)
      if (length(param_names) > 10) {
        cat("Parameter names (first 10):", paste(param_names[1:10], collapse = ", "), "...\n")
      } else {
        cat("Parameter names:", paste(param_names, collapse = ", "), "\n")
      }
      
      # Key parameters summary
      if ("N_pop" %in% names(self$config_data)) {
        cat("Population groups:", self$config_data$N_pop, "\n")
      }
      if ("start_date" %in% names(self$config_data)) {
        cat("Start date:", as.character(self$config_data$start_date), "\n")
      }
      if ("end_date" %in% names(self$config_data)) {
        cat("End date:", as.character(self$config_data$end_date), "\n")
      }
      if ("pop_map" %in% names(self$config_data)) {
        cat("Population mapping: [", nrow(self$config_data$pop_map), "rows x", 
            ncol(self$config_data$pop_map), "columns]\n")
      }
      invisible(self)
    },
    
    #' @description Get population mapping data
    #' @return data.table containing population_id and user-defined demographic category columns
    get_pop_map = function() {
      self$config_data$pop_map
    },

    #' @description Get names of all category columns
    #' @details Category columns are automatically detected from the initialization CSV file.
    #'   Any column that is not a reserved column (population_id, N, S0, I0, R0, V0, etc.)
#'   is treated as a demographic category (e.g., age, zone, income_level, occupation).
    #' @return Character vector of category column names, or empty vector if no categories
    #' @examples
    #' \dontrun{
    #' config <- MetaRVMConfig$new("config.yaml")
    #' category_names <- config$get_category_names()  # e.g., c("age", "zone", "risk_group")
    #' }
    get_category_names = function() {
      if ("category_names" %in% names(self$config_data)) {
        return(self$config_data$category_names)
      }
      return(character(0))
    },

    #' @description Get unique values for a specific category
    #' @param category_name Character string specifying the category name
    #' @return Character/numeric vector of unique values for the specified category
    #' @examples
    #' \dontrun{
    #' config <- MetaRVMConfig$new("config.yaml")
    #' ages <- config$get_category_values("age")  # if age is defined
    #' income_levels <- config$get_category_values("income_level")  # if defined
    #' }
    get_category_values = function(category_name) {
      if (!"pop_map" %in% names(self$config_data)) {
        return(NULL)
      }
      if (!category_name %in% names(self$config_data$pop_map)) {
        available_cats <- self$get_category_names()
        stop(sprintf("Category '%s' not found. Available categories: %s",
                     category_name,
                     if (length(available_cats) > 0) paste(available_cats, collapse = ", ") else "none"))
      }
      return(unique(self$config_data$pop_map[[category_name]]))
    },

    #' @description Get all categories as a named list
    #' @return Named list where names are category column names and values are vectors
    #'   of unique values for each category. Returns empty list if no categories.
    #' @examples
    #' \dontrun{
    #' config <- MetaRVMConfig$new("config.yaml")
    #' all_cats <- config$get_all_categories()
    #' # Returns: list(age = c("0-17", "18-64", "65+"), risk_group = c("low", "high"), ...)
    #' }
    get_all_categories = function() {
      category_names <- self$get_category_names()
      if (length(category_names) == 0) {
        return(list())
      }

      result <- lapply(category_names, function(cat) {
        unique(self$config_data$pop_map[[cat]])
      })
      names(result) <- category_names
      return(result)
    }
  ),
  
  private = list(
    validate_config = function() {
      # Basic validation - can be extended
      if (!is.list(self$config_data)) {
        stop("Configuration data must be a list")
      }

      # Normalize and write back so all downstream accessors see the canonical
      # form (lowercase, trimmed) rather than whatever string the user typed.
      disease_id <- normalize_disease_name(self$config_data[["disease"]])
      self$config_data$disease <- disease_id

      # Validate required fields from disease schema.
      required_fields <- get_metarvm_required_config_fields(disease_id)
      missing_fields <- setdiff(required_fields, names(self$config_data))
      if (length(missing_fields) > 0) {
        stop(sprintf("Missing required configuration fields: %s", 
                     paste(missing_fields, collapse = ", ")))
      }
    }
  )
)

#' @title MetaRVM Results Class
#' @description 
#' R6 class to handle MetaRVM simulation results with comprehensive analysis and visualization methods.
#' This class stores formatted simulation results and provides methods for data summarization,
#' subsetting, and visualization with flexible demographic groupings.
#' 
#' @details
#' The MetaRVMResults class automatically formats raw simulation output upon initialization,
#' converting time steps to calendar dates and adding demographic attributes. It provides
#' methods for flexible data summarization across any user-defined demographic categories,
#' plus method chaining for streamlined analysis workflows.
#' 
#' @examples
#' \donttest{
#' options(odin.verbose = FALSE)
#' example_config <- system.file("extdata", "example_config.yaml", package = "MetaRVM")
#' # Run simulation
#' results_obj <- metaRVM(example_config)
#' # Access formatted results
#' head(results_obj$results)
#' 
#' # Subset data with multiple filters
#' subset_data <- results_obj$subset_data(
#'   age = c("18-64", "65+"), 
#'   disease_states = c("H", "D"),
#'   date_range = c(as.Date("2024-01-01"), as.Date("2024-02-01"))
#' )
#' 
#' # Method chaining for analysis and visualization
#' results_obj$subset_data(disease_states = "H")$summarize(
#'   group_by = c("age", "zone"), 
#'   stats = c("median", "quantile"),
#'   quantiles = c(0.25, 0.75)
#' )$plot()
#' }
#' 
#' @import R6
#' @import data.table
#' @import ggplot2
#' @author Arindam Fadikar
#' @export
MetaRVMResults <- R6::R6Class(
  "MetaRVMResults",
  public = list(
    #' @field config MetaRVMConfig object used to generate these results
    config = NULL,
    
    #' @field results data.table containing formatted simulation results
    results = NULL,
    
    #' @field run_info List containing run metadata
    run_info = NULL,
    
    #' @description Initialize a new MetaRVMResults object
    #' @param raw_results Raw simulation results data.table
    #' @param config MetaRVMConfig object used for the simulation
    #' @param run_info Optional metadata about the run
    #' @param formatted_results formatted simulation results data.table
    #' @return New MetaRVMResults object (invisible)
    initialize = function(raw_results, config, run_info = NULL, formatted_results = NULL) {
      if (!inherits(config, "MetaRVMConfig")) {
        stop("config must be a MetaRVMConfig object")
      }
      
      self$config <- config

      # Two initialization paths:
      # - formatted_results: bypasses format_metarvm_output (used by subset_data
      #   to return a new MetaRVMResults without re-processing already-formatted data)
      # - raw_results: full pipeline from engine output → tidy long table
      if (!is.null(formatted_results)) {
        if (!data.table::is.data.table(formatted_results)) {
          stop("formatted_results must be a data.table")
        }
        self$results <- formatted_results
      } else if (!is.null(raw_results)) {
        if (!data.table::is.data.table(raw_results)) {
          stop("raw_results must be a data.table")
        }
        self$results <- format_metarvm_output(raw_results, config)
      } else {
        stop("Either raw_results or formatted_results must be provided")
      }

      disease_id <- private$resolve_disease_id()
      output_spec <- get_metarvm_output_spec(disease_id)
      state_col <- output_spec$state_col
      time_col <- output_spec$time_col
      value_col <- output_spec$value_col
      required_result_cols <- c(time_col, state_col, value_col, "instance")
      missing_result_cols <- setdiff(required_result_cols, names(self$results))
      if (length(missing_result_cols) > 0) {
        stop(sprintf(
          "Results are missing required columns: %s",
          paste(missing_result_cols, collapse = ", ")
        ))
      }
      
      # # Set run_info
      # self$run_info <- run_info %||% list(
      #   created_at = Sys.time(),
      #   n_instances = length(unique(self$results$instance)),
      #   n_populations = length(unique(paste(self$results$age, self$results$race, self$results$zone))),
      #   date_range = if(nrow(self$results) > 0) range(self$results$date, na.rm = TRUE) else c(NA, NA)
      # )
      # Set run_info
      self$run_info <- run_info %||% list(
        created_at = Sys.time(),
        n_instances = length(unique(self$results$instance)),
        n_populations = private$calculate_n_populations(self$results),
        date_range = if(nrow(self$results) > 0) range(self$results[[time_col]], na.rm = TRUE) else c(NA, NA)
      )
      
      invisible(self)
    },
    
    #' @description Print summary of results
    #' @return Self (invisible)
    print = function() {
      cat("MetaRVM Results Object\n")
      cat("=====================\n")
      cat("Instances:", self$run_info$n_instances, "\n")
      cat("Populations:", self$run_info$n_populations, "\n")
      cat("Date range:", paste(self$run_info$date_range, collapse = " to "), "\n")
      if (!is.null(self$run_info$nsim)) {
        cat("Parameter sets (nsim):", self$run_info$nsim, "\n")
      }
      if (!is.null(self$run_info$nrep)) {
        cat("Replicates per set (nrep):", self$run_info$nrep, "\n")
      }
      if (!is.null(self$run_info$random_seed)) {
        cat("Random seed:", self$run_info$random_seed, "\n")
      }
      disease_id <- private$resolve_disease_id()
      output_spec <- get_metarvm_output_spec(disease_id)
      state_col <- output_spec$state_col

      cat("Total observations:", nrow(self$results), "\n")
      if (state_col %in% names(self$results)) {
        cat("Disease states:", paste(unique(self$results[[state_col]]), collapse = ", "), "\n")
      }
      invisible(self)
    },

    #' @description Plot simulation trajectories directly from results
    #' @param group_by Vector of demographic category names to group/facet by
    #' @param disease_states Optional disease states to include
    #' @param date_range Optional date range for filtering
    #' @param instances Optional instance IDs to include
    #' @param stats Statistics for summary plotting. If NULL, plots raw trajectories.
    #' @param quantiles Quantiles for uncertainty bands when summary plotting
    #' @param exclude_p_columns Logical, whether to exclude p_ columns (default: TRUE)
    #' @param ci_level Confidence level label used in summary plot title
    #' @param theme ggplot2 theme function (default: theme_minimal())
    #' @param title Optional custom title
    #' @return ggplot object
    plot = function(group_by = character(0), disease_states = NULL, date_range = NULL,
                    instances = NULL, stats = NULL, quantiles = c(0.25, 0.75),
                    exclude_p_columns = TRUE, ci_level = 0.95,
                    theme = theme_minimal(), title = NULL) {

      plot_data <- self$subset_data(
        disease_states = disease_states,
        date_range = date_range,
        instances = instances,
        exclude_p_columns = exclude_p_columns
      )

      summary_obj <- plot_data$summarize(
        group_by = group_by,
        stats = stats,
        quantiles = quantiles,
        exclude_p_columns = FALSE
      )

      summary_obj$plot(ci_level = ci_level, theme = theme, title = title)
    },

    #' @description Subset the data based on any combination of parameters
    #' @param ... Named arguments for category filters (e.g., age = c("0-17"), income = c("low", "high"))
    #' @param disease_states Vector of disease states to include (default: all, excludes p_ columns)
    #' @param date_range Vector of two dates start_date, and end_date for filtering (default: all)
    #' @param instances Vector of instance numbers to include (default: all)
    #' @param exclude_p_columns Logical, whether to exclude p_ columns (default: TRUE)
    #' @return MetaRVMResults object with subset of results
    subset_data = function(..., disease_states = NULL, date_range = NULL,
                          instances = NULL, exclude_p_columns = TRUE) {

      disease_id <- private$resolve_disease_id()
      output_spec <- get_metarvm_output_spec(disease_id)
      state_col <- output_spec$state_col
      time_col <- output_spec$time_col

      # Start with copy of all results
      subset_results <- data.table::copy(self$results)

      # Parse dynamic category filters from ...
      category_filters <- list(...)
      available_categories <- self$config$get_category_names()

      # Apply category filters
      if (length(category_filters) > 0) {
        filter_names <- names(category_filters)

        # Validate filter names are actual categories
        invalid_filters <- setdiff(filter_names, available_categories)
        if (length(invalid_filters) > 0) {
          stop(sprintf("Invalid category filters: %s. Available categories: %s",
                       paste(invalid_filters, collapse = ", "),
                       if (length(available_categories) > 0) paste(available_categories, collapse = ", ") else "none"))
        }

        # Apply each category filter
        for (cat_name in filter_names) {
          filter_values <- category_filters[[cat_name]]
          if (!is.null(filter_values)) {
            valid_values <- unique(self$config$get_category_values(cat_name))
            valid_values_chr <- as.character(valid_values)
            filter_values_chr <- as.character(filter_values)
            invalid_values <- setdiff(filter_values_chr, valid_values_chr)

            if (length(invalid_values) > 0) {
              stop(sprintf(
                "Invalid values for category '%s': %s. Valid values are: %s",
                cat_name,
                paste(invalid_values, collapse = ", "),
                paste(valid_values_chr, collapse = ", ")
              ))
            }

            # as.character() coercion ensures the filter works even when category
            # columns are read as numeric (e.g., zone IDs) from the population CSV.
            # get() is data.table's way to reference a column by a runtime variable.
            subset_results <- subset_results[as.character(get(cat_name)) %in% filter_values_chr]
          }
        }
      }

      selected_states <- resolve_metarvm_state_selection(disease_id, disease_states)

      # Filter by disease states
      if (!state_col %in% names(subset_results)) {
        stop(sprintf("Results do not contain expected state column '%s'", state_col))
      }
      if (!is.null(disease_states)) {
        available_states <- unique(as.character(subset_results[[state_col]]))
        bad_states <- setdiff(selected_states, available_states)
        if (length(bad_states) > 0) {
          state_groups <- get_metarvm_output_spec(disease_id)$state_groups
          bad_inputs <- disease_states[sapply(disease_states, function(x) {
            resolved <- if (!is.null(state_groups[[x]])) state_groups[[x]] else x
            any(resolved %in% bad_states)
          })]
          stop(sprintf(
            "Unknown disease_states: %s. Valid state names: %s. Valid group aliases: %s",
            paste(bad_inputs, collapse = ", "),
            paste(sort(available_states), collapse = ", "),
            if (length(state_groups) > 0) paste(names(state_groups), collapse = ", ") else "none"
          ))
        }
        subset_results <- subset_results[get(state_col) %in% selected_states]
      } else if (exclude_p_columns) {
        exclude_patterns <- output_spec$default_exclude_patterns
        if (length(exclude_patterns) > 0) {
          state_vals <- as.character(subset_results[[state_col]])
          keep <- rep(TRUE, length(state_vals))
          for (pat in exclude_patterns) {
            keep <- keep & !grepl(pat, state_vals)
          }
          subset_results <- subset_results[keep]
        }
      }

      # Filter by date range
      if (!is.null(date_range)) {
        if (length(date_range) != 2) {
          stop("date_range must be a vector of two dates: c(start_date, end_date)")
        }
        if (!time_col %in% names(subset_results)) {
          stop(sprintf("Results do not contain expected time column '%s'", time_col))
        }
        start_date <- as.Date(date_range[1], tryFormats = c("%m/%d/%Y"))
        end_date <- as.Date(date_range[2], tryFormats = c("%m/%d/%Y"))
        subset_results <- subset_results[get(time_col) >= start_date & get(time_col) <= end_date]
      }

      # Filter by instance
      if (!is.null(instances)) {
        if (!"instance" %in% names(subset_results)) {
          stop("Results do not contain required column 'instance'")
        }
        subset_results <- subset_results[instance %in% instances]
      }

      # Dynamic sorting based on available categories
      sort_cols <- c(time_col, "instance")
      if (length(available_categories) > 0) {
        # Only include category columns that actually exist in the data
        existing_cats <- intersect(available_categories, names(subset_results))
        sort_cols <- c(sort_cols, existing_cats)
      }
      sort_cols <- c(sort_cols, state_col)
      data.table::setorderv(subset_results, sort_cols)

      # Create new run_info to reflect the subset
      instance_manifest_subset <- NULL
      if (!is.null(self$run_info$instance_manifest)) {
        instance_manifest_subset <- data.table::as.data.table(self$run_info$instance_manifest)
        subset_instance_ids <- sort(unique(subset_results$instance))
        if ("instance" %in% names(instance_manifest_subset)) {
          instance_manifest_subset <- instance_manifest_subset[instance %in% subset_instance_ids]
        }
      }
      new_run_info <- list(
        created_at = Sys.time(),
        original_created_at = self$run_info$created_at,
        N_pop = self$run_info$N_pop,
        nsim = self$run_info$nsim,
        nrep = self$run_info$nrep,
        random_seed = self$run_info$random_seed,
        delta_t = self$run_info$delta_t,
        checkpointing_enabled = self$run_info$checkpointing_enabled,
        n_instances = length(unique(subset_results$instance)),
        n_populations = private$calculate_n_populations(subset_results),
        date_range = if(nrow(subset_results) > 0) range(subset_results[[time_col]], na.rm = TRUE) else c(NA, NA),
        instance_manifest = instance_manifest_subset,
        subset_filters = c(category_filters, list(
          disease_states = selected_states,
          date_range = date_range,
          instances = instances,
          exclude_p_columns = exclude_p_columns
        ))
      )

      # Return new MetaRVMResults object
      return(MetaRVMResults$new(
        raw_results = NULL,  # We're using already formatted data
        config = self$config,
        run_info = new_run_info,
        formatted_results = subset_results  # Pass pre-formatted data
      ))
    },

    #' @description Summarize results across specified demographic characteristics
    #' @param group_by Vector of demographic category names to group by. Must be valid category
    #'   names from the configuration (e.g., c("age", "zone"), c("income_level", "occupation")).
    #'   Use config$get_category_names() to see available categories.
    #' @param disease_states Vector of disease states to include (default: all, excludes p_ columns)
    #' @param date_range Optional date range for filtering
    #' @param stats Vector of statistics to calculate: c("mean", "median", "sd", "min", "max", "sum", "quantile"). If NULL, returns all instances
    #' @param quantiles Vector of quantiles to calculate if "quantile" is in stats (default: c(0.25, 0.75))
    #' @param exclude_p_columns Logical, whether to exclude p_ columns (default: TRUE)
    #' @return data.table with summarized time series data or all instances if stats = NULL
    summarize = function(group_by, disease_states = NULL, date_range = NULL, 
                        stats = c("mean", "median", "sd"), quantiles = c(0.25, 0.75),
                        exclude_p_columns = TRUE) {
      
      # Validate group_by parameters against available categories
      valid_groups <- self$config$get_category_names()
      disease_id <- private$resolve_disease_id()
      output_spec <- get_metarvm_output_spec(disease_id)
      state_col <- output_spec$state_col
      time_col <- output_spec$time_col
      value_col <- output_spec$value_col
      required_result_cols <- c(time_col, state_col, value_col, "instance")
      missing_result_cols <- setdiff(required_result_cols, names(self$results))
      if (length(missing_result_cols) > 0) {
        stop(sprintf(
          "Results are missing required columns for summarize(): %s",
          paste(missing_result_cols, collapse = ", ")
        ))
      }

      if (length(group_by) > 0) {
        invalid_groups <- setdiff(group_by, valid_groups)
        if (length(invalid_groups) > 0) {
          stop(sprintf("Invalid group_by parameters: %s. Available categories: %s",
                       paste(invalid_groups, collapse = ", "),
                       if (length(valid_groups) > 0) paste(valid_groups, collapse = ", ") else "none"))
        }
      }
      
      # Validate stats parameters if provided
      if (!is.null(stats)) {
        valid_stats <- c("mean", "median", "sd", "min", "max", "sum", "quantile")
        if (!all(stats %in% valid_stats)) {
          stop("stats must contain only: ", paste(valid_stats, collapse = ", "))
        }
      }
      
      # Get subset of data
      subset_data <- self$subset_data(
        disease_states = disease_states, 
        date_range = date_range,
        exclude_p_columns = exclude_p_columns
      )
      
      # Two-step aggregation:
      # Step 1: Sum value across all sub-populations that share the requested
      #         group_by categories, per instance.  This collapses the N_pop
      #         dimension while keeping simulation instances separate.
      group_vars <- c(time_col, group_by, state_col, "instance")
      summed_data <- subset_data$results[, .(
        value = sum(get(value_col), na.rm = TRUE)
      ), by = group_vars]
      
      # # Step 2: If stats is NULL, return all instances without summarizing
      # if (is.null(stats)) {
      #   setorder(summed_data, date, instance)
      #   return(summed_data)
      # }
      
      # Step 2 (stats path): Reduce across simulation instances to produce
      #         summary statistics for each (date, group_by, disease_state) cell.
      final_group_vars <- c(time_col, group_by, state_col)
      summary_result <- summed_data[, {
        out <- list()

        if ("mean" %in% stats) {
          out$mean_value <- mean(value, na.rm = TRUE)
        }
        if ("median" %in% stats) {
          out$median_value <- median(value, na.rm = TRUE)
        }
        if ("sd" %in% stats) {
          out$sd_value <- stats::sd(value, na.rm = TRUE)
        }
        if ("min" %in% stats) {
          out$min_value <- min(value, na.rm = TRUE)
        }
        if ("max" %in% stats) {
          out$max_value <- max(value, na.rm = TRUE)
        }
        if ("sum" %in% stats) {
          out$sum_value <- sum(value, na.rm = TRUE)
        }
        if ("quantile" %in% stats) {
          for (q in quantiles) {
            q_name <- paste0("q", sprintf("%02d", round(q * 100)))
            out[[q_name]] <- stats::quantile(value, probs = q, na.rm = TRUE, names = FALSE)
          }
        }

        out
      }, by = final_group_vars]
      
      # return(summary_result)
      # Return a chainable object
      if (is.null(stats)) {
        data.table::setorderv(summed_data, c(time_col, state_col, "instance"))
        # Return summary object for chaining
        return(MetaRVMSummary$new(summed_data, self$config, type = "instances"))
      } else {
        data.table::setorderv(summary_result, c(time_col, group_by, state_col))
        # Return summary object for chaining
        return(MetaRVMSummary$new(summary_result, self$config, type = "summary"))
      }
    },

    #' @description Get per-instance simulation manifest (parameter-set id, replicate id, seed, and settings)
    #' @param instances Optional vector of instance ids to filter
    #' @param include_sim_args Logical. If FALSE, omits the `sim_args` list-column for a lighter table
    #' @return data.table with one row per simulation instance
    get_instance_manifest = function(instances = NULL, include_sim_args = TRUE) {
      if (!is.null(self$run_info$instance_manifest)) {
        manifest <- data.table::copy(data.table::as.data.table(self$run_info$instance_manifest))
      } else {
        manifest <- data.table::data.table(instance = sort(unique(self$results$instance)))
      }

      if (!is.null(instances)) {
        instances <- unique(as.integer(instances))
        missing_instances <- setdiff(instances, manifest$instance)
        if (length(missing_instances) > 0) {
          stop(sprintf(
            "Requested instances not found in manifest: %s",
            paste(missing_instances, collapse = ", ")
          ))
        }
        manifest <- manifest[instance %in% instances]
      }

      if (!isTRUE(include_sim_args) && "sim_args" %in% names(manifest)) {
        manifest[, sim_args := NULL]
      }

      data.table::setorder(manifest, instance)
      manifest
    },

    #' @description Get per-instance settings table with parameters list-column (`params`)
    #' @param instances Optional vector of instance ids to filter
    #' @return data.table with instance mapping and exact parameter values used in simulation
    get_instance_params = function(instances = NULL) {
      manifest <- self$get_instance_manifest(instances = instances, include_sim_args = TRUE)
      if (!"sim_args" %in% names(manifest)) {
        stop("Instance parameters are unavailable: no sim_args stored in run_info$instance_manifest")
      }
      manifest <- manifest[, .(
        instance,
        parameter_set = if ("parameter_set" %in% names(manifest)) parameter_set else NA_integer_,
        replicate = if ("replicate" %in% names(manifest)) replicate else NA_integer_,
        seed = if ("seed" %in% names(manifest)) seed else NA_integer_,
        params = sim_args
      )]
      data.table::setorder(manifest, instance)
      manifest
    }
  ),


  private = list(
    resolve_disease_id = function() {
      normalize_disease_name(self$config$config_data[["disease"]])
    },

    calculate_n_populations = function(data) {
      if (nrow(data) == 0) return(0)

      # Get available demographic columns dynamically from config
      category_cols <- self$config$get_category_names()
      demographic_cols <- if (length(category_cols) > 0) {
        intersect(names(data), category_cols)
      } else {
        character(0)
      }

      if (length(demographic_cols) == 0) {
        return(1)
      } else if (length(demographic_cols) == 1) {
        return(length(unique(data[[demographic_cols[1]]])))
      } else {
        # `..demographic_cols` is data.table's "dot-dot" notation for selecting
        # columns whose names are stored in a variable (as opposed to a literal).
        unique_combinations <- data[, ..demographic_cols]
        unique_combinations <- unique(unique_combinations)
        return(nrow(unique_combinations))
      }
    }
  )
)


#' @title MetaRVM Summary Class
#' @description 
#' R6 class for summarized MetaRVM results with plotting capabilities and method chaining support.
#' This class stores summarized simulation data and provides visualization methods that automatically
#' adapt based on the data structure and grouping variables.
#' 
#' @details
#' The MetaRVMSummary class is designed to work seamlessly with method chaining from MetaRVMResults.
#' It stores either summary statistics (mean, median, quantiles, etc.) or individual instance data,
#' and provides intelligent plotting methods that automatically determine appropriate visualizations
#' based on the data structure and demographic groupings.
#' 
#' The class supports two data types:
#' \itemize{
#'   \item \strong{Summary data}: Contains aggregated statistics across simulation instances
#'   \item \strong{Instance data}: Contains individual trajectory data for each simulation instance
#' }
#' 
#' Plotting behavior adapts automatically:
#' \itemize{
#'   \item Single grouping variable: Facets by demographic category, colors by disease state
#'   \item Two grouping variables: Grid layout with both demographics as facet dimensions
#'   \item Three grouping variables: Grid layout with first two as facets, third as color
#' }
#' 
#' @section Public Fields:
#' \describe{
#'   \item{\code{data}}{data.table containing summarized results}
#'   \item{\code{config}}{MetaRVMConfig object from original simulation}
#'   \item{\code{type}}{Character string indicating data type ("summary" or "instances")}
#' }
#' 
#' @examples
#' \donttest{
#' options(odin.verbose = FALSE)
#' example_config <- system.file("extdata", "example_config_dist.yaml", package = "MetaRVM")
#' # Run simulation
#' results <- metaRVM(example_config)
#' # Typically created through method chaining
#' summary_obj <- results$subset_data(disease_states = "H")$summarize(
#'   group_by = c("age", "zone"), 
#'   stats = c("median", "quantile"),
#'   quantiles = c(0.25, 0.75)
#' )
#' 
#' # Direct plotting
#' summary_obj$plot()
#' 
#' # Plot with custom ggplot theme and confidence level
#' summary_obj$plot(theme = ggplot2::theme_bw())
#' }
#' 
#' @import R6
#' @import data.table
#' @import ggplot2
#' @author Arindam Fadikar
#' @export
MetaRVMSummary <- R6::R6Class(
  "MetaRVMSummary",
  public = list(
    #' @field data Summarized data
    data = NULL,
    
    #' @field config Original MetaRVMConfig object
    config = NULL,
    
    #' @field type Type of summary ("instances" or "summary")
    type = NULL,
    
    #' @description Initialize MetaRVMSummary object
    #' @param data data.table containing summarized or instance data
    #' @param config MetaRVMConfig object from original simulation
    #' @param type Character string indicating data type ("summary" or "instances")
    #' @return New MetaRVMSummary object (invisible)
    initialize = function(data, config, type) {
      if (!data.table::is.data.table(data)) {
        stop("data must be a data.table")
      }
      if (!inherits(config, "MetaRVMConfig")) {
        stop("config must be a MetaRVMConfig object")
      }
      if (!type %in% c("summary", "instances")) {
        stop("type must be either 'summary' or 'instances'")
      }
      
      self$data <- data
      self$config <- config
      self$type <- type
      invisible(self)
    },

    #' @description Print summary of the data object
    #' @return Self (invisible)
    print = function() {
      cat("MetaRVM Summary Object\n")
      cat("======================\n")
      cat("Data type:", self$type, "\n")
      cat("Observations:", nrow(self$data), "\n")
      disease_id <- private$resolve_disease_id()
      output_spec <- get_metarvm_output_spec(disease_id)
      state_col <- output_spec$state_col
      time_col <- output_spec$time_col
      
      # Show grouping variables
      columns <- names(self$data)
      available_categories <- self$config$get_category_names()
      group_vars <- if (length(available_categories) > 0) {
        intersect(columns, available_categories)
      } else {
        character(0)
      }
      if (length(group_vars) > 0) {
        cat("Grouped by:", paste(group_vars, collapse = ", "), "\n")
      }
      
      # Show available disease states
      if (state_col %in% columns) {
        disease_states <- unique(self$data[[state_col]])
        if (length(disease_states) <= 5) {
          cat("Disease states:", paste(disease_states, collapse = ", "), "\n")
        } else {
          cat("Disease states:", length(disease_states), "unique states\n")
        }
      }
      
      # Show date range if available
      if (time_col %in% columns) {
        date_range <- range(self$data[[time_col]], na.rm = TRUE)
        cat("Date range:", paste(date_range, collapse = " to "), "\n")
      }
      
      # Show summary columns for summary data
      if (self$type == "summary") {
        summary_cols <- intersect(columns, c("mean_value", "median_value", "sd_value", "min_value", "max_value"))
        quantile_cols <- grep("^q[0-9]", columns, value = TRUE)
        all_summary_cols <- c(summary_cols, quantile_cols)
        if (length(all_summary_cols) > 0) {
          cat("Summary statistics:", paste(all_summary_cols, collapse = ", "), "\n")
        }
      } else if (self$type == "instances") {
        if ("instance" %in% columns) {
          n_instances <- length(unique(self$data$instance))
          cat("Number of instances:", n_instances, "\n")
        }
      }
      
      invisible(self)
    },
    
    #' @description Plot method that shows median with quantile bands
    #' @param ci_level Confidence level for empirical quantiles (default: 0.95). Only used if quantile columns are not pre-specified
    #' @param theme ggplot2 theme function (default: theme_minimal())
    #' @param title Optional custom plot title
    #' @return ggplot object
    #' @details
    #' This method creates time series plots with automatic layout adaptation based on grouping variables:
    #' \itemize{
    #'   \item For summary data: Shows median lines with quantile confidence bands
    #'   \item Automatically determines faceting strategy based on number of grouping variables
    #'   \item Uses disease states for color differentiation when appropriate
    #' }
    #' 
    #' The method requires specific data structure:
    #' \itemize{
    #'   \item Summary data must contain 'median_value' and quantile columns (e.g., 'q25', 'q75')
    #'   \item Instance data must contain 'instance' column for individual trajectory grouping
    #' }
    plot = function(ci_level = 0.95, theme = theme_minimal(), title = NULL) {

      columns <- names(self$data)
      disease_id <- private$resolve_disease_id()
      output_spec <- get_metarvm_output_spec(disease_id)
      state_col <- output_spec$state_col
      value_col <- output_spec$value_col
      time_col <- output_spec$time_col
      required_plot_cols <- c(time_col, value_col)
      missing_plot_cols <- setdiff(required_plot_cols, columns)
      if (length(missing_plot_cols) > 0) {
        stop(sprintf(
          "Plot data are missing required columns: %s",
          paste(missing_plot_cols, collapse = ", ")
        ))
      }

      # Detect grouping variables dynamically
      available_categories <- self$config$get_category_names()
      group_vars <- if (length(available_categories) > 0) {
        intersect(columns, available_categories)
      } else {
        character(0)
      }

      # Instance trajectory plotting mode
      if (identical(self$type, "instances")) {
        if (!"instance" %in% columns) {
          stop("Instance trajectory plotting requires column 'instance'")
        }
        color_var <- if (state_col %in% columns) state_col else "instance"

        p <- ggplot(self$data, aes(x = .data[[time_col]], y = .data[[value_col]],
                                   color = .data[[color_var]],
                                   group = interaction(.data[["instance"]], .data[[color_var]], drop = TRUE))) +
          geom_line(alpha = 0.45, linewidth = 0.5)

        if (length(group_vars) == 1) {
          p <- p + facet_wrap(stats::as.formula(paste("~", group_vars[1])), scales = "free_y")
        } else if (length(group_vars) >= 2) {
          p <- p + facet_grid(stats::as.formula(paste(group_vars[1], "~", group_vars[2])), scales = "free_y")
        }

        p <- p +
          labs(
            title = title %||% "Simulation Trajectories",
            x = "Date",
            y = "Value",
            color = tools::toTitleCase(gsub("_", " ", color_var))
          ) +
          theme +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

        return(p)
      }
      
      # Check if we have the required columns for plotting
      if (!("median_value" %in% columns)) {
        stop("Plot method requires 'median_value' column. Please call summarize() first with stats = c('median', 'quantile')")
      }
      
      # Check for quantile columns
      quantile_cols <- grep("^q[0-9]+$", columns, value = TRUE)
      quantile_levels <- suppressWarnings(as.numeric(sub("^q", "", quantile_cols)))
      valid_q_idx <- which(!is.na(quantile_levels))
      if (length(valid_q_idx) < 2) {
        stop("Plot method requires quantile columns for confidence bands. Please call summarize() with stats = c('median', 'quantile')")
      }
      quantile_cols <- quantile_cols[valid_q_idx][order(quantile_levels[valid_q_idx])]
      
      # Create faceting strategy based on number of grouping variables
      if (length(group_vars) == 0) {
        facet_formula <- NULL
        color_var <- state_col
      } else if (length(group_vars) == 1) {
        # Single grouping variable: facet by that variable, color by state.
        facet_formula <- as.formula(paste("~", group_vars[1]))
        color_var <- state_col
        
      } else if (length(group_vars) == 2) {
        # Two grouping variables: facet by both, color by state.
        facet_formula <- as.formula(paste(group_vars[1], "~", group_vars[2]))
        color_var <- state_col
        
      } else if (length(group_vars) == 3) {
        # Three grouping variables: facet by first two, color by third
        # This creates fewer facets but still shows all information
        facet_formula <- as.formula(paste(group_vars[1], "~", group_vars[2]))
        color_var <- group_vars[3]
      }
      
      # Use lowest and highest quantile columns for confidence bands
      ci_lower_col <- quantile_cols[1]
      ci_upper_col <- quantile_cols[length(quantile_cols)]
      
      # Create the plot
      p <- ggplot(self$data, aes(x = .data[[time_col]], y = median_value, color = .data[[color_var]])) +
        geom_line(linewidth = 1) +
        geom_ribbon(aes(ymin = .data[[ci_lower_col]], ymax = .data[[ci_upper_col]],
                      fill = .data[[color_var]]), alpha = 0.2, color = NA) +
        labs(
          title = title %||% paste0("Median Outcomes with ", ci_level*100, "% Empirical Quantiles"),
          x = "Date",
          y = "Median Value",
          color = tools::toTitleCase(gsub("_", " ", color_var)),
          fill = tools::toTitleCase(gsub("_", " ", color_var))
        ) +
        theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      if (!is.null(facet_formula)) {
        p <- p + facet_grid(facet_formula, scales = "free_y")
      }

      return(p)
    }
  ),

  private = list(
    resolve_disease_id = function() {
      normalize_disease_name(self$config$config_data[["disease"]])
    }
  )
)

# Utility function for NULL coalescing operator
#' @name grapes-or-or-grapes
#' @title NULL Coalescing Operator
#' @description 
#' Returns the left-hand side if it's not NULL, otherwise returns the right-hand side.
#' This is a utility function used internally by MetaRVM classes.
#' 
#' @param x Left-hand side value
#' @param y Right-hand side value (default/fallback)
#' @return x if x is not NULL, otherwise y
#' @examples
#' \dontrun{
#' user_title <- "User Title"
#' # Internal usage in classes
#' title <- user_title %||% "Default Title"
#' }
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

#' @title MetaRVM Checkpoint Class
#' @description
#' R6 class that stores a saved simulation state produced by [meta_sim()].
#' Used to resume or branch a simulation from an intermediate time point.
#'
#' @details
#' Checkpoint objects are written automatically during a simulation run when
#' `do_chk = TRUE` is set in the simulation config. Each checkpoint captures
#' the full compartment state (S, E, I, H, R, D, V, …) and all disease
#' parameters at the saved time step, enabling a resumed run to pick up exactly
#' where it left off.
#'
#' `MetaRVMCheck` extends [`MetaRVMConfig`]: all `$get()`, `$get_category_names()`,
#' and related accessor methods are inherited and work identically. Required
#' checkpoint fields are disease-specific and validated against the disease
#' registry at construction time.
#'
#' @seealso [metaRVM()] for enabling checkpointing via the `simulation_config`
#'   section; [`MetaRVMConfig`] for the parent class and its accessor methods.
#'
#' @examples
#' \dontrun{
#' # Checkpoints are created automatically when do_chk = TRUE in the config.
#' # To inspect a saved checkpoint:
#' chk_data <- readRDS("my_checkpoint.rds")
#' chk <- MetaRVMCheck$new(chk_data)
#' chk$get("chk_time_step")
#' chk$get("N_pop")
#' }
#'
#' @import R6
#' @author Arindam Fadikar
#' @export
MetaRVMCheck <- R6::R6Class(
  "MetaRVMCheck",
  inherit = MetaRVMConfig,
  public = list(
    #' @field check_data List containing all parsed checkpoint data
    check_data = NULL,
    
    #' @description Initialize a new MetaRVMCheck object
    #' @param input A list containing checkpoint data.
    #' @return A new `MetaRVMCheck` object.
    initialize = function(input) {
      if (!is.list(input)) {
        stop("Input must be a list containing checkpoint data.")
      }
      self$check_data <- input
      self$config_data <- input  # Also assign to config_data for inherited methods
      private$validate_config()
      invisible(self)
    }
  ),
  private = list(
    validate_config = function() {
      disease_id <- normalize_disease_name(self$check_data[["disease"]])
      self$check_data$disease <- disease_id
      self$config_data$disease <- disease_id

      required_fields <- get_metarvm_required_checkpoint_fields(disease_id)
      missing_fields <- setdiff(required_fields, names(self$check_data))
      if (length(missing_fields) > 0) {
        stop(sprintf("Missing required checkpoint fields: %s",
                     paste(missing_fields, collapse = ", ")))
      }
    }
  )
)
