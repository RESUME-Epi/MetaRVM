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
#' population mappings, and other simulation settings.
#' 
#' @examples
#' \dontrun{
#' # Initialize from YAML file
#' config <- MetaRVMConfig$new("path/to/config.yaml")
#' 
#' # Access parameters
#' config$get("N_pop")
#' config$get("start_date")
#' 
#' # Get demographic categories
#' ages <- config$get_age_categories()
#' races <- config$get_race_categories()
#' zones <- config$get_zones()
#' }
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
    #' @return data.table containing population mapping with demographic categories
    get_pop_map = function() {
      self$config_data$pop_map
    },
    
    #' @description Get available age categories
    #' @return Character vector of unique age categories, or NULL if no population mapping available
    get_age_categories = function() {
      if ("pop_map" %in% names(self$config_data)) {
        unique(self$config_data$pop_map$age)
      } else {
        NULL
      }
    },
    
    #' @description Get available race categories
    #' @return Character vector of unique race categories, or NULL if no population mapping available
    get_race_categories = function() {
      if ("pop_map" %in% names(self$config_data)) {
        unique(self$config_data$pop_map$race)
      } else {
        NULL
      }
    },
    
    #' @description Get available zones
    #' @return Character vector of unique zone identifiers, or NULL if no population mapping available
    get_zones = function() {
      if ("pop_map" %in% names(self$config_data)) {
        unique(self$config_data$pop_map$hcez)
      } else {
        NULL
      }
    }
  ),
  
  private = list(
    validate_config = function() {
      # Basic validation - can be extended
      if (!is.list(self$config_data)) {
        stop("Configuration data must be a list")
      }
      
      # Check for required fields (adjust based on your needs)
      required_fields <- c("N_pop")
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
#' methods for flexible data summarization across any combination of age, race, and
#' geographic zone categories, plus method chaining for streamlined analysis workflows.
#' 
#' @examples
#' \dontrun{
#' # Create from simulation results
#' results_obj <- MetaRVMResults$new(raw_results, config_obj)
#' 
#' # Access formatted results
#' head(results_obj$results)
#' 
#' # Subset data with multiple filters
#' subset_data <- results_obj$subset_data(
#'   age = c("18-49", "50-64"), 
#'   disease_states = c("H", "D"),
#'   date_range = c(as.Date("2024-01-01"), as.Date("2024-02-01"))
#' )
#' 
#' # Method chaining for analysis and visualization
#' results_obj$summarize(
#'   group_by = c("age", "race"), 
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
    #' @return New MetaRVMResults object (invisible)
    initialize = function(raw_results, config, run_info = NULL) {
      if (!inherits(config, "MetaRVMConfig")) {
        stop("config must be a MetaRVMConfig object")
      }
      if (!data.table::is.data.table(raw_results)) {
        stop("raw_results must be a data.table")
      }
      
      self$config <- config
      # Format the results immediately and store formatted version
      self$results <- format_metarvm_output(raw_results, config)
      
      # Calculate run info from formatted results
      self$run_info <- run_info %||% list(
        created_at = Sys.time(),
        n_instances = length(unique(self$results$instance)),
        n_populations = length(unique(paste(self$results$age, self$results$race, self$results$zone))),
        date_range = range(self$results$date, na.rm = TRUE)
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
      cat("Total observations:", nrow(self$results), "\n")
      cat("Disease states:", paste(unique(self$results$disease_state), collapse = ", "), "\n")
      invisible(self)
    },
    
    # #' @description Summarize results by age categories (excluding p_ columns)
    # summarize_by_age = function(disease_states = NULL, date_range = NULL) {
    #   formatted_data <- copy(self$results)
      
    #   # Filter out p_ columns by default
    #   if (is.null(disease_states)) {
    #     formatted_data <- formatted_data[!grepl("^p_", disease_state)]
    #   } else {
    #     formatted_data <- formatted_data[disease_state %in% disease_states]
    #   }
      
    #   # Filter by date range if specified
    #   if (!is.null(date_range)) {
    #     formatted_data <- formatted_data[date >= date_range[1] & date <= date_range[2]]
    #   }
      
    #   # Summarize by age
    #   summary_result <- formatted_data[, .(
    #     mean_value = mean(value, na.rm = TRUE),
    #     median_value = median(value, na.rm = TRUE),
    #     sum_value = sum(value, na.rm = TRUE),
    #     min_value = min(value, na.rm = TRUE),
    #     max_value = max(value, na.rm = TRUE),
    #     sd_value = sd(value, na.rm = TRUE)
    #     # observations = .N
    #   ), by = .(date, age, disease_state)]
      
    #   setorder(summary_result, age, disease_state)
    #   return(summary_result)
    # },
    
    # #' @description Summarize results by race categories (excluding p_ columns)
    # summarize_by_race = function(disease_states = NULL, date_range = NULL) {
    #   formatted_data <- copy(self$results)
      
    #   # Filter out p_ columns by default
    #   if (is.null(disease_states)) {
    #     formatted_data <- formatted_data[!grepl("^p_", disease_state)]
    #   } else {
    #     formatted_data <- formatted_data[disease_state %in% disease_states]
    #   }
      
    #   # Filter by date range if specified
    #   if (!is.null(date_range)) {
    #     formatted_data <- formatted_data[date >= date_range[1] & date <= date_range[2]]
    #   }
      
    #   # Summarize by race
    #   summary_result <- formatted_data[, .(
    #     mean_value = mean(value, na.rm = TRUE),
    #     median_value = median(value, na.rm = TRUE),
    #     sum_value = sum(value, na.rm = TRUE),
    #     min_value = min(value, na.rm = TRUE),
    #     max_value = max(value, na.rm = TRUE),
    #     sd_value = sd(value, na.rm = TRUE)
    #     # observations = .N
    #   ), by = .(date, race, disease_state)]
      
    #   setorder(summary_result, race, disease_state)
    #   return(summary_result)
    # },
    
    # #' @description Summarize results by geographic zones (excluding p_ columns)
    # summarize_by_zone = function(disease_states = NULL, date_range = NULL) {
    #   formatted_data <- copy(self$results)
      
    #   # Filter out p_ columns by default
    #   if (is.null(disease_states)) {
    #     formatted_data <- formatted_data[!grepl("^p_", disease_state)]
    #   } else {
    #     formatted_data <- formatted_data[disease_state %in% disease_states]
    #   }
      
    #   # Filter by date range if specified
    #   if (!is.null(date_range)) {
    #     formatted_data <- formatted_data[date >= date_range[1] & date <= date_range[2]]
    #   }
      
    #   # Summarize by zone
    #   summary_result <- formatted_data[, .(
    #     mean_value = mean(value, na.rm = TRUE),
    #     median_value = median(value, na.rm = TRUE),
    #     sum_value = sum(value, na.rm = TRUE),
    #     min_value = min(value, na.rm = TRUE),
    #     max_value = max(value, na.rm = TRUE),
    #     sd_value = sd(value, na.rm = TRUE)
    #     # observations = .N
    #   ), by = .(date, zone, disease_state)]
      
    #   setorder(summary_result, zone, disease_state)
    #   return(summary_result)
    # },

    #' @description Subset the data based on any combination of parameters
    #' @param ages Vector of age categories to include (default: all)
    #' @param races Vector of race categories to include (default: all) 
    #' @param zones Vector of zones to include (default: all)
    #' @param disease_states Vector of disease states to include (default: all, excludes p_ columns)
    #' @param date_range Vector of two dates start_date, and end_date for filtering (default: all)
    #' @param instances Vector of instance numbers to include (default: all)
    #' @param exclude_p_columns Logical, whether to exclude p_ columns (default: TRUE)
    #' @return data.table with subset of results
    subset_data = function(ages = NULL, races = NULL, zones = NULL, disease_states = NULL, 
                      date_range = NULL, instances = NULL, exclude_p_columns = TRUE) {
  
      # Start with copy of all results
      subset_results <- copy(self$results)
      
      # Filter by age categories
      if (!is.null(ages)) {
        subset_results <- subset_results[age %in% ages]
      }
      
      # Filter by race categories  
      if (!is.null(races)) {
        subset_results <- subset_results[race %in% races]
      }
      
      # Filter by zones
      if (!is.null(zones)) {
        subset_results <- subset_results[zone %in% zones]
      }
      
      # Filter by disease states
      if (!is.null(disease_states)) {
        subset_results <- subset_results[disease_state %in% disease_states]
      } else if (exclude_p_columns) {
        subset_results <- subset_results[!grepl("^p_", disease_state)]
      }
      
      # Filter by date range
      if (!is.null(date_range)) {
        if (length(date_range) != 2) {
          stop("date_range must be a vector of two dates: c(start_date, end_date)")
        }
        start_date <- date_range[1]
        end_date <- date_range[2]
        subset_results <- subset_results[date >= start_date & date <= end_date]
      }
      
      # Filter by instance
      if (!is.null(instances)) {
        subset_results <- subset_results[instance %in% instances]
      }
      
      # Sort results for better readability
      setorder(subset_results, date, instance, zone, age, race, disease_state)
      
      return(subset_results)
    },

    #' @description Summarize results across specified demographic characteristics
    #' @param group_by Vector of demographic variables to group by: c("age", "race", "zone")
    #' @param disease_states Vector of disease states to include (default: all, excludes p_ columns)
    #' @param date_range Optional date range for filtering
    #' @param stats Vector of statistics to calculate: c("mean", "median", "sd", "min", "max", "sum", "quantile"). If NULL, returns all instances
    #' @param quantiles Vector of quantiles to calculate if "quantile" is in stats (default: c(0.25, 0.75))
    #' @param exclude_p_columns Logical, whether to exclude p_ columns (default: TRUE)
    #' @return data.table with summarized time series data or all instances if stats = NULL
    summarize = function(group_by, disease_states = NULL, date_range = NULL, 
                        stats = c("mean", "median", "sd"), quantiles = c(0.25, 0.75),
                        exclude_p_columns = TRUE) {
      
      # Validate group_by parameters
      valid_groups <- c("age", "race", "zone")
      if (!all(group_by %in% valid_groups)) {
        stop("group_by must contain only: ", paste(valid_groups, collapse = ", "))
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
      
      # Step 1: Sum by demographic categories, date, and disease_state (keeping instances separate)
      group_vars <- c("date", group_by, "disease_state", "instance")
      summed_data <- subset_data[, .(
        value = sum(value, na.rm = TRUE)
      ), by = group_vars]
      
      # # Step 2: If stats is NULL, return all instances without summarizing
      # if (is.null(stats)) {
      #   setorder(summed_data, date, instance)
      #   return(summed_data)
      # }
      
      # Step 3: Calculate statistics across instances for each date/demographic/disease combination
      final_group_vars <- c("date", group_by, "disease_state")
      
      # Start with empty result
      summary_result <- summed_data[, .(temp = mean(value)), by = final_group_vars][, temp := NULL]
      
      # Add each statistic individually
      if ("mean" %in% stats) {
        temp_mean <- summed_data[, .(mean_value = mean(value, na.rm = TRUE)), by = final_group_vars]
        summary_result <- merge(summary_result, temp_mean, by = final_group_vars, all = TRUE)
      }
      
      if ("median" %in% stats) {
        temp_median <- summed_data[, .(median_value = median(value, na.rm = TRUE)), by = final_group_vars]
        summary_result <- merge(summary_result, temp_median, by = final_group_vars, all = TRUE)
      }
      
      if ("sd" %in% stats) {
        temp_sd <- summed_data[, .(sd_value = sd(value, na.rm = TRUE)), by = final_group_vars]
        summary_result <- merge(summary_result, temp_sd, by = final_group_vars, all = TRUE)
      }
      
      if ("min" %in% stats) {
        temp_min <- summed_data[, .(min_value = min(value, na.rm = TRUE)), by = final_group_vars]
        summary_result <- merge(summary_result, temp_min, by = final_group_vars, all = TRUE)
      }
      
      if ("max" %in% stats) {
        temp_max <- summed_data[, .(max_value = max(value, na.rm = TRUE)), by = final_group_vars]
        summary_result <- merge(summary_result, temp_max, by = final_group_vars, all = TRUE)
      }
      
      if ("sum" %in% stats) {
        temp_sum <- summed_data[, .(sum_value = sum(value, na.rm = TRUE)), by = final_group_vars]
        summary_result <- merge(summary_result, temp_sum, by = final_group_vars, all = TRUE)
      }
      
      # Handle quantiles
      if ("quantile" %in% stats) {
        for (q in quantiles) {
          q_name <- paste0("q", sprintf("%02d", round(q * 100)))
          temp_q <- summed_data[, .(temp_quantile = quantile(value, q, na.rm = TRUE)), by = final_group_vars]
          setnames(temp_q, "temp_quantile", q_name)
          summary_result <- merge(summary_result, temp_q, by = final_group_vars, all = TRUE)
        }
      }
      
      # Sort results for better readability
      setorder(summary_result, date)
      
      # return(summary_result)
      # Return a chainable object
      if (is.null(stats)) {
        # Return summary object for chaining
        return(MetaRVMSummary$new(summed_data, self$config, type = "instances"))
      } else {
        # Return summary object for chaining
        return(MetaRVMSummary$new(summary_result, self$config, type = "summary"))
      }
    }


    # #' @description Plot age-specific outcomes as time series
    # #' @param disease_states Disease states to plot
    # #' @param date_range Optional date range for filtering
    # #' @param plot_type Either "mean" (mean with CI) or "individual" (individual instance lines)
    # #' @param ci_level Confidence level for CI (default: 0.95), only used when plot_type = "mean"
    # #' @param theme ggplot2 theme function (default: theme_minimal())
    # #' @return ggplot object
    # plot_by_age = function(disease_states = NULL, date_range = NULL, plot_type = "mean", 
    #                       ci_level = 0.95, theme = theme_minimal()) {
      
    #   if (plot_type == "mean") {
    #     # Get raw data for empirical quantiles
    #     subset_data <- self$subset_data(disease_states = disease_states, date_range = date_range)
        
    #     # Calculate empirical quantiles for confidence intervals
    #     alpha <- 1 - ci_level
    #     lower_quantile <- alpha / 2
    #     upper_quantile <- 1 - alpha / 2
        
    #     # Summarize with empirical quantiles
    #     age_summary <- subset_data[, .(
    #       mean_value = mean(value, na.rm = TRUE),
    #       ci_lower = quantile(value, lower_quantile, na.rm = TRUE),
    #       ci_upper = quantile(value, upper_quantile, na.rm = TRUE)
    #     ), by = .(date, age, disease_state)]
        
    #     p <- ggplot(age_summary, aes(x = date, y = mean_value, color = age)) +
    #       geom_line(size = 1) +
    #       geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = age), alpha = 0.2, color = NA) +
    #       facet_wrap(~age, scales = "free_y") +
    #       labs(
    #         title = paste0("Mean Outcomes by Age Group (", ci_level*100, "% Empirical CI)"),
    #         x = "Date",
    #         y = "Mean Value",
    #         color = "Disease State",
    #         fill = "Disease State"
    #       ) +
    #       theme
          
    #   } else if (plot_type == "individual") {
    #     # Get raw data for individual instance lines
    #     subset_data <- self$subset_data(disease_states = disease_states, date_range = date_range)
        
    #     p <- ggplot(subset_data, aes(x = date, y = value, color = age)) +
    #       geom_line(aes(group = instance), alpha = 0.6, size = 0.5) +
    #       facet_wrap(~age, scales = "free_y") +
    #       labs(
    #         title = "Individual Instance Trajectories by Age Group",
    #         x = "Date", 
    #         y = "Value",
    #         color = "Disease State"
    #       ) +
    #       theme
          
    #   } else {
    #     stop("plot_type must be either 'mean' or 'individual'")
    #   }
      
    #   return(p)
    # },

    # #' @description Plot race-specific outcomes as time series
    # #' @param disease_states Disease states to plot
    # #' @param date_range Optional date range for filtering
    # #' @param plot_type Either "mean" (mean with CI) or "individual" (individual instance lines)
    # #' @param ci_level Confidence level for CI (default: 0.95), only used when plot_type = "mean"
    # #' @param theme ggplot2 theme function (default: theme_minimal())
    # #' @return ggplot object
    # plot_by_race = function(disease_states = NULL, date_range = NULL, plot_type = "mean",
    #                       ci_level = 0.95, theme = theme_minimal()) {
      
    #   if (plot_type == "mean") {
    #     # Get raw data for empirical quantiles
    #     subset_data <- self$subset_data(disease_states = disease_states, date_range = date_range)
        
    #     # Calculate empirical quantiles for confidence intervals
    #     alpha <- 1 - ci_level
    #     lower_quantile <- alpha / 2
    #     upper_quantile <- 1 - alpha / 2
        
    #     # Summarize with empirical quantiles
    #     race_summary <- subset_data[, .(
    #       mean_value = mean(value, na.rm = TRUE),
    #       ci_lower = quantile(value, lower_quantile, na.rm = TRUE),
    #       ci_upper = quantile(value, upper_quantile, na.rm = TRUE)
    #     ), by = .(date, race, disease_state)]
        
    #     p <- ggplot(race_summary, aes(x = date, y = mean_value, color = disease_state)) +
    #       geom_line(size = 1) +
    #       geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = disease_state), alpha = 0.2, color = NA) +
    #       facet_wrap(~race, scales = "free_y") +
    #       labs(
    #         title = paste0("Mean Outcomes by Race (", ci_level*100, "% Empirical CI)"),
    #         x = "Date",
    #         y = "Mean Value", 
    #         color = "Disease State",
    #         fill = "Disease State"
    #       ) +
    #       theme
          
    #   } else if (plot_type == "individual") {
    #     # Get raw data for individual instance lines
    #     subset_data <- self$subset_data(disease_states = disease_states, date_range = date_range)
        
    #     p <- ggplot(subset_data, aes(x = date, y = value, color = disease_state)) +
    #       geom_line(aes(group = interaction(disease_state, instance)), alpha = 0.6, size = 0.5) +
    #       facet_wrap(~race, scales = "free_y") +
    #       labs(
    #         title = "Individual Instance Trajectories by Race",
    #         x = "Date",
    #         y = "Value",
    #         color = "Disease State"
    #       ) +
    #       theme
          
    #   } else {
    #     stop("plot_type must be either 'mean' or 'individual'")
    #   }
      
    #   return(p)
    # },

    # #' @description Plot zone-specific outcomes as time series
    # #' @param disease_states Disease states to plot
    # #' @param date_range Optional date range for filtering
    # #' @param plot_type Either "mean" (mean with CI) or "individual" (individual instance lines)
    # #' @param ci_level Confidence level for CI (default: 0.95), only used when plot_type = "mean"
    # #' @param theme ggplot2 theme function (default: theme_minimal())
    # #' @return ggplot object
    # plot_by_zone = function(disease_states = NULL, date_range = NULL, plot_type = "mean",
    #                       ci_level = 0.95, theme = theme_minimal()) {
      
    #   if (plot_type == "mean") {
    #     # Get raw data for empirical quantiles
    #     subset_data <- self$subset_data(disease_states = disease_states, date_range = date_range)
        
    #     # Calculate empirical quantiles for confidence intervals
    #     alpha <- 1 - ci_level
    #     lower_quantile <- alpha / 2
    #     upper_quantile <- 1 - alpha / 2
        
    #     # Summarize with empirical quantiles
    #     zone_summary <- subset_data[, .(
    #       mean_value = mean(value, na.rm = TRUE),
    #       ci_lower = quantile(value, lower_quantile, na.rm = TRUE),
    #       ci_upper = quantile(value, upper_quantile, na.rm = TRUE)
    #     ), by = .(date, zone, disease_state)]
        
    #     p <- ggplot(zone_summary, aes(x = date, y = mean_value, color = disease_state)) +
    #       geom_line(size = 1) +
    #       geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = disease_state), alpha = 0.2, color = NA) +
    #       facet_wrap(~zone, scales = "free_y") +
    #       labs(
    #         title = paste0("Mean Outcomes by Geographic Zone (", ci_level*100, "% Empirical CI)"),
    #         x = "Date",
    #         y = "Mean Value",
    #         color = "Disease State", 
    #         fill = "Disease State"
    #       ) +
    #       theme
          
    #   } else if (plot_type == "individual") {
    #     # Get raw data for individual instance lines
    #     subset_data <- self$subset_data(disease_states = disease_states, date_range = date_range)
        
    #     p <- ggplot(subset_data, aes(x = date, y = value, color = disease_state)) +
    #       geom_line(aes(group = interaction(disease_state, instance)), alpha = 0.6, size = 0.5) +
    #       facet_wrap(~zone, scales = "free_y") +
    #       labs(
    #         title = "Individual Instance Trajectories by Geographic Zone",
    #         x = "Date",
    #         y = "Value",
    #         color = "Disease State"
    #       ) +
    #       theme
          
    #   } else {
    #     stop("plot_type must be either 'mean' or 'individual'")
    #   }
      
    #   return(p)
    # }
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
#' \dontrun{
#' # Typically created through method chaining
#' summary_obj <- results$summarize(
#'   group_by = c("age", "race"), 
#'   stats = c("median", "quantile"),
#'   quantiles = c(0.25, 0.75)
#' )
#' 
#' # Direct plotting
#' summary_obj$plot()
#' 
#' # Plot with custom theme and confidence level
#' summary_obj$plot(ci_level = 0.5, theme = theme_bw())
#' 
#' # Complete method chaining workflow
#' results$summarize(group_by = "age", stats = c("median", "quantile"))$plot()
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
      
      # Show grouping variables
      columns <- names(self$data)
      group_vars <- intersect(columns, c("age", "race", "zone"))
      if (length(group_vars) > 0) {
        cat("Grouped by:", paste(group_vars, collapse = ", "), "\n")
      }
      
      # Show available disease states
      if ("disease_state" %in% columns) {
        disease_states <- unique(self$data$disease_state)
        if (length(disease_states) <= 5) {
          cat("Disease states:", paste(disease_states, collapse = ", "), "\n")
        } else {
          cat("Disease states:", length(disease_states), "unique states\n")
        }
      }
      
      # Show date range if available
      if ("date" %in% columns) {
        date_range <- range(self$data$date, na.rm = TRUE)
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
      
      # Check if we have the required columns for plotting
      if (!("median_value" %in% columns)) {
        stop("Plot method requires 'median_value' column. Please call summarize() first with stats = c('median', 'quantile')")
      }
      
      # Check for quantile columns
      quantile_cols <- grep("^q[0-9]", columns, value = TRUE)
      if (length(quantile_cols) < 2) {
        stop("Plot method requires quantile columns for confidence bands. Please call summarize() with stats = c('median', 'quantile')")
      }
      
      # Detect grouping variables
      group_vars <- intersect(columns, c("age", "race", "zone"))
      
      if (length(group_vars) == 0) {
        stop("Data must be grouped by at least one demographic variable")
      }
      
      # Create faceting strategy based on number of grouping variables
      if (length(group_vars) == 1) {
        # Single grouping variable: facet by that variable, color by disease_state
        facet_formula <- as.formula(paste("~", group_vars[1]))
        color_var <- "disease_state"
        
      } else if (length(group_vars) == 2) {
        # Two grouping variables: facet by both, color by disease_state
        facet_formula <- as.formula(paste(group_vars[1], "~", group_vars[2]))
        color_var <- "disease_state"
        
      } else if (length(group_vars) == 3) {
        # Three grouping variables: facet by first two, color by third
        # This creates fewer facets but still shows all information
        facet_formula <- as.formula(paste(group_vars[1], "~", group_vars[2]))
        color_var <- group_vars[3]
      }
      
      # Use first and last quantile columns for confidence bands
      ci_lower_col <- quantile_cols[1]
      ci_upper_col <- quantile_cols[length(quantile_cols)]
      
      # Create the plot
      p <- ggplot(self$data, aes(x = date, y = median_value, color = get(color_var))) +
        geom_line(size = 1) +
        geom_ribbon(aes(ymin = get(ci_lower_col), ymax = get(ci_upper_col), 
                      fill = get(color_var)), alpha = 0.2, color = NA) +
        facet_grid(facet_formula, scales = "free_y") +
        labs(
          title = title %||% paste0("Median Outcomes with ", ci_level*100, "% Empirical Quantiles"),
          x = "Date",
          y = "Median Value",
          color = tools::toTitleCase(gsub("_", " ", color_var)),
          fill = tools::toTitleCase(gsub("_", " ", color_var))
        ) +
        theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      return(p)
    }
  )
)

# Utility function for NULL coalescing operator
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
#' # Internal usage in classes
#' title <- user_title %||% "Default Title"
#' }
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x