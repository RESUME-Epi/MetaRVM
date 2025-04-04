
#' Aggregate outcome plot
#'
#' @param long_out_daily
#' @param conf_level
#' @param value_column
#' @param is.plotly
#'
#' @export
#'

plot_metaRVM <- function(long_out_daily,
                         conf_level,
                         value_column,
                         is.plotly = TRUE){

  compartment_colors <- c("Susceptible" = "steelblue3",
                          "Exposed" = "tan1",
                          "Presymptomatic" = "salmon3",
                          "Asymptomatic" = "orangered2",
                          "Symptomatic" = "red4",
                          "Hospitalized" = "mediumpurple",
                          "Recovered" = "green",
                          "Dead" = "grey",
                          "Vaccinated" = "darkgreen")

  # Compute summary statistics (median and selected confidence interval)
  summary_out <- summarize_out(long_out_daily, conf_level, value_column)

  p <- ggplot2::ggplot(summary_out, ggplot2::aes(x = date, y = median)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_bound, ymax = upper_bound, fill = disease_state),
                         alpha = 0.3, color = NA, show.legend = TRUE) +
    ggplot2::geom_line(ggplot2::aes(color = disease_state), linewidth = 1, show.legend = TRUE) +
    ggplot2::scale_color_manual(values = compartment_colors) +
    ggplot2::scale_fill_manual(values = compartment_colors) +
    ggplot2::scale_x_date(
      date_breaks = "1 week",
      date_labels = "%b %d"
    ) +
    ggplot2::labs(
      x = "Date",
      y = "number of people",
      color = "",
      fill = ""
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "right",
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)
    ) +
    ggplot2::guides(color = "none")

  if(is.plotly) return(plotly::ggplotly(p)) else return(p)

}

#' @title Plot of a specific compartment
#'
#' @param long_out_daily
#' @param conf_level
#' @param value_column
#' @param plot_type
#' @param color
#' @param ...
#'
#' @export
#'

plot_compartment <- function(long_out_daily,
                             conf_level,
                             value_column,
                             plot_type,
                             color, ...){

  # compute quantiles
  df_summary <- summarize_out(long_out_daily,
                              conf_level = as.numeric(conf_level),
                              value_column = value_column)

  if(plot_type == "line"){

    p <- ggplot2::ggplot(df_summary, ggplot2::aes(x = date, y = median)) +
          ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_bound, ymax = upper_bound),
                               fill = color, alpha = 0.3, ...) +
          ggplot2::geom_line(linewidth = 1, color = color) +
          # ggplot2::scale_color_manual(values = compartment_colors) +
          # ggplot2::scale_fill_manual(values = compartment_colors) +
          ggplot2::scale_x_date(
            date_breaks = "1 week",
            date_labels = "%b %d"
          ) +
          ggplot2::labs(
            x = "Date",
            y = "% of population",
            color = "",
            fill = ""
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5),
            legend.position = "right",
            axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)
          )
  }

  if(plot_type == "bar"){

    p <- ggplot2::ggplot(df_summary, ggplot2::aes(x = date, y = median)) +
      ggplot2::geom_col(alpha = 0.5, color = color, ...) +
      ggplot2::scale_x_date(
        date_breaks = "1 week",
        date_labels = "%b %d"
      ) +
      ggplot2::labs(
        x = "Date",
        y = "number of people",
        color = "",
        fill = ""
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        legend.position = "right",
        axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)
      )
  }


  return(plotly::ggplotly(p))
}

#' Title
#'
#' @param long_out
#' @param conf_level
#' @param pop_map_df
#' @param start_date
#' @param by_category
#'
#' @export
#'
plot_metaRVM_by_cateory <- function(long_out,
                                    conf_level,
                                    pop_map_df,
                                    start_date,
                                    by_category = NULL){

  compartment_colors <- c("Susceptible" = "steelblue3",
                          "Exposed" = "tan1",
                          "Presymptomatic" = "salmon3",
                          "Asymptomatic" = "orangered2",
                          "Symptomatic" = "red4",
                          "Hospitalized" = "mediumpurple",
                          "Recovered" = "green",
                          "Dead" = "grey",
                          "Vaccinated" = "darkgreen")

  df_cat <- cat_summary(long_out, pop_map_df, start_date, by_category)

  # Get quantiles
  conf_level <- as.numeric(conf_level)
  lower_prob <- (100 - conf_level) / 200
  upper_prob <- 1 - lower_prob

  summary_data <- df_cat %>%
    dplyr::group_by(date, disease_state, !!dplyr::sym(by_category)) %>%
    dplyr::summarize(
      median = stats::median(total_value, na.rm = TRUE),
      lower_bound = stats::quantile(total_value, probs = lower_prob, na.rm = TRUE),
      upper_bound = stats::quantile(total_value, probs = upper_prob, na.rm = TRUE),
      .groups = "drop"
    )

  p <- ggplot2::ggplot(summary_data, ggplot2::aes(x = date, y = median,
                                         color = disease_state,
                                         fill = disease_state)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_bound, ymax = upper_bound),
                         alpha = 0.3, color = NA) +
    ggplot2::geom_line(linewidth = 1, ) +
    ggplot2::facet_wrap(ggplot2::vars(!!dplyr::sym(by_category)), scales = "free_y") +
    ggplot2::scale_color_manual(values = compartment_colors) +
    ggplot2::scale_fill_manual(values = compartment_colors) +
    ggplot2::scale_x_date(
      date_breaks = "1 week",
      date_labels = "%b %d"
    ) +
    ggplot2::labs(
      # title = "Disease Compartments Over Time",
      x = "Date",
      y = "# of people",
      fill = "Compartment"
    ) +
    ggplot2::guides(color="none") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "right",
      legend.spacing = ggplot2::unit(1.5, "cm"),            # Increase spacing between legend items
      # legend.spacing.y = unit(2, "cm"),
      # legend.title.align = 0.5,                  # Align the legend title in the center
      legend.box.margin = ggplot2::margin(10, 10, 10, 10), # Add margin around the legend box
      legend.key.size =ggplot2:: unit(3, "lines"),      # Increase size of legend keys (symbols)
      legend.text = ggplot2::element_text(size = 15),     # Adjust legend text size
      legend.title = ggplot2::element_text(size = 20, margin = ggplot2::margin(b = 10), hjust = 0.5),     # Adjust legend title size
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
      axis.text.y = ggplot2::element_text(size = 15),
      axis.title = ggplot2::element_text(size = 15),
      strip.text = ggplot2::element_text(size = 20))

  return(p)

}


#' Title
#'
#' @param long_out
#' @param conf_level
#' @param pop_map_df
#' @param start_date
#' @param by_category
#' @param gg_theme
#'
#' @export
#'

plot_metaRVM_by_cateory_by_compartment <- function(long_out,
                                                   conf_level,
                                                   pop_map_df,
                                                   start_date,
                                                   by_category = NULL,
                                                   gg_theme = NULL){

  df_cat <- cat_summary(long_out, pop_map_df, start_date, by_category)

  # Get quantiles
  conf_level <- as.numeric(conf_level)
  lower_prob <- (100 - conf_level) / 200
  upper_prob <- 1 - lower_prob

  summary_data <- df_cat %>%
    dplyr::group_by(date, disease_state, !!dplyr::sym(by_category)) %>%
    dplyr::summarize(
      median = stats::median(total_value, na.rm = TRUE),
      lower_bound = stats::quantile(total_value, probs = lower_prob, na.rm = TRUE),
      upper_bound = stats::quantile(total_value, probs = upper_prob, na.rm = TRUE),
      .groups = "drop"
    )

  p <- ggplot2::ggplot(summary_data,
                       ggplot2::aes(x = date, y = median,
                           fill = !!dplyr::sym(by_category))) +
    ggplot2::facet_wrap(~ disease_state, scales = "free_y") +
    ggplot2::geom_bar(position="stack", stat="identity") +
    viridis::scale_fill_viridis(discrete = T) +
    ggplot2::scale_x_date(
      date_breaks = "1 week",  # Breaks every week
      date_labels = "%b %d"   # Format labels as "Month Day" (e.g., Jan 01)
    ) +
    ggplot2::labs(
      # title = "Disease Compartments Over Time",
      x = "Date",
      y = "# of people",
      color = "Compartment",
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "right",
      legend.spacing = ggplot2::unit(1.5, "cm"),            # Increase spacing between legend items
      # legend.spacing.y = unit(5, "cm"),
      # legend.title.align = 0.5,                  # Align the legend title in the center
      legend.box.margin = ggplot2::margin(10, 10, 10, 10), # Add margin around the legend box
      legend.key.size = ggplot2::unit(3, "lines"),      # Increase size of legend keys (symbols)
      legend.text = ggplot2::element_text(size = 15),     # Adjust legend text size
      legend.title = ggplot2::element_text(size = 20, margin = ggplot2::margin(b = 10), hjust = 0.5),     # Adjust legend title size
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
      axis.text.y = ggplot2::element_text(size = 15),
      axis.title = ggplot2::element_text(size = 15),
      strip.text = ggplot2::element_text(size = 20)
    )

  if(!is.null(gg_theme)) p <- p + gg_theme

  return(p)

}
