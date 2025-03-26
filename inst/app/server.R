library(shiny)
library(MetaRVM)
library(dplyr)
library(ggplot2)
library(odin)
library(dde)
library(leaflet)
library(future)
library(promises)
library(yaml)
# future::plan(multisession)

server <- function(input, output, session) {

  # Function to create a formatted title with the confidence interval
  format_ci_title <- function(base_title = "") {
    paste0(base_title, " (", input$conf_level, "% confidence interval)")
  }

  # Function to apply consistent legend formatting for all plots
  apply_legend_format <- function(p) {
    p <- p +
      ggplot2::labs(color = "Compartment", fill = NULL) +
      ggplot2::guides(fill = "none")
    return(p)
  }

  # Custom plot function to create a plot with a single legend entry per disease state
  create_compartment_plot <- function(summary_data, compartment_colors) {
    p <- ggplot2::ggplot(summary_data, aes(x = date, y = median, color = disease_state)) +
          ggplot2::geom_ribbon(aes(ymin = lower_90, ymax = upper_90, fill = disease_state),
                              alpha = 0.3, color = NA, show.legend = FALSE) +
          ggplot2::geom_line(linewidth = 1) +
          ggplot2::scale_color_manual(values = compartment_colors, name = "Compartment") +
          ggplot2::scale_fill_manual(values = compartment_colors) +
          ggplot2::scale_x_date(
            date_breaks = "1 week",
            date_labels = "%b %d"
          ) +
          ggplot2::labs(
            x = "Date",
            y = "# of people"
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = element_text(hjust = 0.5),
            legend.position = "right",
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
          )
    return(p)
  }

  # Display confidence interval text for all plots
  output$ci_display_text <- renderText({
    paste0("Shaded areas represent ", input$conf_level, "% confidence intervals")
  })

  output$ci_display_text_infections <- renderText({
    paste0("Shaded areas represent ", input$conf_level, "% confidence intervals")
  })

  output$ci_display_text_hosp <- renderText({
    paste0("Shaded areas represent ", input$conf_level, "% confidence intervals")
  })

  output$ci_display_text_deaths <- renderText({
    paste0("Shaded areas represent ", input$conf_level, "% confidence intervals")
  })

  output$ci_display_text_vac <- renderText({
    paste0("Shaded areas represent ", input$conf_level, "% confidence intervals")
  })

  output$ci_display_text_zone <- renderText({
    paste0("Shaded areas represent ", input$conf_level, "% confidence intervals")
  })

  output$ci_display_text_categories <- renderText({
    paste0("Shaded areas represent ", input$conf_level, "% confidence intervals")
  })

  output$yaml_content <- renderPrint({
    req(input$config)  # Ensure a file is uploaded

    # Read the uploaded YAML file
    yaml_data <- read_yaml(input$config$datapath)

    # Print the YAML content
    cat(as.yaml(yaml_data))
  })

  # permanently load shapefile
  read_hcz_geo <- ExtendedTask$new(function() {
    future_promise({
      read.csv(system.file("extdata", "Healthy_Chicago_Equity_Zones_20231129.csv",
                           package = "MetaRVM"))
    })
  })
  # hcz_geo <- read.csv(system.file("extdata", "Healthy_Chicago_Equity_Zones_20231129.csv", package = "MetaRVM"))


  # read simulation inputs

  config_yaml <- reactive({
    req(input$config)
    read_yaml(input$config$datapath)
  })


  # Read the CSV file for population data
  population_data <- reactive({
    yaml_data <- parse_config(input$config$datapath)
    yaml_data$pop_init
    # if(!is.null(yaml_data$pop_init)){
    #   data.table::fread(yaml_data$population_data$initialization)
    # } else {
    #   data.table::fread(system.file("extdata", "pop_init_150.csv", package = "MetaRVM"))
    # }
  })

  # Display the population data table in the UI
  output$population_table <- DT::renderDT({
    population_data()
  })

  vac_data <- reactive({
    yaml_data <- parse_config(input$config$datapath)
    yaml_data$vac
    # if(!is.null(yaml_data$vac)){
    #   data.table::fread(yaml_data$vac)
    # } else {
    #   data.table::fread(system.file("extdata", "vac_dates_150.csv", package = "MetaRVM"))
    # }
  })

  # process vaccination data to align with ODIN requirement
  process_vac_data <- reactive({
    yaml_data <- parse_config(input$config$datapath)
    raw_vac_data <- yaml_data$vac
    raw_vac_data$date <- as.Date(raw_vac_data$date,
                                 tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y"))

    date_filtered <- raw_vac_data %>%
      dplyr::filter(date >= as.Date(yaml_data$start_date)) %>%
      dplyr::mutate(t = (date - as.Date(yaml_data$start_date)) / 0.5) %>%
      dplyr::select(-c(date)) %>%
      select(last_col(), everything())

    date_filtered
  })

  # Display the population data table in the UI
  output$vac_table <- DT::renderDT({
    process_vac_data()
    # vac_data()
  })

  # pop_mapping_data <- reactive({
  #   if(!is.null(input$population_map)){
  #     data.table::fread(input$population_map$datapath)
  #   } else {
  #     system.file("extdata", ".csv", package = "MetaRVM")
  #   }
  # })


  # Compartment plot (unchanged)
  output$compartment_plot <- DiagrammeR::renderGrViz({


    dag <- "digraph {

    graph[layout = dot,
          fontsize = 15,
          outputorder = edgesfirst,
          ranksep = 1]

    # Node numbers with labelled text
    node [shape = box,
    width = 2,
    fontname = Helvetica]

    S [label = 'Susceptible', style = 'filled', fillcolor = 'palegreen4']
    E [label = 'Exposed', style = 'filled', fillcolor = 'yellow']
    V [label = 'Vaccinated', style = 'filled', fillcolor = 'palegreen']
    Ip [label = 'Infectious \n Presymptomatic', style = 'filled', fillcolor = 'indianred1']
    Ia [label = 'Infectious \n Asymptomatic', style = 'filled', fillcolor = 'indianred3']
    Is [label = 'Infectious \n Symptomatic', style = 'filled', fillcolor = 'indianred3']
    H [label = 'Hospitalized', style = 'filled', fillcolor = 'indianred4', fontcolor = 'white']
    R [label = 'Recoverd', style = 'filled', fillcolor = 'lightyellow']
    D [label = 'Dead', style = 'filled', fillcolor = 'grey']

    edge[color=darkgrey,arrowhead=vee]
    S -> E ;
    E -> Ia ;
    Ia -> R ;
    S -> V ;
    V -> S ;
    V -> E ;
    E -> Ip ;
    Ip -> Is ;
    Is -> H ;
    Is -> R;
    H -> R;
    H -> D;
    R -> S ;

    {rank = min; S; E; Ia; R}
    {rank = same; Ip; Is; H}
    {rank = max; V; D}

    }"
    DiagrammeR::grViz(dag)
  })


  # Read the mixing matrices
  # read_m1 <- reactive({
  #   yaml_data <- parse_config(input$config$datapath)
  #   if(!is.null(yaml_data$mixing_matrix$weekday_day)){
  #     read.csv(yaml_data$mixing_matrix$weekday_day, header = F)
  #   } else {
  #     read.csv(system.file("extdata", "m_weekday_day_150.csv", package = "MetaRVM"), header = F)
  #   }
  # })
  # read_m2 <- reactive({
  #   yaml_data <- parse_config(input$config$datapath)
  #   if(!is.null(yaml_data$mixing_matrix$weekday_night)){
  #     read.csv(yaml_data$mixing_matrix$weekday_night, header = F)
  #   } else {
  #     read.csv(system.file("extdata", "m_weekday_night_150.csv", package = "MetaRVM"), header = F)
  #   }
  # })
  # read_m3 <- reactive({
  #   yaml_data <- parse_config(input$config$datapath)
  #   if(!is.null(yaml_data$mixing_matrix$weekend_day)){
  #     read.csv(yaml_data$mixing_matrix$weekend_day, header = F)
  #   } else {
  #     read.csv(system.file("extdata", "m_weekend_day_150.csv", package = "MetaRVM"), header = F)
  #   }
  # })
  # read_m4 <- reactive({
  #   yaml_data <- parse_config(input$config$datapath)
  #   if(!is.null(yaml_data$mixing_matrix$weekend_night)){
  #     read.csv(yaml_data$mixing_matrix$weekend_night, header = F)
  #   } else {
  #     read.csv(system.file("extdata", "m_weekend_night_150.csv", package = "MetaRVM"), header = F)
  #   }
  # })

  # Display the mixing matrices in the UI
  # output$m1 <- DT::renderDT({
  #   read_m1()
  # })
  # output$m2 <- DT::renderDT({
  #   read_m2()
  # })
  # output$m3 <- DT::renderDT({
  #   read_m3()
  # })
  # output$m4 <- DT::renderDT({
  #   read_m4()
  # })

  output$tab <- renderText({
    input$navbar
  })

  # Read the population mapping table
  read_pop_map <- reactive({
    yaml_data <- parse_config(input$config$datapath)
    yaml_data$pop_map
    # if(!is.null(yaml_data$population_data$mapping)){
    #   read.csv(yaml_data$population_data$mapping, header = T)
    # } else {
    #   read.csv(system.file("extdata", "pop_mapping_150.csv", package = "MetaRVM"), header = T) ## TODO: change
    # }
  })

  ## ---------------------------------------------------------------------------
  ## ---------------------------------------------------------------------------
  # Run SEIR meta-population simulation when the button is clicked
  observeEvent(input$simulate, {

    yaml_data <- parse_config(input$config$datapath)
    delta_t <- 0.5

    # Extract population data
    pop_df <- yaml_data$pop_init
    N_pop <- nrow(pop_df)
    vac_df <- process_vac_data()
    P_ini <- pop_df[, N]
    S_ini <- pop_df[, S0]
    I_symp_ini <- pop_df[, I0]
    V_ini <- pop_df[, V0]
    R_ini <- pop_df[, R0]

    read_hcz_geo$invoke()

    ## fill in the missing time in vac data
    complete_time <- data.table::data.table(t = seq(0, yaml_data$sim_length / delta_t))

    ## merge
    vac_df <- merge(complete_time, vac_df, by = "t", all.x = TRUE)
    vac_df[is.na(vac_df)] <- 0

    tt <- vac_df[, t]
    vac <- as.matrix(vac_df[, -1])

    m1 <- yaml_data$m_wd_d
    m2 <- yaml_data$m_wd_n
    m3 <- yaml_data$m_we_d
    m4 <- yaml_data$m_we_n

    pop_map_df <- yaml_data$pop_map

    # seed <- input$seed
    # nrep <- input$rep
    start_date <- as.Date(yaml_data$start_date)
    nrep <- 1
    nsteps <- yaml_data$sim_length / delta_t
    if(!is.null(yaml_data$nsim)) nsim <- yaml_data$nsim else nsim <- 1


    # check if the model output should be deterministic
    # is.stoch <- ifelse(input$choice == "stoch", 1, 0)
    is.stoch <- 0

    if(is.stoch){
      if(!is.na(input$seed)) set.seed(input$seed) else set.seed(1)
    }

    out <- data.frame()
    for (ii in 1:nsim){

      # read disease parameters
      ts <- draw_sample(yaml_data$ts, N_pop)
      tv <- draw_sample(yaml_data$tv, N_pop)
      ve <- draw_sample(yaml_data$ve, N_pop)
      dv <- draw_sample(yaml_data$dv, N_pop)
      de <- draw_sample(yaml_data$de, N_pop)
      dp <- draw_sample(yaml_data$dp, N_pop)
      da <- draw_sample(yaml_data$da, N_pop)
      ds <- draw_sample(yaml_data$ds, N_pop)
      dh <- draw_sample(yaml_data$dh, N_pop)
      dr <- draw_sample(yaml_data$dr, N_pop)
      pea <- draw_sample(yaml_data$pea, N_pop)
      psr <- draw_sample(yaml_data$psr, N_pop)
      phr <- draw_sample(yaml_data$phr, N_pop)

      o <- meta_sim(is.stoch = is.stoch,
                    nsteps = nsteps,
                    N_pop = N_pop,
                    # beta_e = beta_e,
                    ts = ts,
                    tv = tv,
                    S0 = S_ini,
                    I0 = I_symp_ini,
                    P0 = P_ini,
                    V0 = V_ini,
                    R0 = R_ini,
                    m_weekday_day = as.matrix(m1),
                    m_weekday_night = as.matrix(m2),
                    m_weekend_day = as.matrix(m3),
                    m_weekend_night = as.matrix(m4),
                    delta_t = delta_t,
                    tvac = tt,
                    vac_mat = vac,
                    dv = dv,
                    de = de,
                    pea = pea,
                    dp = dp,
                    da = da,
                    ds = ds,
                    psr = psr,
                    dh = dh,
                    phr = phr,
                    dr = dr,
                    ve = ve)

      tmp <- data.frame(o)
      out <- rbind(out, cbind(tmp, ii))
    }
    colnames(out)[ncol(out)] <- "instance"

    ## -------------------------------------------------------------------------
    ## -------------------------------------------------------------------------
    # long_out <- out %>%
    #   tidyr::pivot_longer(
    #     cols = -c("step", "time", "rep"),               # Exclude 'time' from being pivoted
    #     names_to = c("disease_state", "population_id"),  # Create new columns for disease state and subpopulation
    #     names_pattern = "([A-Za-z_]+)\\.(\\d+)\\.",  # Regex to extract the disease state and subpopulation ID
    #     values_to = "value"          # Column to store the actual values
    #   )

    ## some post processed data for plotting
    long_out <- format_output(out)
    long_out_daily <- daily_output(long_out, start_date)
    # long_out_rates <- daily_out_rates_sums(long_out, start_date)

    # Display the output data table in the UI
    output$out_table <- DT::renderDT({
      long_out_daily
    })

    ## ===============================================
    # summarized SEIR plot
    output$seir_plot <- plotly::renderPlotly({
      compartment_colors <- c("Susceptible" = "steelblue3",
                              "Exposed" = "tan1",
                              "Presymptomatic" = "salmon3",
                              "Asymptomatic" = "orangered2",
                              "Symptomatic" = "red4",
                              "Hospitalized" = "mediumpurple",
                              "Recovered" = "green",
                              "Dead" = "grey",
                              "Vaccinated" = "darkgreen")

      # Get confidence level from input
      conf_level <- as.numeric(input$conf_level)
      lower_prob <- (100 - conf_level) / 200
      upper_prob <- 1 - lower_prob

      # Compute summary statistics (median and selected confidence interval)
      summary_data <- long_out_daily %>%
        dplyr::group_by(date, disease_state) %>%
        dplyr::summarize(
          median = median(total_value, na.rm = TRUE),
          lower_90 = quantile(total_value, probs = lower_prob, na.rm = TRUE),
          upper_90 = quantile(total_value, probs = upper_prob, na.rm = TRUE),
          .groups = "drop"
        )

      # Create a label for the confidence interval
      ci_label <- paste0(input$conf_level, "% confidence interval")

      plotly::ggplotly(
        ggplot2::ggplot(summary_data, aes(x = date, y = median, color = disease_state)) +
          ggplot2::geom_ribbon(aes(ymin = lower_90, ymax = upper_90, fill = disease_state),
                               alpha = 0.3, color = NA, show.legend = FALSE) +
          ggplot2::geom_line(linewidth = 1) +
          ggplot2::scale_color_manual(values = compartment_colors, guide = "none") +
          ggplot2::scale_fill_manual(values = compartment_colors) +
          ggplot2::scale_x_date(
            date_breaks = "1 week",
            date_labels = "%b %d"
          ) +
          ggplot2::labs(
            x = "Date",
            y = "# of people",
            color = "",
            fill = ""
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = element_text(hjust = 0.5),
            legend.position = "right",
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
          )
      )

    })


    ## =========================================================================
    ## rate and count plots
    # new infection
    output$new_infection_prop <- plotly::renderPlotly({

      df_combined <- daily_out_rates_sums(long_out, start_date, c("n_SE", "n_VE"))

      # Get confidence level from input
      conf_level <- as.numeric(input$conf_level)
      lower_prob <- (100 - conf_level) / 200
      upper_prob <- 1 - lower_prob

      summary_data <- df_combined %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(
          median = median(d_rate, na.rm = TRUE),
          lower_90 = quantile(d_rate, probs = lower_prob, na.rm = TRUE),
          upper_90 = quantile(d_rate, probs = upper_prob, na.rm = TRUE),
          .groups = "drop"
        )


      plotly::ggplotly(
        ggplot2::ggplot(summary_data, aes(x = date, y = median)) +
          ggplot2::geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.3, fill = "orangered2") +
          ggplot2::geom_line(linewidth = 1, color = "orangered2") +
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
            plot.title = element_text(hjust = 0.5),
            legend.position = "right",
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
          )
      )
    })

    output$new_infection_count <- plotly::renderPlotly({

      df_combined <- daily_out_rates_sums(long_out, start_date, c("n_SE", "n_VE"))

      # Get confidence level from input
      conf_level <- as.numeric(input$conf_level)
      lower_prob <- (100 - conf_level) / 200
      upper_prob <- 1 - lower_prob

      summary_data <- df_combined %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(
          median = median(d_sum, na.rm = TRUE),
          lower_90 = quantile(d_sum, probs = lower_prob, na.rm = TRUE),
          upper_90 = quantile(d_sum, probs = upper_prob, na.rm = TRUE),
          .groups = "drop"
        )

      plotly::ggplotly(
        ggplot2::ggplot(summary_data,
                        aes(x = date)) +
          ggplot2::geom_col(aes(y = median), linewidth = 0.5, alpha = 0.5, color = "orangered2") +
          ggplot2::labs(
            # title = "New infection rate",
            x = "Date",
            y = "# of people") +
          scale_x_date(
            date_breaks = "1 week",  # Breaks every week
            date_labels = "%b %d"   # Format labels as "Month Day" (e.g., Jan 01)
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
          )
      )

      # plotly::ggplotly(
      #   ggplot2::ggplot(summary_data, aes(x = date, y = median)) +
      #     ggplot2::geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.3, color = "orangered2") +
      #     ggplot2::geom_line(linewidth = 1, color = "orangered2") +
      #     # ggplot2::scale_color_manual(values = compartment_colors) +
      #     # ggplot2::scale_fill_manual(values = compartment_colors) +
      #     ggplot2::scale_x_date(
      #       date_breaks = "1 week",
      #       date_labels = "%b %d"
      #     ) +
      #     ggplot2::labs(
      #       x = "Date",
      #       y = "# of population",
      #       color = "",
      #       fill = ""
      #     ) +
      #     ggplot2::theme_bw() +
      #     ggplot2::theme(
      #       plot.title = element_text(hjust = 0.5),
      #       legend.position = "right",
      #       axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
      #     )
      # )
    })

    ## =========================================================================
    # new hospitalizations
    output$new_hosp_prop <- plotly::renderPlotly({


      # prepare the data
      df_combined <- daily_out_rates_sums(long_out, start_date, c("n_IsympH"))

      # Get confidence level from input
      conf_level <- as.numeric(input$conf_level)
      lower_prob <- (100 - conf_level) / 200
      upper_prob <- 1 - lower_prob

      summary_data <- df_combined %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(
          median = median(d_rate, na.rm = TRUE),
          lower_90 = quantile(d_rate, probs = lower_prob, na.rm = TRUE),
          upper_90 = quantile(d_rate, probs = upper_prob, na.rm = TRUE),
          .groups = "drop"
        )

      # plotly::ggplotly(
      #   ggplot2::ggplot(df_combined,
      #                   aes(x = date, y = d_rate, group = instance)) +
      #     ggplot2::geom_line(linewidth = 1, alpha = 1, color = "mediumpurple") +
      #     scale_x_date(
      #       date_breaks = "1 week",  # Breaks every week
      #       date_labels = "%b %d"   # Format labels as "Month Day" (e.g., Jan 01)
      #     ) +
      #     # scale_color_manual(values = compartment_colors) +
      #     ggplot2::labs(
      #       # title = "New hospitalization rate",
      #       x = "Date",
      #       y = "% of proportions") +
      #     ggplot2::scale_y_continuous(labels = scales::percent) +
      #     ggplot2::theme_bw() +
      #     ggplot2::theme(
      #       plot.title = element_text(hjust = 0.5),
      #       axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
      #     )
      # )

      plotly::ggplotly(
        ggplot2::ggplot(summary_data, aes(x = date, y = median)) +
          ggplot2::geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.3, fill = "mediumpurple") +
          ggplot2::geom_line(linewidth = 1, color = "mediumpurple") +
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
            plot.title = element_text(hjust = 0.5),
            legend.position = "right",
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
          )
      )
    })

    output$new_hosp_count <- plotly::renderPlotly({


      # prepare the data
      df_combined <- daily_out_rates_sums(long_out, start_date, c("n_IsympH"))

      summary_data <- df_combined %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(
          median = median(d_rate, na.rm = TRUE),
          lower_90 = quantile(d_rate, probs = 0.05, na.rm = TRUE),
          upper_90 = quantile(d_rate, probs = 0.95, na.rm = TRUE),
          .groups = "drop"
        )

      plotly::ggplotly(
        ggplot2::ggplot(summary_data,
                        aes(x = date, y = median)) +
          ggplot2::geom_col(linewidth = 1, alpha = 1, color = "mediumpurple") +
          scale_x_date(
            date_breaks = "1 week",  # Breaks every week
            date_labels = "%b %d"   # Format labels as "Month Day" (e.g., Jan 01)
          ) +
          # scale_color_manual(values = compartment_colors) +
          ggplot2::labs(
            # title = "New hospitalization rate",
            x = "Date",
            y = "# of people") +
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
          )
      )
    })

    ## =========================================================================
    # new deaths
    output$new_death_prop <- plotly::renderPlotly({


      # prepare the data
      df_combined <- daily_out_rates_sums(long_out, start_date, c("n_HD"))

      # Get confidence level from input
      conf_level <- as.numeric(input$conf_level)
      lower_prob <- (100 - conf_level) / 200
      upper_prob <- 1 - lower_prob

      summary_data <- df_combined %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(
          median = median(d_rate, na.rm = TRUE),
          lower_90 = quantile(d_rate, probs = lower_prob, na.rm = TRUE),
          upper_90 = quantile(d_rate, probs = upper_prob, na.rm = TRUE),
          .groups = "drop"
        )

      # plotly::ggplotly(
      #   ggplot2::ggplot(df_combined,
      #                   aes(x = date, y = d_rate, group = instance)) +
      #     ggplot2::geom_line(linewidth = 1, alpha = 1, color = "grey") +
      #     scale_x_date(
      #       date_breaks = "1 week",  # Breaks every week
      #       date_labels = "%b %d"   # Format labels as "Month Day" (e.g., Jan 01)
      #     ) +
      #     # scale_color_manual(values = compartment_colors) +
      #     ggplot2::labs(
      #       # title = "New deaths rate",
      #       x = "Date",
      #       y = "% of proportions") +
      #     ggplot2::scale_y_continuous(labels = scales::percent) +
      #     ggplot2::theme_bw() +
      #     ggplot2::theme(
      #       plot.title = element_text(hjust = 0.5),
      #       axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
      #     )
      # )

      plotly::ggplotly(
        ggplot2::ggplot(summary_data, aes(x = date, y = median)) +
          ggplot2::geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.3, fill = "grey") +
          ggplot2::geom_line(linewidth = 1, color = "black") +
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
            plot.title = element_text(hjust = 0.5),
            legend.position = "right",
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
          )
      )
    })

    output$new_death_count <- plotly::renderPlotly({

      # prepare the data
      df_combined <- daily_out_rates_sums(long_out, start_date, c("n_HD"))

      summary_data <- df_combined %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(
          median = median(d_rate, na.rm = TRUE),
          lower_90 = quantile(d_rate, probs = 0.05, na.rm = TRUE),
          upper_90 = quantile(d_rate, probs = 0.95, na.rm = TRUE),
          .groups = "drop"
        )

      plotly::ggplotly(
        ggplot2::ggplot(summary_data,
                        aes(x = date, y = median)) +
          ggplot2::geom_col(linewidth = 1, alpha = 1, color = "grey") +
          scale_x_date(
            date_breaks = "1 week",  # Breaks every week
            date_labels = "%b %d"   # Format labels as "Month Day" (e.g., Jan 01)
          ) +
          # scale_color_manual(values = compartment_colors) +
          ggplot2::labs(
            # title = "New deaths rate",
            x = "Date",
            y = "# of people") +
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
          )
      )
    })

    ## =========================================================================
    # new vaccinations
    output$new_vac_prop <- plotly::renderPlotly({


      # prepare the data
      df_combined <- daily_out_rates_sums(long_out, start_date, c("n_SV"))

      # Get confidence level from input
      conf_level <- as.numeric(input$conf_level)
      lower_prob <- (100 - conf_level) / 200
      upper_prob <- 1 - lower_prob

      summary_data <- df_combined %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(
          median = median(d_rate, na.rm = TRUE),
          lower_90 = quantile(d_rate, probs = lower_prob, na.rm = TRUE),
          upper_90 = quantile(d_rate, probs = upper_prob, na.rm = TRUE),
          .groups = "drop"
        )

      # plotly::ggplotly(
      #   ggplot2::ggplot(df_combined,
      #                   aes(x = date, y = d_rate, group = instance)) +
      #     ggplot2::geom_line(linewidth = 1, alpha = 1, color = "darkgreen") +
      #     scale_x_date(
      #       date_breaks = "1 week",  # Breaks every week
      #       date_labels = "%b %d"   # Format labels as "Month Day" (e.g., Jan 01)
      #     ) +
      #     # scale_color_manual(values = compartment_colors) +
      #     ggplot2::labs(
      #       # title = "New vaccination",
      #       x = "Time",
      #       y = "proportions") +
      #     ggplot2::scale_y_continuous(labels = scales::percent) +
      #     ggplot2::theme_bw() +
      #     ggplot2::theme(
      #       plot.title = element_text(hjust = 0.5),
      #       axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
      #     )
      # )

      plotly::ggplotly(
        ggplot2::ggplot(summary_data, aes(x = date, y = median)) +
          ggplot2::geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.3, fill = "darkgreen") +
          ggplot2::geom_line(linewidth = 1, color = "darkgreen") +
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
            plot.title = element_text(hjust = 0.5),
            legend.position = "right",
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
          )
      )
    })

    output$new_vac_count <- plotly::renderPlotly({

      # prepare the data
      df_combined <- daily_out_rates_sums(long_out, start_date, c("n_SV"))

      summary_data <- df_combined %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(
          median = median(d_rate, na.rm = TRUE),
          lower_90 = quantile(d_rate, probs = 0.05, na.rm = TRUE),
          upper_90 = quantile(d_rate, probs = 0.95, na.rm = TRUE),
          .groups = "drop"
        )

      plotly::ggplotly(
        ggplot2::ggplot(summary_data,
                        aes(x = date, y = median)) +
          ggplot2::geom_col(alpha = 1, color = "darkgreen") +
          scale_x_date(
            date_breaks = "1 week",  # Breaks every week
            date_labels = "%b %d"   # Format labels as "Month Day" (e.g., Jan 01)
          ) +
          # scale_color_manual(values = compartment_colors) +
          ggplot2::labs(
            # title = "New vaccination",
            x = "Time",
            y = "# of people") +
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
          )
      )
    })


    ## -------------------------------------------------------------------------
    ## -------------------------------------------------------------------------
    ## GEO plot


    # Render Leaflet map with borough boundaries
    output$map <- leaflet::renderLeaflet({
        # if(input$navbar == "HCEZ Figures"){                     # capture tab input

          # Read HCZ geometry
          # hcz_geo <- read.csv(system.file("extdata", "Healthy_Chicago_Equity_Zones_20231129.csv", package = "MetaRVM"))
          hcz_geo <- read_hcz_geo$result()
          hcz_sf <<- sf::st_as_sf(hcz_geo, wkt = "Geometry", crs = 4326)
          hcz_names <<- hcz_geo$Equity.Zone

          # Create a color palette for the boroughs
          palette <- leaflet::colorFactor(palette = "Set1", domain = hcz_names)  # Set1 palette from RColorBrewer

          # map plot
          leaflet::leaflet(hcz_sf) %>%
            leaflet::addProviderTiles("CartoDB.Positron") %>%
            leaflet::addPolygons(
              layerId = ~Equity.Zone,
              fillColor = ~palette(hcz_names),
              color = "black",
              weight = 2,
              fillOpacity = 0.5,
              highlight = highlightOptions(
                weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
              ),
              label = ~hcz_names,                 # Labels showing the borough name on hover
              labelOptions = labelOptions(
                style = list("font-weight" = "bold", "color" = "black"),
                textsize = "15px", direction = "auto"
              )
            )

        # }
      })


    # Observe a click on the map and update the ggplot for the selected borough
    # Observe the click event on the map
    observeEvent(input$map_shape_click, {
      zone <- input$map_shape_click$id

      palette <- leaflet::colorFactor(palette = "Set1", domain = hcz_names)

      # Update the map to highlight the selected region and dim the others
      leaflet::leafletProxy("map", data = hcz_sf) %>%
        leaflet::clearShapes() %>%  # Clear existing shapes

        # Re-add polygons, highlight the selected region and dim others
        leaflet::addPolygons(
          layerId = ~Equity.Zone,
          fillColor = ~ifelse(Equity.Zone == zone, palette(hcz_names), "lightgray"),  # Highlight selected, dim others
          color = ~ifelse(Equity.Zone == zone, "red", "black"),  # Red border for selected region
          weight = ~ifelse(Equity.Zone == zone, 4, 2),           # Thicker border for selected
          fillOpacity = ~ifelse(Equity.Zone == zone, 0.9, 0.3),  # Higher opacity for selected
          highlightOptions = highlightOptions(
            weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
          ),
          label = ~Equity.Zone
        )
    })

    # capture clicked zone
    selected_zone <- reactive({
      input$map_shape_click$id  # Capture the clicked zone (by zone name)
    })

    # Render the ggplot based on the selected zone
    long_out_zones <- zone_summary(long_out, pop_map_df, start_date)

    output$zone_simout <- plotly::renderPlotly({
      req(selected_zone())  # Make sure a zone is selected

      # Map borough name to sub_population_id
      zone_id <- which(hcz_names == selected_zone())

      filtered_data <- long_out_zones %>%
        dplyr::filter(hcez %in% selected_zone())

      # Get confidence level from input
      conf_level <- as.numeric(input$conf_level)
      lower_prob <- (100 - conf_level) / 200
      upper_prob <- 1 - lower_prob

      summary_data <- filtered_data %>%
        dplyr::group_by(date, disease_state) %>%
        dplyr::summarize(
          median = median(total_value, na.rm = TRUE),
          lower_90 = quantile(total_value, probs = lower_prob, na.rm = TRUE),
          upper_90 = quantile(total_value, probs = upper_prob, na.rm = TRUE),
          .groups = "drop"
        )


      # Plot the simulation output for the selected zone

      compartment_colors <- c("Susceptible" = "steelblue3",
                              "Exposed" = "tan1",
                              "Presymptomatic" = "salmon3",
                              "Asymptomatic" = "orangered2",
                              "Symptomatic" = "red4",
                              "Hospitalized" = "mediumpurple",
                              "Recovered" = "green",
                              "Dead" = "grey",
                              "Vaccinated" = "darkgreen")
      # plotly::ggplotly(
      #   ggplot2::ggplot(filtered_data, # %>%
      #                     # dplyr::mutate(disease_state = factor(disease_state,
      #                     #                                      levels = c("S", "E", "H", "D",
      #                     #                                                 "I_presymp", "I_asymp",
      #                     #                                                 "I_symp", "R", "V"))) %>%
      #                     # dplyr::group_by(date, disease_state, rep) %>%
      #                     # dplyr::summarize(total_value = value, .groups = "drop"),
      #                   aes(x = date, y = total_value, color = disease_state, group = instance)) +
      #     ggplot2::geom_line(linewidth = 1, alpha = 1) +
      #     ggplot2::scale_color_manual(values = compartment_colors) +
      #     ggplot2::labs(
      #       title = selected_zone(),
      #       x = "Date",
      #       y = "# of people",
      #       color = "Compartment",
      #     ) +
      #     scale_x_date(
      #       date_breaks = "1 week",  # Breaks every week
      #       date_labels = "%b %d"   # Format labels as "Month Day" (e.g., Jan 01)
      #     ) +
      #     ggplot2::theme_bw() +
      #     ggplot2::theme(
      #       plot.title = element_text(hjust = 0.5),
      #       legend.position = "right",
      #       axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
      #     )
      # )

      plotly::ggplotly(
        ggplot2::ggplot(summary_data, aes(x = date, y = median, color = disease_state, fill = disease_state)) +
          ggplot2::geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.3, color = NA) +
          ggplot2::geom_line(linewidth = 1) +
          ggplot2::scale_color_manual(values = compartment_colors) +
          ggplot2::scale_fill_manual(values = compartment_colors) +
          ggplot2::scale_x_date(
            date_breaks = "1 week",
            date_labels = "%b %d"
          ) +
          ggplot2::labs(
            x = "Date",
            y = "# of people",
            color = "",
            fill = ""
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = element_text(hjust = 0.5),
            legend.position = "right",
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
          )
      )
    })

    ## -------------------------------------------------------------------------
    ## -------------------------------------------------------------------------
    ## caterogy-wise plot

    # output$cat_simout <- plotly::renderPlotly({
    output$cat_simout <- renderCachedPlot({


      compartment_colors <- c("Susceptible" = "steelblue3",
                              "Exposed" = "tan1",
                              "Presymptomatic" = "salmon3",
                              "Asymptomatic" = "orangered2",
                              "Symptomatic" = "red4",
                              "Hospitalized" = "mediumpurple",
                              "Recovered" = "green",
                              "Dead" = "grey",
                              "Vaccinated" = "darkgreen")


      # merge long output with population map
      df_long <- merge(long_out, pop_map_df, by = "population_id")

      cat_long_out <- df_long %>%
        dplyr::filter(time %% 1 == 0) %>%
        dplyr::filter(disease_state %in% c("S", "E", "H", "D",
                                           "I_presymp", "I_asymp",
                                           "I_symp", "R", "V")) %>%
        dplyr::mutate(disease_state = factor(disease_state,
                                             levels = c("S", "E", "H", "D",
                                                        "I_presymp", "I_asymp",
                                                        "I_symp", "R", "V"),
                                             labels = c("Susceptible",
                                                        "Exposed",
                                                        "Hospitalized",
                                                        "Dead",
                                                        "Presymptomatic",
                                                        "Asymptomatic",
                                                        "Symptomatic",
                                                        "Recovered",
                                                        "Vaccinated"))) %>%
        dplyr::group_by(time, disease_state, instance, !!sym(input$Category)) %>%
        dplyr::summarize(total_value = sum(value), .groups = "drop") %>%
        dplyr::mutate(date = start_date + time)

      # Get confidence level from input
      conf_level <- as.numeric(input$conf_level)
      lower_prob <- (100 - conf_level) / 200
      upper_prob <- 1 - lower_prob

      summary_data <- cat_long_out %>%
        dplyr::group_by(date, disease_state, !!sym(input$Category)) %>%
        dplyr::summarize(
          median = median(total_value, na.rm = TRUE),
          lower_90 = quantile(total_value, probs = lower_prob, na.rm = TRUE),
          upper_90 = quantile(total_value, probs = upper_prob, na.rm = TRUE),
          .groups = "drop"
        )

      # plotly::ggplotly(
        ggplot2::ggplot(summary_data, aes(x = date, y = median, color = disease_state, fill = disease_state)) +
          ggplot2::geom_ribbon(aes(ymin = lower_90, ymax = upper_90), alpha = 0.3, color = NA) +
          ggplot2::geom_line(linewidth = 1, ) +
          ggplot2::facet_wrap(vars(!!sym(input$Category)), scales = "free_y") +
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
                plot.title = element_text(hjust = 0.5),
                legend.position = "right",
                legend.spacing = unit(1.5, "cm"),            # Increase spacing between legend items
                # legend.spacing.y = unit(2, "cm"),
                # legend.title.align = 0.5,                  # Align the legend title in the center
                legend.box.margin = margin(10, 10, 10, 10), # Add margin around the legend box
                legend.key.size = unit(3, "lines"),      # Increase size of legend keys (symbols)
                legend.text = element_text(size = 15),     # Adjust legend text size
                legend.title = element_text(size = 20, margin = margin(b = 10), hjust = 0.5),     # Adjust legend title size
                axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
                axis.text.y = element_text(size = 15),
                axis.title = element_text(size = 15),
                strip.text = element_text(size = 20))
      # )

      # plotly::ggplotly(
        # ggplot2::ggplot(summary_data,
        #                 aes(x = date, y = total_value, color = disease_state)) +
        #   ggplot2::facet_wrap(vars(!!sym(input$Category)), scales = "free_y") +
        #   ggplot2::geom_line(linewidth = 1, alpha = 0.5) +
        #   ggplot2::scale_color_manual(values = compartment_colors) +
        #   scale_x_date(
        #     date_breaks = "1 week",  # Breaks every week
        #     date_labels = "%b %d"   # Format labels as "Month Day" (e.g., Jan 01)
        #   ) +
        #   ggplot2::labs(
        #     # title = "Disease Compartments Over Time",
        #     x = "Date",
        #     y = "# of people",
        #     color = "Compartment",
        #   ) +
        #   ggplot2::theme_bw() +
        #   ggplot2::theme(
        #     plot.title = element_text(hjust = 0.5),
        #     legend.position = "right",
        #     legend.spacing = unit(1.5, "cm"),            # Increase spacing between legend items
        #     # legend.spacing.y = unit(2, "cm"),
        #     # legend.title.align = 0.5,                  # Align the legend title in the center
        #     legend.box.margin = margin(10, 10, 10, 10), # Add margin around the legend box
        #     legend.key.size = unit(3, "lines"),      # Increase size of legend keys (symbols)
        #     legend.text = element_text(size = 15),     # Adjust legend text size
        #     legend.title = element_text(size = 20, margin = margin(b = 10), hjust = 0.5),     # Adjust legend title size
        #     axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
        #     axis.text.y = element_text(size = 15),
        #     axis.title = element_text(size = 15),
        #     strip.text = element_text(size = 20)
        #   )
      # )
    }, cacheKeyExpr = { input$Category })

    # output$stacked_simout <- plotly::renderPlotly({
    output$stacked_simout <- renderCachedPlot({

      # merge long output with population map
      df_long <- merge(long_out, pop_map_df, by = "population_id")

      cat_long_out <- df_long %>%
        dplyr::filter(time %% 1 == 0) %>%
        dplyr::filter(disease_state %in% c("S", "E", "H", "D",
                                           "I_presymp", "I_asymp",
                                           "I_symp", "R", "V")) %>%
        dplyr::mutate(disease_state = factor(disease_state,
                                             levels = c("S", "E", "H", "D",
                                                        "I_presymp", "I_asymp",
                                                        "I_symp", "R", "V"),
                                             labels = c("Susceptible",
                                                        "Exposed",
                                                        "Hospitalized",
                                                        "Dead",
                                                        "Presymptomatic",
                                                        "Asymptomatic",
                                                        "Symptomatic",
                                                        "Recovered",
                                                        "Vaccinated"))) %>%
        dplyr::group_by(time, disease_state, instance, !!sym(input$Category)) %>%
        dplyr::summarize(total_value = sum(value), .groups = "drop") %>%
        dplyr::mutate(date = start_date + time)

      # plotly::ggplotly(
        ggplot2::ggplot(cat_long_out,
                        aes(x = date, y = total_value, fill = !!sym(input$Category))) +
          ggplot2::facet_wrap(~ disease_state, scales = "free_y") +
          ggplot2::geom_bar(position="stack", stat="identity") +
          viridis::scale_fill_viridis(discrete = T) +
          scale_x_date(
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
            plot.title = element_text(hjust = 0.5),
            legend.position = "right",
            legend.spacing = unit(1.5, "cm"),            # Increase spacing between legend items
            # legend.spacing.y = unit(5, "cm"),
            # legend.title.align = 0.5,                  # Align the legend title in the center
            legend.box.margin = margin(10, 10, 10, 10), # Add margin around the legend box
            legend.key.size = unit(3, "lines"),      # Increase size of legend keys (symbols)
            legend.text = element_text(size = 15),     # Adjust legend text size
            legend.title = element_text(size = 20, margin = margin(b = 10), hjust = 0.5),     # Adjust legend title size
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
            axis.text.y = element_text(size = 15),
            axis.title = element_text(size = 15),
            strip.text = element_text(size = 20)
          )
      # )
    }, cacheKeyExpr = { input$Category })


    ## subset simulation output, display, download

    age_choices <- unique(pop_map_df$age)
    race_choices <- unique(pop_map_df$race)
    geo_choices <- unique(pop_map_df$hcez)

    observe({
      updateCheckboxGroupInput(session, "ages", choices = age_choices, selected = age_choices[1])
      updateCheckboxGroupInput(session, "races", choices = race_choices, selected = race_choices[1])
      updateCheckboxGroupInput(session, "hcezs", choices = geo_choices, selected = geo_choices[1])
      updateCheckboxGroupInput(session, "disease_states", choices = c("Susceptible",
                                                                      "Exposed",
                                                                      "Hospitalized",
                                                                      "Dead",
                                                                      "Presymptomatic",
                                                                      "Asymptomatic",
                                                                      "Symptomatic",
                                                                      "Recovered",
                                                                      "Vaccinated"), selected = "Exposed")
      updateCheckboxGroupInput(session, "new_counts", choices = c("New Exposed (S)",
                                                                  "New Exposed (V)",
                                                                  "New Hospitalized",
                                                                  "New Recovered (Ia)",
                                                                  "New Recovered (Is)",
                                                                  "New Recovered (H)",
                                                                  "New Dead"), selected = "New Exposed (S)")
    })

    sub_out <- reactive({
      subset_simout(long_out, start_date, pop_map_df,
                    input$ages, input$races, input$hcezs,
                    c(input$disease_states, input$new_counts))
    })

    # Display the filtered results
    output$simulationOutput <- DT::renderDT({
      sub_out()
    })

    # Download filtered results
    output$download <- downloadHandler(

      yaml_data <- config_yaml(),

      filename = function() {
        # paste("out_", Sys.Date(), ".zip", sep = "")
        "out.zip"
      },
      content = function(file) {

        sim_input <- paste("in_", yaml_data$run_id, ".yaml")
        sim_output <- paste("out_", yaml_data$run_id, ".csv", sep = "")

        write_yaml(yaml_data, sim_input)
        write.csv(sub_out(), sim_output, row.names = FALSE)

        zip(file, files = c(sim_input, sim_output))
      },
      contentType = "application/zip"
    )


  })

}

