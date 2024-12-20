library(shiny)
library(MetaRVM)
library(dplyr)
library(ggplot2)
library(odin)
library(dde)
library(leaflet)
library(future)
library(promises)
# future::plan(multisession)

server <- function(input, output, session) {

  # permanently load shapefile
  read_hcz_geo <- ExtendedTask$new(function() {
    future_promise({
      read.csv(system.file("extdata", "Healthy_Chicago_Equity_Zones_20231129.csv",
                           package = "MetaRVM"))
    })
  })
  # hcz_geo <- read.csv(system.file("extdata", "Healthy_Chicago_Equity_Zones_20231129.csv", package = "MetaRVM"))



  # Read the CSV file for population data
  population_data <- reactive({
    if(!is.null(input$population_data)){
      data.table::fread(input$population_data$datapath)
    } else {
      data.table::fread(system.file("extdata", "pop_init_150.csv", package = "MetaRVM"))
    }
  })

  # Display the population data table in the UI
  output$population_table <- DT::renderDT({
    population_data()
  })

  vac_data <- reactive({
    if(!is.null(input$vac_data)){
      data.table::fread(input$vac_data$datapath)
    } else {
      data.table::fread(system.file("extdata", "vac_dates_150.csv", package = "MetaRVM"))
    }
  })

  # process vaccination data to align with ODIN requirement
  process_vac_data <- reactive({
    raw_vac_data <- vac_data()
    raw_vac_data$date <- as.Date(raw_vac_data$date)

    date_filtered <- raw_vac_data %>%
      dplyr::filter(date >= as.Date(input$start_date)) %>%
      dplyr::mutate(t = (date - as.Date(input$start_date)) / input$dt) %>%
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
  read_m1 <- reactive({
    if(!is.null(input$mix_mat1)){
      read.csv(input$mix_mat1$datapath, header = F)
    } else {
      read.csv(system.file("extdata", "m_weekday_day_150.csv", package = "MetaRVM"), header = F)
    }
  })
  read_m2 <- reactive({
    if(!is.null(input$mix_mat2)){
      read.csv(input$mix_mat2$datapath, header = F)
    } else {
      read.csv(system.file("extdata", "m_weekday_night_150.csv", package = "MetaRVM"), header = F)
    }
  })
  read_m3 <- reactive({
    if(!is.null(input$mix_mat3)){
      read.csv(input$mix_mat3$datapath, header = F)
    } else {
      read.csv(system.file("extdata", "m_weekend_day_150.csv", package = "MetaRVM"), header = F)
    }
  })
  read_m4 <- reactive({
    if(!is.null(input$mix_mat4)){
      read.csv(input$mix_mat4$datapath, header = F)
    } else {
      read.csv(system.file("extdata", "m_weekend_night_150.csv", package = "MetaRVM"), header = F)
    }
  })

  # Display the mixing matrices in the UI
  output$m1 <- DT::renderDT({
    read_m1()
  })
  output$m2 <- DT::renderDT({
    read_m2()
  })
  output$m3 <- DT::renderDT({
    read_m3()
  })
  output$m4 <- DT::renderDT({
    read_m4()
  })

  output$tab <- renderText({
    input$navbar
  })

  # Read the population mapping table
  read_pop_map <- reactive({
    if(!is.null(input$population_map)){
      read.csv(input$population_map$datapath, header = T)
    } else {
      read.csv(system.file("extdata", "pop_mapping_150.csv", package = "MetaRVM"), header = T) ## TODO: change
    }
  })

  ## ---------------------------------------------------------------------------
  ## ---------------------------------------------------------------------------
  # Run SEIR meta-population simulation when the button is clicked
  observeEvent(input$simulate, {
    # Extract population data
    pop_df <- population_data()
    N_pop <- nrow(pop_df)
    vac_df <- process_vac_data()
    P_ini <- pop_df[, N]
    S_ini <- pop_df[, S0]
    I_symp_ini <- pop_df[, I0]
    V_ini <- pop_df[, V0]
    R_ini <- pop_df[, R0]

    read_hcz_geo$invoke()

    ## fill in the missing time in vac data
    complete_time <- data.table::data.table(t = seq(0, input$days))

    ## merge
    vac_df <- merge(complete_time, vac_df, by = "t", all.x = TRUE)
    vac_df[is.na(vac_df)] <- 0

    tt <- vac_df[, t]
    vac <- as.matrix(vac_df[, -1])

    m1 <- read_m1()
    m2 <- read_m2()
    m3 <- read_m3()
    m4 <- read_m4()

    pop_map_df <- read_pop_map()

    # seed <- input$seed
    # nrep <- input$rep
    start_date <- as.Date(input$start_date)
    nrep <- 1
    delta_t <- input$dt
    nsteps <- input$days / delta_t
    beta_i <- input$beta_i
    beta_v <- input$beta_v
    VtoS <- 1/input$VtoS
    EtoIpresymp <- 1/input$EtoIpresymp
    etopa <- input$etopa
    pretoIsymp <- 1/input$pretoIsymp
    IasymptoR <- 1/input$IasymptoR
    IsymptoRH <- 1/input$IsymptoRH
    istohr <- input$istohr
    HtoRD <- 1/input$HtoRD
    htor <- input$htor
    RtoS <- 1/input$RtoS
    vac_eff <- input$vac_eff

    # check if the model output should be deterministic
    # is.stoch <- ifelse(input$choice == "stoch", 1, 0)
    is.stoch <- 0

    if(is.stoch){
      if(!is.na(input$seed)) set.seed(input$seed) else set.seed(1)
    }

    out <- data.frame()
    for (ii in 1:nrep){

      o <- meta_sim(is.stoch = is.stoch,
                    nsteps = nsteps,
                    N_pop = N_pop,
                    # beta_e = beta_e,
                    beta_i = beta_i,
                    beta_v = beta_v,
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
                    VtoS = VtoS,
                    EtoIpresymp = EtoIpresymp,
                    etopa = etopa,
                    pretoIsymp = pretoIsymp,
                    IasymptoR = IasymptoR,
                    IsymptoRH = IsymptoRH,
                    istohr = istohr,
                    HtoRD = HtoRD,
                    htor = htor,
                    RtoS = RtoS,
                    vac_eff = vac_eff)

      tmp <- data.frame(o)
      out <- rbind(out, cbind(tmp, ii))
    }
    colnames(out)[ncol(out)] <- "rep"

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


      plotly::ggplotly(
        ggplot2::ggplot(long_out_daily,
                        aes(x = date, y = total_value, color = disease_state, group = rep)) +
          ggplot2::geom_line(linewidth = 1, alpha = 0.7) +
          ggplot2::scale_color_manual(values = compartment_colors) +
          ggplot2::scale_x_date(
            date_breaks = "1 week",  # Breaks every week
            date_labels = "%b %d"   # Format labels as "Month Day" (e.g., Jan 01)
          ) +
          # ggplot2::scale_y_continuous(transform = "log") +
          # ggplot2::ylim(0, max(long_out_daily$total_value[!long_out_daily$disease_state %in% c("Susceptible", "Recovered")])) +
          ggplot2::labs(
            # title = "Disease Compartments Over Time",
            x = "Date",
            y = "# of people",
            color = "",
          ) +
          # ggthemes::theme_tufte() +
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

      plotly::ggplotly(
        ggplot2::ggplot(df_combined,
                        aes(x = date, group = rep)) +
          ggplot2::geom_line(aes(y = d_rate), linewidth = 1, alpha = 1, color = "orangered2") +
          ggplot2::labs(
            # title = "New infection rate",
            x = "Date",
            y = "% of population") +
          scale_x_date(
            date_breaks = "1 week",  # Breaks every week
            date_labels = "%b %d"   # Format labels as "Month Day" (e.g., Jan 01)
          ) +
          ggplot2::scale_y_continuous(labels = scales::percent) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
          )
      )
    })

    output$new_infection_count <- plotly::renderPlotly({

      df_combined <- daily_out_rates_sums(long_out, start_date, c("n_SE", "n_VE"))

      plotly::ggplotly(
        ggplot2::ggplot(df_combined,
                        aes(x = date, group = rep)) +
          ggplot2::geom_col(aes(y = d_sum), linewidth = 0.5, alpha = 0.5, color = "orangered2") +
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
    })

    ## =========================================================================
    # new hospitalizations
    output$new_hosp_prop <- plotly::renderPlotly({


      # prepare the data
      df_combined <- daily_out_rates_sums(long_out, start_date, c("n_IsympH"))

      plotly::ggplotly(
        ggplot2::ggplot(df_combined,
                        aes(x = date, y = d_rate, group = rep)) +
          ggplot2::geom_line(linewidth = 1, alpha = 1, color = "mediumpurple") +
          scale_x_date(
            date_breaks = "1 week",  # Breaks every week
            date_labels = "%b %d"   # Format labels as "Month Day" (e.g., Jan 01)
          ) +
          # scale_color_manual(values = compartment_colors) +
          ggplot2::labs(
            # title = "New hospitalization rate",
            x = "Date",
            y = "% of proportions") +
          ggplot2::scale_y_continuous(labels = scales::percent) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
          )
      )
    })

    output$new_hosp_count <- plotly::renderPlotly({


      # prepare the data
      df_combined <- daily_out_rates_sums(long_out, start_date, c("n_IsympH"))

      plotly::ggplotly(
        ggplot2::ggplot(df_combined,
                        aes(x = date, y = d_sum, group = rep)) +
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

      plotly::ggplotly(
        ggplot2::ggplot(df_combined,
                        aes(x = date, y = d_rate, group = rep)) +
          ggplot2::geom_line(linewidth = 1, alpha = 1, color = "grey") +
          scale_x_date(
            date_breaks = "1 week",  # Breaks every week
            date_labels = "%b %d"   # Format labels as "Month Day" (e.g., Jan 01)
          ) +
          # scale_color_manual(values = compartment_colors) +
          ggplot2::labs(
            # title = "New deaths rate",
            x = "Date",
            y = "% of proportions") +
          ggplot2::scale_y_continuous(labels = scales::percent) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
          )
      )
    })

    output$new_death_count <- plotly::renderPlotly({

      # prepare the data
      df_combined <- daily_out_rates_sums(long_out, start_date, c("n_HD"))

      plotly::ggplotly(
        ggplot2::ggplot(df_combined,
                        aes(x = date, y = d_sum, group = rep)) +
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

      plotly::ggplotly(
        ggplot2::ggplot(df_combined,
                        aes(x = date, y = d_rate, group = rep)) +
          ggplot2::geom_line(linewidth = 1, alpha = 1, color = "darkgreen") +
          scale_x_date(
            date_breaks = "1 week",  # Breaks every week
            date_labels = "%b %d"   # Format labels as "Month Day" (e.g., Jan 01)
          ) +
          # scale_color_manual(values = compartment_colors) +
          ggplot2::labs(
            # title = "New vaccination",
            x = "Time",
            y = "proportions") +
          ggplot2::scale_y_continuous(labels = scales::percent) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
          )
      )
    })

    output$new_vac_count <- plotly::renderPlotly({

      # prepare the data
      df_combined <- daily_out_rates_sums(long_out, start_date, c("n_SV"))

      plotly::ggplotly(
        ggplot2::ggplot(df_combined,
                        aes(x = date, y = d_sum, group = rep)) +
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
      plotly::ggplotly(
        ggplot2::ggplot(filtered_data, # %>%
                          # dplyr::mutate(disease_state = factor(disease_state,
                          #                                      levels = c("S", "E", "H", "D",
                          #                                                 "I_presymp", "I_asymp",
                          #                                                 "I_symp", "R", "V"))) %>%
                          # dplyr::group_by(date, disease_state, rep) %>%
                          # dplyr::summarize(total_value = value, .groups = "drop"),
                        aes(x = date, y = total_value, color = disease_state, group = rep)) +
          ggplot2::geom_line(linewidth = 1, alpha = 1) +
          ggplot2::scale_color_manual(values = compartment_colors) +
          ggplot2::labs(
            title = selected_zone(),
            x = "Date",
            y = "# of people",
            color = "Compartment",
          ) +
          scale_x_date(
            date_breaks = "1 week",  # Breaks every week
            date_labels = "%b %d"   # Format labels as "Month Day" (e.g., Jan 01)
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
        dplyr::group_by(time, disease_state, rep, !!sym(input$Category)) %>%
        dplyr::summarize(total_value = sum(value), .groups = "drop") %>%
        dplyr::mutate(date = start_date + time)

      # plotly::ggplotly(
        ggplot2::ggplot(cat_long_out,
                        aes(x = date, y = total_value, color = disease_state)) +
          ggplot2::facet_wrap(vars(!!sym(input$Category)), scales = "free_y") +
          ggplot2::geom_line(linewidth = 1, alpha = 0.5) +
          ggplot2::scale_color_manual(values = compartment_colors) +
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
            # legend.spacing.y = unit(2, "cm"),
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
        dplyr::group_by(time, disease_state, rep, !!sym(input$Category)) %>%
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
            legend.title.align = 0.5,                  # Align the legend title in the center
            legend.box.margin = margin(10, 10, 10, 10), # Add margin around the legend box
            legend.key.size = unit(3, "lines"),      # Increase size of legend keys (symbols)
            legend.text = element_text(size = 15),     # Adjust legend text size
            legend.title = element_text(size = 20, margin = margin(b = 10)),     # Adjust legend title size
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
    })

    sub_out <- reactive({
      subset_simout(long_out, start_date, pop_map_df,
                             input$ages, input$races, input$hcezs, input$disease_states)
    })

    # Display the filtered results
    output$simulationOutput <- DT::renderDT({
      sub_out()
    })

    # Download filtered results
    output$download <- downloadHandler(
      filename = function() {
        paste("out_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(sub_out(), file, row.names = FALSE)
      }
    )


  })

}

