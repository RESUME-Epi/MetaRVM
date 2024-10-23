library(shiny)
library(MetaRVM)
library(dplyr)
library(ggplot2)
library(odin)
library(dde)
library(leaflet)

server <- function(input, output, session) {

  # Read the CSV file for population data
  population_data <- reactive({
    if(!is.null(input$population_data)){
      data.table::fread(input$population_data$datapath)
    } else {
      data.table::fread(system.file("extdata", "pop_init_zones.csv", package = "MetaRVM"))
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
      data.table::fread(system.file("extdata", "vac_zones.csv", package = "MetaRVM"))
    }
  })

  # Display the population data table in the UI
  output$vac_table <- DT::renderDT({
    vac_data()
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
      read.csv(system.file("extdata", "m_weekday_day.csv", package = "MetaRVM"), header = F)
    }
  })
  read_m2 <- reactive({
    if(!is.null(input$mix_mat2)){
      read.csv(input$mix_mat2$datapath, header = F)
    } else {
      read.csv(system.file("extdata", "m_weekday_night.csv", package = "MetaRVM"), header = F)
    }
  })
  read_m3 <- reactive({
    if(!is.null(input$mix_mat3)){
      read.csv(input$mix_mat3$datapath, header = F)
    } else {
      read.csv(system.file("extdata", "m_weekend_day.csv", package = "MetaRVM"), header = F)
    }
  })
  read_m4 <- reactive({
    if(!is.null(input$mix_mat4)){
      read.csv(input$mix_mat4$datapath, header = F)
    } else {
      read.csv(system.file("extdata", "m_weekend_night.csv", package = "MetaRVM"), header = F)
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
      read.csv(system.file("extdata", "m_weekday_day.csv", package = "MetaRVM"), header = F) ## TODO: change
    }
  })

  ## ---------------------------------------------------------------------------
  ## ---------------------------------------------------------------------------
  # Run SEIR meta-population simulation when the button is clicked
  observeEvent(input$simulate, {
    # Extract population data
    pop_df <- population_data()
    N_pop <- nrow(pop_df)
    vac_df <- vac_data()
    P_ini <- pop_df[, N]
    S_ini <- pop_df[, S0]
    I_symp_ini <- pop_df[, I0]
    V_ini <- pop_df[, V0]


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

    seed <- input$seed
    nrep <- input$rep
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
    is.stoch <- ifelse(input$choice == "stoch", 1, 0)

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
    long_out <- out %>%
      tidyr::pivot_longer(
        cols = -c("step", "time", "rep"),               # Exclude 'time' from being pivoted
        names_to = c("disease_state", "population_id"),  # Create new columns for disease state and subpopulation
        names_pattern = "([A-Za-z_]+)\\.(\\d+)\\.",  # Regex to extract the disease state and subpopulation ID
        values_to = "value"          # Column to store the actual values
      )

    # Display the population data table in the UI
    output$out_table <- renderTable({
      out
    })

    ## ===============================================
    # summarized SEIR plot
    output$seir_plot <- plotly::renderPlotly({

      compartment_colors <- c("S" = "steelblue3",
                              "E" = "tan1",
                              "I_presymp" = "salmon3",
                              "I_asymp" = "orangered2",
                              "I_symp" = "red4",
                              "H" = "mediumpurple",
                              "R" = "green",
                              "D" = "grey",
                              "V" = "darkgreen")

      plotly::ggplotly(
        ggplot2::ggplot(long_out %>%
                          dplyr::filter(disease_state %in% c("S", "E", "H", "D",
                                                             "I_presymp", "I_asymp",
                                                             "I_symp", "R", "V")) %>%
                          dplyr::mutate(disease_state = factor(disease_state,
                                                               levels = c("S", "E", "H", "D",
                                                                          "I_presymp", "I_asymp",
                                                                          "I_symp", "R", "V"))) %>%
                          dplyr::group_by(step, disease_state, rep) %>%
                          dplyr::summarize(total_value = sum(value), .groups = "drop"),
                        aes(x = step, y = total_value, color = disease_state, group = rep)) +
          ggplot2::geom_line(linewidth = 0.5, alpha = 0.5) +
          ggplot2::scale_color_manual(values = compartment_colors) +
          ggplot2::labs(
            # title = "Disease Compartments Over Time",
            x = "Time",
            y = "# of people",
            color = "Compartment",
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = element_text(hjust = 0.5),
            legend.position = "right"
          )
      )
    })


    ## ===============================================
    ## rate plots
    # new infection
    output$new_infection_plot <- plotly::renderPlotly({

      # prepare the data
      df_e <- long_out %>%
        dplyr::filter(disease_state %in% c("n_SE", "n_VE")) %>%
        dplyr::group_by(step, rep) %>%
        dplyr::summarise(E_sum = sum(value), .groups = "drop") %>% ungroup()
      df_pop <- long_out %>%
        dplyr::filter(disease_state == "P") %>%    # Filter for P compartment (total population)
        dplyr::group_by(step, rep) %>%
        dplyr::select(step, rep, P = value) %>%
        dplyr::summarise(P_sum = sum(P), .groups = "drop") %>% ungroup()
      df_combined <- df_e %>%
        dplyr::left_join(df_pop, by = c("step", "rep")) %>%
        dplyr::mutate(E_rate = E_sum / P_sum)

      plotly::ggplotly(
        ggplot2::ggplot(df_combined,
                        aes(x = step, y = E_rate, group = rep)) +
          ggplot2::geom_line(linewidth = 0.5, alpha = 0.5, color = "orangered2") +
          # scale_color_manual(values = compartment_colors) +
          ggplot2::labs(
            # title = "New infection rate",
            x = "Time",
            y = "proportions") +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = element_text(hjust = 0.5)
          )
      )
    })

    # new hospitalizations
    output$new_hosp_plot <- plotly::renderPlotly({


      # prepare the data
      df_h <- long_out %>%
        dplyr::filter(disease_state %in% c("n_IsympH")) %>%
        dplyr::group_by(step, rep) %>%
        dplyr::summarise(H_sum = sum(value), .groups = "drop") %>% ungroup()
      df_pop <- long_out %>%
        dplyr::filter(disease_state == "P") %>%    # Filter for P compartment (total population)
        dplyr::group_by(step, rep) %>%
        dplyr::select(step, rep, P = value) %>%
        dplyr::summarise(P_sum = sum(P), .groups = "drop") %>% ungroup()
      df_combined <- df_h %>%
        dplyr::left_join(df_pop, by = c("step", "rep")) %>%
        dplyr::mutate(H_rate = H_sum / P_sum)

      plotly::ggplotly(
        ggplot2::ggplot(df_combined,
                        aes(x = step, y = H_rate, group = rep)) +
          ggplot2::geom_line(linewidth = 0.5, alpha = 0.5, color = "mediumpurple") +
          # scale_color_manual(values = compartment_colors) +
          ggplot2::labs(
            # title = "New hospitalization rate",
            x = "Time",
            y = "proportions") +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = element_text(hjust = 0.5)
          )
      )
    })

    # new deaths
    output$new_death_plot <- plotly::renderPlotly({


      # prepare the data
      df_d <- long_out %>%
        dplyr::filter(disease_state %in% c("n_HD")) %>%
        dplyr::group_by(step, rep) %>%
        dplyr::summarise(D_sum = sum(value), .groups = "drop") %>% ungroup()
      df_pop <- long_out %>%
        dplyr::filter(disease_state == "P") %>%    # Filter for P compartment (total population)
        dplyr::group_by(step, rep) %>%
        dplyr::select(step, rep, P = value) %>%
        dplyr::summarise(P_sum = sum(P), .groups = "drop") %>% ungroup()
      df_combined <- df_d %>%
        dplyr::left_join(df_pop, by = c("step", "rep")) %>%
        dplyr::mutate(D_rate = D_sum / P_sum)

      plotly::ggplotly(
        ggplot2::ggplot(df_combined,
                        aes(x = step, y = D_rate, group = rep)) +
          ggplot2::geom_line(linewidth = 0.5, alpha = 0.5, color = "grey") +
          # scale_color_manual(values = compartment_colors) +
          ggplot2::labs(
            # title = "New deaths rate",
            x = "Time",
            y = "proportions") +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = element_text(hjust = 0.5)
          )
      )
    })


    # new vaccinations
    output$new_vac_plot <- plotly::renderPlotly({


      # prepare the data
      df_d <- long_out %>%
        dplyr::filter(disease_state %in% c("n_SV")) %>%
        dplyr::group_by(step, rep) %>%
        dplyr::summarise(V_sum = sum(value), .groups = "drop") %>% ungroup()
      df_pop <- long_out %>%
        dplyr::filter(disease_state == "P") %>%    # Filter for P compartment (total population)
        dplyr::group_by(step, rep) %>%
        dplyr::select(step, rep, P = value) %>%
        dplyr::summarise(P_sum = sum(P), .groups = "drop") %>% ungroup()
      df_combined <- df_d %>%
        dplyr::left_join(df_pop, by = c("step", "rep")) %>%
        dplyr::mutate(V_rate = V_sum / P_sum)

      plotly::ggplotly(
        ggplot2::ggplot(df_combined,
                        aes(x = step, y = V_rate, group = rep)) +
          ggplot2::geom_col(alpha = 0.5, color = "darkgreen") +
          # scale_color_manual(values = compartment_colors) +
          ggplot2::labs(
            # title = "New vaccination",
            x = "Time",
            y = "proportions") +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = element_text(hjust = 0.5)
          )
      )
    })


    ## -------------------------------------------------------------------------
    ## -------------------------------------------------------------------------
    ## GEO plot


    # Render Leaflet map with borough boundaries
      output$map <- leaflet::renderLeaflet({
        if(input$navbar == "HCEZ Figures"){                     # capture tab input

          # Read HCZ geometry
          hcz_geo <- read.csv(system.file("extdata", "Healthy_Chicago_Equity_Zones_20231129.csv", package = "MetaRVM"))
          hcz_sf <<- sf::st_as_sf(hcz_geo, wkt = "Geometry", crs = 4326)
          hcz_names <<- hcz_geo$Equity.Zone

          # Create a color palette for the boroughs
          palette <- leaflet::colorFactor(palette = "Set1", domain = hcz_names)  # Set1 palette from RColorBrewer

          # plot(mtcars$mpg, mtcars$cyl)

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

        }
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
    output$zone_simout <- plotly::renderPlotly({
      req(selected_zone())  # Make sure a zone is selected

      # Map borough name to sub_population_id
      zone_id <- which(hcz_names == selected_zone())

      # Filter simulation data for the selected zone
      filtered_data <- long_out %>%
        dplyr::filter(population_id == zone_id, disease_state %in% c("S", "E", "H", "D",
                                                                     "I_presymp", "I_asymp",
                                                                     "I_symp", "R", "V"))

      # Plot the simulation output for the selected zone

      compartment_colors <- c("S" = "steelblue3",
                              "E" = "tan1",
                              "I_presymp" = "salmon3",
                              "I_asymp" = "orangered2",
                              "I_symp" = "red4",
                              "H" = "mediumpurple",
                              "R" = "green",
                              "D" = "grey")
      plotly::ggplotly(
        ggplot2::ggplot(filtered_data %>%
                          dplyr::mutate(disease_state = factor(disease_state,
                                                               levels = c("S", "E", "H", "D",
                                                                          "I_presymp", "I_asymp",
                                                                          "I_symp", "R", "V"))) %>%
                          dplyr::group_by(step, disease_state, rep) %>%
                          dplyr::summarize(total_value = value, .groups = "drop"),
                        aes(x = step, y = total_value, color = disease_state, group = rep)) +
          ggplot2::geom_line(linewidth = 0.5, alpha = 0.5) +
          ggplot2::scale_color_manual(values = compartment_colors) +
          ggplot2::labs(
            title = selected_zone(),
            x = "Time",
            y = "# of people",
            color = "Compartment",
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = element_text(hjust = 0.5),
            legend.position = "right"
          )
      )
    })

    ## -------------------------------------------------------------------------
    ## -------------------------------------------------------------------------
    ## caterogy-wise plot

    output$cat_simout <- plotly::renderPlotly({

      compartment_colors <- c("S" = "steelblue3",
                              "E" = "tan1",
                              "I_presymp" = "salmon3",
                              "I_asymp" = "orangered2",
                              "I_symp" = "red4",
                              "H" = "mediumpurple",
                              "R" = "green",
                              "D" = "grey",
                              "V" = "darkgreen")


      # merge long output with population map
      long_out <- merge(long_out, pop_map_df, by = "population_id")


      plotly::ggplotly(
        ggplot2::ggplot(long_out %>%
                          dplyr::filter(disease_state %in% c("S", "E", "H", "D",
                                                             "I_presymp", "I_asymp",
                                                             "I_symp", "R", "V")) %>%
                          dplyr::mutate(disease_state = factor(disease_state,
                                                               levels = c("S", "E", "H", "D",
                                                                          "I_presymp", "I_asymp",
                                                                          "I_symp", "R", "V"))) %>%
                          dplyr::group_by(step, disease_state, rep, !!sym(input$Category)) %>%
                          dplyr::summarize(total_value = sum(value), .groups = "drop"),
                        aes(x = step, y = total_value, color = disease_state)) +
          ggplot2::facet_wrap(vars(!!sym(input$Category))) +
          ggplot2::geom_line(linewidth = 0.5, alpha = 0.5) +
          ggplot2::scale_color_manual(values = compartment_colors) +
          ggplot2::labs(
            # title = "Disease Compartments Over Time",
            x = "Time",
            y = "# of people",
            color = "Compartment",
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = element_text(hjust = 0.5),
            legend.position = "right"
          )
      )
    })

    output$stacked_simout <- plotly::renderPlotly({

      # merge long output with population map
      long_out <- merge(long_out, pop_map_df, by = "population_id")


      plotly::ggplotly(
        ggplot2::ggplot(long_out %>%
                          dplyr::filter(disease_state %in% c("S", "E", "H", "D",
                                                             "I_presymp", "I_asymp",
                                                             "I_symp", "R", "V")) %>%
                          dplyr::mutate(disease_state = factor(disease_state,
                                                               levels = c("S", "E", "H", "D",
                                                                          "I_presymp", "I_asymp",
                                                                          "I_symp", "R", "V")),
                                        age = factor(age, levels = c("0-4", "5-19", "20-49", "50-79", "80-"))) %>%
                          dplyr::group_by(step, disease_state, rep, !!sym(input$Category)) %>%
                          dplyr::summarize(total_value = sum(value), .groups = "drop"),
                        aes(x = step, y = total_value, fill = !!sym(input$Category))) +
          ggplot2::facet_wrap(~ disease_state, scales = "free_y") +
          ggplot2::geom_bar(position="stack", stat="identity") +
          viridis::scale_fill_viridis(discrete = T) +
          ggplot2::labs(
            # title = "Disease Compartments Over Time",
            x = "Time",
            y = "# of people",
            color = "Compartment",
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = element_text(hjust = 0.5),
            legend.position = "right"
          )
      )
    })



  })

}
