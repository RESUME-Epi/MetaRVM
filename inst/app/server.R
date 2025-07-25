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
library(shinyjs)
# future::plan(multisession)

server <- function(input, output, session) {

  observeEvent(input$reset, {
    session$reload()  # Forces a full session reload
  })

  # Function to create a formatted title with the confidence interval
  format_ci_title <- function(base_title = "") {
    paste0(base_title, " (", input$conf_level, "% confidence interval)")
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
    model_config <- parse_config(input$config$datapath)


    ## Check the consistency in the model inputs

    # Check the dimensions (number of subpopulations)
    # of each input file

    # npop <- model_config$N_pop # assume this is true
    #
    # npop_vac <- ncol(model_config$vac_mat) - 1
    # npop_m_wd_d <- nrow(model_config$m_wd_d)
    # npop_m_wd_n <- nrow(model_config$m_wd_n)
    # npop_m_we_d <- nrow(model_config$m_we_d)
    # npop_m_we_n <- nrow(model_config$m_we_n)
    #
    # # Check if S0 + I0 + V0 + R0 = N
    # chk_N <- sum(model_config$P_ini - (model_config$S_ini +
    #                                    model_config$I_symp_ini +
    #                                    model_config$R_ini +
    #                                    model_config$V_ini))
    #
    # # Check if mixing matrices rowsums are 1
    # chk_m_wd_d <- abs(rowSums(model_config$m_wd_d) - 1) > 1e-10
    # chk_m_wd_n <- abs(rowSums(model_config$m_wd_n) - 1) > 1e-10
    # chk_m_we_d <- abs(rowSums(model_config$m_we_d) - 1) > 1e-10
    # chk_m_we_n <- abs(rowSums(model_config$m_we_n) - 1) > 1e-10

    # if(npop != npop_vac){
    #   showModal(modalDialog(
    #     title = "Error",
    #     paste0("Vaccination data does not have ", npop, " subpopulations"),
    #     easyClose = TRUE,
    #     footer = modalButton("Close")
    #   ))
    #   } else if(npop != npop_m_wd_d){
    #   showModal(modalDialog(
    #     title = "Error",
    #     paste0("Weekday daytime mixing matrix does not have ", npop, " rows"),
    #     easyClose = TRUE,
    #     footer = modalButton("Close")
    #   ))
    #   } else if(npop != npop_m_wd_n){
    #   showModal(modalDialog(
    #     title = "Error",
    #     paste0("Weekday nighttime mixing matrix does not have ", npop, " rows"),
    #     easyClose = TRUE,
    #     footer = modalButton("Close")
    #   ))
    #   } else if(npop != npop_m_we_d){
    #   showModal(modalDialog(
    #     title = "Error",
    #     paste0("Weekend daytime mixing matrix does not have ", npop, " rows"),
    #     easyClose = TRUE,
    #     footer = modalButton("Close")
    #   ))
    #   } else if(npop != npop_m_we_n){
    #   showModal(modalDialog(
    #     title = "Error",
    #     paste0("Weekend nighttime mixing matrix does not have ", npop, " rows"),
    #     easyClose = TRUE,
    #     footer = modalButton("Close")
    #   ))
    #   } else if(abs(chk_N) > 0){
    #   showModal(modalDialog(
    #     title = "Error",
    #     paste0("S0 + I0 + V0 + R0 != N"),
    #     easyClose = TRUE,
    #     footer = modalButton("Close")
    #   ))
    #   } else if(chk_m_wd_d){
    #   showModal(modalDialog(
    #     title = "Error",
    #     "Weekday daytime mixing matrix does not have row sums equal to 1.",
    #     easyClose = TRUE,
    #     footer = modalButton("Close")
    #   ))
    #   } else if(chk_m_wd_n){
    #   showModal(modalDialog(
    #     title = "Error",
    #     "Weekday nighttime mixing matrix does not have row sums equal to 1.",
    #     easyClose = TRUE,
    #     footer = modalButton("Close")
    #   ))
    #   } else if(chk_m_we_d){
    #   showModal(modalDialog(
    #     title = "Error",
    #     "Weekend daytime mixing matrix does not have row sums equal to 1.",
    #     easyClose = TRUE,
    #     footer = modalButton("Close")
    #   ))
    #   } else if(chk_m_we_n){
    #     showModal(modalDialog(
    #       title = "Error",
    #       "Weekend nighttime mixing matrix does not have row sums equal to 1.",
    #       easyClose = TRUE,
    #       footer = modalButton("Close")
    #     ))
    #   } else {
    #     cat(as.yaml(yaml_data))
    # }


    if(any(abs(rowSums(model_config$m_wd_d) - 1) > 1e-10) |
       any(abs(rowSums(model_config$m_wd_n) - 1) > 1e-10) |
       any(abs(rowSums(model_config$m_we_d) - 1) > 1e-10) |
       any(abs(rowSums(model_config$m_we_n) - 1) > 1e-10)){
      # Show the error modal dialog
      showModal(modalDialog(
        title = "Error",
        "One or more mixing matrices have row sums not equal to 1.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    } else {
      # Print the YAML content
      cat(as.yaml(yaml_data))
    }
  })

  # permanently load shapefile
  read_hcz_geo <- ExtendedTask$new(function() {
    future_promise({
      read.csv(system.file("extdata", "Healthy_Chicago_Equity_Zones_20231129.csv",
                           package = "MetaRVM"))
    })
  })
  hcz_geo <- read.csv(system.file("extdata", "Healthy_Chicago_Equity_Zones_20231129.csv", package = "MetaRVM"))

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


  output$tab <- renderText({
    input$navbar
  })

  ## ---------------------------------------------------------------------------
  ## ---------------------------------------------------------------------------
  # Run SEIR meta-population simulation when the button is clicked
  observeEvent(input$simulate, {

    model_config <- parse_config(input$config$datapath)
    start_date <- model_config$start_date
    pop_map_df <- model_config$pop_map

    long_out <- metaRVM(input$config$datapath)
    long_out_daily <- daily_output(long_out, start_date)

    # Display the output data table in the UI
    output$out_table <- DT::renderDT({
      long_out_daily
    })

    ## ===============================================
    # summarized SEIR plot
    output$seir_plot <- plotly::renderPlotly({
      plot_metaRVM(long_out_daily,
                   conf_level = as.numeric(input$conf_level),
                   value_column = "total_value")
    })


    ## =========================================================================
    ## rate and count plots
    # new infection
    output$new_infection_prop <- plotly::renderPlotly({

      columns_to_plot <- c("n_SE", "n_VE")
      color <- "orangered2"

      df_combined <- daily_out_rates_sums(long_out, start_date, columns_to_plot)
      df_combined$disease_state <- "X"

      plot_compartment(df_combined,
                       conf_level = as.numeric(input$conf_level),
                       value_column = "d_rate",
                       color = color,
                       plot_type = "line")
    })

    output$new_infection_count <- plotly::renderPlotly({

      columns_to_plot <- c("n_SE", "n_VE")
      color <- "orangered2"

      df_combined <- daily_out_rates_sums(long_out, start_date, columns_to_plot)
      df_combined$disease_state <- "X"

      plot_compartment(df_combined,
                       conf_level = as.numeric(input$conf_level),
                       value_column = "d_rate",
                       color = color,
                       plot_type = "bar")


    })

    ## =========================================================================
    # new hospitalizations
    output$new_hosp_prop <- plotly::renderPlotly({

      columns_to_plot <- c("n_IsympH")
      color <- "mediumpurple"

      df_combined <- daily_out_rates_sums(long_out, start_date, columns_to_plot)
      df_combined$disease_state <- "X"

      plot_compartment(df_combined,
                       conf_level = as.numeric(input$conf_level),
                       value_column = "d_rate",
                       color = color,
                       plot_type = "line")

    })

    output$new_hosp_count <- plotly::renderPlotly({

      columns_to_plot <- c("n_IsympH")
      color <- "mediumpurple"

      df_combined <- daily_out_rates_sums(long_out, start_date, columns_to_plot)
      df_combined$disease_state <- "X"

      plot_compartment(df_combined,
                       conf_level = as.numeric(input$conf_level),
                       value_column = "d_rate",
                       color = color,
                       plot_type = "bar")
    })

    ## =========================================================================
    # new deaths
    output$new_death_prop <- plotly::renderPlotly({

      columns_to_plot <- c("n_HD")
      color <- "black"

      df_combined <- daily_out_rates_sums(long_out, start_date, columns_to_plot)
      df_combined$disease_state <- "X"

      plot_compartment(df_combined,
                       conf_level = as.numeric(input$conf_level),
                       value_column = "d_rate",
                       color = color,
                       plot_type = "line")
    })

    output$new_death_count <- plotly::renderPlotly({

      columns_to_plot <- c("n_HD")
      color <- "black"

      df_combined <- daily_out_rates_sums(long_out, start_date, columns_to_plot)
      df_combined$disease_state <- "X"

      plot_compartment(df_combined,
                       conf_level = as.numeric(input$conf_level),
                       value_column = "d_rate",
                       color = color,
                       plot_type = "bar")
    })

    ## =========================================================================
    # new vaccinations
    output$new_vac_prop <- plotly::renderPlotly({

      columns_to_plot <- c("n_SV")
      color <- "darkgreen"

      df_combined <- daily_out_rates_sums(long_out, start_date, columns_to_plot)
      df_combined$disease_state <- "X"

      plot_compartment(df_combined,
                       conf_level = as.numeric(input$conf_level),
                       value_column = "d_rate",
                       color = color,
                       plot_type = "line")
    })

    output$new_vac_count <- plotly::renderPlotly({

      columns_to_plot <- c("n_SV")
      color <- "darkgreen"

      df_combined <- daily_out_rates_sums(long_out, start_date, columns_to_plot)
      df_combined$disease_state <- "X"

      plot_compartment(df_combined,
                       conf_level = as.numeric(input$conf_level),
                       value_column = "d_rate",
                       color = color,
                       plot_type = "bar")
    })


    ## -------------------------------------------------------------------------
    ## -------------------------------------------------------------------------
    ## GEO plot


    # Render Leaflet map with borough boundaries
    output$map <- leaflet::renderLeaflet({
        # if(input$navbar == "HCEZ Figures"){                     # capture tab input

          # Read HCZ geometry
          # hcz_geo <- read.csv(system.file("extdata", "Healthy_Chicago_Equity_Zones_20231129.csv", package = "MetaRVM"))
          # hcz_geo <- read_hcz_geo$result()
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
    output$zone_simout <- plotly::renderPlotly({
      req(selected_zone())  # Make sure a zone is selected

      # Map borough name to sub_population_id
      zone_id <- which(hcz_names == selected_zone())

      long_out_zones <- cat_summary(long_out, pop_map_df, start_date,
                                     cat = "hcez")
      filtered_data <- long_out_zones %>%
        dplyr::filter(hcez %in% selected_zone())

      plot_metaRVM(filtered_data,
                   conf_level = as.numeric(input$conf_level),
                   value_column = "total_value")

    })

    ## -------------------------------------------------------------------------
    ## -------------------------------------------------------------------------
    ## caterogy-wise plot

    output$cat_simout <- renderCachedPlot({

      plot_metaRVM_by_cateory(long_out,
                              conf_level = as.numeric(input$conf_level),
                              model_config$pop_map,
                              model_config$start_date,
                              input$Category)
    }, cacheKeyExpr = { input$Category })

    output$stacked_simout <- renderCachedPlot({

      plot_metaRVM_by_cateory_by_compartment(long_out,
                                             conf_level = as.numeric(input$conf_level),
                                             model_config$pop_map,
                                             model_config$start_date,
                                             input$Category)
    }, cacheKeyExpr = { input$Category })

    ## -------------------------------------------------------------------------
    ## -------------------------------------------------------------------------
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
                                                                      "Infectious",
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

      filename = function() {
        # paste("out_", Sys.Date(), ".zip", sep = "")
        "out.zip"
      },
      content = function(file) {

        yaml_data <- parse_config(input$config$datapath)
        yaml_config <- yaml::read_yaml(input$config$datapath)
        run_id <- yaml_config$run_id

        tmp <- sample(10000, 1)

        sim_input <- paste("in_", run_id, ".RDS")
        sim_output <- paste("out_", run_id, ".csv", sep = "")

        saveRDS(yaml_data, file = sim_input)
        write.csv(sub_out(), sim_output, row.names = FALSE)

        zip(file, files = c(sim_input, sim_output))
      },
      contentType = "application/zip"
    )


  })

}

