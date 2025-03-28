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

      yaml_data <- parse_config(input$config$datapath),

      filename = function() {
        # paste("out_", Sys.Date(), ".zip", sep = "")
        "out.zip"
      },
      content = function(file) {

        tmp <- sample(10000, 1)

        sim_input <- paste("in_", tmp, ".yaml")
        sim_output <- paste("out_", tmp, ".csv", sep = "")

        write_yaml(yaml_data, sim_input)
        write.csv(sub_out(), sim_output, row.names = FALSE)

        zip(file, files = c(sim_input, sim_output))
      },
      contentType = "application/zip"
    )


  })

}

