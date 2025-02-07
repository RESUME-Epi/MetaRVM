library(shiny)
library(shinydashboard)
# library(DiagrammeR)
# library(plotly)
# library(DT)
# library(leaflet)
library(shinythemes)
library(bslib)
library(yaml)

ui <- tagList(

  # Add custom CSS for the title background
  tags$head(
    tags$style(HTML("
      .title-background {
        background-color: #6d5f57;
        color: white; /* White text */
        padding: 10px; /* Padding around the text */
        text-align: center; /* Center the text */
      }
    "))
  ),

  # Title above navbar
  div(style = "text-align:center; padding: 20px;",
      class = "title-background",
      h2("Generic Meta-population Model Simulation for Respiratory Viruses")
  ),


  # Set the maximum width of the app using custom CSS
  tags$head(
    tags$style(HTML("
      .app-container {
        max-width: 1200px;
        margin: 20px;
      }
      .spacer {
        margin-top: 20px; /* Adjust this value to increase or decrease the space */
      }
    "))
  ),



  # Add custom CSS for styling the card
  tags$head(
    tags$style(HTML("
      .custom-card {
        padding: 10px;
        box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);  /* Slight shadow */
        border-radius: 5px;                          /* Rounded corners */
        border: 1px solid #ddd;                      /* Light border */
      }

      .custom-card-header {
        background-color: #c7deee;
        color: #09446c;                                /* White font */
        text-align: center;                          /* Center the header text */
        padding: 5px;                               /* Padding for header */
        font-size: 20px;                             /* Font size */
        border-bottom: 1px solid #ddd;               /* Border below the header */
        border-top-left-radius: 5px;                 /* Rounded corners for the top */
        border-top-right-radius: 5px;
      }

      .custom-card-body {
        padding: 20px;                               /* Padding for card body */
        background-color: #ffffff;
      }
    "))
  ),

  # Add custom CSS for box around figures
  tags$style(HTML("
        .custom-box {
          border: 1px solid #3498db;    /* Custom border color */
          border-radius: 10px;          /* Rounded corners */
        }

        .custom-box .box-title {
          background-color: #2ecc71;    /* Custom title background */
          color: white;                 /* Title text color */
          font-weight: bold;            /* Make title text bold */
          text-align: center;           /* Center the title text */
        }

        .custom-box .box-header {
          background-color: #2ecc71 !important;  /* Custom header background */
        }

        .custom-box .box-body {
          padding: 20px;               /* Custom padding for the plot */
        }
      ")),

  div(class = "app-container",
      # page_navbar(id = "navbar",
      navbarPage(id = "navbar",

      # Apply a theme
      # theme = shinytheme("readable"),
      # theme = bs_theme(version = 5, bootswatch = "spacelab"),

      # # for math
      # withMathJax(),
      #

      # Add custom CSS for styling the button
      tags$head(
        tags$style(HTML("
      .custom-button {
        background-color: #4ac1f4; /* background */
        border: none; /* Remove border */
        color: white; /* White text */
        padding: 15px 32px; /* Padding */
        text-align: center; /* Center text */
        text-decoration: none; /* No underline */
        display: inline-block; /* Inline-block */
        font-size: 16px; /* Font size */
        margin: 4px 2px; /* Margin */
        cursor: pointer; /* Pointer cursor on hover */
        border-radius: 8px; /* Rounded corners */
        transition: background-color 0.3s; /* Transition effect */
      }

      .custom-button:hover {
        background-color: #45a049; /* Darker green on hover */
      }
    "))
      ),

      #
      #
      tabPanel("Model Description",
          fluidRow(
            column(12,
                   # h3("Model Description"),
                   p("This is a description of the SEIRV meta-population model. The model consists of multiple compartments:"),
                   tags$ul(
                           tags$li(strong("S:"), "Susceptible - individuals who are not yet infected but may become infected."),
                           tags$li(strong("E:"), "Exposed - individuals who are infected but not yet infectious."),
                           tags$li(strong("Ip:"), "Infectious Presymptomatic - individuals who are infectious but have no symptoms."),
                           tags$li(strong("Ia:"), "Infectious Asymptomatic - individuals who are infectious but will not show symptoms."),
                           tags$li(strong("Is:"), "Infectious Symptomatic - individuals who are infectious and show symptoms."),
                           tags$li(strong("H:"), "Hospitalized - individuals with severe symptoms."),
                           tags$li(strong("R:"), "Recovered - individuals who have recovered and are no longer infectious."),
                           tags$li(strong("D:"), "Dead - individuals who have died from the infection."),
                           tags$li(strong("V:"), "Vaccinated - individuals who have received vaccine.")
                         ),
                   br(),
                   br(),
                   br(),
                   # DiagrammeR::grVizOutput("compartment_plot"),
                   img(src = "metaRVM.png", align = "center", height="90%", width="90%"),
                   br(),
                   br(),
                   br(),
                   h3("Instructions"),
                   p("The model depends on a set of disease parameters and population information to initialize the simulation. The
                      parameters are supplied via a configuration yaml file.
                     Majority of the parameters are scalar valued parameter which can be directly specified in the appropriate
                     fields. Others need to supplied via CSV or text file. Below is a brief description of the list of inputs,
                     their structure and acceptable ranges."),
                   img(src = "example_config.png", align = "center", height="90%", width="90%"),
                    tags$ul(
                      tags$li(strong("File inputs:"),
                              tags$ul(
                                tags$li(strong("initialization:"), tags$br(),
                                        HTML("&nbsp;&nbsp;&nbsp;&nbsp;"), "A CSV file with header consisting of five columns where the number of rows is the same
                                 as the number of subpopulations, The columns are N: total population,
                               S0: number of susceptible at the start of the simulation, I0: number of
                                 infected people at the start of the simulation, V0: number of vaccinated
                                 people at the start of the simulation, R0: number of recovered
                                 people (from an earlier exposure) at the start of the simulation"),
                                tags$li(strong("accination:"), tags$br(),
                                        HTML("&nbsp;&nbsp;&nbsp;&nbsp;"), "A CSV file with header consisting of the number of (subpopulations + 1) many columns.
                                 The first column contains calendar dates and the remaining
                               columns hold the number of vaccinations in each of the subpopulations."),
                                tags$li(strong("Mixing matrix:"), tags$br(),
                                        HTML("&nbsp;&nbsp;&nbsp;&nbsp;"), "A CSV file of mixing matrix. There are four sets of
                                 mixing matrices required for the simulation, accounting for different mixing patterns in
                                 weekdays vs weekends and in day time vs night time. The rows of a mixing matrix should add up to 1."),
                                tags$li(strong("mapping:"), tags$br(),
                                        HTML("&nbsp;&nbsp;&nbsp;&nbsp;"), "A CSV file with header consisting of the sub-population demographic
                                        mapping. "),
                                )
                              )
                    ),
                   tags$ul(
                     tags$li(strong("Disease parameters"),
                             tags$ul(
                               tags$li(strong("Transmissibility for susceptibles (ts):"), "a value between 0 and 1"),
                               tags$li(strong("Transmissibility for vaccinated (tv):"), "a value between 0 and 1"),
                               tags$li(strong("Vaccinated to Susceptible (dv):"), "Mean number of days to transition from Vaccinated to Susceptible, a value between 0 and 1"),
                               tags$li(strong("Exposed to Infectious Asymptomatic or Presymptomatic (de):"), "Mean number of days to transition from Exposed to Infectious presymptomatic or asymptomatic, a value between 0 and 1"),
                               tags$li(strong("Infectious Presymptomatic to symptomatic (dp):"), "Mean number of days to transition from Infectious presymptomatic to Infectious symptomatic, a value between 0 and 1"),
                               tags$li(strong("Infectious Asymptomatic to Recovered (da):"), "Mean number of days to transition from Infectious presymptomatic to Recovered, a value between 0 and 1"),
                               tags$li(strong("Infectious Symptomatic to Recovered or Hospitalized (ds):"), "Mean number of days to transition from Infectious symptomatic to Recovered or Hospitalized, a value between 0 and 1"),
                               tags$li(strong("Hospitalized to Recovered or Dead (dh):"), "Mean number of days to transition from Hospitalized to Recovered or Dead, a value between 0 and 1"),
                               tags$li(strong("Recovered to Susceptible (dr):"), "Mean number of days to transition from Recovered to Susceptible, a value between 0 and 1"),
                               tags$li(strong("Proportion of presymptomatic among asymptomatic + presymptomatic (pep):"), "proportion of people becoming Infectious presymptomatic from Exposed, a value between 0 and 1. "),
                               tags$li(strong("Proportion of recovered among recovered+hospitalized from symptomatic (psr):"), "Proportion of people recovered from Infectious symptomatic, a value between 0 and 1. "),
                               tags$li(strong("Proportion of recovered among recovered+dead from hospitalized (phr):"), "Proportion of people Recovered from Hospitalized, a value between 0 and 1. "),
                             ))
                   ),
                   tags$ul(
                     tags$li(strong("Settings"),
                       tags$ul(
                         tags$li(strong("Start date:"), "a calendar date, must be a Monday"),
                         # tags$li(strong("Model time step:"), "positive real number, simulation time step"),
                         tags$li(strong("Simulation length:"), "days to simulate, a positive integer" ),
                         # tags$li(strong("reps:"), "number of replicates to run, a positive integer" ),
                       ),
                     )
                   )
            )
          )
      ),
      tabPanel("Simulation Control",
          fluidRow(
            column(4,
                   card(
                     div(class = "custom-card",
                         fileInput("config", "Simulation Configuration",
                                   accept = c(".yaml", ".yml"))),
                   actionButton("simulate", "Simulate", class = "custom-button"))),
            br(),
            br(),
            column(12,
                   card(
                     div(class = "custom-card-body",
                         verbatimTextOutput("yaml_content")))),
            # column(4, wellPanel(
            #             h4("File inputs"),
            #             fileInput("population_data", "Population and initialization data",
            #                       accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            #             fileInput("vac_data", "Vaccination data",
            #                       accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            #             fileInput("mix_mat1", "Weekday Day-time mixing matrix"),
            #             fileInput("mix_mat2", "Weekday Night-time mixing matrix"),
            #             fileInput("mix_mat3", "Weekend Day-time mixing matrix"),
            #             fileInput("mix_mat4", "Weekend Night-time mixing matrix"),
            #             fileInput("population_map", "Demographic Mapping",
            #                       accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            #             actionButton("simulate", "Simulate", class = "custom-button")
            # )),

            # disease parameter inputs

           # column(4, wellPanel(
           #                h4("Disease Parameters"),
           #                h5("Rates"),
           #                numericInput("beta_i", "Transmissibility for susceptibles", value = 0.5, min = 0, step = 0.001),
           #                numericInput("beta_v", "Transmissibility for vaccinated", value = 0.1, min = 0, step = 0.001),
           #                numericInput("vac_eff", "Vaccination efficacy", value = 0.5, min = 0, step = 0.01),
           #                hr(),
           #                h5("Mean duration (in days)"),
           #                numericInput("VtoS", "Vaccinated to Susceptible", value = 50, min = 1, step = 1),
           #                numericInput("EtoIpresymp", "Exposed to Infectious asymptomatic or presymptomatic", value = 2, min = 1, step = 1),
           #                numericInput("pretoIsymp", "Infectious presymptomatic to symptomatic", value = 10, min = 1, step = 1),
           #                numericInput("IasymptoR", "Infectious asymptomatic to Recovered", value = 10, min = 1, step = 1),
           #                numericInput("IsymptoRH", "Infectious symptomatic to Recovered or Hospitalized", value = 2, min = 1, step = 1),
           #                numericInput("HtoRD", "Hospitalized to Recovered or Dead", value = 3, min = 1, step = 1),
           #                numericInput("RtoS", "Recovered to Susceptible", value = 50, min = 1, step = 1))
           #      ),
           # column(4, wellPanel(
           #              h4("Proportions"),
           #              numericInput("etopa", "Proportion of asymptomatic among asymptomatic + presymptomatic", value = 0.5, min = 0, step = 0.001),
           #              numericInput("istohr", "Proportion of recovered among recovered+hospitalized from symptomatic", value = 0.5, min = 0, step = 0.001),
           #              numericInput("htor", "Proportion of recovered among recovered+dead from hospitalized", value = 0.7, min = 0, step = 0.001),
           #              br(),
           #              br(),
           #
           #              # numericInput("seed", "Random seed (optional):", value = NA),
           #              # radioButtons("choice", "Model type:",
           #              #              choices = list("Deterministic" = "det", "Stochastic" = "stoch")
           #              ),
           #        h4("Settings"),
           #              dateInput(
           #                inputId = "start_date",        # Unique ID for the input
           #                label = "Start date  (must be a Monday)", # Label for the input
           #                value = "2024-09-30",      # Default value (current date)
           #                min = "2020-01-01",      # Minimum date selectable
           #                max = "2030-12-31",      # Maximum date selectable
           #                format = "yyyy-mm-dd",   # Format for the displayed date
           #                startview = "month",     # Default view (month)
           #                weekstart = 1            # Week starts on Monday
           #              ),
           #              numericInput("dt", "Model time step:", value = 0.5, min = 0, step = .5),
           #              numericInput("days", "Simulation Length:", value = 100, min = 1),
           #              # numericInput("rep", "Number of replicates", value = 1, min = 1, step = 1))
           #        ),
           br(),
            br(),
            br(),
            # column(12,
            #        shinydashboard::box(
            #          title = "Disease Compartments Over Time",
            #          status = "primary",
            #          solidHeader = TRUE,
            #          collapsible = TRUE,
            #          width = 12,  # Full width
            #          class = "custom-box",
            #          plotly::plotlyOutput("seir_plot", height = "500px"))
            #        ),
           column(12,
                  card(
                    div(class = "custom-card",
                        div(class = "custom-card-header", card_header(strong("Disease compartments over time"))),
                        div(class = "custom-card-body", plotly::plotlyOutput("seir_plot", height = "400px"))
                        # div(class = "custom-card-body", plotOutput("seir_plot", height = "500px"))
                    )
                  )
           ),
           br(),
           br(),
           br(),
           br(),
           # column(6,
           #          shinydashboard::box(
           #            title = "",
           #            status = "primary",
           #            solidHeader = TRUE,
           #            collapsible = TRUE,
           #            width = 12,  # Full width
           #            # class = "custom-box",
           #            plotly::plotlyOutput("new_infection_plot", height = "500px"))
           #   ),
           column(12,
                  card(
                    div(class = "custom-card",
                        div(class = "custom-card-header", card_header(strong("New Infections"))),
                        column(6, plotly::plotlyOutput("new_infection_prop", height = "300px")),
                        column(6, plotly::plotlyOutput("new_infection_count", height = "300px"))
                    )
                  )
           ),
             # column(6,
             #        shinydashboard::box(
             #          title = "",
             #          status = "primary",
             #          solidHeader = TRUE,
             #          collapsible = TRUE,
             #          width = 12,  # Full width
             #          # class = "custom-box",
             #          plotly::plotlyOutput("new_hosp_plot", height = "500px"))
             # ),
           column(12,
                  card(
                    div(class = "custom-card",
                        div(class = "custom-card-header", card_header(strong("New Hospitalizations"))),
                        column(6, plotly::plotlyOutput("new_hosp_prop", height = "300px")),
                        column(6, plotly::plotlyOutput("new_hosp_count", height = "300px"))
                    )
                  )
           ),
             # column(6,
             #        shinydashboard::box(
             #          title = "",
             #          status = "primary",
             #          solidHeader = TRUE,
             #          collapsible = TRUE,
             #          width = 12,  # Full width
             #          # class = "custom-box",
             #          plotly::plotlyOutput("new_death_plot", height = "500px"))
             # ),
           column(12,
                  card(
                    div(class = "custom-card",
                        div(class = "custom-card-header", card_header(strong("New Deaths"))),
                        column(6, plotly::plotlyOutput("new_death_prop", height = "300px")),
                        column(6, plotly::plotlyOutput("new_death_count", height = "300px"))
                        # div(class = "custom-card-body", plotly::plotlyOutput("new_death_plot", height = "300px")),
                        # div(class = "custom-card-body", plotly::plotlyOutput("new_death_plot", height = "300px"))
                    )
                  )
           ),
             # column(6,
             #        shinydashboard::box(
             #          title = "",
             #          status = "primary",
             #          solidHeader = TRUE,
             #          collapsible = TRUE,
             #          width = 12,  # Full width
             #          # class = "custom-box",
             #          plotly::plotlyOutput("new_vac_plot", height = "500px"))
             # ),
           column(12,
                  card(
                    div(class = "custom-card",
                        div(class = "custom-card-header", card_header(strong("New Vaccinations"))),
                        column(6, plotly::plotlyOutput("new_vac_prop", height = "300px")),
                        column(6, plotly::plotlyOutput("new_vac_count", height = "300px"))
                    )
                  )
           ),
          )

      ),
      tabPanel("HCEZ",
                fluidRow(
                  br(),
                   br(),
                   column(12, leaflet::leafletOutput("map", height = "500px")),     # Choropleth map
                  # column(12, plotOutput("map", height = "500px")),
                   br(),
                   br(),
                   # column(12,
                   #        shinydashboard::box(
                   #          title = "Disease Compartments Over Time",
                   #          status = "primary",
                   #          solidHeader = TRUE,
                   #          collapsible = TRUE,
                   #          width = 12,  # Full width
                   #          class = "custom-box",
                   #          plotly::plotlyOutput("zone_simout", height = "500px"))
                   # ),
                  column(12,
                         card(
                           div(class = "custom-card",
                               div(class = "custom-card-header", card_header("Disease Compartments for a Zone")),
                               div(class = "custom-card-body", plotly::plotlyOutput("zone_simout", height = "500px"))
                           )
                         )
                  ),
                )
      ),

      tabPanel("Demographic Categories",
               fluidRow(
                 br(),
                 br(),
                 column(12, wellPanel(radioButtons("Category", "Choose an option:",
                                                   choices = list("Age" = "age",
                                                                  "Race/ethnicity" = "race",
                                                                  "HCEZ" = "hcez"),
                                                   selected = "age", inline = TRUE)
                                      )
                        ),
                 br(),
                 br(),
                 column(12,
                        card(
                          div(class = "custom-card",
                              div(class = "custom-card-header", card_header("By categories")),
                              div(class = "custom-card-body", plotOutput("cat_simout", height = "500px"))
                          )
                        )
                 ),
                 br(),
                 br(),
                 column(12,
                        card(
                          div(class = "custom-card",
                              div(class = "custom-card-header", card_header("By disease compartments")),
                              div(class = "custom-card-body", plotOutput("stacked_simout", height = "500px"))
                          )
                        )
                 ),
               )
      ),

      # tabPanel("Input Data",
      #           fluidRow(
      #             br(),
      #             br(),
      #             h4("Population data"),
      #             # column(12, DT::dataTableOutput("population_table")),
      #             column(12, DT::dataTableOutput("out_table")),
      #             br(),
      #             br(),
      #             h4("Vaccination data"),
      #             column(12, DT::dataTableOutput("vac_table")),
      #             br(),
      #             br(),
      #             h4("Mixing matrix - weekday day"),
      #             column(12, DT::dataTableOutput("m1")),
      #             br(),
      #             br(),
      #             h4("Mixing matrix - weekday night"),
      #             column(12, DT::dataTableOutput("m2")),
      #             br(),
      #             br(),
      #             h4("Mixing matrix - weekend day"),
      #             column(12, DT::dataTableOutput("m3")),
      #             br(),
      #             br(),
      #             h4("Mixing matrix - weekend night"),
      #             column(12, DT::dataTableOutput("m4")),
      #             h4("Selected tab"),
      #             column(12, textOutput("tab")),
      #           )
      # ),

      tabPanel("Download results",
               fluidRow(
                 br(),
                 br(),
                 column(3, wellPanel(checkboxGroupInput("ages", "Age categories",
                                                         choices = NULL)
                                    ),
                        downloadButton("download", "Download zip", class = "custom-button")
                 ),
                 column(3, wellPanel(checkboxGroupInput("races", "Race categories",
                                                         choices = NULL)
                                    )
                 ),
                 column(3, wellPanel(checkboxGroupInput("hcezs", "HCEZ",
                                                         choices = NULL)
                                    )
                 ),
                 column(3, wellPanel(checkboxGroupInput("disease_states", "Disease States",
                                                        choices = NULL)
                                    )
                 ),
                 br(),
                 br(),
                 column(12, DT::dataTableOutput("simulationOutput"))

               )
      ),

              # tabPanel("Data",
              #          div(style = "max-width: 1200px; margin: 20px;",
              #              fluidRow(
              #                br(),
              #                br(),
              #                # h4("out_table"),
              #                column(12, DT::dataTableOutput("population_table")),
              #                br(),
              #                column(12, tableOutput("out_table")),
              #                br(),
              #                h4("Vaccination"),
              #                column(12, DT::dataTableOutput("vac_table")),
              #                # br(),
              #                # br(),
              #                # h4("Mixing day"),
              #                # column(12, dataTableOutput("mix_mat1")),
              #                # h4("Mixing night"),
              #                # column(12, dataTableOutput("mix_mat2"))
              #              )
              #          )
              # )
      #       )
      #     )
      # )
    )
  )
)
