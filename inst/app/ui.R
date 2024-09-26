library(shiny)
# library(shinydashboard)
# library(DiagrammeR)
# library(plotly)
# library(DT)
# library(leaflet)

ui <- fluidPage(

  # for math
  withMathJax(),

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


  # Add custom CSS for box around figures
  tags$style(HTML("
    .custom-box {
      border: 2px solid #3498db;    /* Custom border color */
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
      titlePanel("Generic Meta-population Model Simulation for Respiratory Viruses"),

      br(),
      br(),
      h3("Model Description"),
      br(),
      p("This is a description of the SEIRV meta-population model. The model consists of multiple compartments:"),
      tags$ul(
        tags$li("S: Susceptible - individuals who are not yet infected but may become infected."),
        tags$li("E: Exposed - individuals who are infected but not yet infectious."),
        tags$li("Ip: Infectious Presymptomatic - individuals who are infectious but have no symptoms."),
        tags$li("Ia: Infectious Asymptomatic - individuals who are infectious but will not show symptoms."),
        tags$li("Is: Infectious Symptomatic - individuals who are infectious and show symptoms."),
        tags$li("H: Hospitalized - individuals with severe symptoms."),
        tags$li("R: Recovered - individuals who have recovered and are no longer infectious."),
        tags$li("D: Dead - individuals who have died from the infection."),
        tags$li("V: Vaccinated - individuals who have received vaccine.")
      ),

      br(),
      br(),
      h3("Graphical Description of the compartmental model"),
      br(),
      grVizOutput("compartment_plot"),

      br(),
      br(),

      h3("Instruction for running the model"),
      p("The model depends on a set of disease parameters and population information to initialize the simulation. Majority
      of the parameters are scalar valued parameter which can be directly entered into the appropriate input boxes. Others
      need to supplied via CSV or text file. Below is a brief description of the list of inputs, their structure and acceptable
      ranges."),
      tags$ul(
        tags$li(strong("Population and initialization data:"), tags$br(),
                HTML("&nbsp;&nbsp;&nbsp;&nbsp;"), "A CSV file with four columns where the number of rows is the same as the number of subpopulations, The columns are N: total population,
              S0: number of susceptible at the start of the simulation, I0: number of infected people at the start of the simulation, V0: number of vaccinated people at the start of the simulation"),
        tags$li(strong("Vaccination data:"), tags$br(),
                HTML("&nbsp;&nbsp;&nbsp;&nbsp;"), "A CSV file with number of (subpopulations + 1) many columns. The first column indicates the time indices and the remaining
              columns hold the number of vaccinations in each of the subpopulations."),
        tags$li(strong("Mixing matrix:"), tags$br(),
                HTML("&nbsp;&nbsp;&nbsp;&nbsp;"), "A CSV file of mixing matrix"),
        tags$li(strong("dt:"), "positive real number, simulation time step"),
        tags$li(strong("beta_e:"), "transmission rate for Susceptible to Exposed, a value between 0 and 1"),
        tags$li(strong("beta_v:"), "transmission rate for Vaccinated to Exposed, a value between 0 and 1"),
        tags$li(strong("VtoS:"), "rate for transitioning from Vaccinated to Susceptible, a value between 0 and 1"),
        tags$li(strong("EtoIpresymp:"), "rate for transitioning from Exposed to Infectious presymptomatic or asymptomatic, a value between 0 and 1"),
        tags$li(strong("etopa:"), "proportion of people becoming Infectious presymptomatic from Exposed, a value between 0 and 1. "),
        tags$li(strong("pretoIsymp:"), "rate for transitioning from Infectious presymptomatic to Infectious symptomatic, a value between 0 and 1"),
        tags$li(strong("IasymptoR:"), "rate for transitioning from Infectious presymptomatic to Recovered, a value between 0 and 1"),
        tags$li(strong("IsymptoRH:"), "rate for transitioning from Infectious symptomatic to Recovered or Hospitalized, a value between 0 and 1"),
        tags$li(strong("istohr:"), "proportion of people hospitalized from Infectious symptomatic, a value between 0 and 1. "),
        tags$li(strong("HtoRD:"), "rate for transitioning from Hospitalized to Recovered or Dead, a value between 0 and 1"),
        tags$li(strong("htor:"), "proportion of people Recovered from Hospitalized, a value between 0 and 1. "),
        tags$li(strong("RtoS:"), "rate for transitioning from Recovered to Susceptible, a value between 0 and 1"),
        tags$li(strong("days:"), "days to simulate, a positive integer" ),
        tags$li(strong("reps:"), "number of replicates to run, a positive integer" ),
      ),

      # Add vertical space before the tabs
      div(class = "spacer"),

      # Main Panel with Tabset Panel for Model Description and Simulation
      fluidRow(


        tabsetPanel(
          id = "tabs",

          # Simulation Tab with CSV File Upload for Meta-Population
          tabPanel("Simulation",
                   div(style = "max-width: 1200px; margin: 20px;",

                       # file inputs
                       fluidRow(
                         column(4,
                                h3("File inputs"),
                                fileInput("population_data", "Population and initialization data",
                                          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                fileInput("vac_data", "Vaccination data",
                                          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                fileInput("mix_mat1", "Weekday Day-time mixing matrix"),
                                fileInput("mix_mat2", "Weekday Night-time mixing matrix"),
                                fileInput("mix_mat3", "Weekend Day-time mixing matrix"),
                                fileInput("mix_mat4", "Weekend Night-time mixing matrix"),
                                fileInput("population_map", "Sub population demographic info",
                                          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                actionButton("simulate", "Simulate")),

                         # disease parameter inputs

                         column(4,
                                h3("Transition rates"),
                                numericInput("beta_i", "S to E", value = 0.5, min = 0, step = 0.001),
                                numericInput("VtoS", "V to S:", value = 0.02, min = 0, step = 0.001),
                                numericInput("beta_v", "V to E", value = 0.1, min = 0, step = 0.001),
                                numericInput("EtoIpresymp", "E to Ia/Ip", value = 0.7, min = 0, step = 0.001),
                                numericInput("pretoIsymp", "Ip to Is", value = 0.1, min = 0, step = 0.001),
                                numericInput("IasymptoR", "Ia to R", value = 0.1, min = 0, step = 0.001),
                                numericInput("IsymptoRH", "Is to R/H", value = 0.5, min = 0, step = 0.001),
                                numericInput("HtoRD", "H to R/D", value = 0.33, min = 0, step = 0.001),
                                numericInput("RtoS", "R to S", value = 0.02, min = 0, step = 0.001),
                                numericInput("vac_eff", "Vaccination efficacy", value = 0.5, min = 0, step = 0.01)),

                         column(4,
                                h3("Proportions"),
                                numericInput("etopa", "Proportion of Ia among Ia+Ip", value = 0.5, min = 0, step = 0.001),
                                numericInput("istohr", "Proportion of Is -> R among R+H", value = 0.5, min = 0, step = 0.001),
                                numericInput("htor", "Proportion of H -> R among R+D", value = 0.7, min = 0, step = 0.001),
                                br(),
                                br(),
                                h3("Settings"),
                                numericInput("seed", "Random seed (optional):", value = NA),
                                radioButtons("choice", "Model type:",
                                             choices = list("Deterministic" = "det", "Stochastic" = "stoch")),
                                numericInput("dt", "Time step:", value = 0.5, min = 0, step = .5),
                                numericInput("days", "Simulation Length:", value = 100, min = 1),
                                numericInput("rep", "Number of replicates", value = 1, min = 1, step = 1))),
                       br(),
                       br(),
                       br(),
                       column(12,
                              box(
                                title = "Disease Compartments Over Time",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = 12,  # Full width
                                class = "custom-box",
                                plotly::plotlyOutput("seir_plot", height = "500px"))
                              ),
                       column(6,
                              box(
                                title = "",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = 12,  # Full width
                                # class = "custom-box",
                                plotly::plotlyOutput("new_infection_plot", height = "500px"))
                       ),
                       column(6,
                              box(
                                title = "",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = 12,  # Full width
                                # class = "custom-box",
                                plotly::plotlyOutput("new_hosp_plot", height = "500px"))
                       ),
                       column(6,
                              box(
                                title = "",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = 12,  # Full width
                                # class = "custom-box",
                                plotly::plotlyOutput("new_death_plot", height = "500px"))
                       ),
                       column(6,
                              # h4("Simulation Results"),
                              # tableOutput("population_table"),  # Show the uploaded population data as a table
                              plotly::plotlyOutput("new_death_plot", height = "500px")
                       ),

                   )
          ) ,
          # Additional visualization tab
          tabPanel("Visualization",
                   div(style = "max-width: 1200px; margin: 20px;",
                       fluidRow(
                         br(),
                         br(),
                         column(12, leaflet::leafletOutput("map", height = "500px")),     # Choropleth map
                         br(),
                         br(),
                         column(12,
                                box(
                                  title = "Disease Compartments Over Time",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  width = 12,  # Full width
                                  class = "custom-box",
                                  plotly::plotlyOutput("zone_simout", height = "500px"))
                         )
                       )
                   )
          ),
          tabPanel("Data",
                   div(style = "max-width: 1200px; margin: 20px;",
                       fluidRow(
                         br(),
                         br(),
                         # h4("out_table"),
                         column(12, DT::dataTableOutput("population_table")),
                         br(),
                         column(12, tableOutput("out_table")),
                         # br(),
                         # h4("Vaccination"),
                         # column(12, dataTableOutput("vac_table")),
                         # br(),
                         # br(),
                         # h4("Mixing day"),
                         # column(12, dataTableOutput("mix_mat1")),
                         # h4("Mixing night"),
                         # column(12, dataTableOutput("mix_mat2"))
                       )
                   )
          )
        )
      )
  )
)
