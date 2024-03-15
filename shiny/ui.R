library(shiny)
library(BrAPI)


# ====================================================== #
#
# SHINY UI
# This file defines the user interface of the shiny app
#
# ====================================================== #


#
# SUPPORTED DATABASES
# The list of supported databases and their connection info
# These are defined in the BrAPI R library
#
databases = getBrAPIConnections()


#
# INPUT PANEL
#
inputPanel = fluidPage(

  # Page Header
  h2("Data Selection"),

  # Create a Row with two Columns
  fluidRow(

    # The left column has the trial selection and analysis parameter inputs
    column(4,

      # Trial Selection
      wellPanel( 
        h3("Trial Selection"),
        p("Select a database to query and then choose trials to include in the analysis"),
        
        # Dropdown menus for selecting the input trials
        # The choices for the database come from the BrAPI library
        # The choices for the breeding program will be added when the database is selected (in server.R)
        # The choices for the trials will be added when a breeding program is selected (in server.R)
        selectInput("database", "Database", choices = c("", names(databases)), width="100%"),
        selectInput("breeding_program", "Breeding Program", choices = c(), width="100%"),
        selectInput("trials", "Trials", choices = c(), width="100%", multiple=TRUE, selectize=FALSE),

        # Buttons to add / remove trials
        actionButton("add_trials", "Add Trials to Analysis"),
        tags$hr(),
        actionButton("remove_trials", "Remove All Trials from Analysis")
      ),

      # Analysis Parameters
      wellPanel(
        h3("Analysis Parameters"),
        p("We can also include any other parameters that the user can set")
      )

    ),

    # The right column has a table of the selected trials
    column(8,
      h3("Selected Trials"),
      dataTableOutput("selected_trials")
    )

  )
)

#
# ANALYSIS PANEL
#
analysisPanel = fluidPage(

  # Page Header
  h2("Run Analysis"),

  # A row with two columns
  fluidRow(

    # Left column: button to download phenotypes
    column(4,
      h4("Step 1"),
      p("Download phenotype data from the database"),
      actionButton("get_phenotype_data", "Get Phenotype Data")
    ),

    # Right column: table of downloaded phenotypes
    column(8,
      h3("Phenotype Data"),
      downloadButton("download_phenotype_data", "Download Phenotype Data"),
      dataTableOutput("phenotype_data")
    )

  )

)


#
# PAGE LAYOUT
# Define the main UI as a Navbar Page
# 
# This is a toolbar with navigation bar at the top of the page
# that can be used to toggle the display of different panels
#
ui = navbarPage(

  # Toolbar title
  title = "T3/Breedbase BrAPI Test",

  # Navigation panels
  tabPanel("Data Selection", inputPanel, icon = icon("database")),
  tabPanel("Run Analysis", analysisPanel, icon = icon("play")),

  # Set the navbar to be "sticky" at the top
  position = "fixed-top",

  # Add custom css to move the notification panel to the top-right corner of the page
  header = tags$head(
    tags$style(".shiny-notification { position: fixed; top: 5px; right: 15px; width: 300px }"),
    tags$style(".progress-message, .progress-detail { display: block }")
  )

)