library(shiny)
library(DT)
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
DATABASES = getBrAPIConnections()
DATABASES$`T3/WheatCAP` = createBrAPIConnection("wheatcap.triticeaetoolbox.org")
DATABASES = DATABASES[order(names(DATABASES))]


#
# PHENOTYPE DATA PANEL
#
phenotypePanel = fluidPage(

  # Create a Row with two Columns
  fluidRow(

    # The left column has the trial selection and analysis parameter inputs
    column(4,

      # Download Data via BrAPI
      wellPanel( 
        h3("Fetch Data from Database"),
        p("Select trials from a BrAPI-compliant database to fetch for the analysis."),
        
        # Dropdown menus for selecting the input trials
        # The choices for the database come from the BrAPI library
        # The choices for the breeding program will be added when the database is selected (in server.R)
        # The choices for the trials will be added when a breeding program is selected (in server.R)
        selectInput("database", "Database", choices = c("", names(DATABASES)), width="100%"),
        selectInput("breeding_program", "Breeding Program", choices = c(), width="100%"),
        selectInput("trials", "Trials", choices = c(), width="100%", multiple=TRUE, selectize=FALSE),

        # Buttons to add / remove trials
        fluidRow(
          column(6, actionButton("add_trials", "Add Trials to Selection")),
          column(6, actionButton("remove_trials", "Remove All Selected Trials"))
        ),
        
        tags$hr(),

        # Button to download trials
        actionButton(
          "fetch_trials",
          "Fetch Phenotype Data for Selected Trials",
          icon("database"),
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )
        
      ),

      # Upload Data from File
      wellPanel(
        h3("Upload Data from Files"),
        p("Upload previously saved data files for use in the analysis."),

        fileInput("upload_phenotype_data", "Upload Phenotype Data")
      )

    ),

    # The right column has a table of the selected trials
    column(8,
      h3("Selected Trials"),
      dataTableOutput("selected_trials"),

      tags$hr(),

      h3("Phenotype Data"),
      downloadButton("download_phenotype_data", "Download Phenotype Data"),
      dataTableOutput("phenotype_data")
    ),

    style = "margin-top: 80px"
  )
)

#
# GENOTYPE DATA PANEL
#
genotypePanel = fluidPage(

  # A row with two columns
  fluidRow(

    # Left column: genotype file selection
    column(4,

      # Upload Data from File
      wellPanel(
        h3("Upload Data from Files"),
        p("Upload a Dosage Matrix file for use in the analysis."),

        fileInput("upload_genotype_data", "Upload Marker Data")
      )

    ),

    # Right column:
    column(8,

      h3("Genotype Data"),
      dataTableOutput("genotype_data")

    ),

    style = "margin-top: 80px"
  )

)

#
# ANALYSIS PANEL
#
analysisPanel = fluidPage(

  # A row with two columns
  fluidRow(

    # Left column: button to download phenotypes
    column(3,
      wellPanel(

        h4("Step 1"),
        p("Select one or more traits to include in the analysis"),
        selectInput("traits", "Traits", choices = c(), width="100%", multiple=TRUE, selectize=FALSE),

        hr(),

        h4("Step 2"),
        p("Start the analysis"),
        actionButton("start_analysis", "Start Analysis")

      )
    ),

    # Right column: table of selected phenotypes
    column(9,
      h3("Analysis Results"),

      hr(),
      h4("BLUE Output"),
      dataTableOutput("blue_results"),

      hr(),
      h4("Genomic Relationship Matrix"),
      dataTableOutput("grm_results"),

      hr(),
      h4("GEBVs"),
      dataTableOutput("gebv_g_results"),
      dataTableOutput("gebv_gxe_results")

    ),

    style = "margin-top: 80px"
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
  tabPanel("Phenotype Data", phenotypePanel, icon = icon("wheat-awn")),
  tabPanel("Genotype Data", genotypePanel, icon = icon("dna")),
  tabPanel("Run Analysis", analysisPanel, icon = icon("play")),

  # Set the navbar to be "sticky" at the top
  position = "fixed-top",

  # Add custom css to move the notification panel to the top-right corner of the page
  header = tags$head(
    tags$style(".shiny-notification { position: fixed; top: 5px; right: 15px; width: 300px }"),
    tags$style(".progress-message, .progress-detail { display: block }")
  )

)