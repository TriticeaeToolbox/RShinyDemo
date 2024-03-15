library(shiny)
library(BrAPI)
library(tidyverse)

source("utils/inputEventObservers.R")
source("utils/getPhenotypeData.R")


# ====================================================== #
#
# SHINY SERVER
# This file defines the backend functionality of the app
# It handles changes to inputs and generates the outputs
#
# ====================================================== #


server = function(input, output, session) {


  #
  # REACTIVE DATA
  # This is data that will be set and used in different
  # functions of the server, such as the tables of 
  # selected trials and their downloaded phenotypes
  #
  data = reactiveValues(
    bp_trials = list(),
    selected_trials = tibble(
      studyDbId = c(9091, 9411),
      studyName = c("CornellMaster_2021_Snyder", "CornellMaster_2022_Helfer"),
      programName = c("Cornell University", "Cornell University"),
      year = c("2021", "2022"),
      locationName = c("Ithaca, NY - Snyder", "Ithaca, NY - Helfer")
    ),
    phenotype_data = tibble(
      studyDbId = numeric(),
      studyName = character(),
      year = character(),
      locationName = character(),
      observationUnitDbId = numeric(),
      observationUnitName = character(),
      germplasmDbId = numeric(),
      germplasmName = character(),
      row_number = numeric(),
      col_number = numeric(),
      plot = character(),
      rep = character(),
      block = character()
    )
  )


  #
  # HANDLER: Database Selection
  # Update the choices for breeding program when the selected database changes
  #
  observeEvent(input$database, onDatabaseChange(input, output, session, data))


  #
  # HANDLER: Breeding Program Selection
  # Update the choices for trials when the selected breeding program changes
  #
  observeEvent(input$breeding_program, onBreedingProgramChange(input, output, session, data))


  #
  # HANDLER: Add Trials Button
  # Add the user-selected trials to the table
  #
  observeEvent(input$add_trials, onAddTrials(input, output, session, data))


  #
  # HANDLER: Remove Trials Button
  # Clear all selected trials from the table
  #
  observeEvent(input$remove_trials, onRemoveTrials(input, output, session, data))


  #
  # HANDLER: Retrieve Phenotypes
  # Download all observations for all selected trials
  #
  observeEvent(input$get_phenotype_data, getPhenotypeData(input, output, session, data))

  #
  # HANDLER: Download Phenotypes
  # Download the current phenotype_data table to a CSV file
  #
  output$download_phenotype_data = downloadPhenotypeData(input, output, session, data)
}