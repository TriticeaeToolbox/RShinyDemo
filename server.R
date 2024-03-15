library(shiny)
library(BrAPI)
library(tidyverse)


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
  DATA = reactiveValues(
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
  observeEvent(input$database, {
    choices = list()
    db_name = input$database

    if ( db_name != "" ) {
      withProgress(message = "Fetching Breeding Programs", value = NULL, {

        # Set breeding program choices (key = program name, value = program id)
        db = getBrAPIConnection(db_name)
        resp = db$get("/programs", page="all")
        choices = sapply(resp$combined_data, \(x) { c(x$programDbId) })
        names(choices) = sapply(resp$combined_data, \(x) { c(x$programName) })

      })
    }

    # Update the drop down menu choices
    updateSelectInput(session, "breeding_program", choices = choices, selected = NULL)
  })


  #
  # HANDLER: Breeding Program Selection
  # Update the choices for trials when the selected breeding program changes
  #
  observeEvent(input$breeding_program, {
    choices = list()
    db_name = input$database
    bp_id = input$breeding_program

    if ( db_name != "" && bp_id != "" ) {
      withProgress(message = "Fetching Trials", value = NULL, {

        # Set trial choices (key = trial name, value = trial id)
        db = getBrAPIConnection(db_name)
        resp = db$get("/studies", query=list(programDbId=bp_id), page="all")
        choices = sapply(resp$combined_data, \(x) { c(x$studyDbId) })
        names(choices) = sapply(resp$combined_data, \(x) { c(x$studyName) })

        # Cache trial metadata for display in selected trials table,
        # if the trial is selected by the user
        for ( t in resp$combined_data ) {
          DATA$bp_trials[[t$studyDbId]] = t
        }

      })
    }

    # Update the drop down menu choices
    updateSelectInput(session, "trials", choices = choices, selected = NULL)
  })


  #
  # HANDLER: Add Trials Button
  # Add the user-selected trials to the table
  #
  observeEvent(input$add_trials, {
    selected_trial_ids = input$trials

    # Loop through each id of the selected trial ids
    for ( id in selected_trial_ids ) {

      # Only add the trial if it's not already in the table
      if ( ! id %in% DATA$selected_trials$studyDbId ) {
        t = DATA$bp_trials[[id]]

        # Add the trial metadata to the table of selected trials
        DATA$selected_trials = add_row(DATA$selected_trials, tibble(
          studyDbId = as.numeric(t$studyDbId),
          studyName = as.character(t$studyName),
          programName = as.character(t$additionalInfo$programName),
          year = paste(t$seasons, collapse=", "),
          locationName = as.character(t$locationName)
        ))
      }
    }

    # Render the table in the UI
    output$selected_trials = renderDataTable(DATA$selected_trials)
  })


  #
  # HANDLER: Remove Trials Button
  # Clear all selected trials from the table
  #
  observeEvent(input$remove_trials, {
    DATA$selected_trials = DATA$selected_trials[0,]
    output$selected_trials = renderDataTable(DATA$selected_trials)
  })


  #
  # HANDLER: Retrieve Phenotypes
  # Download all observations for all selected trials
  #
  observeEvent(input$get_phenotype_data, {
    db_name = input$database
    DATA$retrieved_phenotypes = DATA$retrieved_phenotypes[0,]
    trial_count = nrow(DATA$selected_trials)

    # Temporary table to hold observations
    data_observations = tibble(
      observationUnitDbId = numeric(),
      trait = character(),
      value = character()
    )

    if ( db_name != "" ) {
      withProgress(message = "Fetching Observations", value = 0, min = 0, max = trial_count*2, {
        db = getBrAPIConnection(db_name)

        # Loop through each of the trials in the selected_trials table
        for ( i in c(1:nrow(DATA$selected_trials)) ) {
          
          # Get trial info
          t = DATA$selected_trials[i,]
          studyDbId = t$studyDbId
          studyName = t$studyName

          # Get all of the observationUnits (plots) for the trial
          setProgress(value = (i*2)-2, message = studyName, detail = "Fetching plots...")
          resp = db$get("/observationunits", query=list(studyDbId=studyDbId), page="all", pageSize=100)
          observationUnits = resp$combined_data

          # Loop through each observationUnit, adding it to the table
          for ( observationUnit in observationUnits ) {
            r = tibble(
              studyDbId = as.numeric(observationUnit$studyDbId),
              studyName = as.character(observationUnit$studyName),
              year = as.character(t$year),
              locationName = as.character(observationUnit$locationName),
              observationUnitDbId = as.numeric(observationUnit$observationUnitDbId),
              observationUnitName = as.character(observationUnit$observationUnitName),
              germplasmDbId = as.numeric(observationUnit$germplasmDbId),
              germplasmName = as.character(observationUnit$germplasmName),
              row_number = observationUnit$observationUnitPosition$positionCoordinateY,
              col_number = observationUnit$observationUnitPosition$positionCoordinateX,
              plot = "",
              rep = "",
              block = ""
            )
            levels = observationUnit$observationUnitPosition$observationLevelRelationships
            for ( l in c(1:length(levels)) ) {
              level = levels[[l]]
              if ( level$levelName == "plot" ) {
                r$plot = as.character(level$levelCode)
              }
              if ( level$levelName == "rep" ) {
                r$rep = as.character(level$levelCode)
              }
              if ( level$levelName == "block" ) {
                r$block = as.character(level$levelCode)
              }
            }
            DATA$phenotype_data  = bind_rows(DATA$phenotype_data , r)
          }

          # Get all of the observations for the trial
          setProgress(value = (i*2)-1, message = studyName, detail = "Fetching observations...")
          resp = db$get("/observations", query=list(studyDbId=studyDbId), page="all", pageSize=500)
          observations = resp$combined_data

          # Loop through each observation, adding it to the table
          for ( observation in observations ) {
            data_observations = bind_rows(data_observations, tibble(
              observationUnitDbId = as.numeric(observation$observationUnitDbId),
              trait = as.character(observation$observationVariableName),
              value = as.character(observation$value)
            ))
          }

        }


        # Add the traits and their values to the observationUnits table
        incProgress(amount = 1, message = "Generating input data...")

        # Add trait columns
        traits = sort(unique(data_observations$trait))
        for ( trait in traits ) {
          DATA$phenotype_data = bind_cols(DATA$phenotype_data, !!trait := NA_character_)
        }

        # Add trait values from the observations table to the plot table
        for ( i in c(1:nrow(data_observations)) ) {
          o = data_observations[i,]
          x = which(DATA$phenotype_data $observationUnitDbId == o$observationUnitDbId)
          y = which(colnames(DATA$phenotype_data ) == o$trait)
          DATA$phenotype_data [x,y] = o$value
        }

        # Render the retrieved phenotypes table in the UI
        print(DATA$phenotype_data)
        output$phenotype_data = renderDataTable(DATA$phenotype_data)
      })
    }
    else {

    }
  })
}