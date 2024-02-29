library(shiny)
library(BrAPI)

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
    selected_trials = data.frame(
      trial_id = integer(),
      trial_name = character(),
      breeding_program = character(),
      year = character(),
      location = character()
    ),
    retrieved_phenotypes = data.frame(
      trial_id = integer(),
      trial_name = character(),
      plot_id = integer(),
      plot_name = character(),
      trait_name = character(),
      value = numeric()
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
      if ( ! id %in% DATA$selected_trials$trial_id ) {
        t = DATA$bp_trials[[id]]

        # Add the trial metadata to the table of selected trials
        DATA$selected_trials = rbind(DATA$selected_trials, data.frame(
          trial_id = t$studyDbId,
          trial_name = t$studyName,
          breeding_program = t$additionalInfo$programName,
          year = paste(t$seasons, collapse=", "),
          location = t$locationName
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
  observeEvent(input$retrieve_phenotypes, {
    db_name = input$database
    DATA$retrieved_phenotypes = DATA$retrieved_phenotypes[0,]
    trial_count = nrow(DATA$selected_trials)

    if ( db_name != "" ) {
      withProgress(message = "Fetching Observations", value = 0, min = 0, max = trial_count, {
        db = getBrAPIConnection(db_name)

        # Loop through each of the trials in the selected_trials table
        for ( i in c(1:nrow(DATA$selected_trials)) ) {
          
          # Get trial info
          t = DATA$selected_trials[i,]
          trial_id = t$trial_id
          trial_name = t$trial_name
          incProgress(amount = 1, detail = trial_name)

          # Get all of the observations for the trial
          resp = db$get("/observations", query=list(studyDbId=trial_id), page="all", pageSize=500)
          observations = resp$combined_data

          # Loop through each observation, adding it to the table
          for ( observation in observations ) {
            DATA$retrieved_phenotypes = rbind(DATA$retrieved_phenotypes, data.frame(
              trial_id = as.numeric(trial_id),
              trial_name = as.character(trial_name),
              plot_id = as.numeric(observation$observationUnitDbId),
              plot_name = observation$observationUnitName,
              accession_name = observation$germplasmName,
              trait_name = observation$observationVariableName,
              value = observation$value
            ))
          }

        }
      })

      # Render the retrieved phenotypes table in the UI
      output$retrieved_phenotypes = renderDataTable(DATA$retrieved_phenotypes)
    }
  })
}