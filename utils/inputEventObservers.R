library(shiny)
library(BrAPI)
source("./utils/getTraitNames.R")

#
# Update the Breeding Programs when the Database changes
#
onDatabaseChange = function(input, output, session, data) {
  choices = list()
  db_name = input$database

  if ( db_name != "" ) {
    withProgress(message = "Fetching Breeding Programs", value = NULL, {

      # Set breeding program choices (key = program name, value = program id)
      db = DATABASES[[db_name]]
      resp = db$get("/programs", page="all")
      print(resp)
      choices = sapply(resp$combined_data, \(x) { c(x$programDbId) })
      names(choices) = sapply(resp$combined_data, \(x) { c(x$programName) })

    })
  }

  # Update the drop down menu choices
  updateSelectInput(session, "breeding_program", choices = choices)
}


#
# Update the Trials when the Breeding Program changes
#
onBreedingProgramChange = function(input, output, session, data) {
  choices = list()
  db_name = input$database
  bp_id = input$breeding_program

  if ( db_name != "" && bp_id != "" ) {
    withProgress(message = "Fetching Trials", value = NULL, {

      # Set trial choices (key = trial name, value = trial id)
      db = DATABASES[[db_name]]
      resp = db$get("/studies", query=list(programDbId=bp_id), page="all")
      choices = sapply(resp$combined_data, \(x) { c(x$studyDbId) })
      names(choices) = sapply(resp$combined_data, \(x) { c(x$studyName) })

      # Cache trial metadata for display in selected trials table,
      # if the trial is selected by the user
      for ( t in resp$combined_data ) {
        data$bp_trials[[t$studyDbId]] = t
      }

    })
  }

  # Update the drop down menu choices
  updateSelectInput(session, "trials", choices = choices)
}


#
# Add the selected trials to the selected_trials table
#
onAddTrials = function(input, output, session, data) {
  selected_trial_ids = input$trials

  # Loop through each id of the selected trial ids
  for ( id in selected_trial_ids ) {

    # Only add the trial if it's not already in the table
    if ( ! id %in% data$selected_trials$studyDbId ) {
      t = data$bp_trials[[id]]

      # Add the trial metadata to the table of selected trials
      data$selected_trials = add_row(data$selected_trials, tibble(
        studyDbId = as.numeric(t$studyDbId),
        studyName = as.character(t$studyName),
        programName = as.character(t$additionalInfo$programName),
        year = paste(t$seasons, collapse=", "),
        locationName = as.character(t$locationName)
      ))
    }
  }

  # Render the table in the UI
  output$selected_trials = renderDT(data$selected_trials)
}


#
# Remove all of the trials from the selected_trials table
#
onRemoveTrials = function(input, output, session, data) {
  data$selected_trials = data$selected_trials[0,]
  output$selected_trials = renderDT(data$selected_trials)
}


#
# Parse the contents of an uploaded phenotype file
# - set the selected trials
# - set the phenotype data
onUploadTrials = function(input, output, session, data) {
  
  # clear existing data
  data$selected_trials = data$selected_trials[0,]
  data$retrieved_phenotypes = data$retrieved_phenotypes[0,]

  # read file contents
  content = as_tibble(read.csv(input$upload_trials$datapath, check.names = FALSE))

  # build selected trials table
  studyDbIds = unique(content$studyDbId)
  for ( i in studyDbIds ) {
    m = filter(content, studyDbId == i)
    data$selected_trials = add_row(data$selected_trials, tibble(
      studyDbId = as.numeric(unique(m$studyDbId)),
      studyName = as.character(unique(m$studyName)),
      programName = as.character(unique(m$programName)),
      year = as.character(unique(m$year)),
      locationName = as.character(unique(m$locationName))
    ))
  }

  # set the phenotype data
  data$phenotype_data = content

  # Render the tables in the UI
  output$selected_trials = renderDT(data$selected_trials)
  output$phenotype_data = renderDT(data$phenotype_data)
  updateSelectInput(session, "traits", choices = getTraitNames(content), selected = NULL)
}


onStartAnalysis = function(input, output, session, data) {
  print("====> START SPATIAL ANALYSIS:")
  results = spatial_analysis(
    input$traits,
    data$phenotype_data,
  )
  print("====> SPATIAL ANALYSIS RESULTS")
  print(results)
  output$results = renderDT(results)
}