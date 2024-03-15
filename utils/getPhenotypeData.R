library(shiny)
library(BrAPI)

getPhenotypeData = function(input, output, session, data) {
  db_name = input$database
  data$retrieved_phenotypes = data$retrieved_phenotypes[0,]
  trial_count = nrow(data$selected_trials)

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
      for ( i in c(1:nrow(data$selected_trials)) ) {
        
        # Get trial info
        t = data$selected_trials[i,]
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
          data$phenotype_data  = bind_rows(data$phenotype_data , r)
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
        data$phenotype_data = bind_cols(data$phenotype_data, !!trait := NA_character_)
      }

      # Add trait values from the observations table to the plot table
      for ( i in c(1:nrow(data_observations)) ) {
        o = data_observations[i,]
        x = which(data$phenotype_data $observationUnitDbId == o$observationUnitDbId)
        y = which(colnames(data$phenotype_data ) == o$trait)
        data$phenotype_data [x,y] = o$value
      }

      # Render the retrieved phenotypes table in the UI
      output$phenotype_data = renderDataTable(data$phenotype_data)
    })
  }
}