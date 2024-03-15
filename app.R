library(shiny)

source("./shiny/ui.R")
source("./shiny/server.R")


#
# DEFINE THE SHINY APP
# Set the UI and Server components, 
# sourced from separate files
#
shinyApp(ui = ui, server = server)
