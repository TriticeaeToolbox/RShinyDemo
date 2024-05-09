library(shiny)

# Increase maxium upload file size (in MB)
options(shiny.maxRequestSize=50*1024^2)

source("./shiny/ui.R")
source("./shiny/server.R")


#
# DEFINE THE SHINY APP
# Set the UI and Server components, 
# sourced from separate files
#
shinyApp(ui = ui, server = server)
