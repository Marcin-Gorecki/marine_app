# if (interactive()) {
    library(shiny)
    library(shiny.semantic)
    library(leaflet)
    library(testthat)
    library(data.table)
    require(bit64)
    library(dplyr)
    library(geosphere)
    library(lubridate)
    
    source("modules_load_data.R")
    

# UI ----------------------------------------------------------------------
    
    ui <- semanticPage(titlePanel("Ship info"),
                       dropdownUI("dropdown")
                       )

# server ------------------------------------------------------------------
    
    server <- shinyServer(function(input, output, session) {
        
        dropdownServer("dropdown")
        outputServer("dropdown")
       
    })

# run app -----------------------------------------------------------------
    
    shinyApp(ui = ui, server = server)
# }
