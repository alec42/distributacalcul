# Library
{
    library(shiny)
    library(actuar)
    library(shinydashboard)
    library(shinydashboardPlus)
    library(ggplot2)
    library(rsconnect)
    library(plotly)
    library(shinyWidgets)
    library(shinyjs)
    library(Distributacalcul)
    library(tvarPackage)
    # devtools::install_github("alec42/Distributacalcul_package")
    # devtools::install_github("gabrielcrepeault/tvarPackage")
    
    library(copula)
    library(dplyr)
    
    # library(shinymaterial)
    # library(dashboardthemes)
} 
# shinyWidgets::shinyWidgetsGallery()
source(file = "myUI.R", local = T)
source(file = "myserver.R")
# source(file = "functions.R")

shinyApp(
    ui <- myUI,
    server <- myserver
)


    