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
    library(tvarPackage)
    # devtools::install_github("alec42/Distributacalcul_package")
    library(Distributacalcul)

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


    