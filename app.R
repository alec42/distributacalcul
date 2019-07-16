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
    library(shinymaterial)
    # library(dashboardthemes)
} 
source(file = "myUI.R", local = T)
source(file = "myserver.R")
source(file = "functions.R")

shinyApp(
    ui <- myUI,
    server <- myserver
)


