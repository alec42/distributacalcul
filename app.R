# Library
{
    library(shiny)
    library(actuar)
    library(shinydashboard)
    library(shinydashboardPlus)
    library(ggplot2)
    library(rsconnect)
    library(plotly)
}
source(file = "myUI.R", local = T)
source(file = "myserver.R")
source(file = "functions.R")

shinyApp(
    ui <- myUI,
    server <- myserver
)
