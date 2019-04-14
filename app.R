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
}
source(file = "myUI.R", local = T, encoding = "UTF-8")
source(file = "myserver.R")
source(file = "functions.R")

shinyApp(
    ui <- myUI,
    server <- myserver
)

