# Library
{
    library(shiny)
    library(actuar)
    library(shinydashboard)
    library(shinydashboardPlus)
    library(ggplot2)
    library(dplyr)
    library(rsconnect)
    library(plotly)
    library(shinyWidgets)
    library(shinyjs) 
    # devtools::install_github("alec42/Distributacalcul_package")
    library(Distributacalcul)
    # devtools::install_github("Appsilon/shiny.i18n")
    library(shiny.i18n)
    library(copula)
}
source(file = "myUI.R", local = T)
source(file = "serveur/myserver.R", local = T)

shinyApp(
    ui <- myUI,
    server <- myserver
)
