library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)

library(tidyverse)
library(plotly)
# devtools::install_github("alec42/Distributacalcul_package")
library(Distributacalcul)

library(latex2exp)
source(file = "modules.R", local = F)

ui <- shinyUI({
    dashboardPagePlus(
        skin = "blue",
        header = dashboardHeaderPlus(
            title = paste0("Module testing: ", law_param)
        ),
        sidebar = dashboardSidebar(
            sidebarMenu(
                id = "tabs",
                menuItemOutput("sidebar_output_cont")
            )
        ),
        body = dashboardBody(
            tabItems(
                tab_LNORM_UI <- tabItem(
                    tabName = "Lognormale",
                    lawParametersBoxUI(id = "LNORM")
                ),
                tab_NORM_UI <- tabItem(
                    tabName = "Normale",
                    lawParametersBoxUI(id = "NORM")
                )
            )
        )
    )
})

server <- function(input, output, session) {
    callModule(
        module = lawParametersBox, 
        id = "NORM", 
        law = "norm"
    )
    callModule(
        module = lawParametersBox, 
        id = "LNORM", 
        law = "lnorm"
    )
    
    output$sidebar_output_cont <- renderMenu({
        menuItem(
            text = "Distributions continues",
            icon = icon("chart-area"),
            menuSubItem(
                text = "Normale",
                icon = icon("neos"),
                tabName = "Normale"
            ),
            menuSubItem(
                text = "Lognormale",
                icon = icon("ruler-combined"),
                tabName = "Lognormale"
            )
        )
    })
}

shinyApp(ui, server)