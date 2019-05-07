source('tabs_UI.R')
myUI <- shinyUI({
    dashboardPage(
        skin = "blue",
        dashboardHeader(title = "Lois de probabilité", titleWidth = NULL),
        
        # Paneau Latéral
        {
            dashboardSidebar(
                collapsed = F,
                sidebarSearchForm(
                    textId = "searchbar",
                    buttonId = "search",
                    label = "Recherche"
                ),
                #Ajout d'une search bar (À retravailler)
                sidebarMenu(
                    id = "tabs",
                    menuItem(
                        "Lois continues",
                        icon = icon("chart-area"),
                        menuSubItem(
                            "Loi normale",
                            tabName = "Normale",
                            icon = icon("neos")
                        ),
                        menuSubItem(
                            " Loi gamma",
                            icon = icon("google"),
                            tabName = "gamma"
                        )
                    ),
                    
                    menuItem(
                        "Lois discrètes",
                        icon = icon("chart-bar"),
                        menuSubItem(
                            "Loi binomiale",
                            tabName = "Binomiale",
                            icon = icon("bold")
                        )
                    ),
                    br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                    menuItem(
                        "À propos",
                        icon = icon("info-circle"),
                        menuSubItem(
                            " Développeurs",
                            icon = icon("user-tie"),
                            tabName = "about"
                        ),
                        menuSubItem(" Site du projet", icon = icon("compass"), href = "https://alec42.github.io/distributacalcul/"),
                        menuSubItem(" GitHub", icon = icon("github"), href = "https://github.com/alec42/distributacalcul.git")
                    )
                )
            )
        }, 
              
        # corps de la page
        {dashboardBody(
            tags$head(
                tags$style(
                    type = "text/css",
                    "label{ display: table-cell; text-align: center; vertical-align: center; width: 50px; font-size: 13pt} .form-group { display: table-row;}"
                )
            ), 
            
            tabItems(
            ## Lois Continues
                tab_NORM_UI,
                tab_GAMMA_UI,
                # tab_PARETO_UI,
                
            ## Lois Discrètes
            tab_BIN_UI,
                {
                ## À propos ----
                    tabItem(
                        tabName = "about",
                        h2("Nous contacter "),
                        align = "center",
                        box(
                            title = "Alec James van Rassel",
                            status = "primary",
                            solidHeader = T,
                            boxProfile(
                                src = "alec.jpg",
                                title = "Alec James van Rassel",
                                subtitle = "alec.van-rassel.1@ulaval.ca"
                            )),
                        box(
                            title = "Marc-André Devost",
                            status = "primary",
                            solidHeader = T,
                            boxProfile(
                                src = "marc.jpg",
                                title = "Marc-André Devost",
                                subtitle = "marc-andre.devost.1@ulaval.ca"
                            )
                        )
                    )
                }
            
            )
        )
        }
    )
})