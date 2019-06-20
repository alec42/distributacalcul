source('tabs_UI.R')

myUI <- shinyUI({
    dashboardPage(
        skin = "blue", 
        dashboardHeader(title = "Lois de probabilité", titleWidth = "275px"),
        
        # Paneau Latéral
        {
            dashboardSidebar(width = "275px",
                collapsed = F,
                
                sidebarMenu(
                    id = "tabs",
                    menuItem(
                        "Lois continues",
                        icon = icon("chart-area"),
                        menuSubItem(
                            "Normale",
                            tabName = "Normale",
                            icon = icon("neos")
                        ),
                        menuSubItem(
                            "Lognormale",
                            tabName = "Lognormale",
                            icon = icon("ruler-combined")
                        ),
                        menuSubItem(
                            "Gamma, Exponentielle, & Khi-Carré",
                            icon = icon("google"),
                            tabName = "gamma"
                        ),
                        menuSubItem(
                            "Weibull",
                            icon = icon("wikipedia-w"),
                            tabName = "Weibull"
                        ),
                        menuSubItem(
                            "Pareto",
                            icon = icon("product-hunt"),
                            tabName = "Pareto"
                        ),
                        menuSubItem(
                            "Burr",
                            icon = icon("btc"),
                            tabName = "Burr"
                        ),
                        menuSubItem(
                            "Beta",
                            icon = icon("behance"),
                            tabName = "Beta"
                        ),
                        menuSubItem(
                            "Erlang",
                            icon = icon("erlang"),
                            tabName = "Erlang"
                        ),
                        menuSubItem(
                            "Log-logistique",
                            icon = icon("dolly"),
                            tabName = "LOGLOGIS"
                        ),
                        menuSubItem(
                            "Loi Inverse Gaussienne",
                            icon = icon("yahoo"),
                            tabName = "IG"
                        )
                        
                    ),
                    
                    menuItem(
                        "Lois discrètes",
                        icon = icon("chart-bar"),
                        menuSubItem(
                            "Loi Binomiale et Bernoulli",
                            tabName = "Binomiale",
                            icon = icon("bold")
                        ),
                        menuSubItem(
                            "Binomiale Négative et Géométrique",
                            tabName = "Binneg",
                            icon = icon("minus")
                        ),
                        menuSubItem(
                            "Loi Poisson",
                            icon = icon("fish"),
                            tabName = "Poisson"
                        ),
                        menuSubItem(
                            "Loi Hypergéométrique",
                            icon = icon("hire-a-helper"),
                            tabName = "HG"
                        ),
                        menuSubItem(
                            "Loi Logarithmique",
                            icon = icon("yahoo"),
                            tabName = "Logarithmique"
                        )
                    ),
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
                tab_WEIBULL_UI,
                tab_PARETO_UI,
                tab_BURR_UI,
                tab_LNORM_UI,
                tab_BETA_UI,
                tab_ERLANG_UI,
                tab_LOGLOGIS_UI,
                tab_IG_UI,
                
            ## Lois Discrètes
            tab_BIN_UI,
            tab_BN_UI,
            tab_POI_UI,
            tab_HG_UI,
            tab_LOGARITHMIQUE_UI,
            
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
                        )
                    ),
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
            
            )
        )
        }
    )
})