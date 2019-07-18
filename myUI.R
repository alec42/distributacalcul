source('tabs_UI.R')

myUI <- shinyUI({
    dashboardPage(
        skin = "blue", 
        dashboardHeader(
            title = "Lois de probabilité",
            titleWidth = "275px",
            tags$li(class = "dropdown",
                a(actionButton(
                    inputId = "email1",
                    label = "Nous contacter",
                    icon = icon("envelope", lib = "font-awesome")
                ),
                href = "mailto:alec.van-rassel.1@ulaval.ca"
                ))
        ),             
        
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
                            "Loi Uniforme",
                            # icon = icon("fish"),
                            tabName = "UniformeC"
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
                            icon = icon("italic"),
                            tabName = "IG"
                        )
                        
                    ),
                    
                    menuItem(
                        "Lois discrètes",
                        icon = icon("chart-bar"),
                        menuSubItem(
                            "Loi Uniforme",
                            # icon = icon("fish"),
                            tabName = "UniformeD"
                        ),
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
                        "Lois composées",
                        icon = icon("chart-line"),
                        menuSubItem(
                            "Binomiale Négative",
                            tabName = "BNCOMP"
                            # ,icon = icon("neos")
                        ),
                        menuSubItem(
                            "Binomiale",
                            tabName = "BINCOMP"
                            # ,icon = icon("neos")
                        ),
                        menuSubItem(
                            "Poisson",
                            tabName = "PCOMP"
                            #   , icon = icon("neos")
                        )
                    ),
                    menuItem(
                        "Outils",
                        menuSubItem("Excès-Moyen", tabName = "excess_mean")
                    ),
                    menuItem(
                        "À propos",
                        icon = icon("info-circle"),
                        menuSubItem("Description du projet", tabName = "description", selected = T),
                        menuSubItem(
                            " Développeurs",
                            icon = icon("user-tie"),
                            tabName = "about"
                        ),                      
                        menuSubItem("Théorie et formules", icon = icon("wikipedia-w"), href = "https://gitlab.com/alec42/distributacalcul-wiki/wikis/Home"),
                        menuSubItem(" GitHub", icon = icon("github"), href = "https://github.com/alec42/distributacalcul.git"),
                        menuSubItem(" Site du projet", icon = icon("compass"), href = "https://alec42.github.io/distributacalcul/")
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
                tab_UNIC_UI,
                tab_BETA_UI,
                tab_ERLANG_UI,
                tab_LOGLOGIS_UI,
                tab_IG_UI,
                
                ## Lois Discrètes
                tab_UNID_UI,
                tab_BIN_UI,
                tab_BN_UI,
                tab_POI_UI,
                tab_HG_UI,
                tab_LOGARITHMIQUE_UI,
                
                ## Lois composées
                tab_BNCOMP_UI,
                tab_PCOMP_UI,
                tab_BINCOMP_UI,
            
            ## À propos ----
                tab_excess_mean,
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
                ),
            tabItem(
                tabName = "description",
                h1("Description du projet"),
                accordion(
                    accordionItem(
                        id = 1,
                        title = "But du projet",
                        color = "danger",
                        collapsible = F,
                        collapsed = F,
                        "Ce projet a comme but de simplifier la vie des étudiants en actuariat à l'Université Laval et est conçu pour les cours d'introduction à l'actuariat 2 et d'analyse probabiliste des risques actuariels.",
                        br(),
                        "Le site inclut une 'calculatrice' de plusieurs paramètres, mesures de risques, moments, etc. pour plusieurs distributions.",
                        br(),
                        "Également, on peut accéder à un wiki qui contient d'avantage d'information sur les diverses distributions; entre autres les formules pour les fonctions de densité, répartition, mesures de risques, etc.",
                        align = "left"
                    ),
                    accordionItem(
                        id = 2,
                        title = "Remerciements",
                        color = "info",
                        collapsed = F,
                        "Cette calculatrice est crée grâce à l'enseignment des professeurs de l'école d'actuariat de l'Université Laval et les notes de ses cours. Particulièrement, un gros merci à Étienne Marceau et Hélène Cossette.",
                        br(),
                        "Un gros merci à tous ceux qui nous ont donné des suggestions, commentaires, reccomendations, etc. sans qui cette calculatrice n'aurait pas plus être réalisée.",
                        br(),
                        "Un gros merci à ceux ayant fournis des résumés de cours, des notes et des explications pour aider à compléter le wiki. Particulièrement, un gros merci à Jean-Christophe Langlois et Mathieu Rhéaume."
                    ),
                    accordionItem(
                        id = 3,
                        title = "Liens",
                        color = "warning",
                        collapsible = F,
                        collapsed = F,
                        "Pour plus d'information sur les créateurs du projet, voir l'onglet Dévelopeurs.",
                        br(),
                        "Pour plus d'information sur le code du projet, voir le lien vers le GitHub du projet.",
                        br(),
                        "Pour plus d'information sur les packages utilisé pour le projet, voir cette page du wiki",
                        br(),
                        "Pour nous faire part de commentaires, suggestions, questions, ou quoi autre SVP nous contacter"
                    )
                )
                ,fixedPanel(
                    a(actionButton(inputId = "email1", label = "Nous contacter", 
                                   icon = icon("envelope", lib = "font-awesome")),
                      href="mailto:alec.van-rassel.1@ulaval.ca"),
                    right = 40,
                    bottom = 40
                )

                
                
            )
            
            )
        )
        }
    )
})