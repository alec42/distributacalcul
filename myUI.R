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
                        icon = icon("wrench"),
                        menuSubItem("Excès-Moyen", tabName = "excess_mean"),
                        menuSubItem("Test d'hypothèse T", href = "https://casertamarco.shinyapps.io/power/"),
                        menuSubItem("Tests statistiques", tabName = "stat_tests"),
                        menuSubItem("Copules", tabName = "copulas_tool")
                        
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
                
                ## Outils
                
                tab_excess_mean,
                tab_stat_tests,
                tab_copulas_tool,
            
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
                    ),
                    box(
                        title = "Remerciements",
                        status = "info",
                        collapsed = F,
                        "Ce projet est crée grâce à l'enseignement des professeurs de l'école d'actuariat de l'Université Laval et les notes de ses cours. Particulièrement, aux professeurs des cours de probabilités Étienne Marceau et Hélène Cossette et des cours d'informatique Vincent Goulet.",
                        br(),
                        "Un gros merci à tous ceux qui nous ont donné des suggestions, commentaires, avis, etc. sans qui ce projet ne serait pas proche de ce qu'il l'est aujourd'hui.",
                        br(),
                        "Un gros merci à ceux ayant fourni des résumés de cours, des notes et des explications pour aider à faire le wiki. Particulièrement, un gros merci à Jean-Christophe Langlois.",
                        align = "left"
                    )
                ),
            tabItem(
                tabName = "description",
                h1("Distributacalcul"),
                h3("À propos du projet"),
                align = "center",
                
                accordion(
                    accordionItem(
                        id = 1,
                        title = "But du projet",
                        color = "danger",
                        collapsible = F,
                        collapsed = F,
                        "Ce projet a comme but de simplifier la vie des étudiants en actuariat à l'Université Laval et est conçu particulièrement pour les cours d'introduction à l'actuariat 2 et d'analyse probabiliste des risques actuariels.",
                        br(),
                        "Le site inclut une 'calculatrice' de plusieurs fonctions, mesures de risques, moments, etc. pour plusieurs distributions discrètes, continues et composées.",
                        br(),
                        "Également en développement est un onglet d'outils qui pour l'instant contient l'ébauche d'une graphique de fonctions d'excès-moyen pour plusieurs distributions.",
                        br(),
                        "Également, on peut accéder à un wiki qui contient davantage d'information sur les diverses distributions. Entre autres, il contient les formules pour les fonctions de densité, répartition, mesures de risques, etc.",
                        align = "left"
                    ),
                    accordionItem(
                        id = 2,
                        title = "État du projet",
                        color = "warning",
                        collapsed = F,
                        "Ce projet de calculatrice est encore en développement et le sera sûrement pour toujours. Si vous êtes intéressés à y contribuer, contacter-nous afin qu'on puisse collaborer!",
                        align = "left")
                    ,
                    accordionItem(
                        id = 3,
                        title = "Liens",
                        color = "warning",
                        collapsible = F,
                        collapsed = F,
                        "Pour plus de détails sur le code du projet, voir le lien vers le GitHub.",
                        br(),
                        "Pour plus d'information sur les packages utilisés pour le projet, voir cette page (À VENIR) du wiki",
                        br(),
                        "Pour nous faire part de tout feed-back (erreurs, commentaires, suggestions, questions...) SVP nous contacter!",
                        align = "left"
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