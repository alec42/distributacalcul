source('tabs_UI.R')

myUI <- shinyUI({
    dashboardPage(
        skin = "blue", 
        dashboardHeader(
            title = textOutput("main_title"),
            titleWidth = "275px",
            
            # Permet de changer la notation selon le cours
            # Présentemment, change la fonction de survie, la fonction quantile ainsi que le paramètre beta pour lambda de la loi Gamma
            tags$li(class = "dropdown",
                    p("Notation"),
                    align = "center",
            radioGroupButtons(
                inputId = "notation_indicator",
                # label = "Notation",
                choices = c("ACT-2001", 
                            "ACT-1002")
                # ,justified = TRUE     # prends trops d'espace, voir si il y a un meilleur sélecteur.
            )),
            tags$li(class = "dropdown",
                    a(actionButton(
                        inputId = "email1",
                        label = "Nous contacter",
                        icon = icon("envelope", lib = "font-awesome")
                    ),
                    href = "mailto:alec.van-rassel.1@ulaval.ca"
                    )),
            # Sélecteur de langage
            tags$li(class = "dropdown",
                    uiOutput("language_selector_UI")
            )
        ),
        
        # Paneau Latéral
        {
            dashboardSidebar(width = "275px",
                collapsed = F,
                
                sidebarMenu(
                    id = "tabs",
                    menuItem(
                        textOutput("sidebar_title_cont"),
                        # icon = icon("chart-area"),
                        menuSubItem(
                            textOutput("NORM_title"),
                            icon = NULL,
                            # icon = icon("neos"),
                            tabName = "Normale"
                        ),
                        menuSubItem(
                            textOutput("LNORM_title"),
                            icon = NULL,
                            # icon = icon("ruler-combined"),
                            tabName = "Lognormale"
                        ),
                        menuSubItem(
                            textOutput("expo_fam_title"),
                            icon = NULL,
                            # icon = icon("google"),
                            tabName = "gamma"
                        ),
                        menuSubItem(
                            textOutput("WEI_title"),
                            icon = NULL,
                            # icon = icon("wikipedia-w"),
                            tabName = "Weibull"
                        ),
                        menuSubItem(
                            textOutput("PARETO_title"),
                            icon = NULL,
                            # icon = icon("product-hunt"),
                            tabName = "Pareto"
                        ),
                        menuSubItem(
                            textOutput("BURR_title"),
                            icon = NULL,
                            # icon = icon("btc"),
                            tabName = "Burr"
                        ),
                        menuSubItem(
                            textOutput("UNIC_title"),
                            icon = NULL,
                            # icon = icon("fish"),
                            tabName = "UniformeC"
                        ),
                        menuSubItem(
                            textOutput("BETA_title"),
                            icon = NULL,
                            # icon = icon("behance"),
                            tabName = "Beta"
                        ),
                        menuSubItem(
                            textOutput("ERLANG_title"),
                            icon = NULL,
                            # icon = icon("erlang"),
                            tabName = "Erlang"
                        ),
                        menuSubItem(
                            textOutput("LOGLOGIS_title"),
                            icon = NULL,
                            # icon = icon("dolly"),
                            tabName = "LOGLOGIS"
                        ),
                        menuSubItem(
                            textOutput("IG_title"),
                            icon = NULL,
                            # icon = icon("italic"),
                            tabName = "IG"
                        )
                        
                    ),
                    
                    menuItem(
                        textOutput("sidebar_title_disc"),
                        # icon = icon("chart-bar"),
                        menuSubItem(
                            textOutput("UNID_title"),
                            icon = NULL,
                            # icon = icon("fish"),
                            tabName = "UniformeD"
                        ),
                        menuSubItem(
                            textOutput("BIN_title"),
                            icon = NULL,
                            # icon = icon("bold"),
                            tabName = "Binomiale"
                        ),
                        menuSubItem(
                            textOutput("BN_title"),
                            icon = NULL,
                            # icon = icon("minus"),
                            tabName = "Binneg"
                        ),
                        menuSubItem(
                            textOutput("POI_title"),
                            icon = NULL,
                            # icon = icon("fish"),
                            tabName = "Poisson"
                        ),
                        menuSubItem(
                            textOutput("HG_title"),
                            icon = NULL,
                            # icon = icon("hire-a-helper"),
                            tabName = "HG"
                        ),
                        menuSubItem(
                            textOutput("LOGARITHMIQUE_title"),
                            icon = NULL,
                            # icon = icon("yahoo"),
                            tabName = "Logarithmique"
                        )
                    ),
                    menuItem(
                        textOutput("sidebar_title_comp"),
                        # icon = icon("chart-line"),
                        menuSubItem(
                            textOutput("BNCOMP_title"),
                            icon = NULL,
                            # icon = icon("neos"),
                            tabName = "BNCOMP"
                        ),
                        menuSubItem(
                            textOutput("BINCOMP_title"),
                            icon = NULL,
                            # icon = icon("neos"),
                            tabName = "BINCOMP"
                        ),
                        menuSubItem(
                            textOutput("POICOMP_title"),
                            icon = NULL,
                            # icon = icon("neos"),
                            tabName = "PCOMP"
                        )
                    ),
                    menuItem(
                        "Outils",
                        icon = icon("wrench"),
                        menuSubItem("Excès-Moyen", tabName = "excess_mean"),
                        menuSubItem("Approximations", tabName = "approx_tool")
                        # ,menuSubItem("Test d'hypothèse T", href = "https://casertamarco.shinyapps.io/power/"),
                        # menuSubItem("Tests statistiques", tabName = "stat_tests"),
                        # menuSubItem("Copules", tabName = "copulas_tool")
                        
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
                tab_approx_tool,
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