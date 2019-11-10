source('tabs/tabs_UI.R')
source('tabs/LLN_tool.R')
source('code/modules.R')

## Fichier CSS
fichiers_CSS <- tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/principal.css")
)
largeur_barre_menu <- 300

myUI <- shinyUI({
    dashboardPage(
        skin = "blue", 
        header = dashboardHeaderPlus(
            
            # fixed = T,
            title = textOutput("main_title"),
            titleWidth = largeur_barre_menu,
            .list = list(
                # Sélecteur de langage
                tags$li(class = "dropdown",
                        uiOutput("language_selector_UI")
                ),
                # Permet de changer la notation selon le cours
                # Présentemment, change la fonction de survie, la fonction quantile ainsi que le paramètre beta pour lambda de la loi Gamma
                tags$li(class = "dropdown",
                        textOutput("Notation_transl"),
                        align = "center",
                        uiOutput("notation_indicator_UI")
                ),
                tags$li(class = "dropdown",
                        uiOutput("email1_UI")
                )
            )
        ),
        # Paneau Latéral
        
        sidebar = dashboardSidebar(
            width = largeur_barre_menu,
            # collapsed = F,
            sidebarMenu(
            shinyjs::useShinyjs(),
                id = "tabs",
                menuItemOutput("sidebar_output_cont"),
                menuItemOutput("sidebar_output_disc"),
                menuItemOutput("sidebar_output_comp"),
                menuItemOutput("sidebar_output_tools"),
                menuItemOutput("sidebar_output_about")
            ## Attempt to hide panel and show based on code
            ## Not yet functional
            # ,textInput(inputId = "accessToken", 
            #                    # label = "Access Code",
            #                    label = "",
            #                    placeholder = "Access code hidden items"
            #                    )
            )
        ), 
        # corps de la page
        body = dashboardBody(
            
            tags$head(
                
                tags$style(
                    type = "text/css",
                    "label { 
                        display: table-cell; 
                        text-align: center; 
                        vertical-align: center; 
                        width: 50px; 
                        font-size: 13pt
                    } 
                    .form-group { 
                        display: table-row;
                    }"
                )
            ), 
            fichiers_CSS,
            
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
                tab_LLN_tool,
                tab_MGF_tool,
                tab_stat_tests,
                tab_copulas_tool,
            
            ## À propos ----
                
                tabItem(
                    tabName = "about",
                    h2("Nous contacter"),
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
                h1("Distributacalcul"),
                # h3("À propos du projet"),
                textOutput("title_proj_about_transl"),
                align = "center",
                
                accordion(
                    accordionItem(
                        id = 1,
                        title = textOutput("goal_transl"),
                        # title = textOutput("proj_goal_transl"),
                        color = "danger",
                        collapsible = F,
                        collapsed = F,
                        textOutput("goal_line1_transl"),
                        br(),
                        textOutput("goal_line2_transl"),
                        br(),
                        textOutput("goal_line3_transl"),
                        br(),
                        textOutput("goal_line4_transl"),
                        align = "left"
                    ),
                    accordionItem(
                        id = 2,
                        title = textOutput("status_transl"),
                        color = "warning",
                        collapsed = F,
                        textOutput("status_line1_transl"),
                        align = "left")
                    ,
                    accordionItem(
                        id = 3,
                        title = textOutput("links_transl"),
                        color = "warning",
                        collapsible = F,
                        collapsed = F,
                        textOutput("links_line1_transl"),
                        br(),
                        textOutput("links_line2_transl"),
                        br(),
                        textOutput("links_line3_transl"),
                        align = "left"
                    )
                )
                ,fixedPanel(
                    a(actionButton(inputId = "email1", 
                                   label = "Nous contacter", 
                                   icon = icon("envelope", 
                                               lib = "font-awesome")),
                      href="mailto:alec.van-rassel.1@ulaval.ca"),
                    right = 40,
                    bottom = 40
                )

                
                
            )
            
            )
        )
        

    )
})