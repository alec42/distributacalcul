#### Loi Normale UI ----
tab_NORM_UI <- tabItem(tabName = "Normale",
        fluidPage(
            useShinyjs(),
            titlePanel("Loi Normale"),
            withMathJax(),
            helpText("\\(X \\sim\\mathcal{N}(\\mu, \\sigma^2)\\)"),
            align = "center"
        ), 
        
        fluidRow(
            {
                ### Paramètres Normale ----
                column(
                    width = 2,
                    box(
                        title = "Paramètres",
                        status = "primary",
                        solidHeader = T,
                        width = NULL,
                        numericInput('muNORM', withMathJax('$$\\mu$$'), value = 0),
                        numericInput('sigmaNORM', '$$\\sigma^2$$', value = 1)
                    ),
                    align = "center"
                )
            },
            
            {
                ### Moments Normale  ----
                column(
                    width = 3,
                    # tags$style(" * {font-size:40000px}"), # grosseur du tezte
                    box(
                        title = "Moments",
                        width = NULL,
                        solidHeader = TRUE,
                        status = "warning",
                        uiOutput("meanNORM"),
                        uiOutput("varNORM")
                    ),
                    
                    box(
                        title = "Autres Moments",
                        width = NULL,
                        solidHeader = TRUE,
                        status = "warning",
                        numericInput('dNORM', withMathJax('$$d$$'), value = 0, width = "20px"),
                        # radioButtons('equalityNORM', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                        uiOutput("EspTronqNORM"),
                        uiOutput("EspLimNORM"),
                        uiOutput("StopLossNORM"),
                        uiOutput("ExcesMoyNORM")#,
                        # align = "center"
                    ),
                    align = "center"
                )
            },
            
            {
                ### Fonctions Normale ----
                column(
                    width = 3,
                    box(
                        title = "Fonctions",
                        width = NULL,
                        solidHeader = TRUE,
                        # tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                        status = "danger", # pour couleur de la boite, diff couleur pour statut
                        numericInput('xNORM', '$$x$$', value = 0),
                        uiOutput("densityNORM"),
                        uiOutput("repartNORM"),
                        plotlyOutput("FxNORM")
                    ),
                    align = "center"
                )
            }, 
            
            {
                ### Mesures de risque Normale  ----
                column(
                    width = 3,
                    boxPlus(
                        title = "Mesure de risques",
                        width = NULL,
                        solidHeader = TRUE,
                        status = "success",
                        # tags$style(" * {font-size:20px }"), # ligne qui augmente la grosseur du texte
                        numericInput('kNORM', '$$\\kappa$$', value = 0.99, step = 0.005),
                        uiOutput("VaRNORM"),
                        uiOutput("TVaRNORM")
                    ),
                    align = "center"
                )
            }
        )
)


#### Loi Gamma UI ####
tab_GAMMA_UI <- tabItem(
        tabName = "gamma",
        fluidPage(
            titlePanel("Loi Gamma"),
            withMathJax(),
            helpText("\\(X \\sim\\mathcal{G}(\\alpha, \\beta)\\)"),
            radioGroupButtons(
                inputId = "distrchoiceEXPOFAM",
                label = "", 
                choices = c("Gamma", "Exponentielle", "Khi carré"),
                status = "primary"
            ),
            align = "center"
        ),
        
        fluidRow(
            {
                ### Paramètres Gamma ----
                column(
                    width = 3,
                    box(
                        title = "Paramètres",
                        status = "primary",
                        solidHeader = T,
                        width = NULL,
                        numericInput('alphaGAMMA', withMathJax('$$\\alpha$$'), value = 2),
                        numericInput('betaGAMMA', '$$\\beta$$', value = 0.1),
                        switchInput(
                            inputId = "distrchoiceGAMMA",
                            onLabel = "Fréquence (Rate)",
                            offLabel = "Échelle (Scale)",
                            value = T
                        ),
                        align = "center"
                    ), 
                    align = "center"
                )
            },
            
            {
                ### Moments Gamma ----
                column(
                    width = 3,
                    tags$style(" * {font-size:20px;}"), # grosseur du tezte
                    box(
                        title = "Moments",
                        width = NULL,
                        solidHeader = TRUE,
                        status = "warning",
                        uiOutput("meanGAMMA"),
                        uiOutput("varianceGAMMA")
                    ),
                    align = "center",
                    
                    box(
                        title = "Autres Moments",
                        width = NULL,
                        solidHeader = TRUE,
                        status = "warning",
                        numericInput('dGAMMA', withMathJax('$$d$$'), value = 0, width = "20px"),
                        # radioButtons('equalityGAMMA', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                        uiOutput("EspTronqGAMMA"),
                        uiOutput("EspLimGAMMA"),
                        uiOutput("StopLossGAMMA"),
                        uiOutput("ExcesMoyGAMMA"),
                        align = "center"
                    ),
                    align = "center"
                )
            }, 
            
            {
                ### Fonctions Gamma ----
                column(
                    width = 3,
                    box(
                        title = "Fonctions",
                        width = NULL,
                        solidHeader = TRUE, # grosseur du tezte
                        status = "danger", # couleur de la boite
                        numericInput('xGAMMA', '$$x$$', value = 10),
                        uiOutput("densityGAMMA"),
                        uiOutput("repartGAMMA"),
                        plotlyOutput("FxGAMMA")
                    ),
                    align = "center"
                )
            },
            
            {
                ### Mesures de risque Gamma ----
                column(
                    width = 3,
                    boxPlus(
                        title = "Mesure de risques",
                        width = NULL,
                        solidHeader = TRUE,
                        status = "success", # grosseur du tezte
                        numericInput('kGAMMA', '$$\\kappa$$', value = 0.99, step = 0.005),
                        uiOutput("VaRGAMMA"),
                        uiOutput("TVaRGAMMA")
                    ),
                    align = "center"
                )
            }
        )
    )


#### Loi binomiale UI #### 
tab_BIN_UI <- tabItem(tabName = "Binomiale",
         fluidPage(
             titlePanel("Loi Binomiale"), withMathJax(), helpText("\\(X \\sim\\mathcal{B}(n, p)\\)"), align = "center"),
         fluidRow(
             column(width = 2, 
                    box(title = "Paramètres", status = "primary", solidHeader = T, width = NULL,
                        numericInput('nBIN', withMathJax('$$n$$'), value = 5, step = 1),
                        numericInput('pBIN', '$$p$$', value = 0.5, min = 0, max = 1, step = 0.05)), align = "center"
                    
             ),
             
             ## Moments
             column(width = 3,
                    tags$style(" * {font-size:20px}"), # grosseur du tezte
                    box(
                        title = "Moments", width = NULL, solidHeader = TRUE, status = "warning",
                        uiOutput("meanBIN"), 
                        uiOutput("varBIN")), 
                    align = "center",
                    
                    box(
                        title = "Autres Moments", width = NULL, solidHeader = TRUE, status = "warning", 
                        numericInput('dBIN', withMathJax('$$d$$'), value = 0, width = "20px"),
                        # radioButtons('equalityNORM', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                        uiOutput("EspTronqBIN"), 
                        uiOutput("EspLimBIN"), 
                        uiOutput("StopLossBIN"), 
                        uiOutput("ExcesMoyBIN"),
                        align = "center")
             ),
             
             ## Fonctions
             column(width = 3,
                    box(
                        title = "Fonctions", width = NULL, solidHeader = TRUE, 
                        tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                        status = "danger", # pour couleur de la boite, diff couleur pour statut
                        numericInput('xBIN', '$$x$$', min = 0, max = 5, value = 0, step = 1), 
                        uiOutput("densityBIN"), 
                        uiOutput("repartBIN"),
                        plotlyOutput("FxBIN")),
                    align = "center"
                    
                    
             ),
             
             column(width =3,
                    boxPlus(
                        title = "Mesure de risques", width = NULL, solidHeader = TRUE, status = "success",
                        tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du texte
                        numericInput('kBIN', '$$\\kappa$$', value = 0.99, step = 0.005), 
                        uiOutput("VaRBIN"), 
                        uiOutput("TVaRBIN")), 
                    
                    align = "center"
             )
         )
)
