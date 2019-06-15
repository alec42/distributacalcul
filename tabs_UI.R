#### Loi Normale UI ----
tab_NORM_UI <- tabItem(tabName = "Normale",
        fluidRow(
            useShinyjs(), # utilisé to gray out les paramètres de la gamma qu'on désire fixe
            titlePanel("Loi Normale"),
            # withMathJax(),
            helpText("\\(X \\sim\\mathcal{N}(\\mu, \\sigma^2)\\)"),
            align = "center"
        ), 
        fluidRow(
            {
                ### Paramètres Normale ----
                column(
                    width = 2,
                    boxPlus(
                        title = "Paramètres",
                        status = "primary",
                        solidHeader = T,
                        width = NULL,closable = F,
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
                    width = 4,
                    box(
                        title = "Fonctions",
                        width = NULL,
                        solidHeader = TRUE,
                        # tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                        status = "danger", # pour couleur de la boite, diff couleur pour statut
                        numericInput('xNORM', '$$x$$', value = 0),
                        uiOutput("densityNORM"),
                    
                    tabBox(
                        width = NULL,
                        tabPanel("Répartition",
                            uiOutput("repartNORM"),
                            plotlyOutput("FxNORM")
                        ),
                        tabPanel("Survie",
                            uiOutput("survieNORM"),
                            plotlyOutput("SxNORM")
                        )
                        
                    )
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
                        closable = F,
                        status = "success",
                        # tags$style(" * {font-size:20px }"), # ligne qui augmente la grosseur du texte
                        numericInput('kNORM', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                        uiOutput("VaRNORM"),
                        uiOutput("TVaRNORM")
                    ),
                    align = "center"
                )
            }
        )
)


#### Loi Lognormale UI ----
tab_LNORM_UI <- tabItem(tabName = "Lognormale",
                       fluidRow(
                           useShinyjs(), # utilisé to gray out les paramètres de la gamma qu'on désire fixe
                           titlePanel("Loi Lognormale"),
                           # withMathJax(),
                           helpText("\\(X \\sim\\mathcal{LN}(\\mu, \\sigma^2)\\)"),
                           align = "center"
                       ), 
                       fluidRow(
                           {
                               ### Paramètres LNORMale ----
                               column(
                                   width = 2,
                                   boxPlus(
                                       title = "Paramètres",
                                       status = "primary",
                                       solidHeader = T,
                                       width = NULL,closable = F,
                                       numericInput('muLNORM', withMathJax('$$\\mu$$'), value = 0),
                                       numericInput('sigmaLNORM', '$$\\sigma^2$$', value = 1)
                                   ),
                                   align = "center"
                               )
                           },
                           
                           {
                               ### Moments Lognormale  ----
                               column(
                                   width = 3,
                                   # tags$style(" * {font-size:40000px}"), # grosseur du tezte
                                   box(
                                       title = "Moments",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       uiOutput("meanLNORM"),
                                       uiOutput("varLNORM")
                                   ),
                                   
                                   box(
                                       title = "Autres Moments",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       numericInput('dLNORM', withMathJax('$$d$$'), value = 1, width = "20px"),
                                       # radioButtons('equalityLNORM', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                                       uiOutput("EspTronqLNORM"),
                                       uiOutput("EspLimLNORM"),
                                       uiOutput("StopLossLNORM"),
                                       uiOutput("ExcesMoyLNORM"),
                                       uiOutput("kthmomentLNORM")
                                       # align = "center"
                                   ),
                                   align = "center"
                               )
                           },
                           
                           {
                               ### Fonctions Lognormale ----
                               column(
                                   width = 4,
                                   box(
                                       title = "Fonctions",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       # tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                       status = "danger", # pour couleur de la boite, diff couleur pour statut
                                       numericInput('xLNORM', '$$x$$', value = 0),
                                       uiOutput("densityLNORM"),
                                       
                                       tabBox(
                                           width = NULL,
                                           tabPanel("Répartition",
                                                    uiOutput("repartLNORM"),
                                                    plotlyOutput("FxLNORM")
                                           ),
                                           tabPanel("Survie",
                                                    uiOutput("survieLNORM"),
                                                    plotlyOutput("SxLNORM")
                                           )
                                           
                                       )
                                   ),
                                   align = "center"
                               )
                           }, 
                           
                           {
                               ### Mesures de risque Lognormale  ----
                               column(
                                   width = 3,
                                   boxPlus(
                                       title = "Mesure de risques",
                                       width = NULL,
                                       solidHeader = TRUE, 
                                       closable = F,
                                       status = "success",
                                       # tags$style(" * {font-size:20px }"), # ligne qui augmente la grosseur du texte
                                       numericInput('kLNORM', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                                       uiOutput("VaRLNORM"),
                                       uiOutput("TVaRLNORM")
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
            tags$style("#loi_gamma {font-size:30px;}"),
            textOutput("loi_gamma"),
            withMathJax(),
            textOutput("distr_gamma"),
            radioGroupButtons(
                inputId = "distrchoiceEXPOFAM",
                label = "", 
                choices = c("Gamma", "Exponentielle", "Khi carré"),
                status = "primary"
            ),
            align = "center"
        ),
        
        fluidRow(
            
            #### Paramètres Gamma ####
            column(
                width = 3,
                box(
                    title = "Paramètres",
                    status = "primary",
                    solidHeader = T,
                    width = NULL,
                    # numericInput('alphaGAMMA', label = withMathJax('$$\\alpha$$'), value = 2),
                    uiOutput("changingalpha"),
                    # numericInput('betaGAMMA', '$$\\beta$$', value = 0.1),
                    uiOutput("betaGAMMAUI"),
                    switchInput(
                        inputId = "distrchoiceGAMMA",
                        onLabel = "Fréquence (Rate)",
                        offLabel = "Échelle (Scale)",
                        value = T
                    ),
                    align = "center"
                ), 
                align = "center"
            ),
            
            #### Moments Gamma ####
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
                    numericInput('dGAMMA', withMathJax('$$d$$'), value = 0, width = "20px", min = 0),
                    # radioButtons('equalityGAMMA', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                    uiOutput("EspTronqGAMMA"),
                    uiOutput("EspLimGAMMA"),
                    uiOutput("StopLossGAMMA"),
                    uiOutput("ExcesMoyGAMMA"),
                    align = "center"
                ),
                align = "center"
            ),
            
            #### Fonctions Gamma ####
            column(
                width = 3,
                box(
                    title = "Fonctions",
                    width = NULL,
                    solidHeader = TRUE, # grosseur du tezte
                    status = "danger", # couleur de la boite
                    numericInput('xGAMMA', '$$x$$', value = 10, min = 0),
                    uiOutput("densityGAMMA"),
                    tabBox(
                        width = NULL,
                        tabPanel("Répartition",
                                 uiOutput("repartGAMMA"),
                                 plotlyOutput("FxGAMMA")
                        ),
                        tabPanel("Survie",
                                 uiOutput("survieGAMMA"),
                                 plotlyOutput("SxGAMMA")
                        )
                        
                    )
                ),
                align = "center"
            ),
            
            #### Mesures de risque Gamma ####
                column(
                    width = 3,
                    boxPlus(
                        title = "Mesure de risques",
                        width = NULL,
                        solidHeader = TRUE,
                        closable = F,
                        status = "success", # grosseur du tezte
                        numericInput('kGAMMA', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                        uiOutput("VaRGAMMA"),
                        uiOutput("TVaRGAMMA")
                    ),
                    align = "center"
                )
            
            #### ####
        )
    )


#### Loi Pareto UI ----
tab_PARETO_UI <- tabItem(tabName = "Pareto",
                       fluidPage(
                           useShinyjs(), # utilisé to gray out les paramètres de la gamma qu'on désire fixe
                           titlePanel("Loi Pareto"),
                           # withMathJax(),
                           helpText("\\(X \\sim\\mathcal{Pareto}(\\alpha, \\lambda)\\)"),
                           align = "center"
                       ), 
                       
                       fluidRow(
                           {
                               ### Paramètres Pareto ----
                               column(
                                   width = 2,
                                   boxPlus(
                                       title = "Paramètres",
                                       status = "primary",
                                       solidHeader = T,
                                       width = NULL,closable = F,
                                       numericInput('alphaPARETO', withMathJax('$$\\alpha$$'), value = 3, min = 0),
                                       numericInput('lambdaPARETO', '$$\\lambda$$', value = 1, min = 0)
                                   ),
                                   align = "center"
                               )
                           },
                           
                           {
                               ### Moments Pareto  ----
                               column(
                                   width = 3,
                                   box(
                                       title = "Moments",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       uiOutput("meanPARETO"),
                                       uiOutput("varPARETO")
                                   ),
                                   
                                   box(
                                       title = "Autres Moments",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       numericInput('dPARETO', withMathJax('$$d$$'), value = 1, width = "20px", min = 0),
                                       # radioButtons('equalityPARETO', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                                       uiOutput("EspTronqPARETO"),
                                       uiOutput("EspLimPARETO"),
                                       uiOutput("StopLossPARETO"),
                                       uiOutput("ExcesMoyPARETO"),
                                       uiOutput("kthmomentPARETO")
                                       # align = "center"
                                   ),
                                   align = "center"
                               )
                           },
                           
                           {
                               ### Fonctions Pareto ----
                               column(
                                   width = 4,
                                   box(
                                       title = "Fonctions",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       # tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                       status = "danger", # pour couleur de la boite, diff couleur pour statut
                                       numericInput('xPARETO', '$$x$$', value = 0, min = 0),
                                       uiOutput("densityPARETO"),
                                       
                                       tabBox(
                                           width = NULL,
                                           tabPanel("Répartition",
                                                    uiOutput("repartPARETO"),
                                                    plotlyOutput("FxPARETO")
                                           ),
                                           tabPanel("Survie",
                                                    uiOutput("surviePARETO"),
                                                    plotlyOutput("SxPARETO")
                                           )
                                           
                                       )
                                   ),
                                   align = "center"
                               )
                           }, 
                           
                           {
                               ### Mesures de risque Pareto  ----
                               column(
                                   width = 3,
                                   boxPlus(
                                       title = "Mesure de risques",
                                       width = NULL,
                                       solidHeader = TRUE, 
                                       closable = F,
                                       status = "success",
                                       # tags$style(" * {font-size:20px }"), # ligne qui augmente la grosseur du texte
                                       numericInput('kPARETO', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                                       uiOutput("VaRPARETO"),
                                       uiOutput("TVaRPARETO")
                                   ),
                                   align = "center"
                               )
                           }
                       )
)

#### Loi Burr UI ----
tab_BURR_UI <- tabItem(tabName = "Burr",
                         fluidPage(
                             useShinyjs(), # utilisé to gray out les paramètres de la gamma qu'on désire fixe
                             titlePanel("Loi Burr"),
                             # withMathJax(),
                             helpText("\\(X \\sim\\mathcal{Burr}(\\alpha, \\lambda, \\tau)\\)"),
                             align = "center"
                         ), 
                         
                         fluidRow(
                             {
                                 ### Paramètres Burr ----
                                 column(
                                     width = 2,
                                     boxPlus(
                                         title = "Paramètres",
                                         status = "primary",
                                         solidHeader = T,
                                         width = NULL,closable = F,
                                         numericInput('alphaBURR', withMathJax('$$\\alpha$$'), value = 3, step = 1, min = 0),
                                         numericInput('tauBURR', withMathJax('$$\\tau$$'), value = 1, step = 1, min = 0),
                                         numericInput('lambdaBURR', '$$\\lambda$$', value = 1, step = 1, min = 0)
                                     ),
                                     align = "center"
                                 )
                             },
                             
                             {
                                 ### Moments BURR  ----
                                 column(
                                     width = 3,
                                     box(
                                         title = "Moments",
                                         width = NULL,
                                         solidHeader = TRUE,
                                         status = "warning",
                                         uiOutput("meanBURR"),
                                         uiOutput("varBURR")
                                     ),
                                     
                                     box(
                                         title = "Autres Moments",
                                         width = NULL,
                                         solidHeader = TRUE,
                                         status = "warning",
                                         numericInput('dBURR', withMathJax('$$d$$'), value = 0, width = "20px", min = 0, step = 1),
                                         # radioButtons('equalityBURR', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                                         uiOutput("EspTronqBURR"),
                                         uiOutput("EspLimBURR"),
                                         uiOutput("StopLossBURR"),
                                         uiOutput("ExcesMoyBURR"),
                                         uiOutput("kthmomentBURR")
                                         # align = "center"
                                     ),
                                     align = "center"
                                 )
                             },
                             
                             {
                                 ### Fonctions BURR ----
                                 column(
                                     width = 4,
                                     box(
                                         title = "Fonctions",
                                         width = NULL,
                                         solidHeader = TRUE,
                                         # tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                         status = "danger", # pour couleur de la boite, diff couleur pour statut
                                         numericInput('xBURR', '$$x$$', value = 0, min = 0, step = 1),
                                         uiOutput("densityBURR"),
                                         
                                         tabBox(
                                             width = NULL,
                                             tabPanel("Répartition",
                                                      uiOutput("repartBURR"),
                                                      plotlyOutput("FxBURR")
                                             ),
                                             tabPanel("Survie",
                                                      uiOutput("survieBURR"),
                                                      plotlyOutput("SxBURR")
                                             )
                                             
                                         )
                                     ),
                                     align = "center"
                                 )
                             }, 
                             
                             {
                                 ### Mesures de risque BURR  ----
                                 column(
                                     width = 3,
                                     boxPlus(
                                         title = "Mesure de risques",
                                         width = NULL,
                                         solidHeader = TRUE, 
                                         closable = F,
                                         status = "success",
                                         # tags$style(" * {font-size:20px }"), # ligne qui augmente la grosseur du texte
                                         numericInput('kBURR', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                                         uiOutput("VaRBURR"),
                                         uiOutput("TVaRBURR")
                                     ),
                                     align = "center"
                                 )
                             }
                         )
)

#### Loi Weibull UI ----
tab_WEIBULL_UI <- tabItem(tabName = "Weibull",
                         fluidPage(
                             useShinyjs(), # utilisé to gray out les paramètres de la gamma qu'on désire fixe
                             titlePanel("Loi Weibull"),
                             # withMathJax(),
                             helpText("\\(X \\sim\\mathcal{Wei}(\\tau, \\beta)\\)"),
                             align = "center"
                         ), 
                         
                         fluidRow(
                             {
                                 ### Paramètres Weibull ----
                                 column(
                                     width = 2,
                                     boxPlus(
                                         title = "Paramètres",
                                         status = "primary",
                                         solidHeader = T,
                                         width = NULL,closable = F,
                                         numericInput('tauWEIBULL', withMathJax('$$\\tau$$'), value = 2, min = 0),
                                         numericInput('betaWEIBULL', '$$\\beta$$', value = 1, min = 0)
                                     ),
                                     align = "center"
                                 )
                             },
                             
                             {
                                 ### Moments Weibull  ----
                                 column(
                                     width = 3,
                                     box(
                                         title = "Moments",
                                         width = NULL,
                                         solidHeader = TRUE,
                                         status = "warning",
                                         uiOutput("meanWEIBULL"),
                                         uiOutput("varianceWEIBULL")
                                     ),
                                     
                                     box(
                                         title = "Autres Moments",
                                         width = NULL,
                                         solidHeader = TRUE,
                                         status = "warning",
                                         numericInput('dWEIBULL', withMathJax('$$d$$'), value = 1, width = "20px", min = 0),
                                         # radioButtons('equalityWEIBULL', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                                         uiOutput("EspTronqWEIBULL"),
                                         uiOutput("EspLimWEIBULL"),
                                         uiOutput("StopLossWEIBULL"),
                                         uiOutput("ExcesMoyWEIBULL"),
                                         uiOutput("kthmomentWEIBULL")
                                         #,
                                         # align = "center"
                                     ),
                                     align = "center"
                                 )
                             },
                             
                             {
                                 ### Fonctions Weibull ----
                                 column(
                                     width = 4,
                                     box(
                                         title = "Fonctions",
                                         width = NULL,
                                         solidHeader = TRUE,
                                         # tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                         status = "danger", # pour couleur de la boite, diff couleur pour statut
                                         numericInput('xWEIBULL', '$$x$$', value = 0, min = 0),
                                         uiOutput("densityWEIBULL"),
                                         
                                         tabBox(
                                             width = NULL,
                                             tabPanel("Répartition",
                                                      uiOutput("repartWEIBULL"),
                                                      plotlyOutput("FxWEIBULL")
                                             ),
                                             tabPanel("Survie",
                                                      uiOutput("survieWEIBULL"),
                                                      plotlyOutput("SxWEIBULL")
                                             )
                                             
                                         )
                                     ),
                                     align = "center"
                                 )
                             }, 
                             
                             {
                                 ### Mesures de risque Weibull  ----
                                 column(
                                     width = 3,
                                     boxPlus(
                                         title = "Mesure de risques",
                                         width = NULL,
                                         solidHeader = TRUE, 
                                         closable = F,
                                         status = "success",
                                         # tags$style(" * {font-size:20px }"), # ligne qui augmente la grosseur du texte
                                         numericInput('kWEIBULL', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                                         uiOutput("VaRWEIBULL"),
                                         uiOutput("TVaRWEIBULL")
                                     ),
                                     align = "center"
                                 )
                             }
                         )
)

#### Loi Beta UI ----
tab_BETA_UI <- tabItem(tabName = "Beta",
                        fluidRow(
                            useShinyjs(), # utilisé to gray out les paramètres de la gamma qu'on désire fixe
                            titlePanel("Beta"),
                            # withMathJax(),
                            helpText("\\(X \\sim\\mathcal{B}(\\alpha, \\beta)\\)"),
                            align = "center"
                        ), 
                        fluidRow(
                            {
                                ### Paramètres Beta ----
                                column(
                                    width = 2,
                                    boxPlus(
                                        title = "Paramètres",
                                        status = "primary",
                                        solidHeader = T,
                                        width = NULL,closable = F,
                                        numericInput('alphaBETA', withMathJax('$$\\alpha$$'), value = 2, min = 1),
                                        numericInput('betaBETA', '$$\\beta$$', value = 1, min = 1)
                                    ),
                                    align = "center"
                                )
                            },
                            
                            {
                                ### Moments Beta  ----
                                column(
                                    width = 3,
                                    box(
                                        title = "Moments",
                                        width = NULL,
                                        solidHeader = TRUE,
                                        status = "warning",
                                        uiOutput("meanBETA"),
                                        uiOutput("varianceBETA")
                                    ),
                                    
                                    box(
                                        title = "Autres Moments",
                                        width = NULL,
                                        solidHeader = TRUE,
                                        status = "warning",
                                        numericInput('dBETA', withMathJax('$$d$$'), value = .5, width = "20px", min = 0, max = 1, step = .1),
                                        uiOutput("EspLimBETA"),
                                        uiOutput("StopLossBETA"),
                                        uiOutput("ExcesMoyBETA"),
                                        uiOutput("kthmomentBETA"),
                                        numericInput('d2BETA', withMathJax('$$d$$'), value = 1, width = "20px"),
                                        uiOutput("EspTronqBETA")
                                        # align = "center"
                                    ),
                                    align = "center"
                                )
                            },
                            
                            {
                                ### Fonctions Beta ----
                                column(
                                    width = 4,
                                    box(
                                        title = "Fonctions",
                                        width = NULL,
                                        solidHeader = TRUE,
                                        # tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                        status = "danger", # pour couleur de la boite, diff couleur pour statut
                                        numericInput('xBETA', '$$x$$', value = 0.5, min = 0, max = 1, step = .1),
                                        uiOutput("densityBETA"),
                                        
                                        tabBox(
                                            width = NULL,
                                            tabPanel("Répartition",
                                                     uiOutput("repartBETA"),
                                                     plotlyOutput("FxBETA")
                                            ),
                                            tabPanel("Survie",
                                                     uiOutput("survieBETA"),
                                                     plotlyOutput("SxBETA")
                                            )
                                            
                                        )
                                    ),
                                    align = "center"
                                )
                            }, 
                            
                            {
                                ### Mesures de risque Beta  ----
                                column(
                                    width = 3,
                                    boxPlus(
                                        title = "Mesure de risques",
                                        width = NULL,
                                        solidHeader = TRUE, 
                                        closable = F,
                                        status = "success",
                                        # tags$style(" * {font-size:20px }"), # ligne qui augmente la grosseur du texte
                                        numericInput('kBETA', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                                        uiOutput("VaRBETA"),
                                        uiOutput("TVaRBETA")
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
                    align = "center"
                    # ,
                    
                    # box(
                    #     title = "Fonctions g", width = NULL, solidHeader = TRUE, status = "warning", 
                    #     numericInput('dBIN', withMathJax('$$d$$'), value = 0, width = "20px"),
                    #     # radioButtons('equalityNORM', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                    #     uiOutput("EspTronqBIN"), 
                    #     uiOutput("EspLimBIN"), 
                    #     uiOutput("StopLossBIN"), 
                    #     uiOutput("ExcesMoyBIN"),
                    #     align = "center")
             ),
             
             ## Fonctions
             column(width = 3,
                    box(
                        title = "Fonctions", width = NULL, solidHeader = TRUE, 
                        tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                        status = "danger", # pour couleur de la boite, diff couleur pour statut
                        numericInput('xBIN', '$$x$$', min = 0, max = 5, value = 0, step = 1), 
                        uiOutput("densityBIN"), 
                        tabBox(
                            width = NULL,
                            tabPanel("Répartition",
                                     uiOutput("repartBIN")
                                     # ,
                                     # plotlyOutput("FxBIN")
                            ),
                            tabPanel("Survie",
                                     uiOutput("survieBIN")
                                     # ,
                                     # plotlyOutput("SxBIN")
                            )
                            
                        )
                    ),
                    align = "center"
                    
                    
             )
             ,

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

#### Loi binomiale Négative UI #### 
tab_BN_UI <- tabItem(
    tabName = "Binomiale_Négative",
    
    fluidPage(
        tags$style("#loi_BN {font-size:30px;}"),
        textOutput("loi_BN"),
        withMathJax(),
        textOutput("distr_BN"),
        radioGroupButtons(
            inputId = "distrchoiceBNFAM",
            label = "", 
            choices = c("Binomiale Négative", "Géometrique"),
            status = "primary"
        ),
        align = "center"
    ),
    
    fluidRow(
        
        #### Paramètres ####
        column(
            width = 2, 
            box(
                title = "Paramètres", 
                status = "primary", 
                solidHeader = T, 
                width = NULL,
                # numericInput('rBN', withMathJax('$$r$$'), value = 2, step = 1),
                uiOutput("changingrBN"),
                # numericInput('qBN', '$$q$$', value = 0.5, min = 0, max = 1, step = 0.05)), align = "center"
                uiOutput("changingqBN"),
                switchInput(
                    inputId = "distrchoiceqBN",
                    onLabel = "Probabilité (q)",
                    offLabel = "Échelle ($$\\beta$$)",
                    value = T
                ),
                align = "center"
            ), 
            align = "center"
        ),
        
        
        #### Moments BN ####
        column(
            width = 3,
            tags$style(" * {font-size:20px}"), # grosseur du tezte
            box(
                title = "Moments", width = NULL, solidHeader = TRUE, status = "warning",
                uiOutput("meanBN"), 
                uiOutput("varBN")), 
            align = "center"
            # ,
               # box(
               #     title = "Autres Moments", 
               #     width = NULL, solidHeader = TRUE, status = "warning", 
               #     numericInput('dBN', withMathJax('$$d$$'), value = 0, width = "20px"),
               #     # radioButtons('equalityNORM', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
               #     uiOutput("EspTronqBN"), 
               #     uiOutput("EspLimBN"), 
               #     uiOutput("StopLossBN"), 
               #     uiOutput("ExcesMoyBN"),
               #     align = "center")
        ),
        
        #### Fonctions BN ####
        column(width = 3,
               box(
                   title = "Fonctions", width = NULL, solidHeader = TRUE, 
                   status = "danger", # pour couleur de la boite, diff couleur pour statut
                   numericInput('xBN', '$$x$$', min = 0, value = 0, step = 1), 
                   uiOutput("densityBN"), 
                   tabBox(
                       width = NULL,
                       tabPanel("Répartition",
                                uiOutput("repartBN")
                                # ,
                                # plotlyOutput("FxBN")
                       ),
                       tabPanel("Survie",
                                uiOutput("survieBN")
                                # ,
                                # plotlyOutput("SxBN")
                       )
                       
                   )
               ),
               align = "center"
        )
        # ,
        #### Mesures de risque BN ####
        # column(width = 3,
        #        boxPlus(
        #            title = "Mesure de risques", 
        #            width = NULL, 
        #            solidHeader = TRUE,
        #            closable = F,
        #            status = "success",
        #            tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du texte
        #            numericInput('kBN', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
        #            uiOutput("VaRBN"),
        #            uiOutput("TVaRBN")
        #        ),
        #        align = "center"
        # )
        
        #### ####
    )
)

#### Loi Poisson UI #### 
tab_POI_UI <- tabItem(tabName = "Poisson",
                      fluidPage(
                          titlePanel("Loi Poisson"), withMathJax(), helpText("\\(X \\sim\\mathcal{P}(\\lambda)\\)"), align = "center"),
                      fluidRow(
                          column(width = 2,
                                 box(title = "Paramètres", status = "primary", solidHeader = T, width = NULL,
                                     numericInput('lamPOI', '$$\\lambda$$', value = 1, min = 0, step = 1)), align = "center"
                                 
                          ),
                          
                          ## Moments
                          column(width = 3,
                                 tags$style(" * {font-size:20px}"), # grosseur du tezte
                                 box(
                                     title = "Moments", width = NULL, solidHeader = TRUE, status = "warning",
                                     uiOutput("meanPOI"),
                                     uiOutput("varPOI")),
                                 align = "center"
                                 # ,
                                 
                                 # box(
                                 #     title = "Fonctions g", width = NULL, solidHeader = TRUE, status = "warning",
                                 #     numericInput('dPOI', withMathJax('$$d$$'), value = 0, width = "20px"),
                                 #     # radioButtons('equalityNORM', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                                 #     uiOutput("EspTronqPOI"),
                                 #     uiOutput("EspLimPOI"),
                                 #     uiOutput("StopLossPOI"),
                                 #     uiOutput("ExcesMoyPOI"),
                                 #     align = "center")
                          ),
                          
                          ## Fonctions
                          column(width = 3,
                                 box(
                                     title = "Fonctions", width = NULL, solidHeader = TRUE,
                                     tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                     status = "danger", # pour couleur de la boite, diff couleur pour statut
                                     numericInput('xPOI', '$$x$$', min = 0, value = 0, step = 1),
                                     uiOutput("densityPOI"),
                                     
                                     tabBox(
                                         width = NULL,
                                         tabPanel("Répartition",
                                                  uiOutput("repartPOI")
                                                  # ,
                                                  # plotlyOutput("FxPOI")
                                         ),
                                         tabPanel("Survie",
                                                  uiOutput("surviePOI")
                                                  # ,
                                                  # plotlyOutput("SxBETA")
                                         )
                                         
                                     )
                                 ),
                                 align = "center"
                                     
                                 ),
                                 
                                 column(width =3,
                                        boxPlus(
                                            title = "Mesure de risques", width = NULL, solidHeader = TRUE, status = "success",
                                            tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du texte
                                            numericInput('kPOI', '$$\\kappa$$', value = 0.99, step = 0.005),
                                            uiOutput("VaRPOI"),
                                            uiOutput("TVaRPOI")),
                                        
                                        align = "center"
                                 )
                          )
                      )
