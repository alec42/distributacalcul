#### Loi Normale UI ----
tab_NORM_UI <- tabItem(tabName = "Normale",
        fluidRow(
            useShinyjs(), # utilisé to gray out les paramètres de la gamma qu'on désire fixe
            # titlePanel("Loi Normale"),
            titlePanel(tags$a("Loi Normal",href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Normal")),
            # withMathJax(),
            helpText("\\(X \\sim\\mathcal{Normale} \\ (\\mu, \\sigma^2)\\)"),
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
                        uiOutput("TVaRNORM"),
                        plotlyOutput("QxNORM")
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
                           # titlePanel("Loi Lognormale"),
                           titlePanel(tags$a("Loi Lognormale",href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Lognormale")),
                           # withMathJax(),
                           helpText("\\(X \\sim\\mathcal{Lognormale} \\ (\\mu, \\sigma^2)\\)"),
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
{

tab_GAMMA_UI <- tabItem(
        tabName = "gamma",
        fluidPage(
            tags$style("#loi_gamma {font-size:30px;}"),
            uiOutput("loi_gamma"),
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
}

#### Loi Pareto UI ----
tab_PARETO_UI <- tabItem(tabName = "Pareto",
                       fluidPage(
                           useShinyjs(), # utilisé to gray out les paramètres de la gamma qu'on désire fixe
                           titlePanel("Loi Pareto"),
                           # withMathJax(),
                           helpText("\\(X \\sim\\mathcal{Pareto} \\ (\\alpha, \\lambda)\\)"),
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
                             helpText("\\(X \\sim\\mathcal{Burr} \\ (\\alpha, \\lambda, \\tau)\\)"),
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
                             helpText("\\(X \\sim\\mathcal{Weibull} \\ (\\tau, \\beta)\\)"),
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

#### Loi Inverse Gaussienne UI ----
tab_IG_UI <- tabItem(tabName = "IG",
                          fluidPage(
                              useShinyjs(), # utilisé to gray out les paramètres de la gamma qu'on désire fixe
                              titlePanel("Loi Inverse Gaussienne"),
                              # withMathJax(),
                              helpText("\\(X \\sim\\mathcal{IG} \\ (\\mu, \\beta)\\)"),
                              align = "center"
                          ),

                          fluidRow(
                              {
                                  ### Paramètres IG ----
                                  column(
                                      width = 2,
                                      boxPlus(
                                          title = "Paramètres",
                                          status = "primary",
                                          solidHeader = T,
                                          width = NULL,closable = F,
                                          numericInput('muIG', withMathJax('$$\\mu$$'), value = 2, min = 0),
                                          numericInput('betaIG', '$$\\beta$$', value = 1, min = 0)
                                      ),
                                      align = "center"
                                  )
                              },

                              {
                                  ### Moments IG  ----
                                  column(
                                      width = 3,
                                      box(
                                          title = "Moments",
                                          width = NULL,
                                          solidHeader = TRUE,
                                          status = "warning",
                                          uiOutput("meanIG"),
                                          uiOutput("varianceIG")
                                      ),

                                      box(
                                          title = "Autres Moments",
                                          width = NULL,
                                          solidHeader = TRUE,
                                          status = "warning",
                                          numericInput('dIG', withMathJax('$$d$$'), value = 1, width = "20px", min = 0),
                                          # radioButtons('equalityIG', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                                          uiOutput("EspTronqIG"),
                                          uiOutput("EspLimIG"),
                                          uiOutput("StopLossIG")
                                          # ,uiOutput("ExcesMoyIG"),
                                          # uiOutput("kthmomentIG")
                                          #,
                                          # align = "center"
                                      ),
                                      align = "center"
                                  )
                              },

                              {
                                  ### Fonctions IG ----
                                  column(
                                      width = 4,
                                      box(
                                          title = "Fonctions",
                                          width = NULL,
                                          solidHeader = TRUE,
                                          # tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                          status = "danger", # pour couleur de la boite, diff couleur pour statut
                                          numericInput('xIG', '$$x$$', value = 5, min = 0),
                                          uiOutput("densityIG"),

                                          tabBox(
                                              width = NULL,
                                              tabPanel("Répartition",
                                                       uiOutput("repartIG"),
                                                       plotlyOutput("FxIG")
                                              ),
                                              tabPanel("Survie",
                                                       uiOutput("survieIG"),
                                                       plotlyOutput("SxIG")
                                              )

                                          )
                                      ),
                                      align = "center"
                                  )
                              },

                              {
                                  ### Mesures de risque IG  ----
                                  column(
                                      width = 3,
                                      boxPlus(
                                          title = "Mesure de risques",
                                          width = NULL,
                                          solidHeader = TRUE,
                                          closable = F,
                                          status = "success",
                                          # tags$style(" * {font-size:20px }"), # ligne qui augmente la grosseur du texte
                                          numericInput('kIG', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                                          uiOutput("VaRIG"),
                                          uiOutput("TVaRIG")
                                      ),
                                      align = "center"
                                  )
                              }
                          )
)

#### Loi Uniforme Continue UI ####
tab_UNIC_UI <- tabItem(tabName = "UniformeC",
                       fluidPage(
                           # titlePanel("Loi Uniforme"),
                           titlePanel(tags$a("Loi Uniforme",href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Uniforme-Continue")),
                           withMathJax(),
                           helpText("\\(X \\sim\\mathcal{U} \\ (a, b)\\)"),
                           align = "center"
                       ),
                       
                       fluidRow(
                           column(width = 3,
                                  box(title = "Paramètres",
                                      status = "primary",
                                      solidHeader = T,
                                      width = NULL,
                                      numericInput('aUNIC', '$$a$$', value = 1, min = 0, step = 1),
                                      numericInput('bUNIC', '$$b$$', value = 2, min = 0, step = 1)
                                  ),
                                  align = "center"
                           ),
                           
                           ## Moments
                           column(width = 3,
                                  tags$style(" * {font-size:20px}"), # grosseur du tezte
                                  box(
                                      title = "Moments", width = NULL, solidHeader = TRUE, status = "warning",
                                      uiOutput("meanUNIC"),
                                      uiOutput("varUNIC")
                                  ),
                                  align = "center"
                                  # ,
                                  
                                  # box(
                                  #     title = "Fonctions g", width = NULL, solidHeader = TRUE, status = "warning",
                                  #     numericInput('dUNIC', withMathJax('$$d$$'), value = 0, width = "20px"),
                                  #     # radioButtons('equalityNORM', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                                  #     uiOutput("EspTronqUNIC"),
                                  #     uiOutput("EspLimUNIC"),
                                  #     uiOutput("StopLossUNIC"),
                                  #     uiOutput("ExcesMoyUNIC"),
                                  #     align = "center")
                           ),
                           
                           ## Fonctions
                           column(width = 3,
                                  box(
                                      title = "Fonctions",
                                      width = NULL,
                                      solidHeader = TRUE,
                                      tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                      status = "danger", # pour couleur de la boite, diff couleur pour statut
                                      numericInput('xUNIC', '$$x$$', min = 0, value = 0, step = 1),
                                      uiOutput("densityUNIC"),
                                      
                                      tabBox(
                                          width = NULL,
                                          tabPanel("Répartition",
                                                   uiOutput("repartUNIC")
                                                   # ,
                                                   # plotlyOutput("FxUNIC")
                                          ),
                                          tabPanel("Survie",
                                                   uiOutput("survieUNIC")
                                                   # ,
                                                   # plotlyOutput("SxBETA")
                                          )
                                          
                                      )
                                  ),
                                  align = "center"
                                  
                           ),

                           column(width = 3,
                                  boxPlus(
                                      title = "Mesure de risques",
                                      width = NULL,
                                      solidHeader = TRUE,
                                      status = "success",
                                      closable = F,
                                      tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du texte
                                      numericInput('kUNIC', '$$\\kappa$$', value = 0.99, step = 0.005),
                                      uiOutput("VaRUNIC")
                                      # ,uiOutput("TVaRUNIC")
                                      ),

                                  align = "center"
                           )
                       )
)

#### Loi Beta UI ----
tab_BETA_UI <- tabItem(tabName = "Beta",
                        fluidRow(
                            useShinyjs(), # utilisé to gray out les paramètres de la gamma qu'on désire fixe
                            titlePanel("Beta"),
                            # withMathJax(),
                            helpText("\\(X \\sim\\mathcal{Beta} \\ (\\alpha, \\beta)\\)"),
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

#### Loi Erlang UI ----
tab_ERLANG_UI <- tabItem(tabName = "Erlang",
                         fluidPage(
                             useShinyjs(), # utilisé to gray out les paramètres de la gamma qu'on désire fixe
                             titlePanel("Loi Erlang"),
                             # withMathJax(),
                             helpText("\\(X \\sim\\mathcal{Erlang} \\ (n, \\beta)\\)"),
                             align = "center"
                         ),

                       fluidRow(
                           {
                               ### Paramètres ERLANG ----
                               column(
                                   width = 3,
                                   boxPlus(
                                       title = "Paramètres",
                                       status = "primary",
                                       solidHeader = T,
                                       width = NULL,
                                       closable = F,
                                       numericInput('nERLANG',
                                                    withMathJax('$$n$$'),
                                                    value = 2, step = 1, min = 0),
                                       numericInput('betaERLANG',
                                                    withMathJax('$$\\beta$$'),
                                                    value = 1, step = 1, min = 0)
                                   ),
                                   align = "center"
                               )
                           },

                           {
                               ### Moments Erlang  ----
                               column(
                                   width = 4,
                                   box(
                                       title = "Moments",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       uiOutput("meanERLANG"),
                                       uiOutput("varERLANG")
                                   ),

                                   box(
                                       title = "Autres Moments",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       numericInput('dERLANG',
                                                    withMathJax('$$d$$'),
                                                    value = 0, width = "20px", min = 0, step = 1),
                                       # radioButtons('equalityERLANG', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                                       uiOutput("EspTronqERLANG"),
                                       uiOutput("EspLimERLANG"),
                                       uiOutput("StopLossERLANG"),
                                       uiOutput("ExcesMoyERLANG"),
                                       uiOutput("kthmomentERLANG")
                                       # align = "center"
                                   ),
                                   align = "center"
                               )
                           },

                           {
                               ### Fonctions ERLANG ----
                               column(
                                   width = 5,
                                   box(
                                       title = "Fonctions",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       # tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                       status = "danger", # pour couleur de la boite, diff couleur pour statut
                                       numericInput('xERLANG', '$$x$$', value = 0, min = 0, step = 1),
                                       uiOutput("densityERLANG"),

                                       tabBox(
                                           width = NULL,
                                           tabPanel("Répartition",
                                                    uiOutput("repartERLANG")
                                                    # ,plotlyOutput("FxERLANG")
                                           ),
                                           tabPanel("Survie",
                                                    uiOutput("survieERLANG")
                                                    # ,plotlyOutput("SxERLANG")
                                           )

                                       )
                                   ),
                                   align = "center"
                               )
                           },

                           {
                               ### Mesures de risque ERLANG  ----
                               # column(
                               #     width = 3,
                               #     boxPlus(
                               #         title = "Mesure de risques",
                               #         width = NULL,
                               #         solidHeader = TRUE,
                               #         closable = F,
                               #         status = "success",
                               #         # tags$style(" * {font-size:20px }"), # ligne qui augmente la grosseur du texte
                               #         numericInput('kERLANG', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                               #         uiOutput("VaRERLANG"),
                               #         uiOutput("TVaRERLANG")
                               #     ),
                               #     align = "center"
                               # )
                           }
                       )
)

#### Loi Log-logistique UI ----
tab_LOGLOGIS_UI <- tabItem(tabName = "LOGLOGIS",
                       fluidPage(
                           useShinyjs(), # utilisé to gray out les paramètres de la gamma qu'on désire fixe
                           titlePanel("Loi Log-logistique"),
                           # withMathJax(),
                           helpText("\\(X \\sim\\mathcal{LL} \\ (\\lambda, \\tau)\\)"),
                           align = "center"
                       ),

                       fluidRow(
                           {
                               ### Paramètres LOGLOGIS ----
                               column(
                                   width = 2,
                                   boxPlus(
                                       title = "Paramètres",
                                       status = "primary",
                                       solidHeader = T,
                                       width = NULL,closable = F,
                                       numericInput('tauLOGLOGIS',
                                                    withMathJax('$$\\tau$$'),
                                                    value = 4, step = 1, min = 0),
                                       numericInput('lambdaLOGLOGIS',
                                                    '$$\\lambda$$',
                                                    value = 3, step = 1, min = 0)
                                   ),
                                   align = "center"
                               )
                           },

                           {
                               ### Moments LOGLOGIS  ----
                               column(
                                   width = 3,
                                   box(
                                       title = "Moments",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       uiOutput("meanLOGLOGIS"),
                                       uiOutput("varLOGLOGIS")
                                   ),

                                   box(
                                       title = "Autres Moments",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       numericInput('dLOGLOGIS', withMathJax('$$d$$'), value = 0, width = "20px", min = 0, step = 1),
                                       # radioButtons('equalityLOGLOGIS', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                                       uiOutput("EspTronqLOGLOGIS"),
                                       uiOutput("EspLimLOGLOGIS"),
                                       uiOutput("StopLossLOGLOGIS"),
                                       uiOutput("ExcesMoyLOGLOGIS"),
                                       uiOutput("kthmomentLOGLOGIS")
                                       # align = "center"
                                   ),
                                   align = "center"
                               )
                           },

                           {
                               ### Fonctions LOGLOGIS ----
                               column(
                                   width = 4,
                                   box(
                                       title = "Fonctions",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       # tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                       status = "danger", # pour couleur de la boite, diff couleur pour statut
                                       numericInput('xLOGLOGIS', '$$x$$', value = 0, min = 0, step = 1),
                                       uiOutput("densityLOGLOGIS"),

                                       tabBox(
                                           width = NULL,
                                           tabPanel("Répartition",
                                                    uiOutput("repartLOGLOGIS"),
                                                    plotlyOutput("FxLOGLOGIS")
                                           ),
                                           tabPanel("Survie",
                                                    uiOutput("survieLOGLOGIS"),
                                                    plotlyOutput("SxLOGLOGIS")
                                           )

                                       )
                                   ),
                                   align = "center"
                               )
                           },

                           {
                               ### Mesures de risque LOGLOGIS  ----
                               column(
                                   width = 3,
                                   boxPlus(
                                       title = "Mesure de risques",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       closable = F,
                                       status = "success",
                                       # tags$style(" * {font-size:20px }"), # ligne qui augmente la grosseur du texte
                                       numericInput('kLOGLOGIS', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                                       uiOutput("VaRLOGLOGIS"),
                                       uiOutput("TVaRLOGLOGIS")
                                   ),
                                   align = "center"
                               )
                           }
                       )
)

#### Loi binomiale UI ####
tab_BIN_UI <- tabItem(tabName = "Binomiale",

         fluidPage(
             tags$style("#loi_BIN {font-size:30px;}"),
             uiOutput("loi_BIN"),
             withMathJax(),
             textOutput("distr_BIN"),
             radioGroupButtons(
                 inputId = "distrchoiceBINFAM",
                 label = "",
                 choices = c("Binomiale", "Bernoulli"),
                 status = "primary"
             ),
             align = "center"
         ),

         fluidRow(
             column(width = 3,
                    box(title = "Paramètres", status = "primary", solidHeader = T, width = NULL,
                        numericInput('nBIN', withMathJax('$$n$$'), value = 5, step = 1, min = 0),
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

             column(width = 3,
                    boxPlus(
                        title = "Mesure de risques",
                        width = NULL,
                        solidHeader = TRUE,
                        status = "success",
                        closable = F,
                        tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du texte
                        numericInput('kBIN', '$$\\kappa$$', value = 0.99, step = 0.005),
                        uiOutput("VaRBIN"),
                        uiOutput("TVaRBIN")),

                    align = "center"
             )
         )
)

#### Loi binomiale Négative UI ####
{
tab_BN_UI <- tabItem(
    tabName = "Binneg",

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
            width = 4,
            box(
                title = "Paramètres",
                status = "primary",
                solidHeader = T,
                width = NULL,
                # numericInput('rBN', withMathJax('$$r$$'), value = 2, step = 1),
                uiOutput("changingrBN"),
                uiOutput("changingqBN"),
                # numericInput('qBN', '$$q$$', value = 0.5, min = 0, max = 1, step = 0.05)), align = "center"

                switchInput(labelWidth = "10px",handleWidth = "400px",
                    inputId = "distrchoiceqBN",
                    onLabel = 'q',
                    offLabel = "&beta;", size = "large",
                    value = T
                )
            ),
            box(
                title = "Plus d'information",
                status = "primary",
                solidHeader = T,
                collapsed = T,
                collapsible = T,
                width = NULL,
                helpText("Lien entre les paramétrisations
                         $$\\beta = \\frac{1 - q}{q}$$
                         $$q = \\frac{1}{1 + \\beta}$$"),
                align = "center"
            ),
            align = "center"
        ),


        #### Moments BN ####
        column(
            width = 4,
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
        column(width = 4,
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
}
#### Loi Poisson UI ####
tab_POI_UI <- tabItem(tabName = "Poisson",
                      fluidPage(
                          titlePanel("Loi Poisson"),
                          withMathJax(),
                          helpText("\\(X \\sim\\mathcal{Poisson} \\ (\\lambda)\\)"),
                          align = "center"
                      ),

                      fluidRow(
                          column(width = 3,
                                 box(title = "Paramètres",
                                     status = "primary",
                                     solidHeader = T,
                                     width = NULL,
                                     numericInput('lamPOI', '$$\\lambda$$', value = 1, min = 0, step = 1)
                                 ),
                                 align = "center"
                          ),

                          ## Moments
                          column(width = 3,
                                 tags$style(" * {font-size:20px}"), # grosseur du tezte
                                 box(
                                     title = "Moments", width = NULL, solidHeader = TRUE, status = "warning",
                                     uiOutput("meanPOI"),
                                     uiOutput("varPOI")
                                 ),
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
                                     title = "Fonctions",
                                     width = NULL,
                                     solidHeader = TRUE,
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

                          column(width = 3,
                                 boxPlus(
                                     title = "Mesure de risques",
                                     width = NULL,
                                     solidHeader = TRUE,
                                     status = "success",
                                     closable = F,
                                     tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du texte
                                     numericInput('kPOI', '$$\\kappa$$', value = 0.99, step = 0.005),
                                     uiOutput("VaRPOI"),
                                     uiOutput("TVaRPOI")),

                                 align = "center"
                          )
                      )
)

#### Loi Hypergéométrique UI ####
tab_HG_UI <- tabItem(tabName = "HG",
                      fluidPage(
                          titlePanel("Loi Hypergéométrique"),
                          withMathJax(),
                          helpText("\\(X \\sim\\mathcal{HyperGéo} \\ (N, n, m)\\)"),
                          align = "center"
                      ),

                      fluidRow(
                          column(width = 4,
                                 box(title = "Paramètres",
                                     status = "primary",
                                     solidHeader = T,
                                     width = NULL,
                                     numericInput('grosNHG', '$$N$$', value = 3, min = 0, step = 1),
                                     numericInput('petitNHG', '$$n$$', value = 2, min = 0, step = 1),
                                     numericInput('mHG', '$$m$$', value = 1, min = 0, step = 1)
                                 ),
                                 align = "center"
                          ),

                          ## Moments
                          column(width = 4,
                                 tags$style(" * {font-size:20px}"), # grosseur du tezte
                                 box(
                                     title = "Moments", width = NULL, solidHeader = TRUE, status = "warning",
                                     uiOutput("meanHG"),
                                     uiOutput("varHG")
                                 ),
                                 align = "center"
                                 # ,

                                 # box(
                                 #     title = "Fonctions g", width = NULL, solidHeader = TRUE, status = "warning",
                                 #     numericInput('dHG', withMathJax('$$d$$'), value = 0, width = "20px"),
                                 #     # radioButtons('equalityHG', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                                 #     uiOutput("EspTronqHG"),
                                 #     uiOutput("EspLimHG"),
                                 #     uiOutput("StopLossHG"),
                                 #     uiOutput("ExcesMoyHG"),
                                 #     align = "center")
                          ),

                          ## Fonctions
                          column(width = 4,
                                 box(
                                     title = "Fonctions",
                                     width = NULL,
                                     solidHeader = TRUE,
                                     tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                     status = "danger", # pour couleur de la boite, diff couleur pour statut
                                     numericInput('xHG', '$$x$$', min = 0, value = 0, step = 1),
                                     uiOutput("densityHG"),

                                     tabBox(
                                         width = NULL,
                                         tabPanel("Répartition",
                                                  uiOutput("repartHG")
                                                  # ,
                                                  # plotlyOutput("FxHG")
                                         ),
                                         tabPanel("Survie",
                                                  uiOutput("survieHG")
                                                  # ,
                                                  # plotlyOutput("SxBETA")
                                         )

                                     )
                                 ),
                                 align = "center"

                          )
                          # ,
                          #
                          # column(width = 3,
                          #        boxPlus(
                          #            title = "Mesure de risques",
                          #            width = NULL,
                          #            solidHeader = TRUE,
                          #            status = "success",
                          #            closable = F,
                          #            tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du texte
                          #            numericInput('kHG', '$$\\kappa$$', value = 0.99, step = 0.005),
                          #            uiOutput("VaRHG"),
                          #            uiOutput("TVaRHG")),
                          #
                          #        align = "center"
                          # )
                      )
)

#### Loi Logarithmique UI ####
tab_LOGARITHMIQUE_UI <- tabItem(tabName = "Logarithmique",
                     fluidPage(
                         titlePanel("Loi Logarithmique"),
                         withMathJax(),
                         helpText("\\(X \\sim\\mathcal{Logarithmique} \\ (\\gamma)\\)"),
                         align = "center"
                     ),

                     fluidRow(
                         column(width = 3,
                                box(title = "Paramètres",
                                    status = "primary",
                                    solidHeader = T,
                                    width = NULL,
                                    numericInput('gammaLOGARITHMIQUE',
                                                 '$$\\gamma$$',
                                                 value = .2, min = 0, max = 1, step = .1)
                                ),
                                align = "center"
                         ),

                         ## Moments
                         column(width = 3,
                                tags$style(" * {font-size:20px}"), # grosseur du tezte
                                box(
                                    title = "Moments", width = NULL, solidHeader = TRUE, status = "warning",
                                    uiOutput("meanLOGARITHMIQUE"),
                                    uiOutput("varLOGARITHMIQUE")
                                ),
                                align = "center"
                                # ,

                                # box(
                                #     title = "Fonctions g", width = NULL, solidHeader = TRUE, status = "warning",
                                #     numericInput('dLOGARITHMIQUE', withMathJax('$$d$$'), value = 0, width = "20px"),
                                #     # radioButtons('equalityLOGARITHMIQUE', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                                #     uiOutput("EspTronqLOGARITHMIQUE"),
                                #     uiOutput("EspLimLOGARITHMIQUE"),
                                #     uiOutput("StopLossLOGARITHMIQUE"),
                                #     uiOutput("ExcesMoyLOGARITHMIQUE"),
                                #     align = "center")
                         ),

                         ## Fonctions
                         column(width = 3,
                                box(
                                    title = "Fonctions",
                                    width = NULL,
                                    solidHeader = TRUE,
                                    tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                    status = "danger", # pour couleur de la boite, diff couleur pour statut
                                    numericInput('xLOGARITHMIQUE', '$$x$$', min = 0, value = 0, step = 1),
                                    uiOutput("densityLOGARITHMIQUE"),

                                    tabBox(
                                        width = NULL,
                                        tabPanel("Répartition",
                                                 uiOutput("repartLOGARITHMIQUE")
                                                 # ,
                                                 # plotlyOutput("FxLOGARITHMIQUE")
                                        ),
                                        tabPanel("Survie",
                                                 uiOutput("survieLOGARITHMIQUE")
                                                 # ,
                                                 # plotlyOutput("SxBETA")
                                        )

                                    )
                                ),
                                align = "center"

                         )
                         ,

                         column(width = 3,
                                boxPlus(
                                    title = "Mesure de risques",
                                    width = NULL,
                                    solidHeader = TRUE,
                                    status = "success",
                                    closable = F,
                                    tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du texte
                                    numericInput('kLOGARITHMIQUE', '$$\\kappa$$', value = 0.99, step = 0.005),
                                    uiOutput("VaRLOGARITHMIQUE")
                                    # ,uiOutput("TVaRLOGARITHMIQUE")
                                    ),

                                align = "center"
                         )
                     )
)


#### Loi Uniforme Discrète UI ####
tab_UNID_UI <- tabItem(tabName = "UniformeD",
                      fluidPage(
                          titlePanel(tags$a("Loi  Uniforme",href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Uniforme-Discr%C3%A8te")),
                          withMathJax(),
                          helpText("\\(X \\sim\\mathcal{U} \\ (a, b)\\)"),
                          align = "center"
                      ),
                      
                      fluidRow(
                          column(width = 3,
                                 box(title = "Paramètres",
                                     status = "primary",
                                     solidHeader = T,
                                     width = NULL,
                                     numericInput('aUNID', '$$a$$', value = 1, min = 0, step = 1),
                                     numericInput('bUNID', '$$b$$', value = 2, min = 0, step = 1)
                                 ),
                                 align = "center"
                          ),
                          
                          ## Moments
                          column(width = 3,
                                 tags$style(" * {font-size:20px}"), # grosseur du tezte
                                 box(
                                     title = "Moments", width = NULL, solidHeader = TRUE, status = "warning",
                                     uiOutput("meanUNID"),
                                     uiOutput("varUNID")
                                 ),
                                 align = "center"
                                 # ,
                                 
                                 # box(
                                 #     title = "Fonctions g", width = NULL, solidHeader = TRUE, status = "warning",
                                 #     numericInput('dUNID', withMathJax('$$d$$'), value = 0, width = "20px"),
                                 #     # radioButtons('equalityNORM', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                                 #     uiOutput("EspTronqUNID"),
                                 #     uiOutput("EspLimUNID"),
                                 #     uiOutput("StopLossUNID"),
                                 #     uiOutput("ExcesMoyUNID"),
                                 #     align = "center")
                          ),
                          
                          ## Fonctions
                          column(width = 3,
                                 box(
                                     title = "Fonctions",
                                     width = NULL,
                                     solidHeader = TRUE,
                                     tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                     status = "danger", # pour couleur de la boite, diff couleur pour statut
                                     numericInput('xUNID', '$$x$$', min = 0, value = 0, step = 1),
                                     uiOutput("densityUNID"),
                                     
                                     tabBox(
                                         width = NULL,
                                         tabPanel("Répartition",
                                                  uiOutput("repartUNID")
                                                  # ,
                                                  # plotlyOutput("FxUNID")
                                         ),
                                         tabPanel("Survie",
                                                  uiOutput("survieUNID")
                                                  # ,
                                                  # plotlyOutput("SxBETA")
                                         )
                                         
                                     )
                                 ),
                                 align = "center"
                                 
                          )
                          # ,
                          # 
                          # column(width = 3,
                          #        boxPlus(
                          #            title = "Mesure de risques",
                          #            width = NULL,
                          #            solidHeader = TRUE,
                          #            status = "success",
                          #            closable = F,
                          #            tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du texte
                          #            numericInput('kUNID', '$$\\kappa$$', value = 0.99, step = 0.005),
                          #            uiOutput("VaRUNID"),
                          #            uiOutput("TVaRUNID")),
                          #        
                          #        align = "center"
                          # )
                      )
)

#### Loi Binomiale Négative Composée UI ####
{

    tab_BNCOMP_UI <- tabItem(
        tabName = "BNCOMP",
        fluidPage(
            tags$style("#loi_BNCOMP {font-size:30px;}"),
            textOutput("loi_BNCOMP"),
            withMathJax(),
            textOutput("loi_BNCOMP_severity"),
            textOutput("distr_BNCOMP"),
            radioGroupButtons(
                inputId = "severityBNCOMP",
                label = "",
                choices = c("Gamma", 
                            "Lognormale"
                            ),
                status = "primary"
            ),
            align = "center"
        ),

        fluidRow(

            #### Paramètres BNCOMP ####
            column(
                width = 3,
                box(
                    title = "Paramètres",
                    status = "primary",
                    solidHeader = T,
                    width = NULL,
                    numericInput('rBNCOMP', label = withMathJax('$$r$$'), value = 5),
                    numericInput('qBNCOMP', label = withMathJax('$$q$$'), min = 0, max = 1, step = .1, value = .5),
                    numericInput('koBNCOMP', label = withMathJax('$$k_{0}$$'), value = 300, step = 100),
                    uiOutput("shapeBNCOMPUI"),
                    uiOutput("rateBNCOMPUI"),
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

            #### Moments BNCOMP ####
            column(
                width = 3,
                tags$style(" * {font-size:20px;}"), # grosseur du tezte
                box(
                    title = "Moments",
                    width = NULL,
                    solidHeader = TRUE,
                    status = "warning",
                    uiOutput("meanBNCOMP"),
                    uiOutput("varianceBNCOMP")
                ),
                align = "center"
            ),

            #### Fonctions BNCOMP ####
            column(
                width = 3,
                box(
                    title = "Fonctions",
                    width = NULL,
                    solidHeader = TRUE, # grosseur du tezte
                    status = "danger", # couleur de la boite
                    numericInput('xBNCOMP', '$$x$$', value = 10, min = 0),
                    # uiOutput("densityBNCOMP"),
                    tabBox(
                        width = NULL,
                        tabPanel("Répartition",
                                 uiOutput("repartBNCOMP")

                        )
                        ,tabPanel("Survie",
                              uiOutput("survieBNCOMP")

                        )

                    )
                ),
                align = "center"
            ),

            #### Mesures de risque BNCOMP ####
            column(
                width = 3,
                boxPlus(
                    title = "Mesure de risques",
                    width = NULL,
                    solidHeader = TRUE,
                    closable = F,
                    status = "success", # grosseur du tezte
                    numericInput('kBNCOMP', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                    uiOutput("VaRBNCOMP"),
                    uiOutput("TVaRBNCOMP")
                ),
                align = "center"
            )

        )
    )
}

#### Loi Poisson Composée UI ####
{

    tab_PCOMP_UI <- tabItem(
        tabName = "PCOMP",
        fluidPage(
            tags$style("#loi_PCOMP {font-size:30px;}"),
            textOutput("loi_PCOMP"),
            withMathJax(),
            textOutput("distr_PCOMP"),
            radioGroupButtons(
                inputId = "severityPCOMP",
                label = "",
                choices = c("Gamma", 
                            "Lognormale"
                            ),
                status = "primary"
            ),
            align = "center"
        ),

        fluidRow(

            #### Paramètres PCOMP ####
            column(
                width = 3,
                box(
                    title = "Paramètres",
                    status = "primary",
                    solidHeader = T,
                    width = NULL,
                    numericInput('lambdaPCOMP', label = withMathJax('$$\\lambda$$'), value = 5, min = 0),
                    numericInput('koPCOMP', label = withMathJax('$$k_{0}$$'), value = 200, step = 100),
                    uiOutput("shapePCOMPUI"),
                    uiOutput("ratePCOMPUI"),
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

            #### Moments PCOMP ####
            column(
                width = 3,
                tags$style(" * {font-size:20px;}"), # grosseur du texte
                box(
                    title = "Moments",
                    width = NULL,
                    solidHeader = TRUE,
                    status = "warning",
                    uiOutput("meanPCOMP"),
                    uiOutput("variancePCOMP")
                ),
                align = "center"
            ),

            #### Fonctions PCOMP ####
            column(
                width = 3,
                box(
                    title = "Fonctions",
                    width = NULL,
                    solidHeader = TRUE, # grosseur du tezte
                    status = "danger", # couleur de la boite
                    numericInput('xPCOMP', '$$x$$', value = 10, min = 0),
                    tabBox(
                        width = NULL,
                        tabPanel("Répartition",
                                 uiOutput("repartPCOMP")
                        ),
                        tabPanel("Survie",
                                 uiOutput("surviePCOMP")
                        )

                    )
                ),
                align = "center"
            ),

            #### Mesures de risque PCOMP ####
            column(
                width = 3,
                boxPlus(
                    title = "Mesure de risques",
                    width = NULL,
                    solidHeader = TRUE,
                    closable = F,
                    status = "success", # grosseur du tezte
                    numericInput('kPCOMP', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                    uiOutput("VaRPCOMP"),
                    uiOutput("TVaRPCOMP")
                ),
                align = "center"
            )

        )
    )
}

#### Loi Binomiale Négative Composée UI ####
{
    
    tab_BINCOMP_UI <- tabItem(
        tabName = "BINCOMP",
        fluidPage(
            tags$style("#loi_BINCOMP {font-size:30px;}"),
            textOutput("loi_BINCOMP"),
            withMathJax(),
            textOutput("loi_BINCOMP_severity"),
            textOutput("distr_BINCOMP"),
            radioGroupButtons(
                inputId = "severityBINCOMP",
                label = "",
                choices = c("Gamma", 
                            "Lognormale"
                ),
                status = "primary"
            ),
            align = "center"
        ),
        
        fluidRow(
            
            #### Paramètres BINCOMP ####
            column(
                width = 3,
                box(
                    title = "Paramètres",
                    status = "primary",
                    solidHeader = T,
                    width = NULL,
                    numericInput('nBINCOMP', label = withMathJax('$$n$$'), value = 5),
                    numericInput('qBINCOMP', label = withMathJax('$$q$$'), min = 0, max = 1, step = .1, value = .5),
                    numericInput('koBINCOMP', label = withMathJax('$$k_{0}$$'), value = 300, step = 100),
                    uiOutput("shapeBINCOMPUI"),
                    uiOutput("rateBINCOMPUI"),
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
            
            #### Moments BINCOMP ####
            column(
                width = 3,
                tags$style(" * {font-size:20px;}"), # grosseur du tezte
                box(
                    title = "Moments",
                    width = NULL,
                    solidHeader = TRUE,
                    status = "warning",
                    uiOutput("meanBINCOMP"),
                    uiOutput("varianceBINCOMP")
                ),
                align = "center"
            ),
            
            #### Fonctions BINCOMP ####
            column(
                width = 3,
                box(
                    title = "Fonctions",
                    width = NULL,
                    solidHeader = TRUE, # grosseur du tezte
                    status = "danger", # couleur de la boite
                    numericInput('xBINCOMP', '$$x$$', value = 10, min = 0),
                    # uiOutput("densityBINCOMP"),
                    tabBox(
                        width = NULL,
                        tabPanel("Répartition",
                                 uiOutput("repartBINCOMP")
                                 
                        )
                        ,tabPanel("Survie",
                                  uiOutput("survieBINCOMP")
                                  
                        )
                        
                    )
                ),
                align = "center"
            ),
            
            #### Mesures de risque BINCOMP ####
            column(
                width = 3,
                boxPlus(
                    title = "Mesure de risques",
                    width = NULL,
                    solidHeader = TRUE,
                    closable = F,
                    status = "success", # grosseur du tezte
                    numericInput('kBINCOMP', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                    uiOutput("VanBINCOMP"),
                    uiOutput("TVanBINCOMP")
                ),
                align = "center"
            )
            
        )
    )
}
