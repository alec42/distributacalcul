#### Copules ####

copulaTypes <- c("EFGM", "AMH", "Clayton", "Frank", "Gumbel", "Normal", "Student")

dEFGM <- function(u1, u2, kendallTau) {
    param <- 9 * kendallTau / 2
    1 + param * (1 - 2 * u1) * (1 - 2 * u2)
}

dClayton <- function(u1, u2, kendallTau) {
    param <- 2 * (1 / kendallTau - 1) ^ -1
    (1 + param) / (u1 * u2) ^ (param + 1) * (u1 ^ -param + u2 ^ -param - 1) ^ (-2 - 1 / param)
}

dAMH <- function(u1, u2, kendallTau) {
    MinimizeTauAMH <- function(param) {
        abs( kendallTau - (1 - 2 * ( (1 - param) ^ 2 * log(1 - param) + param) / (3 * param ^ 2)))
    }
    param <- optimize(MinimizeTauAMH, c(-1, 1))$minimum
    (1 - param + 2 * param * u1 * u2 / (1 - param * (1 - u1) * (1 - u2))) / (1 - param * (1 - u1) * (1 - u2)) ^ 2
}

dFrank <- function(u1, u2, kendallTau) {
    MinimizeTauFrank <- function(param) {
        if(param == 0) {
            10000
        } else {
            abs( kendallTau - (1 + 4 / param * (debye1(param) - 1)))
        }
    }
    param <- optimize(MinimizeTauFrank, c(-100, 100))$minimum
    param * exp(- param * (u1 + u2)) * (1 - exp(-param)) /
        (exp(-param * (u1 + u2)) - exp(-param * u1) - exp(-param * u2) + exp(-param)) ^ 2
}

pGumbel <- function(u1, u2, param) {
    exp(- ((-(log(u1))) ^ param + (-(log(u2))) ^ param) ^ (1 / param))
}

dGumbel <- function(u1, u2, kendallTau) {
    param <- (1 - kendallTau) ^ -1
    pGumbel(u1, u2, param) * ((-log(u1)) ^ (param - 1) * (-log(u2)) ^ (param - 1)) / (u1 * u2) *
        ((-log(u1)) ^ param + (-log(u2)) ^ param) ^ (1 / param - 2) * 
        (param - 1 + ((-log(u1)) ^ param + (-log(u2)) ^ param) ^ (1 / param))
}

dNormal <- function(u1, u2, kendallTau) {
    param <- sin(kendallTau * pi / 2)
    1 / sqrt(1 - param ^ 2) * exp(- ( qnorm(u1) ^ 2 - 2 * param * qnorm(u1) * qnorm(u2) + qnorm(u2) ^ 2) / (2 * (1 - param ^ 2))) *
        exp((qnorm(u1) ^ 2 + qnorm(u2) ^ 2) / 2)
}

dStudent <- function(u1, u2, kendallTau, v = 6) {
    MinimizeTauStudent <- function(current_param) {
        abs(kendallTau - tau(tCopula(dim = 2, param = current_param)))
    }
    param <- optimize(MinimizeTauStudent, c(-1, 1))$minimum
    densite <- param ^ (-0.5) * (gamma((v + 2) / 2 * gamma(v / 2))) / (gamma((v + 1) / 2)) ^ 2 *
        (1 + (qt(u1, df = v) ^ 2 - 2 * param * qt(u1, df = v) * qt(u2, df = v) + qt(u2, df = v) ^ 2) / (v * (1 - param ^ 2))) ^((v + 2) / 2) *
        (1 + qt(u1, df = v) / v) ^ ((v + 2) / 2) * (1 + qt(u2, df = v) / v) ^ ((v + 2) / 2)
    pmax(pmin(densite, 1000), -1000)
}

#### Copules fin ####

tab_copulas_tool <- tabItem(tabName = "copulas_tool",
                        fluidPage(
                        fluidRow(
                            titlePanel("Copula density function"),
                            align = "center"
                        ),
                          fluidRow(
                            column(width = 4,
                            # sidebarPanel(
                                # h3("Title"),
                            # boxPlus(
                            wellPanel(
                                withMathJax(),
                                title = "Paramètre",
                                status = "primary",
                                solidHeader = T,
                                width = NULL,
                                closable = F,
                                p("Choice of copula"),
                            pickerInput(
                                inputId = "name",
                                # label = "Choice of copula", 
                                choices = copulaTypes,
                                width = "100%",
                                options = list(maxItems = 1, placeholder = 'Select a copula'),
                                selected = "EFGM"
                            ),
                            p("Copula parameter"),
                            # tags$style(type="text/css", ".irs {max-width: 200px;}"),
                            sliderInput('kendallTau',label = NULL,
                                        # 'Copula parameter',
                                        width = "100%",
                                        min = -1, max = 1, value = 0, step = 0.01)

                            )
                            # )
                            ),
                            # mainPanel(
                            column(width = 8,
                                   boxPlus(
                                       withMathJax(),
                                       title = "Graphique",
                                       status = "primary",
                                       solidHeader = T,
                                       width = NULL,
                                       closable = F,
                                textOutput("caption"), 
                                plotlyOutput("copulaPlot"))
                            )
                            
                            # )
                            ))
                        )

tab_stat_tests <- tabItem(tabName = "stat_tests",
                      
                      fluidRow(
                          titlePanel("Tests statistiques _(à venir)_"),
                          withMathJax(),
                          align = "center"
                      ),
                      fluidRow(
                          withMathJax(),
                          column(
                              width = 8,
                              boxPlus(
                                  withMathJax(),
                                  title = "$$\\mathcal{H}_0$$",
                                  status = "primary",
                                  solidHeader = T,
                                  width = NULL,
                                  closable = F,
                                  p("Variable à estimer"),
                                  # shinyWidgetsGallery()
                                  fluidRow(
                                      withMathJax(),
                                      align = "center",
                                      column(width = 3,
                                  splitLayout(
                                      # cellWidths = c("15%", "15%"),
                                      tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                      ## pour renderer les choix en latex
                                      tags$head(
                                          tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css", integrity="sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH", crossorigin="anonymous"),
                                          tags$script(src="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js", integrity="sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm", crossorigin="anonymous")
                                      ),
                                      ## faut utiliser un selectize pour la compatibilité avec KaTex
                                      selectizeInput(
                                          "paramESTIM_STATTOOL",
                                          "",
                                          choices = NULL
                                      ),
                                      selectizeInput(
                                          "borneSTIM_STATTOOL", 
                                          "", 
                                          choices = NULL)
                                  )
                                  ),
                              column(width = 5,
                                  p("$$\\mathcal{H}_0$$"),
                                  numericInput("valueESTIM_STATTOOL", "a ", value = 0, min = 0, step = 1)
                                  )),
                                  p("$$\\text{Distribution sous }\\mathcal{H}_0$$"),
                                  uiOutput("distr_H0_ESTIM_STATTOOL"),
                                  p("$$\\text{Région critique au seuil }\\alpha$$"),
                                  uiOutput("seuil_H0_ESTIM_STATTOOL")
                              ),
                              boxPlus(
                                  title = "Hypothèses",
                                  status = "primary",
                                  solidHeader = T,
                                  width = NULL,
                                  closable = F
                              )
                              # ,align = "center"
                          ),
                          column(
                              width = 4,
                              boxPlus(
                                  title = "Postulats",
                                      withMathJax(),
                                  status = "danger",
                                  solidHeader = T,
                                  width = NULL,
                                  closable = F,
                                  # p("$$\\sigma^2$$ est"),
                                  # prettyRadioButtons("variancehypothesisESTIM_STATTOOL",  label = "$$\\sigma^2$$", choices = list("connu", "inconnu"), fill = T, status = "danger"),
                                  
                                  radioGroupButtons(direction = "horizontal", 
                                                       inputId = "variancehypothesisESTIM_STATTOOL",  
                                                       label = "$$\\sigma^2$$", 
                                                       choices = list("connu", "inconnu"),
                                                       justified = T,
                                                       status = "danger"),
                                  radioGroupButtons(inputId = "nsizeESTIM_STATTOOL",  
                                                    direction = "horizontal", 
                                                    label = "$$n$$", 
                                                    choices = list("grand", "petit"), 
                                                    justified = T,
                                                    status = "danger"),
                                  selectInput(
                                      "testESTIM_STATTOOL",
                                      "",
                                      choices = c("UMP", "Test T", "Central limite", "Wald", "Sur une proportion", "Sur la variance", "Rapport de vraisemblance", "Khi-carré de Pearson")
                                  ),
                                  uiOutput("distr_ESTIM_STATTOOL")
                              )
                          )
                      )
)

tab_excess_mean <- tabItem(tabName = "excess_mean",
                           
                           fluidRow(
                               titlePanel("Fonction d'excès-moyen"),
                               # withMathJax(),
                               # helpText("\\(X \\sim\\mathcal{Normale} \\ (\\mu, \\sigma^2)\\)"),
                               align = "center"
                           ),
                           fluidRow(
                               column(
                                   width = 2,
                                   boxPlus(
                                       title = "Paramètres",
                                       status = "primary",
                                       solidHeader = T,
                                       width = NULL,
                                       closable = F,
                                       numericInput('shapeEXCESS_MEAN', withMathJax('$$\\alpha$$'), value = 2),
                                       numericInput('rateEXCESS_MEAN', '$$\\beta$$', value = 1)
                                   ),
                                   boxPlus(
                                       title = "Distributions",
                                       status = "primary",
                                       solidHeader = T,
                                       width = NULL,
                                       closable = F,
                                       uiOutput("gammaEXCESS_MEAN"),
                                       tags$head(tags$style("#gammaEXCESS_MEAN{color: red}")),
                                       uiOutput("paretoEXCESS_MEAN"),
                                       tags$head(tags$style("#paretoEXCESS_MEAN{color: green}")),
                                       uiOutput("normEXCESS_MEAN"),
                                       tags$head(tags$style("#normEXCESS_MEAN{color: blue}")),
                                       # uiOutput("lnormEXCESS_MEAN"),
                                       # tags$head(tags$style("#lnormEXCESS_MEAN{color: orange}"))
                                       uiOutput("weibullEXCESS_MEAN"),
                                       tags$head(tags$style("#weibullEXCESS_MEAN{color: purple}"))
                                   ),
                                   align = "center"
                               ),
                               column(
                                   width = 10,
                                   boxPlus(
                                       title = "Plot",
                                       status = "primary",
                                       solidHeader = T,
                                       width = NULL,
                                       closable = F,
                                       plotOutput("plotEXCESS_MEAN")
                                   )
                               )
                               
                           ),
                           fluidRow(
                               boxPlus(
                                   title = "Description",
                                   status = "primary",
                                   solidHeader = T,
                                   width = NULL,
                                   closable = F,
                                   collapsible = T,
                                   collapsed = F,
                                   textOutput("descriptionEXCESS_MEAN")
                               )
                           )
                           )

tab_approx_tool <- tabItem(tabName = "approx_tool",
                           
                           fluidPage(
                               tags$style("#distr_APPROX_TOOL {font-size:30px;}"),
                               uiOutput("distr_APPROX_TOOL"),
                               withMathJax(),
                               textOutput("distr_name_APPROX_TOOL"),
                               radioGroupButtons(
                                   inputId = "approx_choice_APPROX_TOOL",
                                   label = "",
                                   choices = c("Poisson", 
                                               "Binomiale",
                                               "Hypergeometrique"),
                                   status = "primary"
                               ),
                               align = "center"
                           ),
                           # fluidRow(
                           #     titlePanel("Approximations de lois discrètes"),
                           #     # withMathJax(),
                           #     # helpText("\\(X \\sim\\mathcal{Normale} \\ (\\mu, \\sigma^2)\\)"),
                           #     align = "center"
                           # ),
                           fluidRow(
                               column(
                                   width = 4,
                                   boxPlus(
                                       title = "Paramètres",
                                       status = "primary",
                                       solidHeader = T,
                                       width = NULL,
                                       closable = F,
                                       uiOutput("changing_nBIN_APPROX_TOOL"),
                                       uiOutput("changing_pBIN_APPROX_TOOL"),
                                           uiOutput("changing_N_HG_APPROX_TOOL"),
                                           uiOutput("changing_petitN_HG_APPROX_TOOL"),
                                           uiOutput("changing_m_HG_APPROX_TOOL")
                                   ),
                                   boxPlus(
                                       title = "Distributions",
                                       status = "primary",
                                       solidHeader = T,
                                       width = NULL,
                                       closable = F,
                                       uiOutput("distr_BIN_APPROX_TOOL"),
                                       # tags$head(tags$style("#distr_BIN_APPROX_TOOL{color: blue}")),
                                       uiOutput("distr_POI_APPROX_TOOL")
                                       # ,tags$head(tags$style("#distr_POI_APPROX_TOOL{color: red}"))
                                   ),
                                   align = "center"
                               ),
                               column(
                                   width = 8,
                                   boxPlus(
                                       title = "Plot",
                                       status = "primary",
                                       solidHeader = T,
                                       width = NULL,
                                       closable = F,
                                       plotlyOutput("plot_APPROX_TOOL")
                                   )
                               )
                               
                           )
                           ,fluidRow(
                               boxPlus(
                                   title = "Description",
                                   status = "primary",
                                   solidHeader = T,
                                   width = NULL,
                                   closable = F,
                                   collapsible = T,
                                   collapsed = F,
                                   textOutput("descriptionAPPROX_TOOL")
                               )
                           )
)


#### Loi Normale UI ----
tab_NORM_UI <- tabItem(tabName = "Normale",
        fluidRow(
            useShinyjs(), # utilisé to gray out les paramètres de la gamma qu'on désire fixe
            # titlePanel("Loi Normale"),
            titlePanel(tags$a("Loi Normale", target = "_blank", href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Normal")),
            # withMathJax(),
            helpText("\\(X \\sim\\mathcal{Normale} \\ (\\mu, \\sigma^2)\\)"),
            align = "center"
        ),
        fluidRow(
            {
                ### Paramètres Normale ----
                column(
                    width = 3,
                    boxPlus(
                        title = "Paramètres",
                        status = "primary",
                        solidHeader = T,
                        width = NULL,closable = F,
                        numericInput('muNORM', withMathJax('$$\\mu$$'), value = 0),
                        numericInput('sigmaNORM', '$$\\sigma^2$$', value = 1)
                    ),
                    box(
                        title = "Moments",
                        width = NULL,
                        solidHeader = TRUE,
                        status = "warning",
                        uiOutput("meanNORM"),
                        uiOutput("varNORM"),
                        numericInput('dNORM', withMathJax('$$d$$'), value = 0, width = "20px"),
                        # radioButtons('equalityNORM', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                        uiOutput("EspTronqNORM"),
                        uiOutput("EspLimNORM"),
                        uiOutput("StopLossNORM"),
                        uiOutput("ExcesMoyNORM")
                    ),
                    align = "center"
                )
            },
            
            {
                ### Fonctions Normale ----
                column(
                    width = 5,
                    box(
                        title = "Fonctions",
                        width = NULL,
                        solidHeader = TRUE,
                        # tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                        status = "danger", # pour couleur de la boite, diff couleur pour statut
                        numericInput('xNORM', '$$x$$', value = 0),
                        uiOutput("densityNORM"),
                        
                        switchInput(
                            inputId = "xlim_NORM",
                            onStatus = "success",
                            onLabel = "Répartition",
                            offStatus = "info",
                            offLabel = "Survie",
                            value = T,
                            labelWidth = "10px"
                        ),
                        uiOutput("repartsurvieNORM"),
                        p("Graphique"),
                        radioGroupButtons(inputId = "plot_choice_NORM", 
                                          choices = c("Densité", 
                                                      "Fonction de répartition"),
                                          selected = "Densité",
                                          justified = TRUE),
                        plotlyOutput("FxNORM")
                    ),
                    align = "center"
                )
            },

            {
                ### Mesures de risque Normale  ----
                column(
                    width = 4,
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
                        radioGroupButtons(inputId = "plot_choice_NORM_QX", 
                                          choices = c("Densité", 
                                                      "Fonction de répartition"),
                                          selected = "Fonction de répartition",
                                          justified = TRUE),
                        plotlyOutput("QxNORM"),
                        plotlyOutput("QuantileNORM")
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
                           titlePanel(tags$a("Loi Lognormale", target = "_blank", href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Lognormale")),
                           helpText("\\(X \\sim\\mathcal{Lognormale} \\ (\\mu, \\sigma^2)\\)"),
                           align = "center"
                       ),
                       fluidRow(
                           {
                               ### Paramètres LNORMale ----
                               column(
                                   width = 3,
                                   boxPlus(
                                       title = "Paramètres",
                                       status = "primary",
                                       solidHeader = T,
                                       width = NULL,closable = F,
                                       numericInput('muLNORM', withMathJax('$$\\mu$$'), value = 0),
                                       numericInput('sigmaLNORM', '$$\\sigma^2$$', value = 1)
                                       # shinyWidgetsGallery()
                                   ),
                                   box(
                                       title = "Moments",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       uiOutput("meanLNORM"),
                                       uiOutput("varLNORM"),
                                       numericInput('dLNORM', withMathJax('$$d$$'), value = 1, width = "20px"),
                                       # radioButtons('equalityLNORM', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                                       uiOutput("EspTronqLNORM"),
                                       uiOutput("EspLimLNORM"),
                                       uiOutput("StopLossLNORM"),
                                       uiOutput("ExcesMoyLNORM"),
                                       uiOutput("kthmomentLNORM")
                                   ),
                                   align = "center"
                               )
                           },

                           {
                               ### Fonctions Lognormale ----
                               column(
                                   width = 5,
                                   box(
                                       title = "Fonctions",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       status = "danger", # pour couleur de la boite, diff couleur pour statut
                                       numericInput('xLNORM', '$$x$$', value = 0.5, min = 0),
                                       uiOutput("densityLNORM"),
                                       
                                       switchInput(
                                           inputId = "xlim_LNORM",
                                           onStatus = "success",
                                           onLabel = "Répartition",
                                           offStatus = "info",
                                           offLabel = "Survie",
                                           value = T,
                                           labelWidth = "10px"
                                       ),
                                       uiOutput("repartsurvieLNORM"),
                                       p("Graphique"),
                                       radioGroupButtons(inputId = "plot_choice_LNORM", 
                                                         choices = c("Fonction de densité", 
                                                                     "Fonction de répartition"),
                                                         selected = "Fonction de densité",
                                                         justified = TRUE),
                                       plotlyOutput("FxLNORM")
                                   ),
                                   align = "center"
                               )
                           },

                           {
                               ### Mesures de risque Lognormale  ----
                               column(
                                   width = 4,
                                   # dropdownButton(
                                   #     # tags$h3("Range"),  
                                   #     uiOutput("range_LNORM_FX_UI"),
                                   #     circle = TRUE, 
                                   #     status = "danger",
                                   #     icon = icon("gear"), 
                                   #     width = "300px",
                                   #     tooltip = tooltipOptions(title = "Presser pour modifier domaine !")
                                   # ),
                                   boxPlus(
                                       title = "Mesure de risques",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       closable = F,
                                       status = "success",
                                       # tags$style(" * {font-size:20px }"), # ligne qui augmente la grosseur du texte
                                       
                                       numericInput('kLNORM', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                                       uiOutput("VaRLNORM"),
                                       uiOutput("TVaRLNORM"),
                                       pickerInput(
                                           inputId = "plot_choice_LNORM_QX", 
                                           # label = "Style : primary", 
                                           choices = c("Fonction de densité",
                                                       "Fonction de répartition",
                                                       "Fonction quantile"),
                                           selected = "Fonction de répartition",
                                           options = list(
                                               style = "btn-success")
                                       ),
                                       # radioGroupButtons(inputId = "plot_choice_LNORM_QX", 
                                       #                   choices = c("Fonction de densité",
                                       #                               "Fonction de répartition",
                                       #                               "Fonction quantile"),
                                       #                   selected = "Fonction de répartition",
                                       #                   justified = TRUE),
                                       plotlyOutput("QxLNORM")
                                       # ,plotlyOutput("QuantileLNORM")
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
                
                #### Moments Gamma ####
                tags$style(" * {font-size:20px;}"), # grosseur du tezte
                box(
                    title = "Moments",
                    width = NULL,
                    solidHeader = TRUE,
                    status = "warning",
                    uiOutput("meanGAMMA"),
                    uiOutput("varianceGAMMA"),
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
                width = 5,
                box(
                    title = "Fonctions",
                    width = NULL,
                    solidHeader = TRUE, # grosseur du tezte
                    status = "danger", # couleur de la boite
                    numericInput('xGAMMA', '$$x$$', value = 4, min = 0),
                    uiOutput("densityGAMMA"),
                    switchInput(
                        inputId = "xlim_GAMMA",
                        onStatus = "success",
                        onLabel = "Répartition",
                        offStatus = "info",
                        offLabel = "Survie",
                        value = T,
                        labelWidth = "10px"
                    ),
                    uiOutput("repartsurvieGAMMA"),
                    p("Graphique"),
                    radioGroupButtons(inputId = "plot_choice_GAMMA", 
                                      choices = c("Densité", 
                                                  "Fonction de répartition"),
                                      selected = "Densité",
                                      justified = TRUE),
                    plotlyOutput("FxGAMMA")
                ),
                align = "center"
            ),

            #### Mesures de risque Gamma ####
                column(
                    width = 4,
                    boxPlus(
                        title = "Mesure de risques",
                        width = NULL,
                        solidHeader = TRUE,
                        closable = F,
                        status = "success", # grosseur du tezte
                        numericInput('kGAMMA', '$$\\kappa$$', value = 0.95, step = 0.005, min = 0, max = 1),
                        uiOutput("VaRGAMMA"),
                        uiOutput("TVaRGAMMA"),
                        radioGroupButtons(inputId = "plot_choice_GAMMA_QX", 
                                          choices = c("Densité", 
                                                      "Fonction de répartition"),
                                          selected = "Fonction de répartition",
                                          justified = TRUE),
                        plotlyOutput("QxGAMMA")
                        # ,plotlyOutput("QuantileGAMMA")
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
                           titlePanel(tags$a("Loi Pareto", target = "_blank", href ="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Pareto")),
                           # withMathJax(),
                           helpText("\\(X \\sim\\mathcal{Pareto} \\ (\\alpha, \\lambda)\\)"),
                           align = "center"
                       ),

                       fluidRow(
                           {
                               ### Paramètres Pareto ----
                               column(
                                   width = 3,
                                   boxPlus(
                                       title = "Paramètres",
                                       status = "primary",
                                       solidHeader = T,
                                       width = NULL,closable = F,
                                       numericInput('alphaPARETO', withMathJax('$$\\alpha$$'), value = 3, min = 0),
                                       numericInput('lambdaPARETO', '$$\\lambda$$', value = 1, min = 0)
                                   ),
                               ### Moments Pareto  ----
                                   box(
                                       title = "Moments",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       uiOutput("meanPARETO"),
                                       uiOutput("varPARETO"),
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
                                   width = 5,
                                   box(
                                       title = "Fonctions",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       # tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                       status = "danger", # pour couleur de la boite, diff couleur pour statut
                                       numericInput('xPARETO', '$$x$$', value = 0.5, min = 0),
                                       uiOutput("densityPARETO"),

                                       switchInput(
                                           inputId = "xlim_PARETO",
                                           onStatus = "success",
                                           onLabel = "Répartition",
                                           offStatus = "info",
                                           offLabel = "Survie",
                                           value = T,
                                           labelWidth = "10px"
                                       ),
                                       uiOutput("repartsurviePARETO"),
                                       p("Graphique"),
                                       radioGroupButtons(inputId = "plot_choice_PARETO", 
                                                         choices = c("Densité", 
                                                                     "Fonction de répartition"),
                                                         selected = "Densité",
                                                         justified = TRUE),
                                       plotlyOutput("FxPARETO")
                                   ),
                                   align = "center"
                               )
                           },

                           {
                               ### Mesures de risque Pareto  ----
                               column(
                                   width = 4,
                                   boxPlus(
                                       title = "Mesure de risques",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       closable = F,
                                       status = "success",
                                       # tags$style(" * {font-size:20px }"), # ligne qui augmente la grosseur du texte
                                       numericInput('kPARETO', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                                       uiOutput("VaRPARETO"),
                                       uiOutput("TVaRPARETO"),
                                       radioGroupButtons(inputId = "plot_choice_PARETO_QX", 
                                                         choices = c("Densité", 
                                                                     "Fonction de répartition"),
                                                         selected = "Fonction de répartition",
                                                         justified = TRUE),
                                       plotlyOutput("QxPARETO"),
                                       plotlyOutput("QuantilePARETO")
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
                             titlePanel(tags$a("Loi Burr", target = "_blank", href ="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Burr")),
                             # withMathJax(),
                             helpText("\\(X \\sim\\mathcal{Burr} \\ (\\alpha, \\lambda, \\tau)\\)"),
                             align = "center"
                         ),

                         fluidRow(
                             {
                                 ### Paramètres Burr ----
                                 column(
                                     width = 3,
                                     boxPlus(
                                         title = "Paramètres",
                                         status = "primary",
                                         solidHeader = T,
                                         width = NULL,closable = F,
                                         numericInput('alphaBURR', withMathJax('$$\\alpha$$'), value = 3, step = 1, min = 0),
                                         numericInput('tauBURR', withMathJax('$$\\tau$$'), value = 1, step = 1, min = 0),
                                         numericInput('lambdaBURR', '$$\\lambda$$', value = 1, step = 1, min = 0)
                                     ),
                                 ### Moments BURR  ----
                                 box(
                                     title = "Moments",
                                     width = NULL,
                                     solidHeader = TRUE,
                                     status = "warning",
                                     uiOutput("meanBURR"),
                                     uiOutput("varBURR"),
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
                                     width = 5,
                                     box(
                                         title = "Fonctions",
                                         width = NULL,
                                         solidHeader = TRUE,
                                         # tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                         status = "danger", # pour couleur de la boite, diff couleur pour statut
                                         numericInput('xBURR', '$$x$$', value = 0.5, min = 0, step = 1),
                                         uiOutput("densityBURR"),

                                         switchInput(
                                             inputId = "xlim_BURR",
                                             onStatus = "success",
                                             onLabel = "Répartition",
                                             offStatus = "info",
                                             offLabel = "Survie",
                                             value = T,
                                             labelWidth = "10px"
                                         ),
                                         uiOutput("repartsurvieBURR"),
                                         p("Graphique"),
                                         radioGroupButtons(inputId = "plot_choice_BURR", 
                                                           choices = c("Densité", 
                                                                       "Fonction de répartition"),
                                                           selected = "Densité",
                                                           justified = TRUE),
                                         plotlyOutput("FxBURR")
                                     ),
                                     align = "center"
                                 )
                             },

                             {
                                 ### Mesures de risque BURR  ----
                                 column(
                                     width = 4,
                                     boxPlus(
                                         title = "Mesure de risques",
                                         width = NULL,
                                         solidHeader = TRUE,
                                         closable = F,
                                         status = "success",
                                         # tags$style(" * {font-size:20px }"), # ligne qui augmente la grosseur du texte
                                         numericInput('kBURR', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                                         uiOutput("VaRBURR"),
                                         uiOutput("TVaRBURR"),
                                         radioGroupButtons(inputId = "plot_choice_BURR_QX", 
                                                           choices = c("Densité", 
                                                                       "Fonction de répartition"),
                                                           selected = "Fonction de répartition",
                                                           justified = TRUE),
                                         plotlyOutput("QxBURR"),
                                         plotlyOutput("QuantileBURR")
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
                             titlePanel(tags$a("Loi Weibull", target = "_blank", href ="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Weibull")),
                             # withMathJax(),
                             helpText("\\(X \\sim\\mathcal{Weibull} \\ (\\tau, \\beta)\\)"),
                             align = "center"
                         ),

                         fluidRow(
                             {
                                 ### Paramètres Weibull ----
                                 column(
                                     width = 3,
                                     boxPlus(
                                         title = "Paramètres",
                                         status = "primary",
                                         solidHeader = T,
                                         width = NULL,closable = F,
                                         numericInput('tauWEIBULL', withMathJax('$$\\tau$$'), value = 2, min = 0),
                                         numericInput('betaWEIBULL', '$$\\beta$$', value = 1, min = 0)
                                     ),
                                     ### Moments Weibull  ----
                                     box(
                                         title = "Moments",
                                         width = NULL,
                                         solidHeader = TRUE,
                                         status = "warning",
                                         uiOutput("meanWEIBULL"),
                                         uiOutput("varianceWEIBULL"),
                                         numericInput('dWEIBULL', withMathJax('$$d$$'), value = 1, width = "20px", min = 0),
                                         # radioButtons('equalityWEIBULL', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                                         uiOutput("EspTronqWEIBULL"),
                                         uiOutput("EspLimWEIBULL"),
                                         uiOutput("StopLossWEIBULL"),
                                         uiOutput("ExcesMoyWEIBULL"),
                                         uiOutput("kthmomentWEIBULL")
                                         # align = "center"
                                     ),
                                     align = "center"
                                 )
                             },

                             {
                                 ### Fonctions Weibull ----
                                 column(
                                     width = 5,
                                     box(
                                         title = "Fonctions",
                                         width = NULL,
                                         solidHeader = TRUE,
                                         # tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                         status = "danger", # pour couleur de la boite, diff couleur pour statut
                                         numericInput('xWEIBULL', '$$x$$', value = 0.5, min = 0),
                                         uiOutput("densityWEIBULL"),
                                         
                                         switchInput(
                                             inputId = "xlim_WEIBULL",
                                             onStatus = "success",
                                             onLabel = "Répartition",
                                             offStatus = "info",
                                             offLabel = "Survie",
                                             value = T,
                                             labelWidth = "10px"
                                         ),
                                         uiOutput("repartsurvieWEIBULL"),
                                         p("Graphique"),
                                         radioGroupButtons(inputId = "plot_choice_WEIBULL", 
                                                           choices = c("Densité", 
                                                                       "Fonction de répartition"),
                                                           selected = "Densité",
                                                           justified = TRUE),
                                         plotlyOutput("FxWEIBULL")
                                     ),
                                     align = "center"
                                 )
                             },

                             {
                                 ### Mesures de risque Weibull  ----
                                 column(
                                     width = 4,
                                     boxPlus(
                                         title = "Mesure de risques",
                                         width = NULL,
                                         solidHeader = TRUE,
                                         closable = F,
                                         status = "success",
                                         # tags$style(" * {font-size:20px }"), # ligne qui augmente la grosseur du texte
                                         numericInput('kWEIBULL', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                                         uiOutput("VaRWEIBULL"),
                                         uiOutput("TVaRWEIBULL"),
                                         radioGroupButtons(inputId = "plot_choice_WEIBULL_QX", 
                                                           choices = c("Densité", 
                                                                       "Fonction de répartition"),
                                                           selected = "Fonction de répartition",
                                                           justified = TRUE),
                                         plotlyOutput("QxWEIBULL"),
                                         plotlyOutput("QuantileWEIBULL")
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
                              titlePanel(tags$a("Loi Inverse Gaussienne", target = "_blank", href ="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Inverse-Gaussienne")),
                              # withMathJax(),
                              helpText("\\(X \\sim\\mathcal{IG} \\ (\\mu, \\beta)\\)"),
                              align = "center"
                          ),

                          fluidRow(
                              {
                                  ### Paramètres IG ----
                                  column(
                                      width = 3,
                                      boxPlus(
                                          title = "Paramètres",
                                          status = "primary",
                                          solidHeader = T,
                                          width = NULL,closable = F,
                                          numericInput('muIG', withMathJax('$$\\mu$$'), value = 2, min = 0),
                                          numericInput('betaIG', '$$\\beta$$', value = 1, min = 0)
                                      ),
                                      ### Moments IG  ----
                                      box(
                                          title = "Moments",
                                          width = NULL,
                                          solidHeader = TRUE,
                                          status = "warning",
                                          uiOutput("meanIG"),
                                          uiOutput("varianceIG"),
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
                                      width = 5,
                                      box(
                                          title = "Fonctions",
                                          width = NULL,
                                          solidHeader = TRUE,
                                          # tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                          status = "danger", # pour couleur de la boite, diff couleur pour statut
                                          numericInput('xIG', '$$x$$', value = 5, min = 0),
                                          uiOutput("densityIG"),

                                          switchInput(
                                              inputId = "xlim_IG",
                                              onStatus = "success",
                                              onLabel = "Répartition",
                                              offStatus = "info",
                                              offLabel = "Survie",
                                              value = T,
                                              labelWidth = "10px"
                                          ),
                                          uiOutput("repartsurvieIG"),
                                          p("Graphique"),
                                          radioGroupButtons(inputId = "plot_choice_IG", 
                                                            choices = c("Densité", 
                                                                        "Fonction de répartition"),
                                                            selected = "Densité",
                                                            justified = TRUE),
                                          plotlyOutput("FxIG")
                                      ),
                                      align = "center"
                                  )
                              },

                              {
                                  ### Mesures de risque IG  ----
                                  column(
                                      width = 4,
                                      boxPlus(
                                          title = "Mesure de risques",
                                          width = NULL,
                                          solidHeader = TRUE,
                                          closable = F,
                                          status = "success",
                                          # tags$style(" * {font-size:20px }"), # ligne qui augmente la grosseur du texte
                                          numericInput('kIG', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                                          uiOutput("VaRIG"),
                                          #,
                                          #uiOutput("TVaRIG") -- ne fonctionne pas, à revoir --
                                          radioGroupButtons(inputId = "plot_choice_IG_QX", 
                                                            choices = c("Densité", 
                                                                        "Fonction de répartition"),
                                                            selected = "Fonction de répartition",
                                                            justified = TRUE),
                                          plotlyOutput("QxIG")
                                          # ,plotlyOutput("QuantileIG")
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
                           titlePanel(tags$a("Loi Uniforme", target = "_blank", href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Uniforme-Continue")),
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
                                      numericInput('aUNIC', '$$a$$', value = 0, min = 0, step = 1),
                                      numericInput('bUNIC', '$$b$$', value = 1, min = 0, step = 1)
                                  ),
                                  ## Moments
                                  tags$style(" * {font-size:20px}"), # grosseur du tezte
                                  box(
                                      title = "Moments", width = NULL, solidHeader = TRUE, status = "warning",
                                      uiOutput("meanUNIC"),
                                      uiOutput("varUNIC")
                                      #     ,numericInput('dUNIC', withMathJax('$$d$$'), value = 0, width = "20px"),
                                      #     # radioButtons('equalityNORM', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                                      #     uiOutput("EspTronqUNIC"),
                                      #     uiOutput("EspLimUNIC"),
                                      #     uiOutput("StopLossUNIC"),
                                      #     uiOutput("ExcesMoyUNIC")
                                  ),
                                  align = "center"
                           ),
                           
                           ## Fonctions
                           column(width = 5,
                                  box(
                                      title = "Fonctions",
                                      width = NULL,
                                      solidHeader = TRUE,
                                      tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                      status = "danger", # pour couleur de la boite, diff couleur pour statut
                                      uiOutput("xUNIC_SERVER"),
                                      uiOutput("densityUNIC"),
                                      
                                      switchInput(
                                          inputId = "xlim_UNIC",
                                          onStatus = "success",
                                          onLabel = "Répartition",
                                          offStatus = "info",
                                          offLabel = "Survie",
                                          value = T,
                                          labelWidth = "10px"
                                      ),
                                      uiOutput("repartsurvieUNIC"),
                                      p("Graphique"),
                                      radioGroupButtons(inputId = "plot_choice_UNIC", 
                                                        choices = c("Densité", 
                                                                    "Fonction de répartition"),
                                                        selected = "Densité",
                                                        justified = TRUE),
                                      plotlyOutput("FxUNIC")
                                      
                                  ),
                                  align = "center"
                                  
                           ),

                           column(width = 4,
                                  boxPlus(
                                      title = "Mesure de risques",
                                      width = NULL,
                                      solidHeader = TRUE,
                                      status = "success",
                                      closable = F,
                                      tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du texte
                                      numericInput('kUNIC', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                                      uiOutput("VaRUNIC"),
                                      # ,uiOutput("TVaRUNIC")
                                      radioGroupButtons(inputId = "plot_choice_UNIC_QX", 
                                                        choices = c("Densité", 
                                                                    "Fonction de répartition"),
                                                        selected = "Fonction de répartition",
                                                        justified = TRUE),
                                      plotlyOutput("QxUNIC"),
                                      plotlyOutput("QuantileUNIC")
                                      ),

                                  align = "center"
                           )
                       )
)

#### Loi Beta UI ----
tab_BETA_UI <- tabItem(tabName = "Beta",
                        fluidRow(
                            useShinyjs(), # utilisé to gray out les paramètres de la gamma qu'on désire fixe
                            titlePanel(tags$a("Loi Bêta", target = "_blank", href ="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Beta")),
                            # withMathJax(),
                            helpText("\\(X \\sim\\mathcal{Beta} \\ (\\alpha, \\beta)\\)"),
                            align = "center"
                        ),
                        fluidRow(
                            {
                                ### Paramètres Beta ----
                                column(
                                    width = 3,
                                    boxPlus(
                                        title = "Paramètres",
                                        status = "primary",
                                        solidHeader = T,
                                        width = NULL,closable = F,
                                        numericInput('alphaBETA', withMathJax('$$\\alpha$$'), value = 2, min = 1),
                                        numericInput('betaBETA', '$$\\beta$$', value = 1, min = 1)
                                    ),
                                ### Moments Beta  ----
                                box(
                                    title = "Moments",
                                    width = NULL,
                                    solidHeader = TRUE,
                                    status = "warning",
                                    uiOutput("meanBETA"),
                                    uiOutput("varianceBETA"),
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
                                    width = 5,
                                    box(
                                        title = "Fonctions",
                                        width = NULL,
                                        solidHeader = TRUE,
                                        # tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                        status = "danger", # pour couleur de la boite, diff couleur pour statut
                                        numericInput('xBETA', '$$x$$', value = 0.5, min = 0, max = 1, step = .1),
                                        uiOutput("densityBETA"),

                                        switchInput(
                                            inputId = "xlim_BETA",
                                            onStatus = "success",
                                            onLabel = "Répartition",
                                            offStatus = "info",
                                            offLabel = "Survie",
                                            value = T,
                                            labelWidth = "10px"
                                        ),
                                        uiOutput("repartsurvieBETA"),
                                        p("Graphique"),
                                        radioGroupButtons(inputId = "plot_choice_BETA", 
                                                          choices = c("Densité", 
                                                                      "Fonction de répartition"),
                                                          selected = "Densité",
                                                          justified = TRUE),
                                        plotlyOutput("FxBETA")
                                    ),
                                    align = "center"
                                )
                            },

                            {
                                ### Mesures de risque Beta  ----
                                column(
                                    width = 4,
                                    boxPlus(
                                        title = "Mesure de risques",
                                        width = NULL,
                                        solidHeader = TRUE,
                                        closable = F,
                                        status = "success",
                                        # tags$style(" * {font-size:20px }"), # ligne qui augmente la grosseur du texte
                                        numericInput('kBETA', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                                        uiOutput("VaRBETA"),
                                        uiOutput("TVaRBETA"),
                                        radioGroupButtons(inputId = "plot_choice_BETA_QX", 
                                                          choices = c("Densité", 
                                                                      "Fonction de répartition"),
                                                          selected = "Fonction de répartition",
                                                          justified = TRUE),
                                        plotlyOutput("QxBETA"),
                                        plotlyOutput("QuantileBETA")
                                        
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
                             titlePanel(tags$a("Loi Erlang", target = "_blank", href ="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Erlang")),
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
                                   ### Moments Erlang  ----
                                   box(
                                       title = "Moments",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       uiOutput("meanERLANG"),
                                       uiOutput("varERLANG"),
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
                                       numericInput('xERLANG', '$$x$$', value = 0.5, min = 0, step = 1),
                                       uiOutput("densityERLANG"),

                                       switchInput(
                                           inputId = "xlim_ERLANG",
                                           onStatus = "success",
                                           onLabel = "Répartition",
                                           offStatus = "info",
                                           offLabel = "Survie",
                                           value = T,
                                           labelWidth = "10px"
                                       ),
                                       uiOutput("repartsurvieERLANG"),
                                       p("Graphique"),
                                       # radioGroupButtons(inputId = "plot_choice_ERLANG",
                                       #                   choices = c("Densité",
                                       #                               "Fonction de répartition"),
                                       #                   selected = "Densité",
                                       #                   justified = TRUE),
                                       plotlyOutput("FxERLANG")
                                   ),
                                   align = "center"
                               )
                           },

                           # {
                               # Mesures de risque ERLANG  ----
                               # column(
                               #     width = 4,
                               #     boxPlus(
                               #         title = "Mesure de risques",
                               #         width = NULL,
                               #         solidHeader = TRUE,
                               #         closable = F,
                               #         status = "success",
                                       # tags$style(" * {font-size:20px }"), # ligne qui augmente la grosseur du texte
                                       # numericInput('kERLANG', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                                       # uiOutput("VaRERLANG"),
                                       # uiOutput("TVaRERLANG"),
                                       # radioGroupButtons(inputId = "plot_choice_ERLANG_QX",
                                       #                   choices = c("Densité",
                                       #                               "Fonction de répartition"),
                                       #                   selected = "Fonction de répartition",
                                                         # justified = TRUE),
                                       # plotlyOutput("QxERLANG"),
                                       # plotlyOutput("QuantileERLANG")
                                   # ),
                                   # align = "center"
                               # )
                           # }
                       )
)

#### Loi Log-logistique UI ----
tab_LOGLOGIS_UI <- tabItem(tabName = "LOGLOGIS",
                       fluidPage(
                           useShinyjs(), # utilisé to gray out les paramètres de la gamma qu'on désire fixe
                           titlePanel(tags$a("Loi log-logistique", target = "_blank", href ="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Log-Logistique")),
                           # withMathJax(),
                           helpText("\\(X \\sim\\mathcal{LL} \\ (\\lambda, \\tau)\\)"),
                           align = "center"
                       ),

                       fluidRow(
                           {
                               ### Paramètres LOGLOGIS ----
                               column(
                                   width = 3,
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
                                   ### Moments LOGLOGIS  ----
                                   box(
                                       title = "Moments",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       status = "warning",
                                       uiOutput("meanLOGLOGIS"),
                                       uiOutput("varLOGLOGIS"),
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
                                   width = 5,
                                   box(
                                       title = "Fonctions",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       # tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                       status = "danger", # pour couleur de la boite, diff couleur pour statut
                                       numericInput('xLOGLOGIS', '$$x$$', value = 2, min = 0, step = 1),
                                       uiOutput("densityLOGLOGIS"),

                                       switchInput(
                                           inputId = "xlim_LOGLOGIS",
                                           onStatus = "success",
                                           onLabel = "Répartition",
                                           offStatus = "info",
                                           offLabel = "Survie",
                                           value = T,
                                           labelWidth = "10px"
                                       ),
                                       uiOutput("repartsurvieLOGLOGIS"),
                                       p("Graphique"),
                                       radioGroupButtons(inputId = "plot_choice_LOGLOGIS", 
                                                         choices = c("Densité", 
                                                                     "Fonction de répartition"),
                                                         selected = "Densité",
                                                         justified = TRUE),
                                       plotlyOutput("FxLOGLOGIS")
                                   ),
                                   align = "center"
                               )
                           },

                           {
                               ### Mesures de risque LOGLOGIS  ----
                               column(
                                   width = 4,
                                   boxPlus(
                                       title = "Mesure de risques",
                                       width = NULL,
                                       solidHeader = TRUE,
                                       closable = F,
                                       status = "success",
                                       # tags$style(" * {font-size:20px }"), # ligne qui augmente la grosseur du texte
                                       numericInput('kLOGLOGIS', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                                       uiOutput("VaRLOGLOGIS"),
                                       uiOutput("TVaRLOGLOGIS"),
                                       radioGroupButtons(inputId = "plot_choice_LOGLOGIS_QX", 
                                                         choices = c("Densité", 
                                                                     "Fonction de répartition"),
                                                         selected = "Fonction de répartition",
                                                         justified = TRUE),
                                       plotlyOutput("QxLOGLOGIS"),
                                       plotlyOutput("QuantileLOGLOGIS")
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
                        numericInput('pBIN', '$$p$$', value = 0.5, min = 0, max = 1, step = 0.05)
                    ), 
                    ## Moments
                    tags$style(" * {font-size:20px}"), # grosseur du texte
                    box(
                        title = "Moments", width = NULL, solidHeader = TRUE, status = "warning",
                        uiOutput("meanBIN"),
                        uiOutput("varBIN")
                    ),
                    align = "center"
             ),
             
             ## Fonctions
             column(width = 5,
                    box(
                        title = "Fonctions", width = NULL, solidHeader = TRUE,
                        tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                        status = "danger", # pour couleur de la boite, diff couleur pour statut
                        numericInput('xBIN', '$$x$$', min = 0, max = 5, value = 0, step = 1),
                        uiOutput("densityBIN"),
                        
                        switchInput(
                            inputId = "xlim_BIN",
                            onStatus = "danger",
                            onLabel = "Répartition",
                            offStatus = "info",
                            offLabel = "Survie",
                            value = T,
                            labelWidth = "10px"
                        ),
                        uiOutput("repartsurvieBIN"),
                        p("Graphique"),
                        radioGroupButtons(inputId = "plot_choice_BIN", 
                                          choices = c("Fonction de masse", 
                                                      "Fonction de répartition"),
                                          selected = "Fonction de masse",
                                          justified = TRUE),
                        plotlyOutput("FxBIN")
                        # tabBox(
                        #     width = NULL,
                        #     tabPanel("Fonction de répartition",
                        #              uiOutput("repartBIN")
                        #              # ,
                        #              # plotlyOutput("FxBIN")
                        #     ),
                        #     tabPanel("Survie",
                        #              uiOutput("survieBIN")
                        #              # ,
                        #              # plotlyOutput("SxBIN")
                        #     )
                        # 
                        # )
                    ),
                    align = "center"


             )
             ,

             column(width = 4,
                    boxPlus(
                        title = "Mesure de risques",
                        width = NULL,
                        solidHeader = TRUE,
                        status = "success",
                        closable = F,
                        tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du texte
                        numericInput('kBIN', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                        uiOutput("VaRBIN"),
                        uiOutput("TVaRBIN"),
                        plotlyOutput("QxBIN")
                    ),

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
        uiOutput("loi_BN"),
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
                title = "Plus d'information",
                status = "primary",
                solidHeader = T,
                collapsed = T,
                collapsible = T,
                width = NULL,
                helpText("Lien entre les paramétrisations
                         $$\\beta = \\frac{1 - q}{q}$$
                         $$q = \\frac{1}{1 + \\beta}$$",
                         "Si on défini la binomiale négative (ou géométrique) comme étant le nombre d'échecs avant un r-ème éssai alors son support commence à 0 (on peut avoir r succès avec aucun échec.",
                         "Cependant, si on la défini comme étant le nombre d'essais pour un r-ème succès, alors forcément son support doit débuter à r car on ne peut pas avoir moins que r essais pour r succès."),
                align = "center"
            ),
            box(
                title = "Paramètres",
                status = "primary",
                solidHeader = T,
                width = NULL,
                uiOutput("changingrBN"),
                uiOutput("changingqBN"),
                
                switchInput(labelWidth = "10px",handleWidth = "400px",
                            inputId = "definitionBN",
                            onLabel = 'essais',
                            offLabel = "échecs", 
                            size = "large",
                            value = T
                ),
                
                switchInput(labelWidth = "10px",handleWidth = "400px",
                    inputId = "distrchoiceqBN",
                    onLabel = 'q',
                    offLabel = "&beta;", size = "large",
                    value = T
                )
            ),
            #### Moments BN ####
            tags$style(" * {font-size:20px}"), # grosseur du tezte
            box(
                title = "Essais vs erreurs",
                status = "primary",
                solidHeader = T,
                collapsed = T,
                collapsible = T,
                width = NULL,
                helpText("Si on défini la binomiale négative (ou géométrique) comme étant le nombre d'échecs avant un r-ème succès alors son support commence à 0 (on peut avoir r succès avec aucun échec).",
                         "Cependant, si on la défini comme étant le nombre d'essais pour un r-ème succès, alors forcément son support doit débuter à r car on ne peut pas avoir moins que r essais pour r succès."),
                align = "left"
            ),
            box(
                title = "Moments", width = NULL, solidHeader = TRUE, status = "warning",
                uiOutput("meanBN"),
                uiOutput("varBN")
            ),
            align = "center"
        ),

        #### Fonctions BN ####
        column(width = 5,
               box(
                   title = "Fonctions", width = NULL, solidHeader = TRUE,
                   status = "danger", # pour couleur de la boite, diff couleur pour statut
                   uiOutput("changingxBN"),
                   # numericInput('xBN', '$$x$$', min = 0, value = 0, step = 1),
                   uiOutput("densityBN"),
                   
                   switchInput(
                       inputId = "xlim_BN",
                       onStatus = "danger",
                       onLabel = "Répartition",
                       offStatus = "info",
                       offLabel = "Survie",
                       value = T,
                       labelWidth = "10px"
                   ),
                   uiOutput("repartsurvieBN"),
                   p("Graphique"),
                   radioGroupButtons(inputId = "plot_choice_BN", 
                                     choices = c("Fonction de masse", 
                                                 "Fonction de répartition"),
                                     selected = "Fonction de masse",
                                     justified = TRUE),
                   plotlyOutput("FxBN")

               ),
               align = "center"
        )
        # ,
        # ### Mesures de risque BN ####
        # column(width = 4,
        #        boxPlus(
        #            title = "Mesure de risques",
        #            width = NULL,
        #            solidHeader = TRUE,
        #            closable = F,
        #            status = "success",
        #            tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du texte
        #            numericInput('kBN', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
        #            uiOutput("VaRBN"),
        #            uiOutput("TVaRBN"),
        #            radioGroupButtons(inputId = "plot_choice_BN_QX", 
        #                              choices = c("Fonction de masse", 
        #                                          "Fonction de répartition"),
        #                              selected = "Fonction de répartition",
        #                              justified = TRUE),
        #            plotlyOutput("QxBN")
        #        ),
               # align = "center"
        # )

        #### ####
    )
)
}
#### Loi Poisson UI ####
tab_POI_UI <- tabItem(tabName = "Poisson",
                      fluidPage(
                          titlePanel(tags$a("Loi de Poisson", target = "_blank", href ="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-de-Poisson")),
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
                                 ## Moments
                                 tags$style(" * {font-size:20px}"), # grosseur du tezte
                                 box(
                                     title = "Moments", width = NULL, solidHeader = TRUE, status = "warning",
                                     uiOutput("meanPOI"),
                                     uiOutput("varPOI")
                                 ),
                                 align = "center"
                          ),

                          ## Fonctions
                          column(width = 5,
                                 box(
                                     title = "Fonctions",
                                     width = NULL,
                                     solidHeader = TRUE,
                                     tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                     status = "danger", # pour couleur de la boite, diff couleur pour statut
                                     numericInput('xPOI', '$$x$$', min = 0, value = 0, step = 1),
                                     uiOutput("densityPOI"),

                                     switchInput(
                                         inputId = "xlim_POI",
                                         onStatus = "danger",
                                         onLabel = "Répartition",
                                         offStatus = "info",
                                         offLabel = "Survie",
                                         value = T,
                                         labelWidth = "10px"
                                     ),
                                     uiOutput("repartsurviePOI"),
                                     p("Graphique"),
                                     radioGroupButtons(inputId = "plot_choice_POI", 
                                                       choices = c("Fonction de masse", 
                                                                   "Fonction de répartition"),
                                                       selected = "Fonction de masse",
                                                       justified = TRUE),
                                     plotlyOutput("FxPOI")
                                 ),
                                 align = "center"

                          ),

                          column(width = 4,
                                 boxPlus(
                                     title = "Mesure de risques",
                                     width = NULL,
                                     solidHeader = TRUE,
                                     status = "success",
                                     closable = F,
                                     tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du texte
                                     numericInput('kPOI', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                                     uiOutput("VaRPOI"),
                                     uiOutput("TVaRPOI")
                                     # ,radioGroupButtons(inputId = "plot_choice_POI_QX", 
                                     #                   choices = c("Fonction de masse", 
                                     #                               "Fonction de répartition"),
                                     #                   selected = "Fonction de répartition",
                                     #                   justified = TRUE),
                                 # plotlyOutput("QxPOI")
                                     ),

                                 align = "center"
                          )
                      )
)

#### Loi Hypergéométrique UI ####
tab_HG_UI <- tabItem(tabName = "HG",
                      fluidPage(
                          titlePanel(tags$a("Loi Hypergéométrique", href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Hypergéométrique", target = "_blank")),
                          withMathJax(),
                          helpText("\\(X \\sim\\mathcal{HyperGéo} \\ (N, n, m)\\)"),
                          align = "center"
                      ),

                      fluidRow(
                          column(width = 3,
                                 box(title = "Paramètres",
                                     status = "primary",
                                     solidHeader = T,
                                     width = NULL,
                                     numericInput('grosNHG', '$$N$$', value = 4, min = 0, step = 1),
                                     uiOutput("changingpetitNHG"),
                                     uiOutput("changingmHG")
                                 ),
                                 ## Moments
                                 box(
                                     title = "Moments", width = NULL, solidHeader = TRUE, status = "warning",
                                     uiOutput("meanHG"),
                                     uiOutput("varHG")
                                 ),
                                 align = "center"
                          ),
                          
                          ## Fonctions
                          column(width = 5,
                                 box(
                                     title = "Fonctions",
                                     width = NULL,
                                     solidHeader = TRUE,
                                     tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                     status = "danger", # pour couleur de la boite, diff couleur pour statut
                                     uiOutput("changingxHG"),
                                     uiOutput("densityHG"),
                                     
                                     switchInput(
                                         inputId = "xlim_HG",
                                         onStatus = "danger",
                                         onLabel = "Répartition",
                                         offStatus = "info",
                                         offLabel = "Survie",
                                         value = T,
                                         labelWidth = "10px"
                                     ),
                                     uiOutput("repartsurvieHG"),
                                     p("Graphique"),
                                     radioGroupButtons(inputId = "plot_choice_HG", 
                                                       choices = c("Fonction de masse", 
                                                                   "Fonction de répartition"),
                                                       selected = "Fonction de masse",
                                                       justified = TRUE),
                                     plotlyOutput("FxHG")

                                 ),
                                 align = "center"

                          )

                      )
)

#### Loi Logarithmique UI ####
tab_LOGARITHMIQUE_UI <- tabItem(tabName = "Logarithmique",
                     fluidPage(
                         titlePanel(tags$a("Loi Logarithmique", target = "_blank", href ="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Logarithmique")),
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
                                ## Moments
                                tags$style(" * {font-size:20px}"), # grosseur du tezte
                                box(
                                    title = "Moments", width = NULL, solidHeader = TRUE, status = "warning",
                                    uiOutput("meanLOGARITHMIQUE"),
                                    uiOutput("varLOGARITHMIQUE")
                                ),
                                align = "center"
                         ),

                         ## Fonctions
                         column(width = 5,
                                box(
                                    title = "Fonctions",
                                    width = NULL,
                                    solidHeader = TRUE,
                                    tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                    status = "danger", # pour couleur de la boite, diff couleur pour statut
                                    numericInput('xLOGARITHMIQUE', '$$x$$', min = 0, value = 0, step = 1),
                                    uiOutput("densityLOGARITHMIQUE"),

                                    switchInput(
                                        inputId = "xlim_LOGARITHMIQUE",
                                        onStatus = "danger",
                                        onLabel = "Répartition",
                                        offStatus = "info",
                                        offLabel = "Survie",
                                        value = T,
                                        labelWidth = "10px"
                                    ),
                                    uiOutput("repartsurvieLOGARITHMIQUE"),
                                    p("Graphique"),
                                    radioGroupButtons(inputId = "plot_choice_LOGARITHMIQUE",
                                                      choices = c("Fonction de masse",
                                                                  "Fonction de répartition"),
                                                      selected = "Fonction de masse",
                                                      justified = TRUE),
                                    plotlyOutput("FxLOGARITHMIQUE")
                                    
                                ),
                                align = "center"

                         )
                         ,

                         column(width = 4,
                                boxPlus(
                                    title = "Mesure de risques",
                                    width = NULL,
                                    solidHeader = TRUE,
                                    status = "success",
                                    closable = F,
                                    tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du texte
                                    numericInput('kLOGARITHMIQUE', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
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
                          titlePanel(tags$a("Loi  Uniforme", target = "_blank", href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Uniforme-Discr%C3%A8te")),
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
                                     uiOutput('changing_aUNID'),
                                     # uiOutput('changing_bUNID')
                                     numericInput('bUNID', 
                                                  '$$b$$', 
                                                  value = 2, 
                                                  min = 0,
                                                  # min = changing_aUNID(),
                                                  step = 1)
                                 ),
                                 ## Moments
                                 tags$style(" * {font-size:20px}"), # grosseur du tezte
                                 box(
                                     title = "Moments", width = NULL, solidHeader = TRUE, status = "warning",
                                     uiOutput("meanUNID"),
                                     uiOutput("varUNID")
                                 ),
                                 align = "center"
                          ),
                          
                          ## Fonctions
                          column(width = 5,
                                 box(
                                     title = "Fonctions",
                                     width = NULL,
                                     solidHeader = TRUE,
                                     tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
                                     status = "danger", # pour couleur de la boite, diff couleur pour statut
                                     uiOutput("xUNID_UI"),
                                     uiOutput("densityUNID"),
                                     
                                     switchInput(
                                         inputId = "xlim_UNID",
                                         onStatus = "danger",
                                         onLabel = "Répartition",
                                         offStatus = "info",
                                         offLabel = "Survie",
                                         value = T,
                                         labelWidth = "10px"
                                     ),
                                     uiOutput("repartsurvieUNID"),
                                     p("Graphique"),
                                     radioGroupButtons(inputId = "plot_choice_UNID", 
                                                       choices = c("Fonction de masse", 
                                                                   "Fonction de répartition"),
                                                       selected = "Fonction de masse",
                                                       justified = TRUE),
                                     plotlyOutput("FxUNID")
                                     
                                 ),
                                 align = "center"
                                 
                          )
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
                choices = c("Gamma" 
                            # ,"Lognormale"
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
                    # numericInput('koBNCOMP', label = withMathJax('$$k_{0}$$'), value = 300, step = 100, min = 0, max = 1),
                    uiOutput("koBNCOMPUI"),
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
                #### Moments BNCOMP ####
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
                width = 5,
                div(id = "Repartition-box-BNCOMP",
                box(
                    title = "Fonctions",
                    width = NULL,
                    solidHeader = TRUE, # grosseur du tezte
                    status = "danger", # couleur de la boite
                    numericInput('xBNCOMP', '$$x$$', value = 10, min = 0),
                    # uiOutput("densityBNCOMP"),
                    
                    switchInput(
                        inputId = "xlim_BNCOMP",
                        onStatus = "danger",
                        onLabel = "Répartition",
                        offStatus = "info",
                        offLabel = "Survie",
                        value = T,
                        labelWidth = "10px"
                    ),
                    
                    uiOutput("repartsurvieBNCOMP"),
                    p("Graphique"),
                    radioGroupButtons(inputId = "plot_choice_BNCOMP",
                                      choices = c(
                                          # "Fonction de masse",
                                                  "Fonction de répartition"),
                                      selected = "Fonction de répartition",
                                      justified = TRUE),
                    plotlyOutput("FxBNCOMP")
                    
                )
                ),
                align = "center"
            ),

            #### Mesures de risque BNCOMP ####
            column(
                width = 4,
                div(id = "Quantile-box-BNCOMP",
                boxPlus(
                    title = "Mesure de risques",
                    width = NULL,
                    solidHeader = TRUE,
                    closable = F,
                    status = "success", # grosseur du tezte
                    numericInput('kBNCOMP', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                    uiOutput("VaRBNCOMP"),
                    uiOutput("TVaRBNCOMP"),
                    plotlyOutput("QxBNCOMP")
                )
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
                choices = c("Gamma" 
                            # ,"Lognormale"
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
                    # numericInput('koPCOMP', label = withMathJax('$$k_{0}$$'), value = 200, step = 100, min = 0, max = 1),
                    uiOutput("koPCOMPUI"),
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
                #### Moments PCOMP ####
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
                width = 5,
                div(id = "Repartition-box-PCOMP",
                box(
                    title = "Fonctions",
                    width = NULL,
                    solidHeader = TRUE, # grosseur du tezte
                    status = "danger", # couleur de la boite
                    numericInput('xPCOMP', '$$x$$', value = 10, min = 0),
                    
                    switchInput(
                        inputId = "xlim_PCOMP",
                        onStatus = "danger",
                        onLabel = "Répartition",
                        offStatus = "info",
                        offLabel = "Survie",
                        value = T,
                        labelWidth = "10px"
                    ),
                    uiOutput("repartsurviePCOMP"),
                    p("Graphique"),
                    # div(id = "DIV-FX_PLOT_RANGE-PCOMP", style="width: 100% ; height: 400px",
                    #     sliderInput(inputId = "PCOMP_FX_PLOT_RANGE", 
                    #                 label = "",
                    #                 min = 0, 
                    #                 max = 100, 
                    #                 value = c(0, 30)
                    #     # )
                    # ),
                    radioGroupButtons(inputId = "plot_choice_PCOMP",
                                      choices = c(
                                          # "Fonction de masse",
                                                  "Fonction de répartition"),
                                      selected = "Fonction de répartition",
                                      justified = TRUE),
                    plotlyOutput("FxPCOMP")
                    
                )
                ),
                align = "center"
            ),

            #### Mesures de risque PCOMP ####
            column(
                width = 4,
                div(id = "Quantile-box-PCOMP",
                boxPlus(
                    title = "Mesure de risques",
                    width = NULL,
                    solidHeader = TRUE,
                    closable = F,
                    status = "success", # grosseur du tezte
                    numericInput('kPCOMP', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                    uiOutput("VaRPCOMP"),
                    uiOutput("TVaRPCOMP"),
                    plotlyOutput("QxPCOMP")
                )
                ),
                align = "center"
            )

        )
    )
}

#### Loi Binomiale Composée UI ####
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
                choices = c("Gamma" 
                            # ,"Lognormale"
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
                    uiOutput("koBINCOMPUI"),
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
                #### Moments BINCOMP ####
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
                width = 5,
                div(id = "Repartition-box-BINCOMP",
                    box(
                    title = "Fonctions",
                    width = NULL,
                    solidHeader = TRUE, # grosseur du tezte
                    status = "danger", # couleur de la boite
                    uiOutput("xBINCOMPUI"),
                    # uiOutput("densityBINCOMP"),
                    
                    switchInput(
                        inputId = "xlim_BINCOMP",
                        onStatus = "danger",
                        onLabel = "Répartition",
                        offStatus = "info",
                        offLabel = "Survie",
                        value = T,
                        labelWidth = "10px"
                    ),
                    uiOutput("repartsurvieBINCOMP"),
                    p("Graphique"),
                    radioGroupButtons(inputId = "plot_choice_BINCOMP",
                                      choices = c(
                                          # "Fonction de masse",
                                          "Fonction de répartition"),
                                      selected = "Fonction de répartition",
                                      justified = TRUE),
                    plotlyOutput("FxBINCOMP")
                    
                    )
                ),
                align = "center"
            ),
            
            #### Mesures de risque BINCOMP ####
            column(
                width = 4,
                div(id = "Quantile-box-BINCOMP",
                    boxPlus(
                        title = "Mesure de risques",
                        width = NULL,
                        solidHeader = TRUE,
                        closable = F,
                        status = "success", # grosseur du tezte
                        numericInput('kBINCOMP', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
                        uiOutput("VaRBINCOMP"),
                        uiOutput("TVaRBINCOMP"),
                        plotlyOutput("QxBINCOMP")
                    )
                ),
                align = "center"
            )
            
        )
    )
}
