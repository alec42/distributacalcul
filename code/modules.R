# tab_NORM_UI <- tabItem(tabName = "Normale",
#                        fluidRow(
#                            useShinyjs(), # utilisé to gray out les paramètres de la gamma qu'on désire fixe
#                            # titlePanel("Loi Normale"),
#                            titlePanel(tags$a("Loi Normale", target = "_blank", href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Normal")),
#                            # withMathJax(),
#                            helpText("\\(X \\sim\\mathcal{Normale} \\ (\\mu, \\sigma^2)\\)"),
#                            align = "center"
#                        ),
#                        fluidRow(
#                            {
#                                
#                            },
#                            
#                            {
#                                ### Fonctions Normale ----
#                                column(
#                                    width = 5,
#                                    box(
#                                        title = "Fonctions",
#                                        width = NULL,
#                                        solidHeader = TRUE,
#                                        # tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
#                                        status = "danger", # pour couleur de la boite, diff couleur pour statut
#                                        # numericInput('xNORM', '$$x$$', value = 0),
#                                        uiOutput("densityNORM"),
#                                        
#                                        # switchInput(
#                                        #     inputId = "xlim_NORM",
#                                        #     onStatus = "success",
#                                        #     onLabel = "Répartition",
#                                        #     offStatus = "info",
#                                        #     offLabel = "Survie",
#                                        #     value = T,
#                                        #     labelWidth = "10px"
#                                        # ),
#                                        uiOutput("repartsurvieNORM"),
#                                        p("Graphique"),
#                                        # radioGroupButtons(inputId = "plot_choice_NORM", 
#                                        #                   choices = c("Densité", 
#                                        #                               "Fonction de répartition"),
#                                        #                   selected = "Densité",
#                                        #                   justified = TRUE),
#                                        plotlyOutput("FxNORM")
#                                    ),
#                                    align = "center"
#                                )
#                            },
#                            
#                            {
#                                ### Mesures de risque Normale  ----
#                                column(
#                                    width = 4,
#                                    boxPlus(
#                                        title = "Mesure de risques",
#                                        width = NULL,
#                                        solidHeader = TRUE,
#                                        closable = F,
#                                        status = "success",
#                                        # tags$style(" * {font-size:20px }"), # ligne qui augmente la grosseur du texte
#                                        # numericInput('kNORM', '$$\\kappa$$', value = 0.99, step = 0.005, min = 0, max = 1),
#                                        uiOutput("VaRNORM"),
#                                        uiOutput("TVaRNORM"),
#                                        # pickerInput(
#                                        #     inputId = "plot_choice_NORM_QX", 
#                                        #     # label = "Style : primary", 
#                                        #     choices = c("Fonction de densité",
#                                        #                 "Fonction de répartition",
#                                        #                 "Fonction quantile"),
#                                        #     selected = "Fonction de répartition",
#                                        #     options = list(
#                                        #         style = "btn-success")
#                                        # ),
#                                        plotlyOutput("QxNORM")
#                                        # radioGroupButtons(inputId = "plot_choice_NORM_QX", 
#                                        #                   choices = c("Densité", 
#                                        #                               "Fonction de répartition"),
#                                        #                   selected = "Fonction de répartition",
#                                        #                   justified = TRUE),
#                                        # plotlyOutput("QxNORM"),
#                                        # plotlyOutput("QuantileNORM")
#                                    ),
#                                    align = "center"
#                                )
#                            }
#                        )
# )
### Paramètres Normale ----

lawParametersBox <- function(input, output, session, parameters) {
    shape <- reactive({
        input$shape
    }) # muNORM
    rate <- reactive({
        input$rate
    }) # sigma2NORM
    d <- reactive({
        input$d
    }) # sigma2NORM
    
    ns <- serverns
    
    Etronq <- reactive({
        Etronq_norm(d = d(), shape(), sqrt(rate()))
    })
    SL <- reactive({
        SL_norm(d = d(), shape(), sqrt(rate()))
    })
    Elim <- reactive({
        Elim_norm(d = d(), shape(), sqrt(rate()))
    })
    Mexcess <- reactive({
        Mexcess_norm(d = d(), shape(), sqrt(rate()))
    })
    
    output$shape <- renderUI({
        numericInput(session$ns("shape"), 
                     withMathJax(parameters[1]), value = 0)# muNORM
    })
    
    output$rate <- renderUI({
        numericInput(session$ns("rate"),
                     withMathJax(parameters[2]), value = 1) # sigmaNORM
    })
    
    output$d <- renderUI({
        numericInput(session$ns("d"),
                     withMathJax("$$d$$"),
                     value = 0,
                     width = "20px") # dNORM
    })
    
    output$mean <- renderUI({
        withMathJax(sprintf("$$\\text{E}[X] = %s$$", shape()))
    })
    
    output$variance <- renderUI({
        withMathJax(sprintf("$$\\text{Var}(X) = %s$$", rate()))
    })
    
    output$Etronq <- renderUI({
            withMathJax(
                sprintf(
                    "$$\\text{E}[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$",
                    d(),
                    Etronq()
                )
            )
        })
    
    output$SL <- renderUI({
            withMathJax(sprintf("$$\\pi_{%s}(X) = %.4f$$",
                                d(),
                                SL()))
        })
    
    output$Elim <- renderUI({withMathJax(sprintf("$$\text{E}[\\text{min}(X;{%s})] = %.4f$$",
                                                 d(),
                                                       Elim()))
    })
    
    output$Mexcess <- renderUI({
            withMathJax(sprintf("$$e_{%s}(X) = %.4f$$",
                                d(),
                                Mexcess()))
        })
}

radioGroupContainer <- function(inputId, ...) {
    class <- "form-group shiny-input-radiogroup shiny-input-container"
    div(id = inputId, class = class, ...)
}

lawParametersBoxUI <- function(law, parameters) {
    ns <- NS(law)
    
    # tagList(
    column(
        width = 3,
        boxPlus(
            title = "Paramètres",
            status = "primary",
            solidHeader = T,
            width = NULL,
            closable = F,
            uiOutput(ns("shape")),
            uiOutput(ns("rate"))
        ),
        box(
            title = "Moments",
            width = NULL,
            solidHeader = TRUE,
            status = "warning",
            uiOutput(ns("mean")), #meanNORM
            uiOutput(ns("variance")), #varNORM
            uiOutput(ns("d")),
            # radioButtons(ns('equality'), label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
            uiOutput(ns("Etronq")), # EspTronqNORM
            uiOutput(ns("Elim")),   # EspLimNORM
            uiOutput(ns("SL")), # StopLossNORM
            uiOutput(ns("Mexcess"))  # ExcesMoyNORM
        # )
    ))
}
