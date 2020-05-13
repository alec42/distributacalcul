#' @export
lawParametersBox <- function(input, output, session, law) {
    
    #### Définir distribution ####
    # parameters_symbol_name <- list(
    #     case_when(
    #         law == "norm" ~ expr("mean"),
    #         law == "lnorm" ~ expr("meanlog"),
    #         law == "beta" ~ expr("shape1"),
    #         TRUE ~ expr("shape")
    #     ),
    #     case_when(
    #         law == "norm" ~ expr("sd"),
    #         law == "lnorm" ~ expr("sdlog"),
    #         law == "beta" ~ expr("shape2"),
    #         TRUE ~ expr("rate")
    #     )
    # )
    # parameters_distr <- list(
    #     as.numeric(shape()), as.numeric(rate())
    # )
    # names(parameters_distr) <- parameters_symbol_name
    
    parameters_latex <- case_when(
        law %in% c("norm", "lnorm") ~ c("$$\\mu$$", "$$\\sigma^2$$"),
        law %in% c("gamma", "exp", "beta") ~ c("$$\\alpha$$", "$$\\beta$$"),
        law == "unif" ~ c("$$a$$", "$$b$$"),
        TRUE ~ c("shape", "rate")
    )
    
    #### Créé paramètres ####
    shape <- reactive({
        input$shape
    })
    rate <- reactive({
        # case_when(
            # law %in% c("norm", "lnorm") ~ sqrt(as.numeric(input$rate)),
            # TRUE ~ input$rate 
        # )
        input$rate
    })
    d <- reactive({
        input$d
    })
    kap <- reactive({
        input$kap
    })
    less.than.d <- reactive({
        input$less.than.d
    })
    
    ns <- session$ns
    
    
    #### Render paramètres ####
    output$shape <- renderUI({
        numericInput(session$ns("shape"),
                     withMathJax(parameters_latex[1]), value = 1)
    })
    output$rate <- renderUI({
        numericInput(session$ns("rate"),
                     withMathJax(parameters_latex[2]), value = 2)
    })
    output$less.than.d <- renderUI({
        radioGroupButtons(
            inputId = session$ns("less.than.d"), 
            label = "", 
            choiceNames = list(withMathJax("$$\\geq$$"), withMathJax("$$\\leq$$")), 
            choiceValues = list(TRUE, FALSE)
        )
    })
    output$d <- renderUI({
        numericInput(session$ns("d"),
                     label = withMathJax("$$d$$"),
                     value = 0,
                     width = "20px"
        )
    })
    output$kap <- renderUI({
        numericInput(session$ns("kap"),
                     label = withMathJax("$$\\kappa$$"),
                     value = 0.99,
                     min = 0, max = 1, step = 0.10,
                     width = "20px"
        )
    })

    #### Calcul mesures de risque ####
    VaR <- reactive({
            format(
                rlang::exec(
                    .fn = paste0("VaR_", law),
                    kap = as.numeric(kap()),
                    as.numeric(shape()), as.numeric(rate())
                ),
                nsmall = 6
            )
    })
    TVaR <- reactive({
        format(
            rlang::exec(
                .fn = paste0("TVaR_", law),
                kap = as.numeric(kap()),
                as.numeric(shape()), as.numeric(rate())
            ),
            nsmall = 6
        )
    })
    
    #### Calcul moments ####
    E <- reactive({
        rlang::exec(
            .fn = paste0("E_", law), 
            as.numeric(shape()), as.numeric(rate())
        )
    })
    V <- reactive({
        rlang::exec(
            .fn = paste0("V_", law), 
            as.numeric(shape()), as.numeric(rate())
        )
    })
    Etronq <- reactive({
        rlang::exec(
            .fn = paste0("Etronq_", law), 
            d = as.numeric(d()), 
            as.numeric(shape()), as.numeric(rate()), 
            less.than.d = as.logical(less.than.d())
        )
    })
    SL <- reactive({
        rlang::exec(
            .fn = paste0("SL_", law), 
            d = as.numeric(d()), 
            as.numeric(shape()), as.numeric(rate())
        )
    })
    Elim <- reactive({
        rlang::exec(
            .fn = paste0("Elim_", law), 
            d = as.numeric(d()), 
            as.numeric(shape()), as.numeric(rate())
        )
    })
    Mexcess <- reactive({
        rlang::exec(
            .fn = paste0("Mexcess_", law), 
            d = as.numeric(d()), 
            as.numeric(shape()), as.numeric(rate())
        )
    })
    
    #### Render moments ####
    output$E <- renderUI({
        withMathJax(sprintf("$$\\text{E}[X] = %s$$", 
                            E()
                            )
                    )
    })
    output$V <- renderUI({
        withMathJax(sprintf("$$\\text{Var}(X) = %s$$", 
                            V()
                            )
                    )
    })
    output$Etronq <- renderUI({
            withMathJax(
                sprintf(
                    "$$\\text{E}[X \\times 1_{\\{X %s %s\\}}] = %.4f$$",
                    ifelse(!is.null(less.than.d) & as.logical(less.than.d()) == TRUE, "\\geq", "\\leq"),
                    d(),
                    Etronq()
                )
            )
        })
    output$SL <- renderUI({
            withMathJax(sprintf("$$\\pi_{%s}(X) = %.4f$$",
                                d(),
                                SL()
                                )
                        )
    })
    output$Elim <- renderUI({
        withMathJax(sprintf("$$\\text{E}[\\min(X;%s)] = %.4f$$",
                            d(),
                            Elim()
                            )
                    )
    })
    output$Mexcess <- renderUI({
            withMathJax(sprintf("$$e_{%s}(X) = %.4f$$",
                                d(),
                                Mexcess()))
        })
    
    #### Render mesures de risque ####
    output$VaR <- renderUI({
        withMathJax(sprintf("$$VaR_{%s} = %s$$",
                            # VaR_Quantile_LATEX(),
                            kap(),
                            VaR()
                            )
                    )
    })
    output$TVaR <- renderUI({
        withMathJax(sprintf("$$TVaR_{%s} = %s$$",
                            kap(),
                            TVaR()
                            )
                    )
    })
    
}

# radioGroupContainer <- function(id, ...) {
#     class <- "form-group shiny-input-radiogroup shiny-input-container"
#     div(id = id, class = class, ...)
# }
#' @export
lawParametersBoxUI <- function(id) {
    ns <- NS(id)
    fluidRow(
    column(
        width = 3,
        
        #### Paramètres ####
        boxPlus(
            title = "Paramètres",
            status = "primary",
            background = "blue",
            solidHeader = TRUE,
            width = NULL,
            closable = FALSE,
            splitLayout(
                uiOutput(ns("shape")),
                uiOutput(ns("rate"))
            )
        ),
        tags$head(
            tags$style(
                type = "text/css", 
                "label { 
                    display: table-cell;
                    text-align: center;
                    vertical-align: middle;
                }
                .form-group {
                    display: table-row;
                }
                "
            )
        ),
        
        #### Moments ####
        box(
            title = "Moments",
            width = NULL,
            solidHeader = TRUE,
            status = "warning",
            uiOutput(ns("E")),
            uiOutput(ns("V")),
            uiOutput(ns("d")),
            splitLayout(
                uiOutput(ns("less.than.d")),
                uiOutput(ns("Etronq"))
            ),
            uiOutput(ns("Elim")),
            uiOutput(ns("SL")), 
            uiOutput(ns("Mexcess")) 
        )
    ),
    
    #### Mesures de risque ####
    column(
        width = 4,
        boxPlus(
            title = "Mesures de risque",
            width = NULL,
            solidHeader = TRUE,
            closable = FALSE,
            status = "success",
            uiOutput(ns("kap")),
            uiOutput(ns("VaR")),
            uiOutput(ns("TVaR"))
        ),
        align = "center"
    )
    )
}
