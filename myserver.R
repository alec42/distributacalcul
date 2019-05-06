myserver <- function(input, output, session) 
{
    {
        ## Loi Normale Serveur ----
        muNORM <- reactive({input$muNORM})
        
        sigma2NORM <- reactive({input$sigmaNORM})
        
        densityNORM <- reactive({format(dnorm(input$xNORM, muNORM(), sqrt(sigma2NORM())), nsmall = 6)})
        
        repartNORM <- reactive({format(pnorm(input$xNORM, muNORM(), sqrt(sigma2NORM())), nsmall = 6)})
        
        survieNORM <- reactive({format(pnorm(input$xNORM, muNORM(), sqrt(sigma2NORM()), lower.tail = F), nsmall = 6)})
        
        VaRNORM <- reactive({format(qnorm(input$kNORM, muNORM(), sqrt(sigma2NORM())), nsmall = 6)})
        
        TVaRNORM <- reactive({format(muNORM() +
                                         (1/(1 - input$kNORM)) *
                                         sqrt(sigma2NORM()/(2*pi)) *
                                         exp(-qnorm(input$kNORM, muNORM(), sqrt(sigma2NORM())) / 2), 
                                     nsmall = 6)
        })
        
        EspTronqNORM <- reactive({muNORM() *
                pnorm((input$dNORM - muNORM()) / sqrt(sigma2NORM()), 0, 1) -
                sqrt(sigma2NORM()/ 2 * pi) *
                exp(-(input$dNORM - muNORM())^2 / 2 * sigma2NORM())
        })
        
        StopLossNORM <- reactive({(muNORM() +
                                       input$dNORM) *
                (1 - pnorm((input$dNORM - muNORM())/sqrt(sigma2NORM()), 0, 1)) -
                sqrt(sigma2NORM()/ 2 * pi) *
                exp(-(input$dNORM - muNORM())^2/2 * sigma2NORM())
        })
        
        EspLimNORM <- reactive({muNORM() * 
                pnorm((input$dNORM - muNORM()) / sqrt(sigma2NORM()), 0, 1) - 
                sqrt(sigma2NORM()/ 2 * pi) * 
                exp(-(input$dNORM - muNORM())^2/2 * sigma2NORM()) + 
                input$dNORM * 
                (1 - pnorm((input$dNORM - muNORM()) / sqrt(sigma2NORM()), 0, 1))
        })
        
        ExcesMoyNORM <- reactive({muNORM() + 
                input$dNORM - 
                1 / (1 - pnorm((input$dNORM - muNORM())/sqrt(sigma2NORM()), 0, 1)) * 
                sqrt(sigma2NORM()/ 2 * pi) * 
                exp(-(input$dNORM - muNORM())^2 / (2 * sigma2NORM()))
        })
        
        output$meanNORM <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                         muNORM()
        ))})
        
        output$varNORM <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                        sigma2NORM()
        ))})
        
        output$densityNORM <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                            input$xNORM,
                                                            densityNORM()
        ))})
        output$repartNORM <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
                                                           input$xNORM,
                                                           repartNORM()
        ))})
        output$survieNORM <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$", 
                                                           input$xNORM,
                                                           survieNORM()
        ))})
        output$VaRNORM <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$", 
                                                        input$kNORM,
                                                        VaRNORM()
        ))})
        output$TVaRNORM <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$", 
                                                         input$kNORM,
                                                         TVaRNORM()
        ))})
        output$EspTronqNORM <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$", 
                                                             input$dNORM,
                                                             EspTronqNORM()
        ))})
        
        output$StopLossNORM <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$", 
                                                             input$dNORM,
                                                             StopLossNORM()
        ))})
        
        output$EspLimNORM <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$", 
                                                           input$dNORM,
                                                           EspLimNORM()
        ))})
        
        output$ExcesMoyNORM <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$", 
                                                             input$dNORM,
                                                             ExcesMoyNORM()
        ))})
        
        output$FxNORM <- renderPlotly({
            ggplot(
                data = data.frame(x = c(muNORM() - 4 * sqrt(sigma2NORM()),
                                        muNORM() + 4 * sqrt(sigma2NORM()))),
                aes(x)
            ) + 
                stat_function(fun = dnorm, 
                              args = list(mean = muNORM(), 
                                          sd = sqrt(sigma2NORM()))
                ) + 
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = dnorm,
                    args = list(mean = muNORM(), sd = sqrt(sigma2NORM())),
                    xlim = c(muNORM() - 4 * sqrt(sigma2NORM()), input$xNORM),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$SxNORM <- renderPlotly({
            ggplot(
                data = data.frame(x = c(muNORM() - 4 * sqrt(sigma2NORM()),
                                        muNORM() + 4 * sqrt(sigma2NORM()))),
                aes(x)
            ) + 
                stat_function(fun = dnorm, 
                              args = list(mean = muNORM(), 
                                          sd = sqrt(sigma2NORM()))
                ) + 
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = dnorm,
                    args = list(mean = muNORM(), sd = sqrt(sigma2NORM())),
                    xlim = c(muNORM() + 4 * sqrt(sigma2NORM()), input$xNORM),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
    }
    
  
    {
        ## Loi Gamma Serveur ----
        betaGAMMA <- reactive({
            if (input$distrchoiceGAMMA == T) {
                input$betaGAMMA
            } else {
                1 / input$betaGAMMA
            }
        })
        
        alphaGAMMA <- reactive({input$alphaGAMMA})
        
        observeEvent(input$distrchoiceGAMMA, {
            y <- input$distrchoiceGAMMA
            updateNumericInput(session, "betaGAMMA", value = 
                                   if (input$distrchoiceGAMMA == T) {
                                       input$betaGAMMA
                                   } else {
                                       1 / input$betaGAMMA
                                   },step =  
                                   if (input$distrchoiceGAMMA == T) {
                                       1
                                   } else {
                                       .1
                                   }
                                   )
        })
       observeEvent(input$distrchoiceEXPOFAM, {
           x <- input$distrchoiceEXPOFAM
           updateNumericInput(session, "betaGAMMA", value = 
                              if(x == "Khi carré")
                              {
                                  betaGAMMA = 0.5
                              } else{
                                  betaGAMMA = 0.1
                              }
       
           )
           if(x == "Khi carré")
               disable("betaGAMMA")
           else
               enable("betaGAMMA")
           updateNumericInput(session, "alphaGAMMA", value =
                   if(x == "Exponentielle")
                   {
                       alphaGAMMA = 1
                   } 
                   else{ 
                       alphaGAMMA = 2
                   }
                       )
           if(x == "Exponentielle")
               disable("alphaGAMMA")
           else
               enable("alphaGAMMA")
       })
        
        
        densityGAMMA <- reactive({format(dgamma(input$xGAMMA, alphaGAMMA(), betaGAMMA()),scientific = F,  nsmall = 6)})
        
        repartGAMMA <- reactive({format(pgamma(input$xGAMMA, alphaGAMMA(), betaGAMMA()), nsmall = 6, scientific = F)})
        
        VaRGAMMA <- reactive({format(qgamma(input$kGAMMA, alphaGAMMA(), betaGAMMA()), nsmall = 6)})
        
        VaRTVARGAMMA <- reactive({qgamma(input$kGAMMA, alphaGAMMA(), betaGAMMA())})
        
        TVaRGAMMA <- reactive({format((alphaGAMMA() / betaGAMMA()) * 
                                          (1/(1 - input$kGAMMA)) * 
                                          pgamma(VaRTVARGAMMA(), alphaGAMMA() + 1, betaGAMMA(), lower.tail = F), 
                                      nsmall = 6)
        })
        
        EspTronqGAMMA <- reactive({(alphaGAMMA()/betaGAMMA()) * pgamma(input$dGAMMA, alphaGAMMA() + 1, betaGAMMA())})
        
        StopLossGAMMA <-reactive({
            (alphaGAMMA() / betaGAMMA()) * 
                pgamma(input$dGAMMA, alphaGAMMA() + 1, betaGAMMA(), lower.tail = F) - 
                input$dGAMMA * 
                pgamma(input$dGAMMA, alphaGAMMA(), betaGAMMA(), lower.tail = F)
        })
        
        EspLimGAMMA <- reactive({
            (alphaGAMMA() / betaGAMMA()) * 
                pgamma(input$dGAMMA, alphaGAMMA() + 1, betaGAMMA()) + 
                input$dGAMMA * 
                pgamma(input$dGAMMA, alphaGAMMA(), betaGAMMA(), lower.tail = F)
        })
        
        ExcesMoyGAMMA <- reactive({
            (alphaGAMMA() / betaGAMMA()) * 
                (pgamma(input$dGAMMA, alphaGAMMA() + 1, betaGAMMA(), lower.tail = F)) / 
                (pgamma(input$dGAMMA, alphaGAMMA(), betaGAMMA(), lower.tail = F)) - 
                input$dGAMMA
        })
        
        meanGAMMA <- reactive({(alphaGAMMA()/betaGAMMA())})
        
        varianceGAMMA <- reactive({(alphaGAMMA()/(betaGAMMA()^2))})
        
        output$meanGAMMA <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                          meanGAMMA()
        ))})
        
        output$varianceGAMMA <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                              varianceGAMMA()
        ))})
        
        output$densityGAMMA <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                             input$xGAMMA,
                                                             densityGAMMA()
        ))})
        output$repartGAMMA <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
                                                            input$xGAMMA,
                                                            repartGAMMA()
        ))})
        output$VaRGAMMA <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$", 
                                                         input$kGAMMA,
                                                         VaRGAMMA()
        ))})
        output$TVaRGAMMA <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$", 
                                                          input$kGAMMA,
                                                          TVaRGAMMA()
        ))})
        output$EspTronqGAMMA <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$", 
                                                              input$dGAMMA,
                                                              EspTronqGAMMA()
        ))})
        
        output$StopLossGAMMA <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$", 
                                                              input$dGAMMA,
                                                              StopLossGAMMA()
        ))})
        
        output$EspLimGAMMA <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$", 
                                                            input$dGAMMA,
                                                            EspLimGAMMA()
        ))})
        
        output$ExcesMoyGAMMA <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$", 
                                                              input$dGAMMA,
                                                              ExcesMoyGAMMA()
        ))})
        
        output$FxGAMMA <- renderPlotly({
            ggplot(data = data.frame(x = c(0,
                                           meanGAMMA() +
                                               3 *
                                               sqrt(varianceGAMMA()))
            ),
            aes(x)) + 
                stat_function(fun = dgamma,
                              args = list(shape = alphaGAMMA(), 
                                          rate = betaGAMMA())) + 
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = dgamma,
                    args = list(shape = alphaGAMMA(), rate = betaGAMMA()),
                    xlim = c(0, input$xGAMMA),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
    
    }
  
  
    {    
        ## Loi Binomiale Serveur ----
        nBIN <- reactive({input$nBIN})
        
        pBIN <- reactive({input$pBIN})
        
        meanBIN <- reactive({nBIN() * pBIN()})
        
        varBIN <- reactive({nBIN() * pBIN() * (1 - pBIN())})   
        
        densityBIN <- reactive({format(dbinom(input$xBIN, nBIN(), pBIN()), nsmall = 6)})
        
        repartBIN <- reactive({format(pbinom(input$xBIN, nBIN(), pBIN()), nsmall = 6)})
        
        VaRBIN <- reactive({format(qbinom(input$kBIN, nBIN(), pBIN()), nsmall = 4)})
        
        TVaRBIN <- reactive({format(0, nsmall = 4)})
        
        EspTronqBIN <- reactive({0})
        
        StopLossBIN <- reactive({0})
        
        EspLimBIN <- reactive({0})
        
        ExcesMoyBIN <- reactive({0})
        
        output$meanBIN <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                        meanBIN()
        ))})
        
        output$varBIN <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                       varBIN()
        ))})
        
        output$densityBIN <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                           input$xBIN,
                                                           densityBIN()
        ))})
        output$repartBIN <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
                                                          input$xBIN,
                                                          repartBIN()
        ))})
        output$VaRBIN <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$", 
                                                       input$kBIN,
                                                       VaRBIN()
        ))})
        output$TVaRBIN <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$", 
                                                        input$kBIN,
                                                        TVaRBIN()
        ))})
        output$EspTronqBIN <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$", 
                                                            input$dBIN,
                                                            EspTronqBIN()
        ))})
        
        output$StopLossBIN <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$", 
                                                            input$dBIN,
                                                            StopLossBIN()
        ))})
        
        output$EspLimBIN <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$", 
                                                          input$dBIN,
                                                          EspLimBIN()
        ))})
        
        output$ExcesMoyBIN <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$", 
                                                            input$dBIN,
                                                            ExcesMoyBIN()
        ))})
        
        output$FxBIN <- renderPlotly({ggplot(data.frame(x = 0:nBIN(), y = dbinom(0:nBIN(), nBIN(), pBIN())), aes(x = x, y = y)) + geom_bar(stat = "identity", col = "red", fill ="red", alpha = 0.7, width = 0.3) + theme_classic() + ylab("P(X=x")
            
        })
        
        # Reactive slider
        observeEvent(input$nBIN,{updateSliderInput(session = session, inputId = "xBIN", max = input$nBIN)
        })
    } 
    

}