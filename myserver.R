myserver <- function(input, output, session) 
{
#### Loi Normale Serveur ####
        
    muNORM <- reactive({input$muNORM})
        
    sigma2NORM <- reactive({input$sigmaNORM})
        
    densityNORM <- reactive({format(dnorm(input$xNORM, muNORM(), sqrt(sigma2NORM())), nsmall = 6)})
        
    repartNORM <- reactive({format(pnorm(input$xNORM, muNORM(), sqrt(sigma2NORM())), nsmall = 6)})
        
    survieNORM <- reactive({format(pnorm(input$xNORM, muNORM(), sqrt(sigma2NORM()), lower.tail = F), nsmall = 6)})
        
    VaRNORM <- reactive({format(VaR_norm(kappa = input$kNORM, mu = muNORM(), sig = sqrt(sigma2NORM())), nsmall = 6)})
        
    TVaRNORM <- reactive({format(TVaR_norm(kappa = input$kNORM, mu = muNORM(), sig = sqrt(sigma2NORM())), nsmall = 6)})
        
    EspTronqNORM <- reactive({Etronq_norm(d = input$dNORM, mu = muNORM(), sig = sqrt(sigma2NORM()))})
        
    StopLossNORM <- reactive({SL_norm(d = input$dNORM, mu = muNORM(), sig = sqrt(sigma2NORM()))})
        
    EspLimNORM <- reactive({Elim_norm(d = input$dNORM, mu = muNORM(), sig = sqrt(sigma2NORM()))})
    
    ExcesMoyNORM <- reactive({Mexcess_norm(d = input$dNORM, mu = muNORM(), sig = sqrt(sigma2NORM()))})
    
    output$meanNORM <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                     muNORM()))
        })
    
    output$varNORM <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                    sigma2NORM()))
        })
    
    output$densityNORM <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                        input$xNORM,
                                                        densityNORM()))
        })
    output$repartNORM <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
                                                       input$xNORM,
                                                       repartNORM()))
        })
    
    output$survieNORM <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                       input$xNORM,
                                                       survieNORM()))
        })
    
    output$VaRNORM <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                    input$kNORM,
                                                    VaRNORM()))
        })
    
    output$TVaRNORM <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
                                                     input$kNORM,
                                                     TVaRNORM()))
        })
    
    output$EspTronqNORM <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$",
                                                         input$dNORM,
                                                         EspTronqNORM()))
        })
    
    output$StopLossNORM <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$",
                                                         input$dNORM,
                                                         StopLossNORM()))
        })
    
    output$EspLimNORM <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$",
                                                       input$dNORM,
                                                       EspLimNORM()))
        })
    
    output$ExcesMoyNORM <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$",
                                                         input$dNORM,
                                                         ExcesMoyNORM()))
        })
    
    output$FxNORM <- renderPlotly({
        ggplot(data = data.frame(x = c(
            muNORM() - 4 * sqrt(sigma2NORM()),
            muNORM() + 4 * sqrt(sigma2NORM())
        )),
        aes(x)) +
            stat_function(fun = dnorm,
                          args = list(mean = muNORM(),
                                      sd = sqrt(sigma2NORM()))) +
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
        ggplot(data = data.frame(x = c(
            muNORM() - 4 * sqrt(sigma2NORM()),
            muNORM() + 4 * sqrt(sigma2NORM())
        )),
        aes(x)) +
            stat_function(fun = dnorm,
                          args = list(mean = muNORM(),
                                      sd = sqrt(sigma2NORM()))) +
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
        
    
    
#### Loi Gamma Serveur ####
        betaGAMMA <- reactive({
            if (input$distrchoiceGAMMA == T) {
                input$betaGAMMA
            } else {
                1 / input$betaGAMMA
            }
        })
        
        alphaGAMMA <- reactive({
            if (input$distrchoiceEXPOFAM == "Khi carré")
            {
                2 * input$alphaGAMMA
            }
            else
            {
                input$alphaGAMMA
            }
        })

        output$betaGAMMAUI <- renderUI({
            numericInput('betaGAMMA', '$$\\beta$$', value = 1, min = 0)
        })
    
        output$changingalpha <- renderUI({
            numericInput('alphaGAMMA', label = '$$\\alpha$$', value = 2, min = 0)
        })
        
        ## Ici on crée un gros observeEvent qui va modifier les paramètres de la gamma/exponentielle/khi-carrée selon les 2 radio buttons:
        ## x: selection de distribution
        ## y: selection de si c'est fréquence ou échelle
        observeEvent({
            input$distrchoiceGAMMA
            input$distrchoiceEXPOFAM #liste des inputs à observer entre {}
        },
        {
            x <- input$distrchoiceEXPOFAM
            y <- input$distrchoiceGAMMA
            
            # modification du beta
            updateNumericInput(session,
                               "betaGAMMA",
                               value =
                                   if (x == "Khi carré")
                                   {
                                       if (y == T) {
                                           betaGAMMA = 0.5
                                       } else {
                                           betaGAMMA = 2
                                       }
                                   },
                               step =
                                   if (y == T) {
                                       .1
                                   } else {
                                       1
                                   })
            # rend le paramètre impossible à modifier pour l'utilisateur
            if (x == "Khi carré")
                hide("betaGAMMA")
            else
                show("betaGAMMA")

            updateNumericInput(session, "alphaGAMMA",
                               value =
                                   if (x == "Exponentielle")
                                   {
                                       alphaGAMMA = 1
                                   }
                               else
                               {
                                   alphaGAMMA = 2
                               },
                               label = {
                                   if (x == "Khi carré")
                                   {
                                       '$$n$$'
                                   }
                                   else{
                                       '$$\\alpha$$'
                                   }
                               }
            )
            # rend le paramètre impossible à modifier pour l'utilisateur
            if (x == "Exponentielle")
                hide("alphaGAMMA")
            else
                show("alphaGAMMA")
        })
        
        
        densityGAMMA <- reactive({format(dgamma(input$xGAMMA, alphaGAMMA(), betaGAMMA()),scientific = F,  nsmall = 6)})
        
        repartGAMMA <- reactive({format(pgamma(input$xGAMMA, alphaGAMMA(), betaGAMMA()), nsmall = 6, scientific = F)})
        
        survieGAMMA <- reactive({format(pgamma(input$xGAMMA, alphaGAMMA(), betaGAMMA(), lower.tail = F), nsmall = 6, scientific = F)})
        
        VaRGAMMA <- reactive({format(VaR_gamma(kappa = input$kGAMMA, a = alphaGAMMA(), b = betaGAMMA()), nsmall = 6)})
        
        TVaRGAMMA <- reactive({format(TVaR_gamma(kappa = input$kGAMMA, a = alphaGAMMA(), b = betaGAMMA()), nsmall = 6)})
        
        EspTronqGAMMA <- reactive({Etronq_gamma(d = input$dGAMMA, a = alphaGAMMA(), b = betaGAMMA())})
        
        StopLossGAMMA <- reactive({SL_gamma(d = input$dGAMMA, a = alphaGAMMA(), b = betaGAMMA())})
        
        EspLimGAMMA <- reactive({Elim_gamma(d = input$dGAMMA, a = alphaGAMMA(), b = betaGAMMA())})
        
        ExcesMoyGAMMA <- reactive({Mexcess_gamma(d = input$dGAMMA, a = alphaGAMMA(), b = betaGAMMA())})
        
        meanGAMMA <- reactive({E_gamma(a = alphaGAMMA(), b = betaGAMMA())})
        
        varianceGAMMA <- reactive({V_gamma(a = alphaGAMMA(), b = betaGAMMA())})
        
        output$loi_gamma <- renderText({
            if(input$distrchoiceEXPOFAM == "Gamma")
                "Loi Gamma"
            else if (input$distrchoiceEXPOFAM == "Exponentielle")
                "Loi Exponentielle"
            else
                "Loi Khi carré"
        })
        
        output$distr_gamma <- renderText({
            if(input$distrchoiceEXPOFAM == "Gamma")
                "\\(X \\sim\\mathcal{G}(\\alpha, \\beta)\\)"
            else if (input$distrchoiceEXPOFAM == "Exponentielle")
                "\\(X \\sim\\mathcal{Exp}(\\beta)\\)"
            else
                "\\(X \\sim\\mathcal{\\chi^2}(n)\\)"
        })
        
        
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
        
        output$survieGAMMA <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                           input$xNORM,
                                                           survieGAMMA()))
        })
        
        
        output$VaRGAMMA <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$", 
                                                         input$kGAMMA,
                                                         VaRGAMMA()
        ))})
        output$TVaRGAMMA <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$", 
                                                          input$kGAMMA,
                                                          TVaRGAMMA()
        ))})
        output$EspTronqGAMMA <- renderUI({sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$", 
                                                              input$dGAMMA,
                                                              EspTronqGAMMA()
        )})
        
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
        
        output$SxGAMMA <- renderPlotly({
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
                    # xlim = c(input$xGAMMA, VaR_gamma(kappa = 1, a = alphaGAMMA(), b = betaGAMMA())),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
#### Loi Pareto Serveur ####
        
        alphaPARETO <- reactive({input$alphaPARETO})
        
        lambdaPARETO <- reactive({input$lambdaPARETO})
        
        densityPARETO <- reactive({format(dpareto(x = input$xPARETO, 
                                                 shape = alphaPARETO(), 
                                                 scale = lambdaPARETO()), 
                                         nsmall = 6)})
        
        repartPARETO <- reactive({format(ppareto(q = input$xPARETO, 
                                                 shape = alphaPARETO(), 
                                                 scale = lambdaPARETO()), 
                                         nsmall = 6)})
        
        surviePARETO <- reactive({format(ppareto(q = input$xPARETO, 
                                                              shape = alphaPARETO(), 
                                                              scale = lambdaPARETO(),
                                                 lower.tail = F), 
                                         nsmall = 6)})
        
        VaRPARETO <- reactive({format(VaR_pareto(kappa = input$kPARETO,
                                                 alph = alphaPARETO(), 
                                                 lam = lambdaPARETO()), 
                                      nsmall = 6)
            })
        
        TVaRPARETO <- reactive({format(TVaR_pareto(kappa = input$kPARETO, 
                                                   alph = alphaPARETO(), 
                                                   lam = lambdaPARETO()), 
                                       nsmall = 6)
            })
        
        EspTronqPARETO <- reactive({Etronq_pareto(d = input$dPARETO, 
                                                  alph = alphaPARETO(),
                                                  lam = lambdaPARETO())
            })
        
        StopLossPARETO <- reactive({SL_pareto(d = input$dPARETO, 
                                              alph = alphaPARETO(), 
                                              lam = lambdaPARETO())
            })
        
        EspLimPARETO <- reactive({Elim_pareto(d = input$dPARETO, 
                                              alph = alphaPARETO(), 
                                              lam = lambdaPARETO())
            })
        
        kthmomentPARETO <- reactive({
            if(alphaPARETO() > input$dPARETO)
                kthmoment_pareto1(k = input$dPARETO, 
                                  alpha = alphaPARETO(), 
                                  lam = lambdaPARETO())
            else
                kthmoment_pareto2(k = input$dPARETO, 
                                  alpha = alphaPARETO(), 
                                  lam = lambdaPARETO())
        })
        
        
        
        meanPARETO <- reactive({E_pareto(alph = alphaPARETO(), 
                                         lam = lambdaPARETO())
            })
        
        variancePARETO <- reactive({V_pareto(alph = alphaPARETO(), 
                                             lam = lambdaPARETO())
            })
        
        ExcesMoyPARETO <- reactive({Mexcess_pareto(d = input$dPARETO, 
                                                   alph = alphaPARETO(),
                                                   lam = lambdaPARETO())
            })
        
        output$meanPARETO <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                          meanPARETO()))
        })
        
        output$varPARETO <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                          variancePARETO()))
        })
        
        output$densityPARETO <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                            input$xPARETO,
                                                            densityPARETO()))
        })
        output$repartPARETO <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
                                                           input$xPARETO,
                                                           repartPARETO()))
        })
        
        output$surviePARETO <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                           input$xPARETO,
                                                           surviePARETO()))
        })
        
        output$VaRPARETO <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                        input$kPARETO,
                                                        VaRPARETO()))
        })
        
        output$TVaRPARETO <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
                                                         input$kPARETO,
                                                         TVaRPARETO()))
        })
        
        output$EspTronqPARETO <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$",
                                                             input$dPARETO,
                                                             EspTronqPARETO()))
        })
        
        output$StopLossPARETO <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$",
                                                             input$dPARETO,
                                                             StopLossPARETO()))
        })
        
        output$EspLimPARETO <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$",
                                                           input$dPARETO,
                                                           EspLimPARETO()))
        })
        
        output$ExcesMoyPARETO <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$",
                                                             input$dPARETO,
                                                             ExcesMoyPARETO()))
        })
        
        output$kthmomentPARETO <- renderUI({withMathJax(sprintf("$$E[X^{%s}] = %.4f$$",
                                                               input$dPARETO,
                                                               kthmomentPARETO()))
        })
        
        
        # output$FxPARETO 
        
        # output$SxPARETO
        
        
        
    
#### Loi Burr Serveur ####
        
        alphaBURR <- reactive({input$alphaBURR})
        
        lambdaBURR <- reactive({input$lambdaBURR})
        
        tauBURR <- reactive({input$tauBURR})
        
        dBURR <- reactive({input$dBURR})
        
        kBURR <- reactive({input$kBURR})
        
        densityBURR <- reactive({format(dburr(x = input$xBURR, 
                                              shape1 = alphaBURR(), 
                                              shape2 = tauBURR(),
                                              scale = lambdaBURR()), 
                                          nsmall = 6)})
        
        repartBURR <- reactive({format(pburr(q = input$xBURR, 
                                             shape1 = alphaBURR(), 
                                             shape2 = tauBURR(),
                                             scale = lambdaBURR()), 
                                         nsmall = 6)})
        
        survieBURR <- reactive({format(pburr(q = input$xBURR, 
                                             shape1 = alphaBURR(), 
                                             shape2 = tauBURR(),
                                             scale = lambdaBURR(),
                                             lower.tail = F), 
                                         nsmall = 6)})
        
        VaRBURR <- reactive({format(VaR_burr(k = input$kBURR,
                                             alpha = alphaBURR(),
                                             lam = lambdaBURR(),
                                             tau = tauBURR()), 
                                      nsmall = 6)
        })
        
        VaRBURR_a <- reactive({VaR_burr(k = input$kBURR,
                                        alpha = alphaBURR(),
                                        lam = lambdaBURR(),
                                        tau = tauBURR()) 
        })
        
        
        TVaRBURR <- reactive({format(TVaR_burr(k = input$kBURR,
                                               var = VaRBURR_a(),
                                               alpha = alphaBURR(),
                                               lam = lambdaBURR(),
                                               tau = tauBURR()), 
                                       nsmall = 6)
        })
        
        EspTronqBURR <- reactive({Etronq_burr(d = dBURR(),
                                              alpha = alphaBURR(),
                                              lam = lambdaBURR(),
                                              tau = tauBURR())
        })
        
        StopLossBURR <- reactive({SL_burr(d = dBURR(),
                                          alpha = alphaBURR(),
                                          lam = lambdaBURR(),
                                          tau = tauBURR())
        })
        
        EspLimBURR <- reactive({Elim_burr(d = dBURR(),
                                          alpha = alphaBURR(),
                                          lam = lambdaBURR(),
                                          tau = tauBURR())
        })
        
        kthmomentBURR <- reactive({kthmoment_burr(k = kBURR(),
                                                  alpha = alphaBURR(),
                                                  lam = lambdaBURR(),
                                                  tau = tauBURR())
        })
        
        ExcesMoyBURR <- reactive({Mexcess_burr(lam = lambdaBURR(), 
                                               alpha = alphaBURR(),
                                               tau = tauBURR(),
                                               d = dBURR())
        })
        
        meanBURR <- reactive({E_burr(lam = lambdaBURR(),
                                     alph = alphaBURR(),
                                     rho = tauBURR())
        })
        
        varianceBURR <- reactive({V_burr(lam = lambdaBURR(),
                                         alph = alphaBURR(),
                                         rho = tauBURR())
        })
        
        output$meanBURR <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                           meanBURR()))
        })
        
        output$varBURR <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                          varianceBURR()))
        })
        
        output$densityBURR <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                              input$xBURR,
                                                              densityBURR()))
        })
        output$repartBURR <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
                                                             input$xBURR,
                                                             repartBURR()))
        })
        
        output$survieBURR <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                             input$xBURR,
                                                             survieBURR()))
        })
        
        output$VaRBURR <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                          input$kBURR,
                                                          VaRBURR()))
        })
        
        output$TVaRBURR <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
                                                           input$kBURR,
                                                           TVaRBURR()))
        })
        
        output$EspTronqBURR <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$",
                                                               input$dBURR,
                                                               EspTronqBURR()))
        })
        
        output$StopLossBURR <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$",
                                                               input$dBURR,
                                                               StopLossBURR()))
        })
        
        output$EspLimBURR <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$",
                                                             input$dBURR,
                                                             EspLimBURR()))
        })
        
        output$ExcesMoyBURR <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$",
                                                               input$dBURR,
                                                               ExcesMoyBURR()))
        })
        
        output$kthmomentBURR <- renderUI({withMathJax(sprintf("$$E[X^{%s}] = %.4f$$",
                                                             input$dBURR,
                                                             ExcesMoyBURR()))
        })
        
        
        # output$FxBURR 
        
        # output$SxBURR
      
#### Loi Weibull Serveur ####
        
        betaWEIBULL <- reactive({input$betaWEIBULL})
        
        tauWEIBULL <- reactive({input$tauWEIBULL})
        
        dWEIBULL <- reactive({input$dWEIBULL})
        
        kWEIBULL <- reactive({input$kWEIBULL})
        
        densityWEIBULL <- reactive({format(dweibull(x = input$xWEIBULL, 
                                              shape = tauWEIBULL(),
                                              scale = betaWEIBULL()), 
                                        nsmall = 6)})
        
        repartWEIBULL <- reactive({format(pweibull(q = input$xWEIBULL, 
                                                   shape = tauWEIBULL(),
                                                   scale = betaWEIBULL()), 
                                       nsmall = 6)})
        
        survieWEIBULL <- reactive({format(pweibull(q = input$xWEIBULL, 
                                                   shape = tauWEIBULL(),
                                                   scale = betaWEIBULL(),
                                                   lower.tail = F), 
                                       nsmall = 6)})
        
        VaRWEIBULL <- reactive({format(VaR_weibull(k = kWEIBULL(),
                                                   tau = tauWEIBULL(), 
                                                   beta = betaWEIBULL()),
                                    nsmall = 6)
        })
        
        TVaRWEIBULL <- reactive({format(TVaR_weibull(k = kWEIBULL(),
                                                     tau = tauWEIBULL(), 
                                                     beta = betaWEIBULL()), 
                                     nsmall = 6)
        })
        
        EspTronqWEIBULL <- reactive({Etronq_weibull(d = dWEIBULL(),
                                                    beta = betaWEIBULL(),
                                                    tau = tauWEIBULL())
        })
        
        StopLossWEIBULL <- reactive({SL_weibull(d = dWEIBULL(),
                                                beta = betaWEIBULL(),
                                                tau = tauWEIBULL())
        })
        
        EspLimWEIBULL <- reactive({Elim_weibull(d = dWEIBULL(),
                                                beta = betaWEIBULL(),
                                                tau = tauWEIBULL())
        })
        
        ExcesMoyWEIBULL <- reactive({Mexcess_weibull(d = dWEIBULL(),
                                                     beta = betaWEIBULL(),
                                                     tau = tauWEIBULL())
        })
        
        meanWEIBULL <- reactive({E_weibull(beta = betaWEIBULL(),
                                           tau = tauWEIBULL())
        })
        
        kthmomentWEIBULL <- reactive({E_weibull(beta = betaWEIBULL(),
                                                tau = tauWEIBULL(),
                                                k = dWEIBULL())
        })
        
        varianceWEIBULL <- reactive({V_weibull(beta = betaWEIBULL(),
                                               tau = tauWEIBULL())
        })
        
        output$meanWEIBULL <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                            format(meanWEIBULL(),
                                                                   nsmall = 6)))
        })
        
        output$kthmomentWEIBULL <- renderUI({withMathJax(sprintf("$$E(X^%s) = %s$$", 
                                                                 input$dWEIBULL,
                                                                 format(kthmomentWEIBULL(),
                                                                        nsmall = 6)))
        })
        
        output$varianceWEIBULL <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                                format(varianceWEIBULL(),
                                                                       nsmall = 6)))
        })
        
        output$densityWEIBULL <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                            input$xWEIBULL,
                                                            densityWEIBULL()))
        })
        output$repartWEIBULL <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
                                                           input$xWEIBULL,
                                                           repartWEIBULL()))
        })
        
        output$survieWEIBULL <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                           input$xWEIBULL,
                                                           survieWEIBULL()))
        })
        
        output$VaRWEIBULL <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                        input$kWEIBULL,
                                                        VaRWEIBULL()))
        })
        
        output$TVaRWEIBULL <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
                                                         input$kWEIBULL,
                                                         TVaRWEIBULL()))
        })
        
        output$EspTronqWEIBULL <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$",
                                                             input$dWEIBULL,
                                                             EspTronqWEIBULL()))
        })
        
        output$StopLossWEIBULL <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$",
                                                             input$dWEIBULL,
                                                             StopLossWEIBULL()))
        })
        
        output$EspLimWEIBULL <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$",
                                                           input$dWEIBULL,
                                                           EspLimWEIBULL()))
        })
        
        output$ExcesMoyWEIBULL <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$",
                                                             input$dWEIBULL,
                                                             ExcesMoyWEIBULL()))
        })
        
        # output$FxWEIBULL 
        
        # output$SxWEIBULL
        
#### Loi Lognormale Serveur ####
        
        muLNORM <- reactive({input$muLNORM})
        
        sigma2LNORM <- reactive({input$sigmaLNORM})
        
        densityLNORM <- reactive({format(dlnorm(input$xLNORM, 
                                                muLNORM(), 
                                                sqrt(sigma2LNORM())), 
                                         nsmall = 6)
            })
        
        repartLNORM <- reactive({format(plnorm(input$xLNORM, 
                                               muLNORM(), 
                                               sqrt(sigma2LNORM())), 
                                        nsmall = 6)
            })
        
        survieLNORM <- reactive({format(plnorm(input$xLNORM, 
                                               muLNORM(), 
                                               sqrt(sigma2LNORM()), 
                                               lower.tail = F), 
                                        nsmall = 6)
            })
        
        VaRLNORM <- reactive({format(VaR_lnorm(kappa = input$kLNORM, 
                                               mu = muLNORM(), 
                                               sig = sqrt(sigma2LNORM())), 
                                     nsmall = 6)
            })
        
        TVaRLNORM <- reactive({format(TVaR_lnorm(kappa = input$kLNORM, 
                                                 mu = muLNORM(), 
                                                 sig = sqrt(sigma2LNORM())), 
                                      nsmall = 6)
            })
        
        EspTronqLNORM <- reactive({Etronq_lnorm(d = input$dLNORM, 
                                                mu = muLNORM(), 
                                                sig = sqrt(sigma2LNORM()))
            })
        
        StopLossLNORM <- reactive({SL_lnorm(d = input$dLNORM, 
                                            mu = muLNORM(), 
                                            sig = sqrt(sigma2LNORM()))
            })
        
        EspLimLNORM <- reactive({Elim_lnorm(d = input$dLNORM, 
                                            mu = muLNORM(), 
                                            sig = sqrt(sigma2LNORM()))
            })
        
        ExcesMoyLNORM <- reactive({Mexcess_lnorm(d = input$dLNORM,
                                                 mu = muLNORM(),
                                                 sig = sqrt(sigma2LNORM()))
            })
        
        meanLNORM <- reactive({E_lnorm(mu = muLNORM(),
                                         sig = sqrt(sigma2LNORM()))
            })
        
        kthmomentLNORM <- reactive({kthmoment_lnorm(k = input$dLNORM,
                                                    mu = muLNORM(),
                                                    sig = sqrt(sigma2LNORM()))
            })
        
        
        varianceLNORM <- reactive({V_lnorm(mu = muLNORM(),
                                          sig = sqrt(sigma2LNORM()))
        })
        
        output$meanLNORM <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                         format(meanLNORM(), 
                                                                nsmall = 6)))
        })
        
        output$kthmomentLNORM <- renderUI({withMathJax(sprintf("$$E(X^%s) = %s$$", 
                                                               input$dLNORM,
                                                               format(kthmomentLNORM(), 
                                                                 nsmall = 6)))
        })
        
        output$varLNORM <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                         format(varianceLNORM(), 
                                                                nsmall = 6)))
        })
        
        output$densityLNORM <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                            input$xLNORM,
                                                            densityLNORM()))
        })
        output$repartLNORM <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
                                                           input$xLNORM,
                                                           repartLNORM()))
        })
        
        output$survieLNORM <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                           input$xLNORM,
                                                           survieLNORM()))
        })
        
        output$VaRLNORM <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                        input$kLNORM,
                                                        VaRLNORM()))
        })
        
        output$TVaRLNORM <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
                                                         input$kLNORM,
                                                         TVaRLNORM()))
        })
        
        output$EspTronqLNORM <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$",
                                                             input$dLNORM,
                                                             EspTronqLNORM()))
        })
        
        output$StopLossLNORM <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$",
                                                             input$dLNORM,
                                                             StopLossLNORM()))
        })
        
        output$EspLimLNORM <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$",
                                                           input$dLNORM,
                                                           EspLimLNORM()))
        })
        
        output$ExcesMoyLNORM <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$",
                                                             input$dLNORM,
                                                             ExcesMoyLNORM()))
        })
        
        # output$FxLNORM
        
        # output$SxLNORM
        
        
        
#### Loi Beta Serveur ####
        
        alphaBETA <- reactive({input$alphaBETA})
        
        betaBETA <- reactive({input$betaBETA})
        
        densityBETA <- reactive({format(dbeta(x = input$xBETA, 
                                                  shape1 = alphaBETA(), 
                                                  shape2 = betaBETA()), 
                                          nsmall = 6)})
        
        repartBETA <- reactive({format(pbeta(q = input$xBETA, 
                                                 shape1 = alphaBETA(), 
                                                 shape2 = betaBETA()), 
                                         nsmall = 6)})
        
        survieBETA <- reactive({format(pbeta(q = input$xBETA, 
                                             shape1 = alphaBETA(), 
                                             shape2 = betaBETA(),
                                             lower.tail = F), 
                                         nsmall = 6)})
        
        VaRBETA <- reactive({format(VaR_beta(k = input$kBETA,
                                             a = alphaBETA(), 
                                             b = betaBETA()), 
                                      nsmall = 6)
        })
        
        TVaRBETA <- reactive({format(TVaR_beta(k = input$kBETA, 
                                               a = alphaBETA(), 
                                               b = betaBETA()), 
                                       nsmall = 6)
        })
        
        EspTronqBETA <- reactive({Etronq_beta(d = input$dBETA, 
                                              a = alphaBETA(),
                                              b = betaBETA())
        })
        
        StopLossBETA <- reactive({SL_beta(d = input$dBETA, 
                                          a = alphaBETA(),
                                          b = betaBETA())
        })
        
        EspLimBETA <- reactive({Elim_beta(d = input$dBETA, 
                                          a = alphaBETA(),
                                          b = betaBETA())
        })
        
        meanBETA <- reactive({E_beta(a = alphaBETA(),
                                     b = betaBETA())
        })
        
        varianceBETA <- reactive({V_beta(a = alphaBETA(),
                                         b = betaBETA())
        })
        
        ExcesMoyBETA <- reactive({Mexcess_beta(d = input$dBETA, 
                                               a = alphaBETA(),
                                               b = betaBETA())
        })
        
        kthmomentBETA <- reactive({kthmoment_beta(k = input$dBETA, 
                                                  a = alphaBETA(),
                                                  b = betaBETA())})
        
        output$meanBETA <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                           format(meanBETA(),
                                                                  nsmall = 6)))
        })
        
        output$kthmomentBETA <- renderUI({withMathJax(sprintf("$$E[X^{%s}] = %s$$", 
                                                              input$dBETA,
                                                              format(kthmomentBETA(),
                                                                     nsmall = 6)))
        })
        
        output$varianceBETA <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                             format(varianceBETA(),
                                                                    nsmall = 6)))
        })
        
        output$densityBETA <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                              input$xBETA,
                                                              densityBETA()))
        })
        output$repartBETA <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
                                                             input$xBETA,
                                                             repartBETA()))
        })
        
        output$survieBETA <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                             input$xBETA,
                                                             survieBETA()))
        })
        
        output$VaRBETA <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                          input$kBETA,
                                                          VaRBETA()))
        })
        
        output$TVaRBETA <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
                                                           input$kBETA,
                                                           TVaRBETA()))
        })
        
        output$EspTronqBETA <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$",
                                                               input$d2BETA,
                                                               EspTronqBETA()))
        })
        
        output$StopLossBETA <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$",
                                                               input$dBETA,
                                                               StopLossBETA()))
        })
        
        output$EspLimBETA <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$",
                                                             input$dBETA,
                                                             EspLimBETA()))
        })
        
        output$ExcesMoyBETA <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$",
                                                               input$dBETA,
                                                               ExcesMoyBETA()))
        })
        
        
        # output$FxBETA 
        
        # output$SxBETA
        
        
#### Loi Binomiale Serveur ####
        nBIN <- reactive({input$nBIN})
        
        pBIN <- reactive({input$pBIN})
        
        meanBIN <- reactive({nBIN() * pBIN()})
        
        varBIN <- reactive({nBIN() * pBIN() * (1 - pBIN())})   
        
        densityBIN <- reactive({format(dbinom(input$xBIN, nBIN(), pBIN()), nsmall = 6)})
        
        repartBIN <- reactive({format(pbinom(input$xBIN, nBIN(), pBIN()), nsmall = 6)})
        
        survieBIN <- reactive({format(pbinom(input$xBIN, nBIN(), pBIN(), lower.tail = F), nsmall = 6)})
        
        # VaRBIN <- reactive({format(qbinom(input$kBIN,
        #                                   nBIN(), 
        #                                   pBIN()), 
        #                            nsmall = 6)
        #     })
        
        # TVaRBIN <- reactive({format(TVaR_binom(kappa = input$kBIN,
        #                                        n = nBIN(),
        #                                        p = pBIN()),
        #                             nsmall = 6)
        #     })
        
        # EspTronqBIN <- reactive({0})
        
        # StopLossBIN <- reactive({0})
        
        # EspLimBIN <- reactive({0})
        
        # ExcesMoyBIN <- reactive({0})
        
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

        output$survieBIN <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                            input$xBIN,
                                                            survieBIN()))
        })
        
        # output$VaRBIN <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$", 
        #                                                input$kBIN,
        #                                                VaRBIN()
        # ))})
        # output$TVaRBIN <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$", 
        #                                                 input$kBIN,
        #                                                 TVaRBIN()
        # ))})
        # output$EspTronqBIN <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$", 
        #                                                     input$dBIN,
        #                                                     EspTronqBIN()
        # ))})
        # 
        # output$StopLossBIN <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$", 
        #                                                     input$dBIN,
        #                                                     StopLossBIN()
        # ))})
        # 
        # output$EspLimBIN <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$", 
        #                                                   input$dBIN,
        #                                                   EspLimBIN()
        # ))})
        # 
        # output$ExcesMoyBIN <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$", 
        #                                                     input$dBIN,
        #                                                     ExcesMoyBIN()
        # ))})
        
        output$FxBIN <- renderPlotly({ggplot(data.frame(x = 0:nBIN(), y = dbinom(0:nBIN(), nBIN(), pBIN())), aes(x = x, y = y)) + geom_bar(stat = "identity", col = "red", fill ="red", alpha = 0.7, width = 0.3) + theme_classic() + ylab("P(X=x")
            
        })
        
        
        
        # Reactive slider
        observeEvent(input$nBIN,{updateSliderInput(session = session, inputId = "xBIN", max = input$nBIN)
        })
    
#### Loi Poisson Serveur ####
    lamPOI <- reactive({input$lamPOI})
    
    densityPOI <- reactive({format(dpois(input$xPOI, lamPOI()), nsmall = 6)})
    
    repartPOI <- reactive({format(ppois(input$xPOI, lamPOI()), nsmall = 6)})
    
    surviePOI <- reactive({format(ppois(input$xPOI, lamPOI(), lower.tail = F), nsmall = 6)})
    
    VaRPOI <- reactive({format(qpois(input$kPOI,
                                     lambda = input$lamPOI),
                               nsmall = 6)
        })
    
    # TVaRPOI <- reactive({format(TVaR_pois(kappa = input$kPOI,
    #                                       lam = input$lamPOI),
    #                             nsmall = 6)
    #     })
    
    # EspTronqPOI <- reactive({0})
    
    # StopLossPOI <- reactive({0})
    
    # EspLimPOI <- reactive({0})
    
    # ExcesMoyPOI <- reactive({0})
    
    output$meanPOI <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                    lamPOI()
    ))})
    
    output$varPOI <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                   lamPOI()
    ))})
    
    output$densityPOI <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                       input$xPOI,
                                                       densityPOI()
    ))})
    output$repartPOI <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
                                                      input$xPOI,
                                                      repartPOI()
    ))})
    
    output$surviePOI <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                      input$xPOI,
                                                      surviePOI()))
    })
    
    output$VaRPOI <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                   input$kPOI,
                                                   VaRPOI()
    ))})
    # output$TVaRPOI <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
    #                                                 input$kPOI,
    #                                                 TVaRPOI()
    # ))})
    # output$EspTronqPOI <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$", 
    #                                                     input$dPOI,
    #                                                     EspTronqPOI()
    # ))})
    # 
    # output$StopLossPOI <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$", 
    #                                                     input$dPOI,
    #                                                     StopLossPOI()
    # ))})
    # 
    # output$EspLimPOI <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$", 
    #                                                   input$dPOI,
    #                                                   EspLimPOI()
    # ))})
    # 
    # output$ExcesMoyPOI <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$", 
    #                                                     input$dPOI,
    #                                                     ExcesMoyPOI()
    # ))})
    
    # output$FxPOI <- renderPlotly({ggplot(data.frame(x = 0:nPOI(), y = dPOIom(0:nPOI(), nPOI(), pPOI())), aes(x = x, y = y)) + geom_bar(stat = "identity", col = "red", fill ="red", alpha = 0.7, width = 0.3) + theme_classic() + ylab("P(X=x")
    #     
    # })
    
}
