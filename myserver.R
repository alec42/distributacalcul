myserver <- function(input, output, session) 
{
    shapeEXCESS_MEAN <- reactive({input$shapeEXCESS_MEAN})
    rateEXCESS_MEAN <- reactive({input$rateEXCESS_MEAN})
    
    # output$gammaEXCESS_MEAN <- renderUI({withMathJax(sprintf("$$e_X(d) = \\frac{%s}{%s}\\frac{\\bar{H}(d; %s + 1, %s)}{\\bar{H}(d; %s, %s)} - d$$", 
    #                                                          shapeEXCESS_MEAN(),
    #                                                          rateEXCESS_MEAN(),
    #                                                          shapeEXCESS_MEAN(),
    #                                                          rateEXCESS_MEAN(),
    #                                                          shapeEXCESS_MEAN(),
    #                                                          rateEXCESS_MEAN()
    # ))
    # })
    
    output$gammaEXCESS_MEAN <- renderUI({withMathJax(sprintf("$$\\Gamma(\\alpha = %s, \\beta = %s)$$", 
                                                             shapeEXCESS_MEAN(),
                                                             rateEXCESS_MEAN()
    ))
    })
    output$paretoEXCESS_MEAN <- renderUI({withMathJax(sprintf("$$\\text{Pa}(\\alpha = %s, \\lambda = %s)$$", 
                                                             shapeEXCESS_MEAN(),
                                                             rateEXCESS_MEAN()
    ))
    })
    output$normEXCESS_MEAN <- renderUI({withMathJax(sprintf("$$\\mathcal{N}(\\mu = %s, \\sigma = %s)$$", 
                                                             shapeEXCESS_MEAN(),
                                                             rateEXCESS_MEAN()
    ))
    })
    # output$lnormEXCESS_MEAN <- renderUI({withMathJax(sprintf("$$\\mathcal{LN}(\\mu = %s, \\sigma = %s)$$", 
    #                                                         shapeEXCESS_MEAN(),
    #                                                         rateEXCESS_MEAN()
    # ))
    # })
    
    output$weibullEXCESS_MEAN <- renderUI({withMathJax(sprintf("$$\\text{Wei}(\\tau = %s, \\beta= %s)$$", 
                                                             shapeEXCESS_MEAN(),
                                                             rateEXCESS_MEAN()
    ))
    })
    
    output$plotEXCESS_MEAN <- renderPlot({  
        
        # curve(Mexcess_gamma(d = x, a = shapeEXCESS_MEAN(), b = rateEXCESS_MEAN()))
        # curve(Mexcess_pareto(d = x, alph = shapeEXCESS_MEAN(), lam = rateEXCESS_MEAN()), add = T)
        x <- seq(0, 10, 1)
        y1 <- Mexcess_gamma(d = x, a = shapeEXCESS_MEAN(), b = rateEXCESS_MEAN())
        y2 <- Mexcess_pareto(d = x, alph = shapeEXCESS_MEAN(), lam = rateEXCESS_MEAN())
        y3 <- Mexcess_norm(d = x, mu = shapeEXCESS_MEAN(), sig = rateEXCESS_MEAN())
        # y4 <- Mexcess_lnorm(d = x, mu = shapeEXCESS_MEAN(), sig = rateEXCESS_MEAN())
        y5 <- Mexcess_weibull(d = x, tau = shapeEXCESS_MEAN(), beta = rateEXCESS_MEAN())
        ggplot(data.frame(x, y1, y2, y3), aes(x)) +          
            geom_line(aes(y=y1), colour="red") +  
            geom_line(aes(y=y2), colour="green") +
            geom_line(aes(y=y3), colour="blue") + 
            # geom_line(aes(y=y4), colour="orange")
            geom_line(aes(y=y5), colour="purple") 
        
        
        
        # lines(year,chartData[[1]],col="aquamarine4",lwd=3)
        # lines(year[2:12],na.omit(chartData[[2]]),col="firebrick3",lwd=3)
        # abline(v=input$vertical,lty=2) 
        # legend(2012,8,c("Real government spending","Real GDP"), 
               # col=c('firebrick3','aquamarine4'),pch=15,ncol=1,bty ="n",cex=1.1)
        
        # if (input$hor) {
            # abline(h=0)  
        # } 
    }
    # ,height = 500, width = 600
    )
    
    output$descriptionEXCESS_MEAN <- renderText({"Embauche d'outil pour observer la fonction d'excès-moyen pour plusieurs distributions. À travailler après que je comprends mieux la Lognormale et Weibull."})
    
    
#### Loi Normale Serveur ####
        
    muNORM <- reactive({input$muNORM})
        
    sigma2NORM <- reactive({input$sigmaNORM})
        
    densityNORM <- reactive({format(dnorm(input$xNORM, muNORM(), sqrt(sigma2NORM())), nsmall = 6)})
        
    repartNORM <- reactive({format(pnorm(input$xNORM, muNORM(), sqrt(sigma2NORM())), nsmall = 6)})
        
    survieNORM <- reactive({format(pnorm(input$xNORM, muNORM(), sqrt(sigma2NORM()), lower.tail = F),
                                   nsmall = 6)
    })
        
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
      
    output$QxNORM <- renderPlotly({
      ggplot(data = data.frame(x = c(0,1)),
      aes(x)) +
        stat_function(fun = qnorm,
                      args = list(mean = muNORM(),
                                  sd = sqrt(sigma2NORM()))) +
        ylab("f(x)") +        # Ne marche pas encore je suis en phase de test.
        theme_classic() +
        stat_function(
          fun = qnorm,
          args = list(mean = muNORM(), sd = sqrt(sigma2NORM())),
          xlim = c(input$kNORM, 1),
          geom = "area",
          fill = "#50CB86",
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
            
            updateNumericInput(session,
                               "distrchoiceGAMMA",
                               value =
                                   if (x == "Khi carré")
                                   {
                                       y = T
                                   })
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
            {
                hide("betaGAMMA")
                hide("distrchoiceGAMMA")
            }
            else
            {
                show("betaGAMMA")
                show("distrchoiceGAMMA")
            }
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
        
        output$loi_gamma <- renderUI({
            if(input$distrchoiceEXPOFAM == "Gamma")
                "Loi Gamma"
            else if (input$distrchoiceEXPOFAM == "Exponentielle")
                titlePanel(tags$a("Loi Exponentielle",href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Exponentielle"))
            else
                "Loi Khi carré"
        })
        
        output$distr_gamma <- renderText({
            if(input$distrchoiceEXPOFAM == "Gamma")
                "\\(X \\sim\\mathcal{Gamma}(\\alpha, \\beta)\\)"
            else if (input$distrchoiceEXPOFAM == "Exponentielle")
                "\\(X \\sim\\mathcal{Exponentielle}(\\beta)\\)"
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
        
        
#### Loi Erlang Serveur ####
        
        nERLANG <- reactive({input$nERLANG})
        
        betaERLANG <- reactive({input$betaERLANG})
        
        dERLANG <- reactive({input$dERLANG})
        
        kERLANG <- reactive({input$kERLANG})
        
        xERLANG <- reactive({input$xERLANG})
        
        densityERLANG <- reactive({format(derlang(x = xERLANG(),
                                                  n = nERLANG(),
                                                  b = betaERLANG()), 
                                        nsmall = 6)})
        
        repartERLANG <- reactive({format(perlang(x = xERLANG(),
                                                 n = nERLANG(),
                                                 b = betaERLANG()), 
                                       nsmall = 6)})
        
        survieERLANG <- reactive({format(perlang(x = xERLANG(),
                                                 n = nERLANG(),
                                                 b = betaERLANG(),
                                                 lower.tail = F), 
                                       nsmall = 6)})
        
        # VaRERLANG <- reactive({format((k = input$kERLANG,
        #                                      n = nERLANG(),
        #                                      lam = betaERLANG(),
        #                                      tau = tauERLANG()), 
        #                             nsmall = 6)
        # })
        # 
        # VaRERLANG_a <- reactive({VaR_ERLANG(k = input$kERLANG,
        #                                 n = nERLANG(),
        #                                 lam = betaERLANG(),
        #                                 tau = tauERLANG()) 
        # })
        # 
        # 
        # TVaRERLANG <- reactive({format(TVaR_ERLANG(k = input$kERLANG,
        #                                        var = VaRERLANG_a(),
        #                                        n = nERLANG(),
        #                                        lam = betaERLANG(),
        #                                        tau = tauERLANG()), 
        #                              nsmall = 6)
        # })
        
        EspTronqERLANG <- reactive({Etronq_erlang(d = dERLANG(),
                                                  n = nERLANG(),
                                                  b = betaERLANG())
        })
        
        StopLossERLANG <- reactive({SL_erlang(d = dERLANG(),
                                              n = nERLANG(),
                                              b = betaERLANG())
        })
        
        EspLimERLANG <- reactive({Elim_erlang(d = dERLANG(),
                                              n = nERLANG(),
                                              b = betaERLANG())
        })
        
        kthmomentERLANG <- reactive({kthmoment_erlang(k = dERLANG(),
                                                      n = nERLANG(),
                                                      b = betaERLANG())
        })
        
        ExcesMoyERLANG <- reactive({Mexcess_erlang(d = dERLANG(),
                                                   n = nERLANG(),
                                                   b = betaERLANG())
        })
        
        meanERLANG <- reactive({E_erlang(n = nERLANG(),
                                         b = betaERLANG())
        })
        
        varianceERLANG <- reactive({V_erlang(n = nERLANG(),
                                             b = betaERLANG())
        })
        
        output$meanERLANG <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                           meanERLANG()))
        })
        
        output$varERLANG <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                          varianceERLANG()))
        })
        
        output$densityERLANG <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                              xERLANG(),
                                                              densityERLANG()))
        })
        output$repartERLANG <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
                                                             xERLANG(),
                                                             repartERLANG()))
        })
        
        output$survieERLANG <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                             xERLANG(),
                                                             survieERLANG()))
        })
        
        # output$VaRERLANG <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
        #                                                   kERLANG(),
        #                                                   VaRERLANG()))
        # })
        # 
        # output$TVaRERLANG <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
        #                                                  kERLANG(),
        #                                                  TVaRERLANG()))
        # })
        
        output$EspTronqERLANG <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$",
                                                             dERLANG(),
                                                             EspTronqERLANG()))
        })
        
        output$StopLossERLANG <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$",
                                                               dERLANG(),
                                                               StopLossERLANG()))
        })
        
        output$EspLimERLANG <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$",
                                                             dERLANG(),
                                                             EspLimERLANG()))
        })
        
        output$ExcesMoyERLANG <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$",
                                                               dERLANG(),
                                                               ExcesMoyERLANG()))
        })
        
        output$kthmomentERLANG <- renderUI({withMathJax(sprintf("$$E[X^{%s}] = %.4f$$",
                                                                dERLANG(),
                                                                ExcesMoyERLANG()))
        })
        
        
        # output$FxERLANG 
        
        # output$SxERLANG
        
#### Loi Log-logistique Serveur ####
        
        lambdaLOGLOGIS <- reactive({input$lambdaLOGLOGIS})
        
        tauLOGLOGIS <- reactive({input$tauLOGLOGIS})
        
        dLOGLOGIS <- reactive({input$dLOGLOGIS})
        
        kLOGLOGIS <- reactive({input$kLOGLOGIS})
        
        xLOGLOGIS <- reactive({input$xLOGLOGIS})
        
        densityLOGLOGIS <- reactive({format(dllogis(x = xLOGLOGIS(), 
                                                    shape = lambdaLOGLOGIS(), 
                                                    rate = tauLOGLOGIS()), 
                                            nsmall = 6)})
        
        repartLOGLOGIS <- reactive({format(pllogis(q = xLOGLOGIS(), 
                                                   shape = lambdaLOGLOGIS(), 
                                                   rate = tauLOGLOGIS()), 
                                           nsmall = 6)})
        
        survieLOGLOGIS <- reactive({format(pllogis(q = xLOGLOGIS(), 
                                                   shape = lambdaLOGLOGIS(), 
                                                   rate = tauLOGLOGIS(),
                                                   lower.tail = F), 
                                           nsmall = 6)})
        
        VaRLOGLOGIS <- reactive({format(VaR_llogis(k = kLOGLOGIS(), 
                                                   lam = lambdaLOGLOGIS(), 
                                                   tau = tauLOGLOGIS()), 
                                        nsmall = 6)
        })
        
        TVaRLOGLOGIS <- reactive({format(TVaR_llogis(k = kLOGLOGIS(),
                                                     lam = lambdaLOGLOGIS(),
                                                     tau = tauLOGLOGIS()), 
                                         nsmall = 6)
        })
        
        EspTronqLOGLOGIS <- reactive({Etronq_llogis(d = dLOGLOGIS(),
                                                    lam = lambdaLOGLOGIS(),
                                                    tau = tauLOGLOGIS())
        })
        
        StopLossLOGLOGIS <- reactive({SL_llogis(d = dLOGLOGIS(),
                                                lam = lambdaLOGLOGIS(),
                                                tau = tauLOGLOGIS())
        })
        
        EspLimLOGLOGIS <- reactive({Elim_llogis(d = dLOGLOGIS(),
                                              lam = lambdaLOGLOGIS(),
                                              tau = tauLOGLOGIS())
        })
        
        kthmomentLOGLOGIS <- reactive({kthmoment_llogis(k = kLOGLOGIS(),
                                                        lam = lambdaLOGLOGIS(),
                                                        tau = tauLOGLOGIS())
        })
        
        ExcesMoyLOGLOGIS <- reactive({Mexcess_llogis(d = dLOGLOGIS(),
                                                     lam = lambdaLOGLOGIS(),
                                                     tau = tauLOGLOGIS())
        })
        
        meanLOGLOGIS <- reactive({kthmoment_llogis(k = 1,
                                                   lam = lambdaLOGLOGIS(),
                                                   tau = tauLOGLOGIS())
        })
        
        varianceLOGLOGIS <- reactive({V_llogis(lam = lambdaLOGLOGIS(),
                                               tau = tauLOGLOGIS())
        })
        
        output$meanLOGLOGIS <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                             meanLOGLOGIS()))
        })
        
        output$varLOGLOGIS <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                            varianceLOGLOGIS()))
        })
        
        output$densityLOGLOGIS <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                                xLOGLOGIS(),
                                                                densityLOGLOGIS()))
        })
        output$repartLOGLOGIS <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
                                                               xLOGLOGIS(),
                                                               repartLOGLOGIS()))
        })
        
        output$survieLOGLOGIS <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                               xLOGLOGIS(),
                                                               survieLOGLOGIS()))
        })
        
        output$VaRLOGLOGIS <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                            kLOGLOGIS(),
                                                            VaRLOGLOGIS()))
        })
        
        output$TVaRLOGLOGIS <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
                                                             kLOGLOGIS(),
                                                             TVaRLOGLOGIS()))
        })
        
        output$EspTronqLOGLOGIS <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$",
                                                                 dLOGLOGIS(),
                                                                 EspTronqLOGLOGIS()))
        })
        
        output$StopLossLOGLOGIS <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$",
                                                                 dLOGLOGIS(),
                                                                 StopLossLOGLOGIS()))
        })
        
        output$EspLimLOGLOGIS <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$",
                                                               dLOGLOGIS(),
                                                               EspLimLOGLOGIS()))
        })
        
        output$ExcesMoyLOGLOGIS <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$",
                                                                 dLOGLOGIS(),
                                                                 ExcesMoyLOGLOGIS()))
        })
        
        output$kthmomentLOGLOGIS <- renderUI({withMathJax(sprintf("$$E[X^{%s}] = %.4f$$",
                                                                  dLOGLOGIS(),
                                                                  kthmomentLOGLOGIS()))
        })
        
        
        # output$FxLOGLOGIS 
        
        # output$SxLOGLOGIS
        
#### Loi Weibull Serveur ####
        
        betaIG <- reactive({input$betaIG})
        
        muIG <- reactive({input$muIG})
        
        dIG <- reactive({input$dIG})
        
        kIG <- reactive({input$kIG})
        
        xIG <- reactive({input$xIG})
        
        densityIG <- reactive({format(d_IG(x = xIG(), 
                                           mu = muIG(),
                                           beta = betaIG()), 
                                      nsmall = 6)
        })
        
        repartIG <- reactive({format(p_IG(q = xIG(), 
                                          mu = muIG(),
                                          beta = betaIG()), 
                                     nsmall = 6)
        })
        
        survieIG <- reactive({format(p_IG(q = xIG(), 
                                         mu = muIG(),
                                         beta = betaIG(),
                                         lower.tail = F), 
                                     nsmall = 6)
        })
        
        VaRIG <- reactive({format(VaR_IG(k = kIG(),
                                         mu = muIG(), 
                                         beta = betaIG()),
                                  nsmall = 6)
        })
        
        VaRIG_a <- reactive({VaR_IG(p = kIG(),
                                    mu = muIG(), 
                                    beta = betaIG())
        })
        
        TVaRIG <- reactive({format(TVaR_IG(vark = VaRIG_a(),
                                           mu = muIG(), 
                                           k = kIG(),
                                           beta = betaIG()), 
                                   nsmall = 6)
        })
        
        EspTronqIG <- reactive({Etronq_IG(d = dIG(),
                                          beta = betaIG(),
                                          mu = muIG())
        })
        
        StopLossIG <- reactive({SL_IG(d = dIG(),
                                      beta = betaIG(),
                                      mu = muIG())
        })
        
        EspLimIG <- reactive({Elim_IG(d = dIG(),
                                      beta = betaIG(),
                                      mu = muIG())
        })
        
        # ExcesMoyIG <- reactive({Mexcess_IG(d = dIG(),
        #                                    beta = betaIG(),
        #                                    mu = muIG())
        # })
        
        meanIG <- reactive({E_IG( mu = muIG())
        })
        
        # kthmomentIG <- reactive({E_IG(beta = betaIG(),
        #                               mu = muIG(),
        #                               k = dIG())
        # })
        
        varianceIG <- reactive({V_IG(beta = betaIG(),
                                     mu = muIG())
        })
        
        output$meanIG <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                       format(meanIG(),
                                                              nsmall = 6)))
        })
        # 
        # output$kthmomentIG <- renderUI({withMathJax(sprintf("$$E(X^%s) = %s$$", 
        #                                                     input$dIG,
        #                                                     format(kthmomentIG(),
        #                                                            nsmall = 6)))
        # })
        
        output$varianceIG <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                           format(varianceIG(),
                                                                  nsmall = 6)))
        })
        
        output$densityIG <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                               input$xIG,
                                                               densityIG()))
        })
        output$repartIG <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
                                                              input$xIG,
                                                              repartIG()))
        })
        
        output$survieIG <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                              input$xIG,
                                                              survieIG()))
        })
        
        output$VaRIG <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                           input$kIG,
                                                           VaRIG()))
        })
        
        output$TVaRIG <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
                                                            input$kIG,
                                                            TVaRIG()))
        })
        
        output$EspTronqIG <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$",
                                                                input$dIG,
                                                                EspTronqIG()))
        })
        
        output$StopLossIG <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$",
                                                                input$dIG,
                                                                StopLossIG()))
        })
        
        output$EspLimIG <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$",
                                                              input$dIG,
                                                              EspLimIG()))
        })
        
        # output$ExcesMoyIG <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$",
        #                                                         input$dIG,
        #                                                         ExcesMoyIG()))
        # })
        
        # output$FxIG 
        
        # output$SxIG
        
#### Loi Uniforme Discrète Serveur ####
        
        aUNIC <- reactive({input$aUNIC})
        bUNIC <- reactive({input$bUNIC})
        xUNIC <- reactive({input$xUNIC})
        kUNIC <- reactive({input$kUNIC})
        
        
        meanUNIC <- reactive({E_unif(a = aUNIC(), b = bUNIC())
        })
        
        varUNIC <- reactive({
            format(V_unif(a = aUNIC(), b = bUNIC()), 
                   nsmall = 6)
        })
        
        densityUNIC <- reactive({format(dunif(x = xUNIC(), min = aUNIC(), max = bUNIC()),
                                        nsmall = 6)
        })
        
        repartUNIC <- reactive({format(punif(q = xUNIC(), min = aUNIC(), max = bUNIC()), 
                                       nsmall = 6)
        })
        
        survieUNIC <- reactive({format(1 - punif(q = xUNIC(), min = aUNIC(), max = bUNIC()), 
                                       nsmall = 6)
        })
        
        VaRUNIC <- reactive({format(VaR_unif(kappa = kUNIC(), a = aUNIC(), b = bUNIC()),
                                   nsmall = 6)
        })
# 
#         TVaRUNIC <- reactive({format(TVaR_UNIC(kappa = input$kUNIC,
#                                               lam = input$lamUNIC),
#                                     nsmall = 6)
#             })
        
        # EspTronqUNIC <- reactive({0})
        
        # StopLossUNIC <- reactive({0})
        
        # EspLimUNIC <- reactive({0})
        
        # ExcesMoyUNIC <- reactive({0})
        
        output$meanUNIC <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                         meanUNIC()
        ))})
        
        output$varUNIC <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                        varUNIC()
        ))})
        
        output$densityUNIC <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                            input$xUNIC,
                                                            densityUNIC()
        ))})
        
        output$repartUNIC <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
                                                           input$xUNIC,
                                                           repartUNIC()
        ))})
        
        output$survieUNIC <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                           input$xUNIC,
                                                           survieUNIC()))
        })
        
        output$VaRUNIC <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                       input$kUNIC,
                                                       VaRUNIC()
        ))})
        # output$TVaRUNIC <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
        #                                                 input$kUNIC,
        #                                                 TVaRUNIC()
        # ))})
        # output$EspTronqUNIC <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$", 
        #                                                     input$dUNIC,
        #                                                     EspTronqUNIC()
        # ))})
        # 
        # output$StopLossUNIC <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$", 
        #                                                     input$dUNIC,
        #                                                     StopLossUNIC()
        # ))})
        # 
        # output$EspLimUNIC <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$", 
        #                                                   input$dUNIC,
        #                                                   EspLimUNIC()
        # ))})
        # 
        # output$ExcesMoyUNIC <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$", 
        #                                                     input$dUNIC,
        #                                                     ExcesMoyUNIC()
        # ))})
        
#### Loi Binomiale Serveur ####
        nBIN <- reactive({input$nBIN})
        
        pBIN <- reactive({input$pBIN})
        
        observeEvent(
            {
                input$distrchoiceBINFAM 
            },
            {
                x <- input$distrchoiceBINFAM
                
                updateNumericInput(session, "nBIN",
                                   value =
                                   if (x == "Bernoulli")
                                   {
                                       nBIN = 1
                                   }
                                   else
                                   {
                                       nBIN = 2
                                   }
                )
                
                # rend le paramètre impossible à modifier pour l'utilisateur
                if (x == "Bernoulli")
                    hide("nBIN")
                else
                    show("nBIN")
                
            }
        )
        
        
        output$loi_BIN <- renderUI({
            if(input$distrchoiceBINFAM == "Bernoulli")
                titlePanel(tags$a("Loi Bernoulli",href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Bernoulli"))
            else
                titlePanel(tags$a("Loi Binomiale",href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Binomiale"))
        })
        
        output$distr_BIN <- renderText({
            if(input$distrchoiceBINFAM == "Bernoulli")
                "\\(X \\sim\\text{Bernoulli} \\ (p)\\)"
            else
                "\\(X \\sim\\text{Binomiale} \\ (n, p)\\)"
        })
        
        meanBIN <- reactive({nBIN() * pBIN()})
        
        varBIN <- reactive({nBIN() * pBIN() * (1 - pBIN())})   
        
        densityBIN <- reactive({format(dbinom(input$xBIN, nBIN(), pBIN()), nsmall = 6)})
        
        repartBIN <- reactive({format(pbinom(input$xBIN, nBIN(), pBIN()), nsmall = 6)})
        
        survieBIN <- reactive({format(pbinom(input$xBIN, nBIN(), pBIN(), lower.tail = F), nsmall = 6)})
        
        VaRBIN <- reactive({format(VaR_binom(input$kBIN,
                                          nBIN(),
                                          pBIN()),
                                   nsmall = 6)
            })
        
        TVaRBIN <- reactive({format(TVaR_binom(kappa = input$kBIN,
                                               n = nBIN(),
                                               p = pBIN()),
                                    nsmall = 6)
            })
        
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
        
        output$VaRBIN <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                       input$kBIN,
                                                       VaRBIN()
        ))})
        output$TVaRBIN <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
                                                        input$kBIN,
                                                        TVaRBIN()
        ))})
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
    
#### Loi Hypergéométrique Serveur ####
    
    grosNHG <- reactive({input$grosNHG})
    
    petitNHG <- reactive({input$petitNHG})
    
    mHG <- reactive({input$mHG})
    
    meanHG <- reactive({E_hyper(N = grosNHG(), m = mHG(), n = petitNHG())})
        
    varHG <- reactive({
        format(V_hyper(N = grosNHG(), 
                       m = mHG(), 
                       n = petitNHG()
        ), 
        nsmall = 6)
    })

    densityHG <- reactive({
        format(dhyper(input$xHG, 
                      m = mHG(), 
                      n = grosNHG() - mHG(), 
                      k = petitNHG()
        ), 
        nsmall = 6)
    })
    
    repartHG <- reactive({
        format(phyper(input$xHG, 
                      m = mHG(), 
                      n = grosNHG() - mHG(), 
                      k = petitNHG()
        ), 
        nsmall = 6)
    })
    
    survieHG <- reactive({
        format(phyper(input$xHG, 
                      m = mHG(), 
                      n = grosNHG() - mHG(), 
                      k = petitNHG(),
                      lower.tail = F 
        ), 
        nsmall = 6)
    })
    
    VaRHG <- reactive({
        format(qhyper(input$xHG, 
                      m = mHG(), 
                      n = grosNHG() - mHG(), 
                      k = petitNHG()
        ), 
        nsmall = 6)
    })
    
    # TVaRHG <- reactive({format(TVaR_HGs(kappa = input$kHG,
    #                                       lam = input$lamHG),
    #                             nsmall = 6)
    #     })
    
    # EspTronqHG <- reactive({0})
    
    # StopLossHG <- reactive({0})
    
    # EspLimHG <- reactive({0})
    
    # ExcesMoyHG <- reactive({0})
    
    output$meanHG <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                   meanHG()
    ))})
    
    output$varHG <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                  varHG()
    ))})
    
    output$densityHG <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                      input$xHG,
                                                      densityHG()
    ))})
    output$repartHG <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
                                                     input$xHG,
                                                     repartHG()
    ))})
    
    output$survieHG <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                     input$xHG,
                                                     survieHG()))
    })
    
    output$VaRHG <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                  input$kHG,
                                                  VaRHG()
    ))})
    # output$TVaRHG <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
    #                                                 input$kHG,
    #                                                 TVaRHG()
    # ))})
    # output$EspTronqHG <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$", 
    #                                                     input$dHG,
    #                                                     EspTronqHG()
    # ))})
    # 
    # output$StopLossHG <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$", 
    #                                                     input$dHG,
    #                                                     StopLossHG()
    # ))})
    # 
    # output$EspLimHG <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$", 
    #                                                   input$dHG,
    #                                                   EspLimHG()
    # ))})
    # 
    # output$ExcesMoyHG <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$", 
    #                                                     input$dHG,
    #                                                     ExcesMoyHG()
    # ))})
    
    # output$FxHG <- renderPlotly({ggplot(data.frame(x = 0:nHG(), y = dHGom(0:nHG(), nHG(), pHG())), aes(x = x, y = y)) + geom_bar(stat = "identity", col = "red", fill ="red", alpha = 0.7, width = 0.3) + theme_classic() + ylab("P(X=x")
    #     
    # })

#### Loi Logarithmique Serveur ####
    
    gammaLOGARITHMIQUE <- reactive({input$gammaLOGARITHMIQUE})
    
    xLOGARITHMIQUE <- reactive({input$xLOGARITHMIQUE})
    
    kLOGARITHMIQUE <- reactive({input$kLOGARITHMIQUE})
    
    meanLOGARITHMIQUE <- reactive({E_logarithmique(gam = gammaLOGARITHMIQUE())})
    
    varLOGARITHMIQUE <- reactive({
        format(V_logarithmique(gam = gammaLOGARITHMIQUE()), 
               nsmall = 6)
    })
    
    densityLOGARITHMIQUE <- reactive({
        format(dlogarithmic(x = xLOGARITHMIQUE(), 
                            prob = gammaLOGARITHMIQUE()), 
        nsmall = 6)
    })
    
    repartLOGARITHMIQUE <- reactive({
        format(plogarithmic(q = xLOGARITHMIQUE(), 
                            prob = gammaLOGARITHMIQUE()), 
               nsmall = 6)
    })
    
    survieLOGARITHMIQUE <- reactive({
        format(plogarithmic(q = xLOGARITHMIQUE(), 
                      prob = gammaLOGARITHMIQUE(),
                      lower.tail = F 
        ), 
        nsmall = 6)
    })
    
    VaRLOGARITHMIQUE <- reactive({
        format(qlogarithmic(p = kLOGARITHMIQUE(),
                            prob = gammaLOGARITHMIQUE()
        ), 
        nsmall = 6)
    })
    
    output$meanLOGARITHMIQUE <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                              meanLOGARITHMIQUE()
    ))})
    
    output$varLOGARITHMIQUE <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                             varLOGARITHMIQUE()
    ))})
    
    output$densityLOGARITHMIQUE <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                                 xLOGARITHMIQUE(),
                                                                 densityLOGARITHMIQUE()
    ))})
    output$repartLOGARITHMIQUE <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
                                                                xLOGARITHMIQUE(),
                                                                repartLOGARITHMIQUE()
    ))})
    
    output$survieLOGARITHMIQUE <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                                xLOGARITHMIQUE(),
                                                                survieLOGARITHMIQUE()))
    })
    
    output$VaRLOGARITHMIQUE <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                             kLOGARITHMIQUE(),
                                                             VaRLOGARITHMIQUE()
    ))})
    
#### Loi Binomiale Négative Serveur ####
    
    rBN <- reactive({input$rBN})
    
    qBN <- reactive({
        if (input$distrchoiceqBN == T) {
            input$qBN
        } else {
            (1 / (1 + input$qBN))
        }
    })
    
    output$changingqBN <- renderUI({
        numericInput('qBN', label = '$$q$$', value = 0.5, min = 0, step = 0.1)
    })
    
    output$changingrBN <- renderUI({
        numericInput('rBN', label = '$$r$$', value = 1, min = 0, step = 1)
    })
    
    ## Ici on crée un gros observeEvent qui va modifier les paramètres de la gamma/exponentielle/khi-carrée selon les 2 radio buttons:
    ## x: selection de distribution
    ## y: selection de si c'est fréquence ou échelle
    observeEvent(
    {
        input$distrchoiceqBN
        input$distrchoiceBNFAM 
    },
    {
        x <- input$distrchoiceBNFAM
        y <- input$distrchoiceqBN
        
        updateNumericInput(session, "rBN",
                           value =
                               if (x == "Géometrique")
                               {
                                   rBN = 1
                               }
                           else
                           {
                               rBN = 2
                           }
        )
        updateNumericInput(session,
                           "qBN",
                           label = {
                               if (y == T)
                               {
                                   '$$q$$'
                               }
                               else{
                                   '$$\\beta$$'
                               }
                           }
        )
        
        
        # rend le paramètre impossible à modifier pour l'utilisateur
        if (x == "Géometrique")
            hide("rBN")
        else
            show("rBN")
        
    })
    
    
    output$loi_BN <- renderText({
        if(input$distrchoiceBNFAM == "Géometrique")
            "Loi Géometrique"
        else
            "Loi Binomiale Négative"
    })
    
    output$distr_BN <- renderText({
        if(input$distrchoiceBNFAM == "Géometrique")
            "\\(X \\sim\\text{Géometrique} \\ (q)\\)"
        else
            "\\(X \\sim\\text{Binomiale Négative} \\ (r, q)\\)"
    })
    
    
    meanBN <- reactive({rBN() * (1 - qBN())/qBN()})
    
    varBN <- reactive({rBN() * (1 - qBN())/(qBN()^2)})   
    
    densityBN <- reactive({format(dnbinom(input$xBN, rBN(), qBN()), nsmall = 6)})
    
    repartBN <- reactive({format(pnbinom(input$xBN, rBN(), qBN()), nsmall = 6)})
    
    survieBN <- reactive({format(pnbinom(input$xBN, rBN(), qBN(), lower.tail = F), nsmall = 6)})
    
    # VaRBN <- reactive({format(VaR_BNom(input$kBN,
    #                                      rBN(),
    #                                      qBN()),
    #                            nsmall = 6)
    # })
    
    # TVaRBN <- reactive({format(TVaR_BNom(kappa = input$kBN,
    #                                        n = rBN(),
    #                                        p = qBN()),
    #                             nsmall = 6)
    # })
    
    # EspTronqBN <- reactive({0})
    
    # StopLossBN <- reactive({0})
    
    # EspLimBN <- reactive({0})
    
    # ExcesMoyBN <- reactive({0})
    
    output$meanBN <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                   meanBN()
    ))})
    
    output$varBN <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                   varBN()
    ))})
    
    output$densityBN <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                       input$xBN,
                                                       densityBN()
    ))})
    output$repartBN <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
                                                      input$xBN,
                                                      repartBN()
    ))})
    
    output$survieBN <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                      input$xBN,
                                                      survieBN()))
    })
    
    # output$VaRBN <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
    #                                                input$kBN,
    #                                                VaRBN()
    # ))})
    # output$TVaRBN <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
    #                                                 input$kBN,
    #                                                 TVaRBN()
    # ))})
    # output$EspTronqBN <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$", 
    #                                                     input$dBN,
    #                                                     EspTronqBN()
    # ))})
    # 
    # output$StopLossBN <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$", 
    #                                                     input$dBN,
    #                                                     StopLossBN()
    # ))})
    # 
    # output$EspLimBN <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$", 
    #                                                   input$dBN,
    #                                                   EspLimBN()
    # ))})
    # 
    # output$ExcesMoyBN <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$", 
    #                                                     input$dBN,
    #                                                     ExcesMoyBN()
    # ))})
    
    # output$FxBN <- renderPlotly({ggplot(data.frame(x = 0:rBN(), y = dBNom(0:rBN(), rBN(), qBN())), aes(x = x, y = y)) + geom_bar(stat = "identity", col = "red", fill ="red", alpha = 0.7, width = 0.3) + theme_classic() + ylab("P(X=x")
        
    # })
    
#### Loi Uniforme Discrète Serveur ####
    
    aUNID <- reactive({input$aUNID})
    bUNID <- reactive({input$bUNID})
    xUNID <- reactive({input$xUNID})
    
    meanUNID <- reactive({E_unifD(a = aUNID(), b = bUNID())
    })
    
    varUNID <- reactive({
        format(V_unifD(a = aUNID(), b = bUNID()), 
               nsmall = 6)
    })
    
    densityUNID <- reactive({format(d_unifD(x = xUNID(), a = aUNID(), b = bUNID()),
                                    nsmall = 6)
    })
    
    repartUNID <- reactive({format(p_unifD(q = xUNID(), a = aUNID(), b = bUNID()), 
                                   nsmall = 6)
    })
    
    survieUNID <- reactive({format(1 - p_unifD(q = xUNID(), a = aUNID(), b = bUNID()), 
                                   nsmall = 6)
    })
    
    # VaRUNID <- reactive({format(qUNIDs(input$kUNID,
    #                                  lambda = input$lamUNID),
    #                            nsmall = 6)
    # })
    # 
    # TVaRUNID <- reactive({format(TVaR_UNIDs(kappa = input$kUNID,
    #                                       lam = input$lamUNID),
    #                             nsmall = 6)
    #     })
    
    # EspTronqUNID <- reactive({0})
    
    # StopLossUNID <- reactive({0})
    
    # EspLimUNID <- reactive({0})
    
    # ExcesMoyUNID <- reactive({0})
    
    output$meanUNID <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                    meanUNID()
    ))})
    
    output$varUNID <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                   varUNID()
    ))})
    
    output$densityUNID <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                       input$xUNID,
                                                       densityUNID()
    ))})
    
    output$repartUNID <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
                                                      input$xUNID,
                                                      repartUNID()
    ))})
    
    output$survieUNID <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                      input$xUNID,
                                                      survieUNID()))
    })

    # output$VaRUNID <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
    #                                                input$kUNID,
    #                                                VaRUNID()
    # ))})
    # output$TVaRUNID <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
    #                                                 input$kUNID,
    #                                                 TVaRUNID()
    # ))})
    # output$EspTronqUNID <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$", 
    #                                                     input$dUNID,
    #                                                     EspTronqUNID()
    # ))})
    # 
    # output$StopLossUNID <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$", 
    #                                                     input$dUNID,
    #                                                     StopLossUNID()
    # ))})
    # 
    # output$EspLimUNID <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$", 
    #                                                   input$dUNID,
    #                                                   EspLimUNID()
    # ))})
    # 
    # output$ExcesMoyUNID <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$", 
    #                                                     input$dUNID,
    #                                                     ExcesMoyUNID()
    # ))})
    
#### Loi Binomiale Négative Composée Serveur ####
    
    
    xBNCOMP <- reactive({input$xBNCOMP})
    rBNCOMP <- reactive({input$rBNCOMP})
    qBNCOMP <- reactive({input$qBNCOMP})
    koBNCOMP <- reactive({input$koBNCOMP})
    kBNCOMP <- reactive({input$kBNCOMP})
    
    rateBNCOMP <- reactive({
        if (input$distrchoiceGAMMA == T) {
            input$rateBNCOMP
        } else {
            1 / input$rateBNCOMP
        }
    })
    
    shapeBNCOMP <- reactive({input$shapeBNCOMP})
    
    output$rateBNCOMPUI <- renderUI({
        if (input$distrchoiceEXPOFAM == "Gamma") {
            numericInput('rateBNCOMP', '$$\\beta$$', value = 1, min = 0)
        } else {
            numericInput('rateBNCOMP', '$$\\sigma^2$$', value = 1, min = 0)
        }
    })
    
    output$shapeBNCOMPUI <- renderUI({
        if (input$distrchoiceEXPOFAM == "Gamma") {
            numericInput('shapeBNCOMP', label = '$$\\alpha$$', value = 2, min = 0)
        } else {
            numericInput('shapeBNCOMP', '$$\\mu$$', value = 2, min = 0)
        }
        
    })
    
    ## Ici on crée un gros observeEvent qui va modifier les paramètres de la BNCOMP/exponentielle/khi-carrée selon les 2 radio buttons:
    ## x: selection de distribution
    ## y: selection de si c'est fréquence ou échelle
    observeEvent({
        input$distrchoiceGAMMA
        input$severityBNCOMP #liste des inputs à observer entre {}
    },
    {
        y <- input$distrchoiceGAMMA
        x <- input$severityBNCOMP
        
        updateNumericInput(session,
                           "distrchoiceGAMMA",
                           value =
                               if (x == "Lognormale")
                               {
                                   y = T
                               })
        
        # modification du beta
        updateNumericInput(session,
                           "rateBNCOMP",
                           step =
                               if (y == T) {
                                   .1
                               } else {
                                   1
                               },
                           label = {
                               if (x == "Lognormale")
                               {
                                   '$$\\sigma^2$$'
                               }
                               else{
                                   '$$\\beta$$'
                               }
                           })
        
        # rend le paramètre impossible à modifier pour l'utilisateur
        if (x == "Lognormale")
        {
            hide("distrchoiceGAMMA")
        }
        else
        {
            show("distrchoiceGAMMA")
        }
        
        updateNumericInput(session, "shapeBNCOMP",
                           label = {
                               if (x == "Lognormale")
                               {
                                   '$$\\mu$$'
                               }
                               else{
                                   '$$\\alpha$$'
                               }
                           }
        )
    })
    
    
    # densityBNCOMP <- reactive({format(dBNCOMP(input$xBNCOMP, shapeBNCOMP(), rateBNCOMP()),scientific = F,  nsmall = 6)})
    
    repartBNCOMP <- reactive({format(p_BNComp(x = xBNCOMP(), 
                                              r = rBNCOMP(),
                                              q = qBNCOMP(),
                                              shape = shapeBNCOMP(), 
                                              rate = rateBNCOMP(),
                                              ko = koBNCOMP(),
                                              distr_severity = input$severityBNCOMP), 
                                     nsmall = 6, scientific = F)
    })
    
    survieBNCOMP <- reactive({format(1 - p_BNComp(x = xBNCOMP(), 
                                                  r = rBNCOMP(),
                                                  q = qBNCOMP(),
                                                  shape = shapeBNCOMP(), 
                                                  rate = rateBNCOMP(),
                                                  ko = koBNCOMP(),
                                                  distr_severity = input$severityBNCOMP), nsmall = 6, scientific = F)})

    VaRBNCOMP <- reactive({format(VaR_BNComp(k = kBNCOMP(), ko = koBNCOMP(),shape = shapeBNCOMP(), 
                                       rate  = rateBNCOMP(),
                                       r     = rBNCOMP(),
                                       q     = qBNCOMP()
    ), nsmall = 6)})
    
    varkBNCOMP <- reactive({VaR_BNComp(k = kBNCOMP(), ko = koBNCOMP(),shape = shapeBNCOMP(), 
                                       rate  = rateBNCOMP(),
                                       r     = rBNCOMP(),
                                       q     = qBNCOMP()
                                       )})
    
    TVaRBNCOMP <- reactive({format(TVaR_BNComp(k     = kBNCOMP(),
                                               shape = shapeBNCOMP(), 
                                               rate  = rateBNCOMP(),
                                               r     = rBNCOMP(),
                                               q     = qBNCOMP(),
                                               vark  = varkBNCOMP(),
                                               ko    = koBNCOMP(),
                                               distr_severity = input$severityBNCOMP
                                               ), 
                                   nsmall = 6)
    })
    
    meanBNCOMP <- reactive({format(E_BNComp(shape = shapeBNCOMP(), 
                                     rate  = rateBNCOMP(),
                                     r     = rBNCOMP(),
                                     q     = qBNCOMP(),
                                     distr_severity = input$severityBNCOMP
                                     ),
                                   nsmall = 6,
                                   scientific = F)
    })

    varianceBNCOMP <- reactive({format(V_BNComp(shape = shapeBNCOMP(), 
                                         rate  = rateBNCOMP(),
                                         r     = rBNCOMP(),
                                         q     = qBNCOMP(),
                                         distr_severity = input$severityBNCOMP
                                         ),
                                       nsmall = 6,
                                       scientific = F)
    })
    
    output$loi_BNCOMP <- renderText({
            "Binomiale Négative Composée"
    })
    
    output$loi_BNCOMP_severity <- renderText({
        if(input$distrchoiceEXPOFAM == "Gamma")
            "Gamma"
        else if (input$distrchoiceEXPOFAM == "Lognormale")
            "Lognormale"
    })
    
    
    output$distr_BNCOMP <- renderText({
        if(input$severityBNCOMP == "Gamma")
            "\\(X \\sim\\mathcal{BNComp}(r, q; F_B \\sim \\Gamma (\\alpha, \\beta))\\)"
        else if (input$severityBNCOMP == "Lognormale")
            "\\(X \\sim\\mathcal{BNComp}(r, q; F_B \\sim \\text{LN} (\\mu, \\sigma^2))\\)"
    })
    
    
    output$meanBNCOMP <- renderUI({withMathJax(sprintf("$$E(X) = %s$$",
                                                      meanBNCOMP()
    ))})

    output$varianceBNCOMP <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$",
                                                          varianceBNCOMP()
    ))})

    # output$densityBNCOMP <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
    #                                                      input$xBNCOMP,
    #                                                      densityBNCOMP()
    # ))})
    
    output$repartBNCOMP <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
                                                         xBNCOMP(),
                                                         repartBNCOMP()
    ))})
    
    output$survieBNCOMP <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                        input$xNORM,
                                                        survieBNCOMP()))
    })

    
    output$VaRBNCOMP <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$", 
                                                     kBNCOMP(),
                                                     VaRBNCOMP()
    ))})
    
    output$TVaRBNCOMP <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$", 
                                                      kBNCOMP(),
                                                      TVaRBNCOMP()
    ))})
#### Loi Poisson Composée Serveur ####
    
    
    xPCOMP <- reactive({input$xPCOMP})
    lambdaPCOMP <- reactive({input$lambdaPCOMP})
    koPCOMP <- reactive({input$koPCOMP})
    kPCOMP <- reactive({input$kPCOMP})
    
    ratePCOMP <- reactive({
        if (input$distrchoiceGAMMA == T) {
            input$ratePCOMP
        } else {
            1 / input$ratePCOMP
        }
    })
    
    shapePCOMP <- reactive({input$shapePCOMP})
    
    output$ratePCOMPUI <- renderUI({
        if (input$severityPCOMP == "Gamma") {
            numericInput('ratePCOMP', '$$\\beta$$', value = 1, min = 0)
        } else {
            numericInput('ratePCOMP', '$$\\sigma^2$$', value = 1, min = 0)
        }
    })
    
    output$shapePCOMPUI <- renderUI({
        if (input$severityPCOMP == "Gamma") {
            numericInput('shapePCOMP', label = '$$\\alpha$$', value = 2, min = 0)
        } else {
            numericInput('shapePCOMP', '$$\\mu$$', value = 2, min = 0)
        }
        
    })
    
    ## Ici on crée un gros observeEvent qui va modifier les paramètres de la PCOMP/exponentielle/khi-carrée selon les 2 radio buttons:
    ## x: selection de distribution
    ## y: selection de si c'est fréquence ou échelle
    observeEvent({
        input$distrchoiceGAMMA
        input$severityPCOMP #liste des inputs à observer entre {}
    },
    {
        y <- input$distrchoiceGAMMA
        x <- input$severityPCOMP
        
        updateNumericInput(session,
                           "distrchoiceGAMMA",
                           value =
                               if (x == "Lognormale")
                               {
                                   y = T
                               })
        
        # modification du beta
        updateNumericInput(session,
                           "ratePCOMP",
                           step =
                               if (y == T) {
                                   .1
                               } else {
                                   1
                               },
                           label = {
                               if (x == "Lognormale")
                               {
                                   '$$\\sigma^2$$'
                               }
                               else{
                                   '$$\\beta$$'
                               }
                           })
        
        # rend le paramètre impossible à modifier pour l'utilisateur
        if (x == "Lognormale")
        {
            hide("distrchoiceGAMMA")
        }
        else
        {
            show("distrchoiceGAMMA")
        }
        
        updateNumericInput(session, "shapePCOMP",
                           label = {
                               if (x == "Lognormale")
                               {
                                   '$$\\mu$$'
                               }
                               else{
                                   '$$\\alpha$$'
                               }
                           }
        )
    })
    
    
    # densityPCOMP <- reactive({format(dPCOMP(input$xPCOMP, shapePCOMP(), ratePCOMP()),scientific = F,  nsmall = 6)})
    
    repartPCOMP <- reactive({format(p_Pcomp(x = xPCOMP(), 
                                            lambda = lambdaPCOMP(),
                                            shape = shapePCOMP(), 
                                            rate = ratePCOMP(),
                                            ko = koPCOMP(),
                                            distr_severity = input$severityPCOMP), 
                                    nsmall = 6, scientific = F)
    })
    
    surviePCOMP <- reactive({format(1 - p_Pcomp(x = xPCOMP(), 
                                            lambda = lambdaPCOMP(),
                                            shape = shapePCOMP(), 
                                            rate = ratePCOMP(),
                                            ko = koPCOMP(),
                                            distr_severity = input$severityPCOMP), 
                                    nsmall = 6, scientific = F)
        })
    
    VaRPCOMP <- reactive({format(VaR_PComp(k = kPCOMP(), 
                                           ko = koPCOMP(), 
                                           shape = shapePCOMP(), 
                                           rate  = ratePCOMP(),
                                           lambda  = lambdaPCOMP()
    ), nsmall = 6)})
    
    varkPCOMP <- reactive({VaR_PComp(k = kPCOMP(), 
                                     ko = koPCOMP(),
                                     shape = shapePCOMP(), 
                                     rate  = ratePCOMP(),
                                     lambda     = lambdaPCOMP()
    )})
    
    TVaRPCOMP <- reactive({format(TVaR_PComp(k = kPCOMP(),
                                             shape = shapePCOMP(), 
                                             rate  = ratePCOMP(),
                                             lamb = lambdaPCOMP(),
                                               vark  = varkPCOMP(),
                                               ko    = koPCOMP(),
                                             distr_severity = input$severityPCOMP), nsmall = 6)
    })
    
    meanPCOMP <- reactive({format(E_PCOMP(lambda = lambdaPCOMP(), 
                                          shape = shapePCOMP(), 
                                          rate = ratePCOMP(),
                                          distr_severity = input$severityPCOMP
    ), 
                                  nsmall = 6, 
                                  scientific = F)
    })
    
    variancePCOMP <- reactive({format(V_PCOMP(lambda = lambdaPCOMP(), shape = shapePCOMP(), rate = ratePCOMP()),
                                      nsmall = 6,
                                      scientific = F)
    })
    
    output$loi_PCOMP <- renderText({
        "Poisson Composée"
    })
    
    output$distr_PComp <- renderText({
        if(input$severityPCOMP == "Gamma")
            "Gamma"
        else if (input$severityPCOMP == "Lognormale")
            "Lognormale"
    })
    
    
    output$distr_PCOMP <- renderText({
        if(input$severityPCOMP == "Gamma")
            "\\(X \\sim\\mathcal{PComp}(\\lambda ; F_B \\sim \\Gamma (\\alpha, \\beta))\\)"
        else if (input$severityPCOMP == "Lognormale")
            "\\(X \\sim\\mathcal{PComp}(\\lambda ; F_B \\sim \\text{LN} (\\mu, \\sigma^2))\\)"
    })
    
    
    output$meanPCOMP <- renderUI({withMathJax(sprintf("$$E(X) = %s$$",
                                                      meanPCOMP()
    ))})

    output$variancePCOMP <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$",
                                                          variancePCOMP()
    ))})
    
    # output$densityPCOMP <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
    #                                                      input$xPCOMP,
    #                                                      densityPCOMP()
    # ))})
    
    output$repartPCOMP <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
                                                         xPCOMP(),
                                                         repartPCOMP()
    ))})
    
    output$surviePCOMP <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                        input$xNORM,
                                                        surviePCOMP())
       )
      })

    
    output$VaRPCOMP <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$", 
                                                      kPCOMP(),
                                                      VaRPCOMP()
    ))})
    output$TVaRPCOMP <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$", 
                                                       kPCOMP(),
                                                       TVaRPCOMP()
    ))})
#### Loi Binomiale Composée Serveur ####
    
    
    xBINCOMP <- reactive({input$xBINCOMP})
    nBINCOMP <- reactive({input$nBINCOMP})
    qBINCOMP <- reactive({input$qBINCOMP})
    koBINCOMP <- reactive({input$koBINCOMP})
    kBINCOMP <- reactive({input$kBINCOMP})
    
    rateBINCOMP <- reactive({
        if (input$distrchoiceGAMMA == T) {
            input$rateBINCOMP
        } else {
            1 / input$rateBINCOMP
        }
    })
    
    shapeBINCOMP <- reactive({input$shapeBINCOMP})
    
    output$rateBINCOMPUI <- renderUI({
        if (input$distrchoiceEXPOFAM == "Gamma") {
            numericInput('rateBINCOMP', '$$\\beta$$', value = 1, min = 0)
        } else {
            numericInput('rateBINCOMP', '$$\\sigma^2$$', value = 1, min = 0)
        }
    })
    
    output$shapeBINCOMPUI <- renderUI({
        if (input$distrchoiceEXPOFAM == "Gamma") {
            numericInput('shapeBINCOMP', label = '$$\\alpha$$', value = 2, min = 0)
        } else {
            numericInput('shapeBINCOMP', '$$\\mu$$', value = 2, min = 0)
        }
        
    })
    
    ## Ici on crée un gros observeEvent qui va modifier les paramètres de la BINCOMP/exponentielle/khi-carrée selon les 2 radio buttons:
    ## x: selection de distribution
    ## y: selection de si c'est fréquence ou échelle
    observeEvent({
        input$distrchoiceGAMMA
        input$severityBINCOMP #liste des inputs à observer entre {}
    },
    {
        y <- input$distrchoiceGAMMA
        x <- input$severityBINCOMP
        
        updateNumericInput(session,
                           "distrchoiceGAMMA",
                           value =
                               if (x == "Lognormale")
                               {
                                   y = T
                               })
        
        # modification du beta
        updateNumericInput(session,
                           "rateBINCOMP",
                           step =
                               if (y == T) {
                                   .1
                               } else {
                                   1
                               },
                           label = {
                               if (x == "Lognormale")
                               {
                                   '$$\\sigma^2$$'
                               }
                               else{
                                   '$$\\beta$$'
                               }
                           })
        
        # rend le paramètre impossible à modifier pour l'utilisateur
        if (x == "Lognormale")
        {
            hide("distrchoiceGAMMA")
        }
        else
        {
            show("distrchoiceGAMMA")
        }
        
        updateNumericInput(session, "shapeBINCOMP",
                           label = {
                               if (x == "Lognormale")
                               {
                                   '$$\\mu$$'
                               }
                               else{
                                   '$$\\alpha$$'
                               }
                           }
        )
    })
    
    
    # densityBINCOMP <- reactive({format(dBINCOMP(input$xBINCOMP, shapeBINCOMP(), rateBINCOMP()),scientific = F,  nsmall = 6)})
    
    repartBINCOMP <- reactive({format(p_BINComp(x = xBINCOMP(), 
                                              n = nBINCOMP(),
                                              q = qBINCOMP(),
                                              shape = shapeBINCOMP(), 
                                              rate = rateBINCOMP(),
                                              ko = koBINCOMP(),
                                              distr_severity = input$severityBINCOMP), 
                                     nsmall = 6, scientific = F)
    })
    
    survieBINCOMP <- reactive({format(1 - p_BINComp(x = xBINCOMP(), 
                                                  n = nBINCOMP(),
                                                  q = qBINCOMP(),
                                                  shape = shapeBINCOMP(), 
                                                  rate = rateBINCOMP(),
                                                  ko = koBINCOMP(),
                                                  distr_severity = input$severityBINCOMP), nsmall = 6, scientific = F)})
    
    VanBINCOMP <- reactive({format(VaR_BINComp(k = kBINCOMP(), ko = koBINCOMP(),shape = shapeBINCOMP(), 
                                             rate  = rateBINCOMP(),
                                             n     = nBINCOMP(),
                                             q     = qBINCOMP()
    ), nsmall = 6)})
    
    varkBINCOMP <- reactive({VaR_BINComp(k = kBINCOMP(), ko = koBINCOMP(),shape = shapeBINCOMP(), 
                                       rate  = rateBINCOMP(),
                                       n     = nBINCOMP(),
                                       q     = qBINCOMP()
    )})
    
    TVanBINCOMP <- reactive({format(TVaR_BINComp(k    = kBINCOMP(),
                                               shape = shapeBINCOMP(), 
                                               rate  = rateBINCOMP(),
                                               n     = nBINCOMP(),
                                               q     = qBINCOMP(),
                                               vark  = varkBINCOMP(),
                                               ko    = koBINCOMP(),
                                               distr_severity = input$severityBINCOMP
    ), 
    nsmall = 6)
    })
    
    meanBINCOMP <- reactive({format(E_BINComp(shape = shapeBINCOMP(), 
                                            rate  = rateBINCOMP(),
                                            n     = nBINCOMP(),
                                            q     = qBINCOMP(),
                                            distr_severity = input$severityBINCOMP
    ),
    nsmall = 6,
    scientific = F)
    })
    
    varianceBINCOMP <- reactive({format(V_BINComp(shape = shapeBINCOMP(), 
                                                rate  = rateBINCOMP(),
                                                n     = nBINCOMP(),
                                                q     = qBINCOMP(),
                                                distr_severity = input$severityBINCOMP
    ),
    nsmall = 6,
    scientific = F)
    })
    
    output$loi_BINCOMP <- renderText({
        "Binomiale Composée"
    })
    
    output$loi_BINCOMP_severity <- renderText({
        if(input$distrchoiceEXPOFAM == "Gamma")
            "Gamma"
        else if (input$distrchoiceEXPOFAM == "Lognormale")
            "Lognormale"
    })
    
    
    output$distr_BINCOMP <- renderText({
        if(input$severityBINCOMP == "Gamma")
            "\\(X \\sim\\text{BINComp}(r, q; F_B \\sim \\Gamma (\\alpha, \\beta))\\)"
        else if (input$severityBINCOMP == "Lognormale")
            "\\(X \\sim\\text{BINComp}(r, q; F_B \\sim \\text{LN} (\\mu, \\sigma^2))\\)"
    })
    
    
    output$meanBINCOMP <- renderUI({withMathJax(sprintf("$$E(X) = %s$$",
                                                       meanBINCOMP()
    ))})
    
    output$varianceBINCOMP <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$",
                                                           varianceBINCOMP()
    ))})
    
    # output$densityBINCOMP <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
    #                                                      input$xBINCOMP,
    #                                                      densityBINCOMP()
    # ))})
    
    output$repartBINCOMP <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
                                                         xBINCOMP(),
                                                         repartBINCOMP()
    ))})
    
    output$survieBINCOMP <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                         input$xNORM,
                                                         survieBINCOMP()))
    })
    
    
    output$VanBINCOMP <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$", 
                                                      kBINCOMP(),
                                                      VanBINCOMP()
    ))})
    
    output$TVanBINCOMP <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$", 
                                                       kBINCOMP(),
                                                       TVanBINCOMP()
    ))})
    
}
