myserver <- function(input, output, session) 
{
#### Serveur outil de tests statistiques ####
    
    valueESTIM_STATTOOL <- reactive({input$valueESTIM_STATTOOL})
    
#### Serveur outil de la fonction d'excès-moyen ####
    
    shapeEXCESS_MEAN <- reactive({input$shapeEXCESS_MEAN})
    rateEXCESS_MEAN <- reactive({input$rateEXCESS_MEAN})
    
    
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
    
    output$weibullEXCESS_MEAN <- renderUI({withMathJax(sprintf("$$\\text{Wei}(\\tau = %s, \\beta = %s)$$", 
                                                             shapeEXCESS_MEAN(),
                                                             rateEXCESS_MEAN()
    ))
    })
    
    output$plotEXCESS_MEAN <- renderPlot({  
        
        # curve(Mexcess_gamma(d = x, shapeEXCESS_MEAN(), rateEXCESS_MEAN()))
        # curve(Mexcess_pareto(d = x, shapeEXCESS_MEAN(), rateEXCESS_MEAN()), add = T)
        x <- seq(0, 10, 1)
        y1 <- Mexcess_gamma(d = x, shapeEXCESS_MEAN(), rateEXCESS_MEAN())
        y2 <- Mexcess_pareto(d = x, shapeEXCESS_MEAN(), rateEXCESS_MEAN())
        y3 <- Mexcess_norm(d = x, shapeEXCESS_MEAN(), rateEXCESS_MEAN())
        # y4 <- Mexcess_lnorm(d = x, shapeEXCESS_MEAN(), rateEXCESS_MEAN())
        y5 <- Mexcess_weibull(d = x, shapeEXCESS_MEAN(), rateEXCESS_MEAN())
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
    
    output$descriptionEXCESS_MEAN <- renderText({"Ébauche d'outil pour observer la fonction d'excès-moyen pour plusieurs distributions. À travailler après que je comprends mieux les distributions Lognormale et Weibull."})
    
    
#### Loi Normale Serveur ####
        
    muNORM <- reactive({input$muNORM})
        
    sigma2NORM <- reactive({input$sigmaNORM})
        
    densityNORM <- reactive({format(dnorm(input$xNORM, muNORM(), sqrt(sigma2NORM())), nsmall = 6)})
        
    repartNORM <- reactive({format(pnorm(input$xNORM, muNORM(), sqrt(sigma2NORM())), nsmall = 6)})
        
    survieNORM <- reactive({format(pnorm(input$xNORM, muNORM(), sqrt(sigma2NORM()), lower.tail = F),
                                   nsmall = 6)
    })
        
    VaRNORM <- reactive({format(VaR_norm(input$kNORM, muNORM(), sqrt(sigma2NORM())), nsmall = 6)})
        
    TVaRNORM <- reactive({format(TVaR_norm(input$kNORM, muNORM(), sqrt(sigma2NORM())), nsmall = 6)})
        
    EspTronqNORM <- reactive({Etronq_norm(d = input$dNORM, muNORM(), sqrt(sigma2NORM()))})
        
    StopLossNORM <- reactive({SL_norm(d = input$dNORM, muNORM(), sqrt(sigma2NORM()))})
        
    EspLimNORM <- reactive({Elim_norm(d = input$dNORM, muNORM(), sqrt(sigma2NORM()))})
    
    ExcesMoyNORM <- reactive({Mexcess_norm(d = input$dNORM, muNORM(), sqrt(sigma2NORM()))})
    
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
                                  sd = sqrt(sigma2NORM()))
                      ) + 
            theme_classic()  
            # +geom_vline(aes(VaRNORM()))
            # geom_vline(xintercept = as.numeric(levels(VaRNORM()))[VaRNORM()])
    })
    
    
#### Loi Gamma Serveur ####
    
    kGAMMA <- reactive({input$kGAMMA})
    
    dGAMMA <- reactive({input$dGAMMA})
    
    xGAMMA <- reactive({input$xGAMMA})
    
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
        
        
        densityGAMMA <- reactive({format(dgamma(xGAMMA(), alphaGAMMA(), betaGAMMA()),scientific = F,  nsmall = 6)})
        
        repartGAMMA <- reactive({format(pgamma(xGAMMA(), alphaGAMMA(), betaGAMMA()), nsmall = 6, scientific = F)})
        
        survieGAMMA <- reactive({format(pgamma(xGAMMA(), alphaGAMMA(), betaGAMMA(), lower.tail = F), nsmall = 6, scientific = F)})
        
        VaRGAMMA <- reactive({format(VaR_gamma(kGAMMA(), alphaGAMMA(), betaGAMMA()), nsmall = 6)})
        
        TVaRGAMMA <- reactive({format(TVaR_gamma(kGAMMA(), alphaGAMMA(), betaGAMMA()), nsmall = 6)})
        
        EspTronqGAMMA <- reactive({Etronq_gamma(d = dGAMMA(), alphaGAMMA(), betaGAMMA())})
        
        StopLossGAMMA <- reactive({SL_gamma(d = dGAMMA(), alphaGAMMA(), betaGAMMA())})
        
        EspLimGAMMA <- reactive({Elim_gamma(d = dGAMMA(), alphaGAMMA(), betaGAMMA())})
        
        ExcesMoyGAMMA <- reactive({Mexcess_gamma(d = dGAMMA(), alphaGAMMA(), betaGAMMA())})
        
        meanGAMMA <- reactive({E_gamma(alphaGAMMA(), betaGAMMA())})
        
        varianceGAMMA <- reactive({V_gamma(alphaGAMMA(), betaGAMMA())})
        
        output$loi_gamma <- renderUI({
            if(input$distrchoiceEXPOFAM == "Gamma")
                titlePanel(tags$a("Loi Gamma", href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Gamma"))
            else if (input$distrchoiceEXPOFAM == "Exponentielle")
                titlePanel(tags$a("Loi Exponentielle", href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Exponentielle"))
            else
                titlePanel(tags$a("Loi du Khi-carré", href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-du-Khi-Carr%C3%A9"))
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
                                                             xGAMMA(),
                                                             densityGAMMA()
        ))})
        output$repartGAMMA <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
                                                            xGAMMA(),
                                                            repartGAMMA()
        ))})
        
        output$survieGAMMA <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                           xGAMMA(),
                                                           survieGAMMA()))
        })
        
        
        output$VaRGAMMA <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$", 
                                                         kGAMMA(),
                                                         VaRGAMMA()
        ))})
        output$TVaRGAMMA <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$", 
                                                          kGAMMA(),
                                                          TVaRGAMMA()
        ))})
        output$EspTronqGAMMA <- renderUI({sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$", 
                                                              dGAMMA(),
                                                              EspTronqGAMMA()
        )})
        
        output$StopLossGAMMA <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$", 
                                                              dGAMMA(),
                                                              StopLossGAMMA()
        ))})
        
        output$EspLimGAMMA <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$", 
                                                            dGAMMA(),
                                                            EspLimGAMMA()
        ))})
        
        output$ExcesMoyGAMMA <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$", 
                                                              dGAMMA(),
                                                              ExcesMoyGAMMA()
        ))})
        
        output$FxGAMMA <- renderPlotly({
            ggplot(data = data.frame(x = c(0, meanGAMMA() + 3 * sqrt(varianceGAMMA()))),
            aes(x)) + 
                stat_function(fun = dgamma,
                              args = list(alphaGAMMA(), 
                                          betaGAMMA())) + 
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = dgamma,
                    args = list(alphaGAMMA(), betaGAMMA()),
                    xlim = c(0, xGAMMA()),
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
                              args = list(alphaGAMMA(), 
                                          betaGAMMA())) + 
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = dgamma,
                    args = list(alphaGAMMA(), betaGAMMA()),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
#### Loi Pareto Serveur ####
        
        alphaPARETO <- reactive({input$alphaPARETO})
        
        lambdaPARETO <- reactive({input$lambdaPARETO})
        
        densityPARETO <- reactive({format(dpareto(x = input$xPARETO, 
                                                 alphaPARETO(), 
                                                 lambdaPARETO()), 
                                         nsmall = 6)})
        
        repartPARETO <- reactive({format(ppareto(q = input$xPARETO, 
                                                 alphaPARETO(), 
                                                 lambdaPARETO()), 
                                         nsmall = 6)})
        
        surviePARETO <- reactive({format(ppareto(q = input$xPARETO, 
                                                              alphaPARETO(), 
                                                              lambdaPARETO(),
                                                 lower.tail = F), 
                                         nsmall = 6)})
        
        VaRPARETO <- reactive({format(VaR_pareto(kappa = input$kPARETO,
                                                 alphaPARETO(), 
                                                 lambdaPARETO()), 
                                      nsmall = 6)
            })
        
        TVaRPARETO <- reactive({format(TVaR_pareto(kappa = input$kPARETO, 
                                                   alphaPARETO(), 
                                                   lambdaPARETO()), 
                                       nsmall = 6)
            })
        
        EspTronqPARETO <- reactive({Etronq_pareto(d = input$dPARETO, 
                                                  alphaPARETO(),
                                                  lambdaPARETO())
            })
        
        StopLossPARETO <- reactive({SL_pareto(d = input$dPARETO, 
                                              alphaPARETO(), 
                                              lambdaPARETO())
            })
        
        EspLimPARETO <- reactive({Elim_pareto(d = input$dPARETO, 
                                              alphaPARETO(), 
                                              lambdaPARETO())
            })
        
        kthmomentPARETO <- reactive({
            if(alphaPARETO() > input$dPARETO)
                kthmoment_pareto1(k = input$dPARETO, 
                                  alphaPARETO(), 
                                  lambdaPARETO())
            else
                kthmoment_pareto2(k = input$dPARETO, 
                                  alphaPARETO(), 
                                  lambdaPARETO())
        })
        
        
        
        meanPARETO <- reactive({E_pareto(alphaPARETO(), 
                                         lambdaPARETO())
            })
        
        variancePARETO <- reactive({V_pareto(alphaPARETO(), 
                                             lambdaPARETO())
            })
        
        ExcesMoyPARETO <- reactive({Mexcess_pareto(d = input$dPARETO, 
                                                   alphaPARETO(),
                                                   lambdaPARETO())
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
        
        
        output$FxPARETO <- renderPlotly({
            ggplot(data = data.frame(x = c(0, 3 * lambdaPARETO())),
            aes(x)) +
                stat_function(fun = dpareto,
                              args = list(alphaPARETO(), 
                                          lambdaPARETO())) +
                ylab("f(x)") +
                theme_classic() +
                stat_function(
                    fun = dpareto,
                    args = list(alphaPARETO(), 
                                lambdaPARETO()),
                xlim = c(0, input$xPARETO),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$SxPARETO <- renderPlotly({
            ggplot(data = data.frame(x = c(
                0, 3 * lambdaPARETO())
            ),
            aes(x)) +
                stat_function(fun = dpareto,
                              args = list(alphaPARETO(), 
                                          lambdaPARETO())) +
                ylab("f(x)") +
                theme_classic() +
                stat_function(
                    fun = dpareto,
                    args = list(alphaPARETO(), 
                                lambdaPARETO()),
                    xlim = c(input$xPARETO, 3 * lambdaPARETO()),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        
        output$QxPARETO <- renderPlotly({
            ggplot(data = data.frame(x = c(0,1)),
                   aes(x)) +
                stat_function(fun = qpareto,
                              args = list(alphaPARETO(), 
                                          lambdaPARETO())) + theme_classic()
        })
        
        
        
    
#### Loi Burr Serveur ####
        
        alphaBURR <- reactive({input$alphaBURR})
        
        lambdaBURR <- reactive({input$lambdaBURR})
        
        tauBURR <- reactive({input$tauBURR})
        
        dBURR <- reactive({input$dBURR})
        
        kBURR <- reactive({input$kBURR})
        
        xBURR <- reactive({input$xBURR})
        
        densityBURR <- reactive({format(dburr(x = xBURR(), 
                                              alphaBURR(), 
                                              tauBURR(),
                                              lambdaBURR()), 
                                          nsmall = 6)})
        
        repartBURR <- reactive({format(pburr(q = xBURR(), 
                                             alphaBURR(), 
                                             tauBURR(),
                                             lambdaBURR()), 
                                         nsmall = 6)})
        
        survieBURR <- reactive({format(pburr(q = xBURR(), 
                                             alphaBURR(), 
                                             tauBURR(),
                                             lambdaBURR(),
                                             lower.tail = F), 
                                         nsmall = 6)})
        
        VaRBURR <- reactive({format(VaR_burr(kappa = kBURR(),
                                             alphaBURR(),
                                             lambdaBURR(),
                                             tauBURR()), 
                                      nsmall = 6)
        })
        
        VaRBURR_a <- reactive({VaR_burr(kappa = kBURR(),
                                        alphaBURR(),
                                        lambdaBURR(),
                                        tauBURR()) 
        })
        
        
        TVaRBURR <- reactive({format(TVaR_burr(kappa = kBURR(),
                                               VaRBURR_a(),
                                               alphaBURR(),
                                               lambdaBURR(),
                                               tauBURR()), 
                                       nsmall = 6)
        })
        
        EspTronqBURR <- reactive({Etronq_burr(d = dBURR(),
                                              alphaBURR(),
                                              lambdaBURR(),
                                              tauBURR())
        })
        
        StopLossBURR <- reactive({SL_burr(d = dBURR(),
                                          alphaBURR(),
                                          lambdaBURR(),
                                          tauBURR())
        })
        
        EspLimBURR <- reactive({Elim_burr(d = dBURR(),
                                          alphaBURR(),
                                          lambdaBURR(),
                                          tauBURR())
        })
        
        kthmomentBURR <- reactive({kthmoment_burr(k = kBURR(),
                                                  alphaBURR(),
                                                  lambdaBURR(),
                                                  tauBURR())
        })
        
        ExcesMoyBURR <- reactive({Mexcess_burr(d = dBURR(),
                                               lambdaBURR(), 
                                               alphaBURR(),
                                               tauBURR())
        })
        
        meanBURR <- reactive({E_burr(lambdaBURR(),
                                     alphaBURR(),
                                     tauBURR())
        })
        
        varianceBURR <- reactive({V_burr(lambdaBURR(),
                                         alphaBURR(),
                                         tauBURR())
        })
        
        output$meanBURR <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                           meanBURR()))
        })
        
        output$varBURR <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                          varianceBURR()))
        })
        
        output$densityBURR <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                              xBURR(),
                                                              densityBURR()))
        })
        output$repartBURR <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
                                                             xBURR(),
                                                             repartBURR()))
        })
        
        output$survieBURR <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                             xBURR(),
                                                             survieBURR()))
        })
        
        output$VaRBURR <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                          kBURR(),
                                                          VaRBURR()))
        })
        
        output$TVaRBURR <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
                                                           kBURR(),
                                                           TVaRBURR()))
        })
        
        output$EspTronqBURR <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$",
                                                               dBURR(),
                                                               EspTronqBURR()))
        })
        
        output$StopLossBURR <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$",
                                                               dBURR(),
                                                               StopLossBURR()))
        })
        
        output$EspLimBURR <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$",
                                                             dBURR(),
                                                             EspLimBURR()))
        })
        
        output$ExcesMoyBURR <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$",
                                                               dBURR(),
                                                               ExcesMoyBURR()))
        })
        
        output$kthmomentBURR <- renderUI({withMathJax(sprintf("$$E[X^{%s}] = %.4f$$",
                                                             dBURR(),
                                                             ExcesMoyBURR()))
        })
        
        
        output$FxBURR <- renderPlotly({
            ggplot(data = data.frame(x = c(
                0, 5 * tauBURR())
            ),
            aes(x)) +
                stat_function(fun = dburr,
                              args = list(alphaBURR(), 
                                          tauBURR(),
                                          lambdaBURR())) +
                ylab("f(x)") +
                theme_classic() +
                stat_function(
                    fun = dburr,
                    args = list(alphaBURR(), 
                                tauBURR(),
                                lambdaBURR()),
                    xlim = c(0, xBURR()),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$SxBURR <- renderPlotly({
            ggplot(data = data.frame(x = c(
                0, 5 * tauBURR())
            ),
            aes(x)) +
                stat_function(fun = dburr,
                              args = list(alphaBURR(), 
                                          tauBURR(),
                                          lambdaBURR())) +
                ylab("f(x)") +
                theme_classic() +
                stat_function(
                    fun = dburr,
                    args = list(alphaBURR(), 
                                tauBURR(),
                                lambdaBURR()),
                    xlim = c(xBURR(), 5 * tauBURR()),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$QxBURR <- renderPlotly({
            ggplot(data = data.frame(x = c(0,1)),
                   aes(x)) +
                stat_function(fun = qburr,
                              args = list(alphaBURR(), 
                                          tauBURR(),
                                          lambdaBURR())) + theme_classic()
        })
        
        
#### Loi Weibull Serveur ####
        
        betaWEIBULL <- reactive({input$betaWEIBULL})
        
        tauWEIBULL <- reactive({input$tauWEIBULL})
        
        dWEIBULL <- reactive({input$dWEIBULL})
        
        kWEIBULL <- reactive({input$kWEIBULL})
        
        xWEIBULL <- reactive({input$xWEIBULL})
        
        densityWEIBULL <- reactive({format(dweibull(x = xWEIBULL(), 
                                                    tauWEIBULL(),
                                                    betaWEIBULL()), 
                                           nsmall = 6)})
        
        repartWEIBULL <- reactive({format(pweibull(q = xWEIBULL(), 
                                                   tauWEIBULL(),
                                                   betaWEIBULL()), 
                                          nsmall = 6)})
        
        survieWEIBULL <- reactive({format(pweibull(q = xWEIBULL(), 
                                                   tauWEIBULL(),
                                                   betaWEIBULL(),
                                                   lower.tail = F), 
                                          nsmall = 6)})
        
        VaRWEIBULL <- reactive({format(VaR_weibull(kappa = kWEIBULL(),
                                                   tauWEIBULL(), 
                                                   betaWEIBULL()),
                                       nsmall = 6)
        })
        
        TVaRWEIBULL <- reactive({format(TVaR_weibull(kappa = kWEIBULL(),
                                                     tauWEIBULL(), 
                                                     betaWEIBULL()), 
                                        nsmall = 6)
        })
        
        EspTronqWEIBULL <- reactive({Etronq_weibull(d = dWEIBULL(),
                                                    betaWEIBULL(),
                                                    tauWEIBULL())
        })
        
        StopLossWEIBULL <- reactive({SL_weibull(d = dWEIBULL(),
                                                betaWEIBULL(),
                                                tauWEIBULL())
        })
        
        EspLimWEIBULL <- reactive({Elim_weibull(d = dWEIBULL(),
                                                betaWEIBULL(),
                                                tauWEIBULL())
        })
        
        ExcesMoyWEIBULL <- reactive({Mexcess_weibull(d = dWEIBULL(),
                                                     betaWEIBULL(),
                                                     tauWEIBULL())
        })
        
        meanWEIBULL <- reactive({E_weibull(betaWEIBULL(),
                                           tauWEIBULL())
        })
        
        kthmomentWEIBULL <- reactive({kthmoment_weibull(k = dWEIBULL(),
                                                        betaWEIBULL(),
                                                        tauWEIBULL())
        })
        
        varianceWEIBULL <- reactive({V_weibull(betaWEIBULL(),
                                               tauWEIBULL())
        })
        
        output$meanWEIBULL <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                            format(meanWEIBULL(),
                                                                   nsmall = 6)))
        })
        
        output$kthmomentWEIBULL <- renderUI({withMathJax(sprintf("$$E(X^%s) = %s$$", 
                                                                 dWEIBULL(),
                                                                 format(kthmomentWEIBULL(),
                                                                        nsmall = 6)))
        })
        
        output$varianceWEIBULL <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                                format(varianceWEIBULL(),
                                                                       nsmall = 6)))
        })
        
        output$densityWEIBULL <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                            xWEIBULL(),
                                                            densityWEIBULL()))
        })
        output$repartWEIBULL <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
                                                           xWEIBULL(),
                                                           repartWEIBULL()))
        })
        
        output$survieWEIBULL <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                           xWEIBULL(),
                                                           survieWEIBULL()))
        })
        
        output$VaRWEIBULL <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                        kWEIBULL(),
                                                        VaRWEIBULL()))
        })
        
        output$TVaRWEIBULL <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
                                                         kWEIBULL(),
                                                         TVaRWEIBULL()))
        })
        
        output$EspTronqWEIBULL <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$",
                                                             dWEIBULL(),
                                                             EspTronqWEIBULL()))
        })
        
        output$StopLossWEIBULL <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$",
                                                             dWEIBULL(),
                                                             StopLossWEIBULL()))
        })
        
        output$EspLimWEIBULL <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$",
                                                           dWEIBULL(),
                                                           EspLimWEIBULL()))
        })
        
        output$ExcesMoyWEIBULL <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$",
                                                             dWEIBULL(),
                                                             ExcesMoyWEIBULL()))
        })
        
        output$FxWEIBULL <- renderPlotly({
            ggplot(data = data.frame(x = c(
                0, 2 * betaWEIBULL())
            ),
            aes(x)) +
                stat_function(fun = dweibull,
                              args = list(tauWEIBULL(),
                                          betaWEIBULL())) +
                ylab("f(x)") +
                theme_classic() +
                stat_function(
                    fun = dweibull,
                    args = list(tauWEIBULL(),
                                betaWEIBULL()),
                    xlim = c(0, xWEIBULL()),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$SxWEIBULL <- renderPlotly({
            ggplot(data = data.frame(x = c(
                0, 2 * betaWEIBULL())
            ),
            aes(x)) +
                stat_function(fun = dweibull,
                              args = list(tauWEIBULL(),
                                          betaWEIBULL())) +
                ylab("f(x)") +
                theme_classic() +
                stat_function(
                    fun = dweibull,
                    args = list(tauWEIBULL(),
                                betaWEIBULL()),
                    xlim = c(xWEIBULL(), 2 * betaWEIBULL()),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$QxWEIBULL <- renderPlotly({
            ggplot(data = data.frame(x = c(0,1)),
                   aes(x)) +
                stat_function(fun = qweibull,
                              args = list(tauWEIBULL(),
                                          betaWEIBULL())) + theme_classic()
        })
        
        
        
#### Loi Lognormale Serveur ####
        
        muLNORM <- reactive({input$muLNORM})
        
        sigma2LNORM <- reactive({input$sigmaLNORM})
        
        xLNORM <- reactive({input$xLNORM})
        
        kLNORM <- reactive({input$kLNORM})
        
        dLNORM <- reactive({input$dLNORM})
        
        densityLNORM <- reactive({format(dlnorm(xLNORM(), 
                                                muLNORM(), 
                                                sqrt(sigma2LNORM())), 
                                         nsmall = 6)
            })
        
        repartLNORM <- reactive({format(plnorm(xLNORM(), 
                                               muLNORM(), 
                                               sqrt(sigma2LNORM())), 
                                        nsmall = 6)
            })
        
        survieLNORM <- reactive({format(plnorm(xLNORM(), 
                                               muLNORM(), 
                                               sqrt(sigma2LNORM()), 
                                               lower.tail = F), 
                                        nsmall = 6)
            })
        
        VaRLNORM <- reactive({format(VaR_lnorm(kLNORM(), 
                                               muLNORM(), 
                                               sqrt(sigma2LNORM())), 
                                     nsmall = 6)
            })
        
        TVaRLNORM <- reactive({format(TVaR_lnorm(kLNORM(), 
                                                 muLNORM(), 
                                                 sqrt(sigma2LNORM())), 
                                      nsmall = 6)
            })
        
        EspTronqLNORM <- reactive({Etronq_lnorm(d = dLNORM(), 
                                                muLNORM(), 
                                                sqrt(sigma2LNORM()))
            })
        
        StopLossLNORM <- reactive({SL_lnorm(d = dLNORM(), 
                                            muLNORM(), 
                                            sqrt(sigma2LNORM()))
            })
        
        EspLimLNORM <- reactive({Elim_lnorm(d = dLNORM(), 
                                            muLNORM(), 
                                            sqrt(sigma2LNORM()))
            })
        
        ExcesMoyLNORM <- reactive({Mexcess_lnorm(d = dLNORM(),
                                                 muLNORM(),
                                                 sqrt(sigma2LNORM()))
            })
        
        meanLNORM <- reactive({E_lnorm(muLNORM(),
                                         sqrt(sigma2LNORM()))
            })
        
        kthmomentLNORM <- reactive({kthmoment_lnorm(k = dLNORM(),
                                                    muLNORM(),
                                                    sqrt(sigma2LNORM()))
            })
        
        
        varianceLNORM <- reactive({kthmoment_lnorm(k = 2, muLNORM(), sqrt(sigma2LNORM())) - 
                kthmoment_lnorm(k = 1, muLNORM(), sqrt(sigma2LNORM()))^2
        })
        
        output$meanLNORM <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                         format(meanLNORM(), 
                                                                nsmall = 6)))
        })
        
        output$kthmomentLNORM <- renderUI({withMathJax(sprintf("$$E(X^%s) = %s$$", 
                                                               dLNORM(),
                                                               format(kthmomentLNORM(), 
                                                                 nsmall = 6)))
        })
        
        output$varLNORM <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                         format(varianceLNORM(), 
                                                                nsmall = 6)))
        })
        
        output$densityLNORM <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                            xLNORM(),
                                                            densityLNORM()))
        })
        output$repartLNORM <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
                                                           xLNORM(),
                                                           repartLNORM()))
        })
        
        output$survieLNORM <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                           xLNORM(),
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
                                                             dLNORM(),
                                                             EspTronqLNORM()))
        })
        
        output$StopLossLNORM <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$",
                                                             dLNORM(),
                                                             StopLossLNORM()))
        })
        
        output$EspLimLNORM <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$",
                                                           dLNORM(),
                                                           EspLimLNORM()))
        })
        
        output$ExcesMoyLNORM <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$",
                                                             dLNORM(),
                                                             ExcesMoyLNORM()))
        })
        
        output$FxLNORM <- renderPlotly({
          ggplot(data = data.frame(x = c(
            0,
            muLNORM() + 4 * sqrt(sigma2LNORM())
          )),
          aes(x)) +
            stat_function(fun = dlnorm,
                          args = list(mean = muLNORM(),
                                      sd = sqrt(sigma2LNORM()))) +
            ylab("f(x)") +
            theme_classic() +
            stat_function(
              fun = dlnorm,
              args = list(mean = muLNORM(), sd = sqrt(sigma2LNORM())),
              xlim = c(0, xLNORM()),
              geom = "area",
              fill = "red",
              alpha = 0.7
            )
        })
        
        output$SxLNORM <- renderPlotly({
          ggplot(data = data.frame(x = c(
            0, muLNORM() + 4 * sqrt(sigma2LNORM())
          )),
          aes(x)) +
            stat_function(fun = dlnorm,
                          args = list(mean = muLNORM(),
                                      sd = sqrt(sigma2LNORM()))) +
            ylab("f(x)") +
            theme_classic() +
            stat_function(
              fun = dlnorm,
              args = list(mean = muLNORM(), sd = sqrt(sigma2LNORM())),
              xlim = c(xLNORM(), muLNORM() + 4 * sqrt(sigma2LNORM())),
              geom = "area",
              fill = "red",
              alpha = 0.7
            )
        })
        
        output$QxLNORM <- renderPlotly({
          ggplot(data = data.frame(x = c(0,1)),
                 aes(x)) +
            stat_function(fun = qlnorm,
                          args = list(mean = muLNORM(),
                                      sd = sqrt(sigma2LNORM()))) + theme_classic()
        })
        
        
        
#### Loi Beta Serveur ####
        
        alphaBETA <- reactive({input$alphaBETA})
        
        betaBETA <- reactive({input$betaBETA})
        
        xBETA <- reactive({input$xBETA})
        
        kBETA <- reactive({input$kBETA})
        
        dBETA <- reactive({input$dBETA})
        
        densityBETA <- reactive({format(dbeta(x = xBETA(), 
                                                  alphaBETA(), 
                                                  betaBETA()), 
                                          nsmall = 6)})
        
        repartBETA <- reactive({format(pbeta(q = xBETA(), 
                                                 alphaBETA(), 
                                                 betaBETA()), 
                                         nsmall = 6)})
        
        survieBETA <- reactive({format(pbeta(q = xBETA(), 
                                             alphaBETA(), 
                                             betaBETA(),
                                             lower.tail = F), 
                                         nsmall = 6)})
        
        VaRBETA <- reactive({format(VaR_beta(kappa = kBETA(),
                                             alphaBETA(), 
                                             betaBETA()), 
                                      nsmall = 6)
        })
        
        TVaRBETA <- reactive({format(TVaR_beta(kappa = kBETA(), 
                                               alphaBETA(), 
                                               betaBETA()), 
                                       nsmall = 6)
        })
        
        EspTronqBETA <- reactive({Etronq_beta(d = dBETA(), 
                                              alphaBETA(),
                                              betaBETA())
        })
        
        StopLossBETA <- reactive({SL_beta(d = dBETA(), 
                                          alphaBETA(),
                                          betaBETA())
        })
        
        EspLimBETA <- reactive({Elim_beta(d = dBETA(), 
                                          alphaBETA(),
                                          betaBETA())
        })
        
        meanBETA <- reactive({E_beta(alphaBETA(),
                                     betaBETA())
        })
        
        varianceBETA <- reactive({V_beta(alphaBETA(),
                                         betaBETA())
        })
        
        ExcesMoyBETA <- reactive({Mexcess_beta(d = dBETA(), 
                                               alphaBETA(),
                                               betaBETA())
        })
        
        kthmomentBETA <- reactive({kthmoment_beta(k = dBETA(), 
                                                  alphaBETA(),
                                                  betaBETA())})
        
        output$meanBETA <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                           format(meanBETA(),
                                                                  nsmall = 6)))
        })
        
        output$kthmomentBETA <- renderUI({withMathJax(sprintf("$$E[X^{%s}] = %s$$", 
                                                              dBETA(),
                                                              format(kthmomentBETA(),
                                                                     nsmall = 6)))
        })
        
        output$varianceBETA <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                             format(varianceBETA(),
                                                                    nsmall = 6)))
        })
        
        output$densityBETA <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                              xBETA(),
                                                              densityBETA()))
        })
        output$repartBETA <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
                                                             xBETA(),
                                                             repartBETA()))
        })
        
        output$survieBETA <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                             xBETA(),
                                                             survieBETA()))
        })
        
        output$VaRBETA <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                          kBETA(),
                                                          VaRBETA()))
        })
        
        output$TVaRBETA <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
                                                           kBETA(),
                                                           TVaRBETA()))
        })
        
        output$EspTronqBETA <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$",
                                                               input$d2BETA,
                                                               EspTronqBETA()))
        })
        
        output$StopLossBETA <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$",
                                                               dBETA(),
                                                               StopLossBETA()))
        })
        
        output$EspLimBETA <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$",
                                                             dBETA(),
                                                             EspLimBETA()))
        })
        
        output$ExcesMoyBETA <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$",
                                                               dBETA(),
                                                               ExcesMoyBETA()))
        })
        
        output$FxBETA <- renderPlotly({
            ggplot(data = data.frame(x = c(0,1)),
            aes(x)) +
                stat_function(fun = dbeta,
                              args = list(alphaBETA(), 
                                          betaBETA())) +
                ylab("f(x)") +
                theme_classic() +
                stat_function(
                    fun = dbeta,
                    args = list(alphaBETA(), 
                                betaBETA()),
                    xlim = c(0, xBETA()),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$SxBETA <- renderPlotly({
            ggplot(data = data.frame(x = c(0,1)),
                   aes(x)) +
                stat_function(fun = dbeta,
                              args = list(alphaBETA(), 
                                          betaBETA())) +
                ylab("f(x)") +
                theme_classic() +
                stat_function(
                    fun = dbeta,
                    args = list(alphaBETA(), 
                                betaBETA()),
                    xlim = c(xBETA(),1),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        
        output$QxBETA <- renderPlotly({
            ggplot(data = data.frame(x = c(0,1)),
                   aes(x)) +
                stat_function(fun = qbeta,
                              args = list(alphaBETA(), 
                                          betaBETA())) + theme_classic()
        })
        
        
        
#### Loi Erlang Serveur ####
        
        nERLANG <- reactive({input$nERLANG})
        
        betaERLANG <- reactive({input$betaERLANG})
        
        dERLANG <- reactive({input$dERLANG})
        
        kERLANG <- reactive({input$kERLANG})
        
        xERLANG <- reactive({input$xERLANG})
        
        densityERLANG <- reactive({format(derlang(x = xERLANG(),
                                                  nERLANG(),
                                                  betaERLANG()), 
                                        nsmall = 6)})
        
        repartERLANG <- reactive({format(perlang(x = xERLANG(),
                                                 nERLANG(),
                                                 betaERLANG()), 
                                       nsmall = 6)})
        
        survieERLANG <- reactive({format(perlang(x = xERLANG(),
                                                 nERLANG(),
                                                 betaERLANG(),
                                                 lower.tail = F), 
                                       nsmall = 6)})
        
        # TVaRERLANG <- reactive({format(TVaR_ERLANG(kappa = input$kERLANG,
        #                                        VaRERLANG_a(),
        #                                        nERLANG(),
        #                                        betaERLANG(),
        #                                        tauERLANG()), 
        #                              nsmall = 6)
        # })
        
        EspTronqERLANG <- reactive({Etronq_erlang(d = dERLANG(),
                                                  nERLANG(),
                                                  betaERLANG())
        })
        
        StopLossERLANG <- reactive({SL_erlang(d = dERLANG(),
                                              nERLANG(),
                                              betaERLANG())
        })
        
        EspLimERLANG <- reactive({Elim_erlang(d = dERLANG(),
                                              nERLANG(),
                                              betaERLANG())
        })
        
        kthmomentERLANG <- reactive({kthmoment_erlang(k = dERLANG(),
                                                      nERLANG(),
                                                      betaERLANG())
        })
        
        ExcesMoyERLANG <- reactive({Mexcess_erlang(d = dERLANG(),
                                                   nERLANG(),
                                                   betaERLANG())
        })
        
        meanERLANG <- reactive({E_erlang(nERLANG(),
                                         betaERLANG())
        })
        
        varianceERLANG <- reactive({V_erlang(nERLANG(),
                                             betaERLANG())
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
        
        output$FxERLANG <- renderPlotly({
            ggplot(data = data.frame(x = c(0, 2 * nERLANG() * betaERLANG())),
                   aes(x)) +
                stat_function(fun = derlang,
                              args = list(nERLANG(),
                                          betaERLANG())) +
                ylab("f(x)") +
                theme_classic() +
                stat_function(
                    fun = derlang,
                    args = list(nERLANG(),
                                betaERLANG()),                    
                    xlim = c(0, xERLANG()),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$SxERLANG <- renderPlotly({
            ggplot(data = data.frame(x = c(0, 2 * nERLANG() * betaERLANG())),
                   aes(x)) +
                stat_function(fun = derlang,
                              args = list(nERLANG(),
                                          betaERLANG())) +
                ylab("f(x)") +
                theme_classic() +
                stat_function(
                    fun = derlang,
                    args = list(nERLANG(),
                                betaERLANG()),                    
                    xlim = c(xERLANG(), 2 * nERLANG() * betaERLANG()),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })        
        
        output$QxERLANG <- renderPlotly({
            ggplot(data = data.frame(x = c(0,1)),
                   aes(x)) +
                stat_function(fun = qerlang,
                              args = list(nERLANG(),
                                          betaERLANG())) + theme_classic()
        })
        
        
        
#### Loi Log-logistique Serveur ####
        
        lambdaLOGLOGIS <- reactive({input$lambdaLOGLOGIS})
        
        tauLOGLOGIS <- reactive({input$tauLOGLOGIS})
        
        dLOGLOGIS <- reactive({input$dLOGLOGIS})
        
        kLOGLOGIS <- reactive({input$kLOGLOGIS})
        
        xLOGLOGIS <- reactive({input$xLOGLOGIS})
        
        densityLOGLOGIS <- reactive({format(dllogis(x = xLOGLOGIS(), 
                                                    lambdaLOGLOGIS(), 
                                                    tauLOGLOGIS()), 
                                            nsmall = 6)})
        
        repartLOGLOGIS <- reactive({format(pllogis(q = xLOGLOGIS(), 
                                                   lambdaLOGLOGIS(), 
                                                   tauLOGLOGIS()), 
                                           nsmall = 6)})
        
        survieLOGLOGIS <- reactive({format(pllogis(q = xLOGLOGIS(), 
                                                   lambdaLOGLOGIS(), 
                                                   tauLOGLOGIS(),
                                                   lower.tail = F), 
                                           nsmall = 6)})
        
        VaRLOGLOGIS <- reactive({format(VaR_llogis(kappa = kLOGLOGIS(), 
                                                   lambdaLOGLOGIS(), 
                                                   tauLOGLOGIS()), 
                                        nsmall = 6)
        })
        
        TVaRLOGLOGIS <- reactive({format(TVaR_llogis(kappa = kLOGLOGIS(),
                                                     lambdaLOGLOGIS(),
                                                     tauLOGLOGIS()), 
                                         nsmall = 6)
        })
        
        EspTronqLOGLOGIS <- reactive({Etronq_llogis(d = dLOGLOGIS(),
                                                    lambdaLOGLOGIS(),
                                                    tauLOGLOGIS())
        })
        
        StopLossLOGLOGIS <- reactive({SL_llogis(d = dLOGLOGIS(),
                                                lambdaLOGLOGIS(),
                                                tauLOGLOGIS())
        })
        
        EspLimLOGLOGIS <- reactive({Elim_llogis(d = dLOGLOGIS(),
                                              lambdaLOGLOGIS(),
                                              tauLOGLOGIS())
        })
        
        kthmomentLOGLOGIS <- reactive({kthmoment_llogis(k = kLOGLOGIS(),
                                                        lambdaLOGLOGIS(),
                                                        tauLOGLOGIS())
        })
        
        ExcesMoyLOGLOGIS <- reactive({Mexcess_llogis(d = dLOGLOGIS(),
                                                     lambdaLOGLOGIS(),
                                                     tauLOGLOGIS())
        })
        
        meanLOGLOGIS <- reactive({kthmoment_llogis(k = 1,
                                                   lambdaLOGLOGIS(),
                                                   tauLOGLOGIS())
        })
        
        varianceLOGLOGIS <- reactive({V_llogis(lambdaLOGLOGIS(),
                                               tauLOGLOGIS())
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
        
        
        output$FxLOGLOGIS <- renderPlotly({
            ggplot(data = data.frame(x = c(0, 1)),
                   aes(x)) +
                stat_function(fun = dllogis,
                              args = list(lambdaLOGLOGIS(), 
                                          tauLOGLOGIS())) +
                ylab("f(x)") +
                theme_classic() +
                stat_function(
                    fun = dllogis,
                    args = list(lambdaLOGLOGIS(), 
                                tauLOGLOGIS()),                    
                    xlim = c(0, xERLANG()),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$SxLOGLOGIS <- renderPlotly({
            ggplot(data = data.frame(x = c(0, 1)),
                   aes(x)) +
                stat_function(fun = dllogis,
                              args = list(lambdaLOGLOGIS(), 
                                          tauLOGLOGIS())) +
                ylab("f(x)") +
                theme_classic() +
                stat_function(
                    fun = dllogis,
                    args = list(lambdaLOGLOGIS(), 
                                tauLOGLOGIS()),                    
                    xlim = c(xERLANG(), 1),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$QxLOGLOGIS <- renderPlotly({
            ggplot(data = data.frame(x = c(0,1)),
                   aes(x)) +
                stat_function(fun = qllogis,
                              args = list(lambdaLOGLOGIS(), 
                                          tauLOGLOGIS())) + theme_classic()
        })
        
        
        
#### Loi inverse gaussienne Serveur ####
        
        betaIG <- reactive({input$betaIG})
        
        muIG <- reactive({input$muIG})
        
        dIG <- reactive({input$dIG})
        
        kIG <- reactive({input$kIG})
        
        xIG <- reactive({input$xIG})
        
        densityIG <- reactive({format(d_IG(x = xIG(), 
                                           muIG(),
                                           betaIG()), 
                                      nsmall = 6)
        })
        
        repartIG <- reactive({format(p_IG(q = xIG(), 
                                          muIG(),
                                          betaIG()), 
                                     nsmall = 6)
        })
        
        survieIG <- reactive({format(p_IG(q = xIG(), 
                                         muIG(),
                                         betaIG(),
                                         lower.tail = F), 
                                     nsmall = 6)
        })
        
        VaRIG <- reactive({format(VaR_IG(kappa = kIG(),
                                         muIG(), 
                                         betaIG()),
                                  nsmall = 6)
        })
        
        VaRIG_a <- reactive({VaR_IG(kappa = kIG(),
                                    muIG(), 
                                    betaIG())
        })
        
        TVaRIG <- reactive({format(TVaR_IG(vark = VaRIG_a(),
                                           muIG(), 
                                           kappa = kIG(),
                                           betaIG()), 
                                   nsmall = 6)
        })
        
        EspTronqIG <- reactive({Etronq_IG(d = dIG(),
                                          betaIG(),
                                          muIG())
        })
        
        StopLossIG <- reactive({SL_IG(d = dIG(),
                                      betaIG(),
                                      muIG())
        })
        
        EspLimIG <- reactive({Elim_IG(d = dIG(),
                                      betaIG(),
                                      muIG())
        })
        
        # ExcesMoyIG <- reactive({Mexcess_IG(d = dIG(),
        #                                    betaIG(),
        #                                    muIG())
        # })
        
        meanIG <- reactive({E_IG( muIG())
        })
        
        varianceIG <- reactive({V_IG(betaIG(),
                                     muIG())
        })
        
        output$meanIG <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                       format(meanIG(),
                                                              nsmall = 6)))
        })
        
        output$varianceIG <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                           format(varianceIG(),
                                                                  nsmall = 6)))
        })
        
        output$densityIG <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                               xIG(),
                                                               densityIG()))
        })
        output$repartIG <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
                                                              xIG(),
                                                              repartIG()))
        })
        
        output$survieIG <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                              xIG(),
                                                              survieIG()))
        })
        
        output$VaRIG <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                           kIG(),
                                                           VaRIG()))
        })
        
        output$TVaRIG <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
                                                            kIG(),
                                                            TVaRIG()))
        })
        
        output$EspTronqIG <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$",
                                                                dIG(),
                                                                EspTronqIG()))
        })
        
        output$StopLossIG <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$",
                                                                dIG(),
                                                                StopLossIG()))
        })
        
        output$EspLimIG <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$",
                                                              dIG(),
                                                              EspLimIG()))
        })
        
        # output$ExcesMoyIG <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$",
        #                                                         dIG(),
        #                                                         ExcesMoyIG()))
        # })
        
        output$FxIG <- renderPlotly({
            ggplot(data = data.frame(x = c(0, 1.5 * muIG() + 0.5 *betaIG())),
                   aes(x)) +
                stat_function(fun = d_IG,
                              args = list(muIG(),
                                          betaIG())) +
                ylab("f(x)") +
                theme_classic() +
                stat_function(
                    fun = d_IG,
                    args = list(muIG(),
                                betaIG()),                    
                    xlim = c(0, xERLANG()),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$SxIG <- renderPlotly({
            ggplot(data = data.frame(x = c(0, 1.5 * muIG() + 0.5 *betaIG())),
                   aes(x)) +
                stat_function(fun = d_IG,
                              args = list(muIG(),
                                          betaIG())) +
                ylab("f(x)") +
                theme_classic() +
                stat_function(
                    fun = d_IG,
                    args = list(muIG(),
                                betaIG()),                    
                    xlim = c(xERLANG(), 1.5 * muIG() + 0.5 *betaIG()),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })        
    
#            output$QxIG <- renderPlotly({
#           ggplot(data = data.frame(x = c(0,1)),
 #                  aes(x)) +
  #              stat_function(fun = q_IG,                  --- ajouter la fonction q_IG---
   #                           args = list(muIG(),
    #                                      betaIG())) + theme_classic()
     #   })
        
        
#### Loi Uniforme Continue Serveur ####
        
        aUNIC <- reactive({input$aUNIC})
        
        bUNIC <- reactive({input$bUNIC})
        
        xUNIC <- reactive({input$xUNIC})
        
        kUNIC <- reactive({input$kUNIC})
        
        dUNIC <- reactive({input$dUNIC})
        
        
        meanUNIC <- reactive({E_unif(aUNIC(), bUNIC())
        })
        
        varUNIC <- reactive({
            format(V_unif(aUNIC(), bUNIC()), 
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
        
        VaRUNIC <- reactive({format(VaR_unif(kappa = kUNIC(), aUNIC(), bUNIC()),
                                   nsmall = 6)
        })

        output$meanUNIC <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                         meanUNIC()
        ))})
        
        output$varUNIC <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                        varUNIC()
        ))})
        
        output$densityUNIC <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                            xUNIC(),
                                                            densityUNIC()
        ))})
        
        output$repartUNIC <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
                                                           xUNIC(),
                                                           repartUNIC()
        ))})
        
        output$survieUNIC <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                           xUNIC(),
                                                           survieUNIC()))
        })
        
        output$VaRUNIC <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                       kUNIC(),
                                                       VaRUNIC()
        ))})

        # output$EspTronqUNIC <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$", 
        #                                                     dUNIC(),
        #                                                     EspTronqUNIC()
        # ))})
        
        # output$StopLossUNIC <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$", 
        #                                                     dUNIC(),
        #                                                     StopLossUNIC()
        # ))})
        # 
        # output$EspLimUNIC <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$", 
        #                                                   dUNIC(),
        #                                                   EspLimUNIC()
        # ))})
        # 
        # output$ExcesMoyUNIC <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$", 
        #                                                     dUNIC(),
        #                                                     ExcesMoyUNIC()
        # ))})
        
        
        
        output$FxUNIC <- renderPlotly({
            ggplot(data = data.frame(x = c(
                aUNIC(), bUNIC())
            ),
            aes(x)) +
                stat_function(fun = dunif,
                              args = list(min = aUNIC(), max = bUNIC())) +
                ylab("f(x)") +
                theme_classic() +
                stat_function(
                    fun = dunif,
                    args = list(min = aUNIC(), max = bUNIC()),
                    xlim = c(aUNIC(), xUNIC()),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$SxUNIC <- renderPlotly({
            ggplot(data = data.frame(x = c(
                aUNIC(), bUNIC())
            ),
            aes(x)) +
                stat_function(fun = dunif,
                              args = list(min = aUNIC(), max = bUNIC())) +
                ylab("f(x)") +
                theme_classic() +
                stat_function(
                    fun = dunif,
                    args = list(min = aUNIC(), max = bUNIC()),
                    xlim = c(xUNIC(), bUNIC()),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })        
        
        output$QxUNIC <- renderPlotly({
            ggplot(data = data.frame(x = c(0,1)),
                   aes(x)) +
                stat_function(fun = qunif,
                              args = list(min = aUNIC(), max = bUNIC())) + theme_classic()
        })
        
        
        
        
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
                titlePanel(tags$a("Loi Bernoulli",
                                  href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Bernoulli"))
            else
                titlePanel(tags$a("Loi Binomiale",
                                  href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Binomiale"))
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
        
        TVaRBIN <- reactive({format(TVaR_binom(input$kBIN,
                                               n = nBIN(),
                                               p = pBIN()),
                                    nsmall = 6)
            })
        
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
        
        output$FxBIN <- renderPlotly({ggplot(data.frame(x = 0:nBIN(), y = dbinom(0:nBIN(), nBIN(), pBIN())), aes(x = x, y = y)) + geom_bar(stat = "identity", col = "red", fill ="red", alpha = 0.7, width = 0.3) + theme_classic() + ylab("P(X=x")
            
        })
        
        # Reactive slider
        observeEvent(nBIN(),{updateSliderInput(session = session, inputId = "xBIN", max = nBIN())
        })
    
#### Loi Poisson Serveur ####
        
    xPOI <- reactive({input$xPOI})
        
    kPOI <- reactive({input$kPOI})
    
    dPOI <- reactive({input$dPOI})
        
    lamPOI <- reactive({input$lamPOI})
    
    densityPOI <- reactive({format(dpois(xPOI(), lamPOI()), nsmall = 6)})
    
    repartPOI <- reactive({format(ppois(xPOI(), lamPOI()), nsmall = 6)})
    
    surviePOI <- reactive({format(ppois(xPOI(), lamPOI(), lower.tail = F), nsmall = 6)})
    
    VaRPOI <- reactive({format(qpois(kPOI(),
                                     lamPOI()),
                               nsmall = 6)
        })
    
    output$meanPOI <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                    lamPOI()
    ))})
    
    output$varPOI <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                   lamPOI()
    ))})
    
    output$densityPOI <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                       xPOI(),
                                                       densityPOI()
    ))})
    output$repartPOI <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
                                                      xPOI(),
                                                      repartPOI()
    ))})
    
    output$surviePOI <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                      xPOI(),
                                                      surviePOI()))
    })
    
    output$VaRPOI <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                   kPOI(),
                                                   VaRPOI()
    ))})
    
    
#### Loi Hypergéométrique Serveur ####
    
    grosNHG <- reactive({input$grosNHG})
    
    petitNHG <- reactive({input$petitNHG})
    
    mHG <- reactive({input$petitNHG})
    
    xHG <- reactive({input$xHG})

    
    output$changingpetitNHG <- renderUI({
        numericInput('petitNHG', 
                     '$$n$$', 
                     value = (grosNHG() - mHG()), 
                     min = 0, 
                     max = (grosNHG() - 1),
                     step = 1)
    })
    
    output$changingmHG <- renderUI({
        numericInput('mHG', 
                     '$$m$$', 
                     value = 2, 
                     min = 0, 
                     max = grosNHG(),
                     step = 1)
    })
    
    output$changingxHG <- renderUI({
        numericInput('xHG', '$$x$$',
                     min = 0, 
                     value = 0, 
                     max = min(petitNHG(), mHG()),
                     step = 1)
    })
    
    kHG <- reactive({input$kHG})
    
    dHG <- reactive({input$dHG})
    
    meanHG <- reactive({E_hyper(N = grosNHG(), m = mHG(), n = petitNHG())})
        
    varHG <- reactive({
        format(V_hyper(N = grosNHG(), 
                       m = mHG(), 
                       n = petitNHG()
        ), 
        nsmall = 6)
    })

    densityHG <- reactive({
        format(dhyper(xHG(), 
                      m = mHG(), 
                      n = grosNHG() - mHG(), 
                      k = petitNHG()
        ), 
        nsmall = 6)
    })
    
    repartHG <- reactive({
        format(phyper(xHG(), 
                      m = mHG(), 
                      n = grosNHG() - mHG(), 
                      k = petitNHG()
        ), 
        nsmall = 6)
    })
    
    survieHG <- reactive({
        format(phyper(xHG(), 
                      m = mHG(), 
                      n = grosNHG() - mHG(), 
                      k = petitNHG(),
                      lower.tail = F 
        ), 
        nsmall = 6)
    })
    
    VaRHG <- reactive({
        format(qhyper(xHG(), 
                      m = mHG(), 
                      n = grosNHG() - mHG(), 
                      k = petitNHG()
        ), 
        nsmall = 6)
    })
    
    output$meanHG <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                   meanHG()
    ))})
    
    output$varHG <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                  varHG()
    ))})
    
    output$densityHG <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                      xHG(),
                                                      densityHG()
    ))})
    output$repartHG <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
                                                     xHG(),
                                                     repartHG()
    ))})
    
    output$survieHG <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                     xHG(),
                                                     survieHG()))
    })
    
    output$VaRHG <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                  kHG(),
                                                  VaRHG()
    ))})
    
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
    
    xBN <- reactive({input$xBN})
    
    kBN <- reactive({input$kBN})
    
    dBN <- reactive({input$dBN})
    
    qBN <- reactive({input$qBN})
    
    definitionBN <- reactive({input$definitionBN})
    
    # definitionBN <- reactive({
    #     if (input$definitionBN == "essais") {
    #         F
    #     } else {
    #         T
    #     }
    # })
    
    output$changingqBN <- renderUI({
        numericInput('qBN', label = '$$q$$', value = 0.5, min = 0, step = 0.1)
    })
    
    output$changingrBN <- renderUI({
        numericInput('rBN', label = '$$r$$', value = 1, min = 0, step = 1)
    })
    
    output$changingxBN <- renderUI({
        numericInput('xBN', '$$x$$', min = 0, 
                     value = {if (definitionBN() == T) {
                         rBN()
                     } else {
                         0
                     }}, 
                     step = 1)
    })
    
    ## Ici on crée un gros observeEvent qui va modifier les paramètres de la gamma/exponentielle/khi-carrée selon les 2 radio buttons:
    ## x: selection de distribution
    ## y: selection de si c'est fréquence ou échelle
    observeEvent(
    {
        input$distrchoiceqBN
        input$definitionBN
        input$distrchoiceBNFAM 
    },
    {
        x <- input$distrchoiceBNFAM
        y <- input$distrchoiceqBN
        z <- input$definitionBN

        updateNumericInput(session, "xBN",
                           min =
                               if (z == T)
                               {
                                   rBN()
                               }
                           else
                           {
                               0
                           }
        )
        
        
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
    
    
    output$loi_BN <- renderUI({
        if(input$distrchoiceBNFAM == "Géometrique")
            titlePanel(tags$a("Loi Géometrique", href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-G%C3%A9om%C3%A9trique"))
        else
            titlePanel(tags$a("Loi Binomiale Négative", href="https://gitlab.com/alec42/distributacalcul-wiki/wikis/Loi-Binomiale-Negative")) 
    })
    
    
    
    output$distr_BN <- renderText({
        if(input$distrchoiceBNFAM == "Géometrique")
            "\\(X \\sim\\text{Géometrique} \\ (q)\\)"
        else
            "\\(X \\sim\\text{Binomiale Négative} \\ (r, q)\\)"
    })
    
    
    meanBN <- reactive({E_negbinom(rBN(), qBN(), nb_tries = definitionBN())})
    
    varBN <- reactive({V_negbinom(rBN(), qBN(), nb_tries = definitionBN())})
    
    densityBN <- reactive({format(d_negbinom(xBN(), rBN(), qBN(), nb_tries = definitionBN()), nsmall = 6)})
        
    repartBN <- reactive({format(p_negbinom(xBN(), rBN(), qBN(), nb_tries = definitionBN()), nsmall = 6)})
    
    survieBN <- reactive({format(p_negbinom(xBN(), rBN(), qBN(), nb_tries = definitionBN(), lower.tail = F), nsmall = 6)})
    
    
    output$meanBN <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                   meanBN()
    ))})
    
    output$varBN <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                   varBN()
    ))})
    
    output$densityBN <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                       xBN(),
                                                       densityBN()
    ))})
    output$repartBN <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
                                                      xBN(),
                                                      repartBN()
    ))})
    
    output$survieBN <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                      xBN(),
                                                      survieBN()))
    })
    
#### Loi Uniforme Discrète Serveur ####
    
    aUNID <- reactive({input$aUNID})
    
    bUNID <- reactive({input$bUNID})
    
    xUNID <- reactive({input$xUNID})
    
    kUNID <- reactive({input$kUNID})
    
    dUNID <- reactive({input$dUNID})
    
    meanUNID <- reactive({E_unifD(aUNID(), bUNID())
    })
    
    varUNID <- reactive({
        format(V_unifD(aUNID(), bUNID()), 
               nsmall = 6)
    })
    
    densityUNID <- reactive({format(d_unifD(x = xUNID(), aUNID(), bUNID()),
                                    nsmall = 6)
    })
    
    repartUNID <- reactive({format(p_unifD(q = xUNID(), aUNID(), bUNID()), 
                                   nsmall = 6)
    })
    
    survieUNID <- reactive({format(1 - p_unifD(q = xUNID(), aUNID(), bUNID()), 
                                   nsmall = 6)
    })
    
    output$meanUNID <- renderUI({withMathJax(sprintf("$$E(X) = %s$$", 
                                                    meanUNID()
    ))})
    
    output$varUNID <- renderUI({withMathJax(sprintf("$$Var(X) = %s$$", 
                                                   varUNID()
    ))})
    
    output$densityUNID <- renderUI({withMathJax(sprintf("$$f_{X}(%s) = %s$$", 
                                                       xUNID(),
                                                       densityUNID()
    ))})
    
    output$repartUNID <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
                                                      xUNID(),
                                                      repartUNID()
    ))})
    
    output$survieUNID <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                      xUNID(),
                                                      survieUNID()))
    })

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
                                              shapeBNCOMP(), 
                                              rateBNCOMP(),
                                              ko = koBNCOMP(),
                                              distr_severity = input$severityBNCOMP), 
                                     nsmall = 6, scientific = F)
    })
    
    survieBNCOMP <- reactive({format(1 - p_BNComp(x = xBNCOMP(), 
                                                  r = rBNCOMP(),
                                                  q = qBNCOMP(),
                                                  ko = koBNCOMP(),
                                                  shapeBNCOMP(), 
                                                  rateBNCOMP(),
                                                  distr_severity = input$severityBNCOMP
                                                  ), 
                                     nsmall = 6, scientific = F)})

    VaRBNCOMP <- reactive({format(VaR_BNComp(kappa = kBNCOMP(), 
                                             ko = koBNCOMP(),
                                             q     = qBNCOMP(),
                                             r     = rBNCOMP(),
                                             shapeBNCOMP(), 
                                             rateBNCOMP()
    ), nsmall = 6)})
    
    varkBNCOMP <- reactive({VaR_BNComp(kappa = kBNCOMP(), 
                                       ko = koBNCOMP(),
                                       r     = rBNCOMP(),
                                       q     = qBNCOMP(),
                                       shapeBNCOMP(), 
                                       rateBNCOMP()
                                       )})
    
    TVaRBNCOMP <- reactive({format(TVaR_BNComp(kappa     = kBNCOMP(),
                                               r     = rBNCOMP(),
                                               q     = qBNCOMP(),
                                               vark  = varkBNCOMP(),
                                               ko    = koBNCOMP(),
                                               shapeBNCOMP(), 
                                               rateBNCOMP(),
                                               distr_severity = input$severityBNCOMP
                                               ), 
                                   nsmall = 6)
    })
    
    meanBNCOMP <- reactive({format(E_BNComp(shapeBNCOMP(), 
                                            rateBNCOMP(),
                                            r     = rBNCOMP(),
                                            q     = qBNCOMP(),
                                            distr_severity = input$severityBNCOMP
    ),
    nsmall = 6,
    scientific = F)
    })
    
    varianceBNCOMP <- reactive({format(V_BNComp(shapeBNCOMP(), 
                                                rateBNCOMP(),
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
                                                        xBNCOMP(),
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
                                            lambdaPCOMP(),
                                            shapePCOMP(), 
                                            ratePCOMP(),
                                            ko = koPCOMP(),
                                            distr_severity = input$severityPCOMP), 
                                    nsmall = 6, scientific = F)
    })
    
    surviePCOMP <- reactive({format(1 - p_Pcomp(x = xPCOMP(), 
                                            lambdaPCOMP(),
                                            shapePCOMP(), 
                                            ratePCOMP(),
                                            ko = koPCOMP(),
                                            distr_severity = input$severityPCOMP), 
                                    nsmall = 6, scientific = F)
        })
    
    VaRPCOMP <- reactive({format(VaR_PComp(kappa = kPCOMP(), 
                                           lambdaPCOMP(),
                                           shapePCOMP(), 
                                           ratePCOMP(),
                                           ko = koPCOMP()
    ), nsmall = 6)})
    
    varkPCOMP <- reactive({VaR_PComp(kappa = kPCOMP(), 
                                     lambdaPCOMP(),
                                     shapePCOMP(), 
                                     ratePCOMP(),
                                     ko = koPCOMP()
    )})
    
    TVaRPCOMP <- reactive({format(TVaR_PComp(kappa = kPCOMP(),
                                             lambdaPCOMP(),
                                             shapePCOMP(), 
                                             ratePCOMP(),
                                             vark  = varkPCOMP(),
                                             ko    = koPCOMP(),
                                             distr_severity = input$severityPCOMP), nsmall = 6)
    })
    
    meanPCOMP <- reactive({format(E_PCOMP(lambdaPCOMP(), 
                                          shapePCOMP(), 
                                          ratePCOMP(),
                                          distr_severity = input$severityPCOMP
    ), 
                                  nsmall = 6, 
                                  scientific = F)
    })
    
    variancePCOMP <- reactive({format(V_PCOMP(lambdaPCOMP(), shapePCOMP(), ratePCOMP()),
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
    #                                                      xPCOMP(),
    #                                                      densityPCOMP()
    # ))})
    
    output$repartPCOMP <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
                                                         xPCOMP(),
                                                         repartPCOMP()
    ))})
    
    output$surviePCOMP <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
                                                        xPCOMP(),
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
                                              shapeBINCOMP(), 
                                              rateBINCOMP(),
                                              ko = koBINCOMP(),
                                              distr_severity = input$severityBINCOMP), 
                                     nsmall = 6, scientific = F)
    })
    
    survieBINCOMP <- reactive({format(1 - p_BINComp(x = xBINCOMP(), 
                                                  n = nBINCOMP(),
                                                  q = qBINCOMP(),
                                                  shapeBINCOMP(), 
                                                  rateBINCOMP(),
                                                  ko = koBINCOMP(),
                                                  distr_severity = input$severityBINCOMP), nsmall = 6, scientific = F)})
    
    VaRBINCOMP <- reactive({format(VaR_BINComp(k = kBINCOMP(), 
                                             n     = nBINCOMP(),
                                             q     = qBINCOMP(),
                                             shapeBINCOMP(), 
                                             rateBINCOMP(),
                                             ko = koBINCOMP()
    ), nsmall = 6)})
    
    varkBINCOMP <- reactive({VaR_BINComp(k = kBINCOMP(), 
                                       n     = nBINCOMP(),
                                       q     = qBINCOMP(),
                                       rateBINCOMP(),
                                       shapeBINCOMP(), 
                                       ko = koBINCOMP()
    )})
    
    TVaRBINCOMP <- reactive({format(TVaR_BINComp(k    = kBINCOMP(),
                                               n     = nBINCOMP(),
                                               q     = qBINCOMP(),
                                               shapeBINCOMP(), 
                                               rateBINCOMP(),
                                               vark  = varkBINCOMP(),
                                               ko    = koBINCOMP(),
                                               distr_severity = input$severityBINCOMP
    ), 
    nsmall = 6)
    })
    
    meanBINCOMP <- reactive({format(E_BINComp(
                                            n     = nBINCOMP(),
                                            q     = qBINCOMP(),
                                            shapeBINCOMP(), 
                                            rateBINCOMP(),
                                            distr_severity = input$severityBINCOMP
    ),
    nsmall = 6,
    scientific = F)
    })
    
    varianceBINCOMP <- reactive({format(V_BINComp(
                                                n     = nBINCOMP(),
                                                q     = qBINCOMP(),
                                                shapeBINCOMP(), 
                                                rateBINCOMP(),
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
                                                         xBINCOMP(),
                                                         survieBINCOMP()))
    })
    
    
    output$VaRBINCOMP <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$", 
                                                      kBINCOMP(),
                                                      VaRBINCOMP()
    ))})
    
    output$TVaRBINCOMP <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$", 
                                                       kBINCOMP(),
                                                       TVaRBINCOMP()
    ))})
    
}
