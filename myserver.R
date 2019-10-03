
myserver <- function(input, output, session) 
{

#### Serveur outil de tests statistiques ####
    valueESTIM_STATTOOL <- reactive({input$valueESTIM_STATTOOL})
    testESTIM_STATTOOL <- reactive({input$testESTIM_STATTOOL}) 
    nsizeESTIM_STATTOOL <- reactive({input$nsizeESTIM_STATTOOL})
    
    ## pour renderer le choix de borne avec KaTex
    updateSelectizeInput(
        session,
        inputId = "borneSTIM_STATTOOL",
        choices = setNames(list("GE", "GT", "LE", "LT", "NE", "E"), 
                           list("\\\\\\ge", "\\\\\\gt", "\\\\\\le", "\\\\\\lt", "\\\\\\not =", "=")),
        options = list(render = I("
                                  {
                                  item:   function(item, escape) { 
                                  var html = katex.renderToString(item.label);
                                  return '<div>' + html + '</div>'; 
                                  },
                                  option: function(item, escape) { 
                                  var html = katex.renderToString(item.label);
                                  return '<div>' + html + '</div>'; 
                                  }
                                  }
                                  "))
        
        )
    ## pour renderer le choix de paramètre avec KaTex
    updateSelectizeInput(
        session,
        inputId = "paramESTIM_STATTOOL",
        choices = setNames(list("mu", "sigma", "p", "theta"), list("\\\\\\mu", "\\\\\\sigma^2", "p", "\\\\\\theta")),
        options = list(render = I("
    {
                                  item:   function(item, escape) { 
                                  var html = katex.renderToString(item.label);
                                  return '<div>' + html + '</div>'; 
                                  },
                                  option: function(item, escape) { 
                                  var html = katex.renderToString(item.label);
                                  return '<div>' + html + '</div>'; 
                                  }
}
"))
        
    )
    
    output$seuil_H0_ESTIM_STATTOOL <- renderUI({
        if(testESTIM_STATTOOL() == "UMP")
        {
            withMathJax("$$Z_{\\text{obs}} \\geq z_{\\alpha}$$")
        }
        else if(testESTIM_STATTOOL() == "Test T")
        {
            withMathJax("$$T_{\\text{n, obs}} \\geq t_{(n - 1), \\ \\alpha}$$")
        }
        else if(testESTIM_STATTOOL() == "Central limite")
        {
            withMathJax("$$Z_{\\text{obs}} \\geq z_{\\alpha}$$")
        }
        else if(testESTIM_STATTOOL() == "Wald")
        {
            withMathJax("$$| \\phi_{\\text{obs}} | \\geq z_{\\alpha / 2}$$")
        }
        else if(testESTIM_STATTOOL() == "Sur une proportion")
        {
            withMathJax("$$| Z_{\\text{obs}} | \\geq z_{\\alpha / 2}$$")
        }
        else if(testESTIM_STATTOOL() == "Sur la variance")
        {
            withMathJax("$$ W_{n, \\text{obs}} \\geq \\chi^2_{\\alpha, \\ (n - 1)}$$")
        }
        else if(testESTIM_STATTOOL() == "Rapport de vraisemblance")
        {
            withMathJax("$$ W_{\\text{obs}} \\geq \\chi^2_{(p - r), \\alpha}$$")
        }
        else if(testESTIM_STATTOOL() == "Khi-carré de Pearson")
        {
            withMathJax("$$X^2_{\\text{obs}} \\geq \\chi^2_{(k - s), \\alpha}$$")
        }
    })
    
    output$distr_H0_ESTIM_STATTOOL <- renderUI({
        if(testESTIM_STATTOOL() == "UMP")
        {
            withMathJax("$$Z \\sim \\text{N}(0, 1)$$")
        }
        else if(testESTIM_STATTOOL() == "Test T")
        {
            withMathJax("$$T_n \\sim t_{(n - 1)}$$")
        }
        else if(testESTIM_STATTOOL() == "Central limite")
        {
            withMathJax("$$\\approx \\text{N}(0, 1)$$")
        }
        else if(testESTIM_STATTOOL() == "Wald")
        {
            withMathJax("$$\\approx \\text{N}(0, 1)$$")
        }
        else if(testESTIM_STATTOOL() == "Sur une proportion")
        {
            withMathJax("$$\\approx \\text{N}(0, 1)$$")
        }
        else if(testESTIM_STATTOOL() == "Sur la variance")
        {
            withMathJax("$$W_n \\sim \\chi^2_{(n - 1)}$$")
        }
        else if(testESTIM_STATTOOL() == "Rapport de vraisemblance")
        {
            withMathJax("$$\\approx \\chi^2_{(p - r)}$$")
        }
        else if(testESTIM_STATTOOL() == "Khi-carré de Pearson")
        {
            withMathJax("$$\\approx \\chi^2_{(k - s)}$$")
        }
    })
    
    output$distr_ESTIM_STATTOOL <- renderUI({
        if(testESTIM_STATTOOL() == "UMP")
        {
            withMathJax("$$X_1 \\sim \\text{N}(\\mu, \\sigma^2)$$")
        }
        else if(testESTIM_STATTOOL() == "Test T")
        {
            withMathJax("$$X_1 \\sim \\text{N}(\\mu, \\sigma^2)$$")
        }
        else if(testESTIM_STATTOOL() == "Central limite")
        {
            withMathJax("$$X_1 \\sim \\text{ loi avec } E[X_1] = \\mu \\\ \\text{ et } V(X) \\text{ inconnue}$$")
        }
        else if(testESTIM_STATTOOL() == "Wald")
        {
            withMathJax("$$X_1 \\text{ à une densité } f( \\ \\dot \\ ; \\theta)$$")
        }
        else if(testESTIM_STATTOOL() == "Sur une proportion")
        {
            withMathJax("$$X_1 \\sim \\text{Bernoulli}(p)$$")
        }
        else if(testESTIM_STATTOOL() == "Sur la variance")
        {
            withMathJax("$$X_1 \\sim \\text{N}(\\mu, \\sigma^2)$$")
        }
        else if(testESTIM_STATTOOL() == "Rapport de vraisemblance")
        {
            withMathJax("$$X_1 \\text{ à une densité } f( \\ . \\ ; \\theta) \\text{ où } \\theta \\text{ a une dimension } p$$")
        }
        else if(testESTIM_STATTOOL() == "Khi-carré de Pearson")
        {
            withMathJax("Tableau de fréquence avec k cellules")
        }
    })
    
#### Serveur outil de copules ####
    
    output$caption <- renderText({
        if (input$name == "Student") {
            "Fonctionne pas"
        } else {
            ""
        }
    })
    
    
    output$copulaPlot <- renderPlotly({
        if (length(input$name) == 0) {
            print("Please select copula type")
        } else {
            
            if (input$name == "EFGM") {
                current_dcopula <- dEFGM
            } else if (input$name == "Clayton") {
                current_dcopula <- dClayton
            } else if(input$name == "AMH") {
                current_dcopula <- dAMH
            } else if(input$name == "Frank") {
                current_dcopula <- dFrank
            } else if(input$name == "Gumbel") {
                current_dcopula <- dGumbel
            } else if(input$name == "Normal") {
                current_dcopula <- dNormal
            } else if(input$name == "Student") {
                current_dcopula <- dStudent
            }
            
            x.seq <- seq(0, 1, 0.01)
            
            densityMatrix <- outer(x.seq, x.seq, FUN = "current_dcopula", kendallTau = input$kendallTau)
            axz <- list(
                nticks = 10,
                range = c(min(densityMatrix, 0), max(densityMatrix))
            )
            plot_ly(z =~densityMatrix) %>% add_surface() %>% layout(scene = list(zaxis=axz))
        }
    })
    
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
        
    VaRNORM <- reactive({format(VaR_norm(input$kNORM, muNORM(), sqrt(sigma2NORM())), nsmall = 6)})
        
    TVaRNORM <- reactive({format(TVaR_norm(input$kNORM, muNORM(), sqrt(sigma2NORM())), nsmall = 6)})
        
    EspTronqNORM <- reactive({Etronq_norm(d = input$dNORM, muNORM(), sqrt(sigma2NORM()))})
        
    StopLossNORM <- reactive({SL_norm(d = input$dNORM, muNORM(), sqrt(sigma2NORM()))})
        
    EspLimNORM <- reactive({Elim_norm(d = input$dNORM, muNORM(), sqrt(sigma2NORM()))})
    
    ExcesMoyNORM <- reactive({Mexcess_norm(d = input$dNORM, muNORM(), sqrt(sigma2NORM()))})
    
    plot_choice_NORM_QX_SERVER <- reactive({
        if(input$plot_choice_NORM_QX == "Densité")
            dnorm
        else
            pnorm
    })
    
    plot_choice_NORM_SERVER <- reactive({
        if(input$plot_choice_NORM == "Densité")
            dnorm
        else
            pnorm
    })
    
    plot_color_NORM_SERVER <- reactive({
        if(input$xlim_NORM == T)
            "Dark Green"
        else
            "Royal Blue"
    })
    
    xlim_NORM_SERVER <- reactive({
        if(input$xlim_NORM == T)
            c(muNORM() - 4 * sqrt(sigma2NORM()), input$xNORM)
        else
            c(input$xNORM, muNORM() + 4 * sqrt(sigma2NORM()))
    })
    
    repartsurvieNORM_LATEX <- reactive({
        if(input$xlim_NORM == T)
        {
            "F_{X}"
        }
        else
        {
            "S_{X}"
        }
    })
    
    repartsurvieNORM <- reactive({
        if(input$xlim_NORM == T)
        {
            format(pnorm(input$xNORM, muNORM(), sqrt(sigma2NORM())), nsmall = 6, scientific = F)
        }
        else
        {
            format(pnorm(input$xNORM, muNORM(), sqrt(sigma2NORM()), lower.tail = F), nsmall = 6, scientific = F)
        }
        
    })
    
    
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
    
    output$repartsurvieNORM <- renderUI({withMathJax(sprintf("$$%s(%s) = %s$$", 
                                                              repartsurvieNORM_LATEX(),
                                                              input$xNORM,
                                                              repartsurvieNORM()
    ))})
    
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
    
    output$QxNORM <- renderPlotly({
        ggplot(data = data.frame(x = c(muNORM() - 4 * sqrt(sigma2NORM()),
                                       muNORM() + 4 * sqrt(sigma2NORM())
                                       )
                                 ),
        aes(x)) + 
            stat_function(fun = plot_choice_NORM_QX_SERVER(),
                          args = list(muNORM(), sqrt(sigma2NORM()))) + 
            ylab("f(x)") + 
            theme_classic() +
            stat_function(
                fun = plot_choice_NORM_QX_SERVER(),
                args = list(muNORM(), sqrt(sigma2NORM())),
                xlim = c(VaRNORM(), muNORM() + 4 * sqrt(sigma2NORM())),
                geom = "area",
                fill = "red",
                alpha = 0.7
            )
    })
    
    output$FxNORM <- renderPlotly({
        ggplot(data = data.frame(x = c(muNORM() - 4 * sqrt(sigma2NORM()),
                                       muNORM() + 4 * sqrt(sigma2NORM())
                                       )
                                 ),
               aes(x)) + 
            stat_function(fun = plot_choice_NORM_SERVER(),
                          args = list(muNORM(), sqrt(sigma2NORM()))) + 
            ylab("f(x)") + 
            theme_classic() +
            stat_function(
                fun = plot_choice_NORM_SERVER(),
                args = list(muNORM(), sqrt(sigma2NORM())),
                xlim = xlim_NORM_SERVER(),
                geom = "area",
                fill = plot_color_NORM_SERVER(),
                alpha = 0.7
            )
    })
    
    
#### Loi Gamma Serveur ####
    
    kGAMMA <- reactive({input$kGAMMA})
    
    dGAMMA <- reactive({input$dGAMMA})
    
    xGAMMA <- reactive({input$xGAMMA})
    
    plot_choice_GAMMA_QX_SERVER <- reactive({
        if(input$plot_choice_GAMMA_QX == "Densité")
            dgamma
        else
            pgamma
    })
    
    plot_choice_GAMMA_SERVER <- reactive({
        if(input$plot_choice_GAMMA == "Densité")
            dgamma
        else
            pgamma
    })
    
    plot_color_GAMMA_SERVER <- reactive({
        if(input$xlim_GAMMA == T)
            "Dark Green"
        else
            "Royal Blue"
    })
    
    xlim_GAMMA_SERVER <- reactive({
        if(input$xlim_GAMMA == T)
            c(0, xGAMMA())
        else
            c(xGAMMA(), VaR_gamma(kappa = 0.999999, alphaGAMMA(), betaGAMMA()))
    })
    
    
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
                shinyjs::hide("betaGAMMA")
                shinyjs::hide("distrchoiceGAMMA")
            }
            else
            {
                shinyjs::show("betaGAMMA")
                shinyjs::show("distrchoiceGAMMA")
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
                shinyjs::hide("alphaGAMMA")
            else if( x == "Gamma")
                shinyjs::show("alphaGAMMA")
        })
        
        
        densityGAMMA <- reactive({format(dgamma(xGAMMA(), alphaGAMMA(), betaGAMMA()),scientific = F,  nsmall = 6)})

        repartsurvieGAMMA_LATEX <- reactive({
            if(input$xlim_GAMMA == T)
            {
                "F_{X}"
            }
            else
            {
                "S_{X}"
            }
        })
        
        repartsurvieGAMMA <- reactive({
            if(input$xlim_GAMMA == T)
            {
                format(pgamma(xGAMMA(), alphaGAMMA(), betaGAMMA()), nsmall = 6, scientific = F)
            }
            else
            {
                format(pgamma(xGAMMA(), alphaGAMMA(), betaGAMMA(), lower.tail = F), nsmall = 6, scientific = F)
            }
            
            })
        
        
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
        
        output$repartsurvieGAMMA <- renderUI({withMathJax(sprintf("$$%s(%s) = %s$$", 
                                                                  repartsurvieGAMMA_LATEX(),
                                                                  xGAMMA(),
                                                                  repartsurvieGAMMA()
        ))})
        
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
        
        output$QxGAMMA <- renderPlotly({
            ggplot(data = data.frame(x = c(0, VaR_gamma(kappa = 0.999999, alphaGAMMA(), betaGAMMA()) 
            )
            ),
            aes(x)) + 
                stat_function(fun = plot_choice_GAMMA_QX_SERVER(),
                              args = list(alphaGAMMA(), 
                                          betaGAMMA())) + 
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = plot_choice_GAMMA_QX_SERVER(),
                    args = list(alphaGAMMA(), betaGAMMA()),
                    xlim = c(VaRGAMMA(), VaR_gamma(kappa = 0.999999, alphaGAMMA(), betaGAMMA())),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$FxGAMMA <- renderPlotly({
            ggplot(data = data.frame(x = c(0, VaR_gamma(kappa = 0.999999, alphaGAMMA(), betaGAMMA()))),
            aes(x)) + 
                stat_function(fun = plot_choice_GAMMA_SERVER(),
                              args = list(alphaGAMMA(), betaGAMMA())) + 
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = plot_choice_GAMMA_SERVER(),
                    args = list(alphaGAMMA(), betaGAMMA()),
                    xlim = xlim_GAMMA_SERVER(),
                    geom = "area",
                    fill = plot_color_GAMMA_SERVER(),
                    alpha = 0.7
                )
        })
        
#### Loi Pareto Serveur ####
        
        alphaPARETO <- reactive({input$alphaPARETO})
        
        lambdaPARETO <- reactive({input$lambdaPARETO})
        
        xPARETO <- reactive({input$xPARETO})
        
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
                kthmoment_pareto(k = input$dPARETO, 
                                  alphaPARETO(), 
                                  lambdaPARETO())
        })
        
        plot_choice_PARETO_SERVER <- reactive({
            if(input$plot_choice_PARETO == "Densité")
                dpareto
            else
                ppareto
        })
        
        plot_choice_PARETO_QX_SERVER <- reactive({
            if(input$plot_choice_PARETO_QX == "Densité")
                dpareto
            else
                ppareto
        })
        
        plot_color_PARETO_SERVER <- reactive({
            if(input$xlim_PARETO == T)
                "Dark Green"
            else
                "Royal Blue"
        })
        
        xlim_PARETO_SERVER <- reactive({
            if(input$xlim_PARETO == T)
                c(0, xPARETO())
            else
                c(xPARETO(), 3 * lambdaPARETO())
        })
        
        repartsurviePARETO_LATEX <- reactive({
            if(input$xlim_PARETO == T)
            {
                "F_{X}"
            }
            else
            {
                "S_{X}"
            }
        })
        
        repartsurviePARETO <- reactive({
            if(input$xlim_PARETO == T)
            {
                format(ppareto(q = xPARETO(), 
                               alphaPARETO(),
                               lambdaPARETO()), 
                       nsmall = 6)
            }
            else
            {
                format(ppareto(q = xPARETO(), 
                               alphaPARETO(),
                               lambdaPARETO(),
                               lower.tail = F), 
                       nsmall = 6)
            }
            
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
        
        
        output$repartsurviePARETO <- renderUI({withMathJax(sprintf("$$%s(%s) = %s$$", 
                                                                   repartsurviePARETO_LATEX(),
                                                                   xPARETO(),
                                                                   repartsurviePARETO()
        ))})
        
        # output$repartPARETO <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
        #                                                    input$xPARETO,
        #                                                    repartPARETO()))
        # })
        # 
        # output$surviePARETO <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
        #                                                    input$xPARETO,
        #                                                    surviePARETO()))
        # })
        
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
        
        output$QxPARETO <- renderPlotly({
            ggplot(data = data.frame(x = c(0,
                                           3 * lambdaPARETO()
            )
            ),
            aes(x)) + 
                stat_function(fun = plot_choice_PARETO_QX_SERVER(),
                              args = list(alphaPARETO(),
                                          lambdaPARETO())) +
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = plot_choice_PARETO_QX_SERVER(),
                    args = list(alphaPARETO(),
                                lambdaPARETO()),
                    xlim = c(VaRPARETO(), 
                             3 * lambdaPARETO()),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$FxPARETO <- renderPlotly({
            ggplot(data = data.frame(x = c(0,
                                           3 * lambdaPARETO()
            )
            ),
            aes(x)) + 
                stat_function(fun = plot_choice_PARETO_SERVER(),
                              args = list(alphaPARETO(),
                                          lambdaPARETO())) +
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = plot_choice_PARETO_SERVER(),
                    args = list(alphaPARETO(),
                                lambdaPARETO()),
                    xlim = xlim_PARETO_SERVER(),
                    geom = "area",
                    fill = plot_color_PARETO_SERVER(),
                    alpha = 0.7
                )
        })
        
        
#### Loi Burr Serveur ####
        
        alphaBURR <- reactive({input$alphaBURR})
        
        lambdaBURR <- reactive({input$lambdaBURR})
        
        tauBURR <- reactive({input$tauBURR})
        
        dBURR <- reactive({input$dBURR})
        
        kBURR <- reactive({input$kBURR})
        
        xBURR <- reactive({input$xBURR})
        
        plot_choice_BURR_SERVER <- reactive({
            if(input$plot_choice_BURR == "Densité")
                dburr
            else
                pburr
        })
        
        plot_choice_BURR_QX_SERVER <- reactive({
            if(input$plot_choice_BURR_QX == "Densité")
                dburr
            else
                pburr
        })
        
        
        plot_color_BURR_SERVER <- reactive({
            if(input$xlim_BURR == T)
                "Dark Green"
            else
                "Royal Blue"
        })
        
        xlim_BURR_SERVER <- reactive({
            if(input$xlim_BURR == T)
                c(0, xBURR())
            else
                c(xBURR(), 5 * tauBURR())
        })
        
        repartsurvieBURR_LATEX <- reactive({
            if(input$xlim_BURR == T)
            {
                "F_{X}"
            }
            else
            {
                "S_{X}"
            }
        })
        
        repartsurvieBURR <- reactive({
            if(input$xlim_BURR == T)
            {
                format(pburr(q = xBURR(), 
                             alphaBURR(), 
                             tauBURR(),
                             lambdaBURR()), 
                       nsmall = 6)
            }
            else
            {
                format(pburr(q = xBURR(), 
                             alphaBURR(), 
                             tauBURR(),
                             lambdaBURR(),
                             lower.tail = F), 
                       nsmall = 6)
            }
            
        })
        
        
        densityBURR <- reactive({format(dburr(x = xBURR(), 
                                              alphaBURR(), 
                                              tauBURR(),
                                              lambdaBURR()), 
                                          nsmall = 6)})
        
        # repartBURR <- reactive({format(pburr(q = xBURR(), 
        #                                      alphaBURR(), 
        #                                      tauBURR(),
        #                                      lambdaBURR()), 
        #                                  nsmall = 6)})
        # 
        # survieBURR <- reactive({format(pburr(q = xBURR(), 
        #                                      alphaBURR(), 
        #                                      tauBURR(),
        #                                      lambdaBURR(),
        #                                      lower.tail = F), 
        #                                  nsmall = 6)})
        
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
                                               vark = VaRBURR_a(),
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
        
        output$repartsurvieBURR <- renderUI({withMathJax(sprintf("$$%s(%s) = %s$$", 
                                                                 repartsurvieBURR_LATEX(),
                                                                 xBURR(),
                                                                 repartsurvieBURR()
        ))})
        
        # output$repartBURR <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
        #                                                      xBURR(),
        #                                                      repartBURR()))
        # })
        # 
        # output$survieBURR <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
        #                                                      xBURR(),
        #                                                      survieBURR()))
        # })
        
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
        
        output$QxBURR <- renderPlotly({
            ggplot(data = data.frame(x = c(0,
                                           5 * tauBURR()
            )
            ),
            aes(x)) + 
                stat_function(fun = plot_choice_BURR_QX_SERVER(),
                              args = list(alphaBURR(), 
                                          tauBURR(),
                                          lambdaBURR())) +
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = plot_choice_BURR_QX_SERVER(),
                    args = list(alphaBURR(), 
                                tauBURR(),
                                lambdaBURR()),
                    xlim = c(VaRBURR(), 
                             5 * tauBURR()),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$FxBURR <- renderPlotly({
            ggplot(data = data.frame(x = c(0,
                                           5 * tauBURR()
            )
            ),
            aes(x)) + 
                stat_function(fun = plot_choice_BURR_SERVER(),
                              args = list(alphaBURR(), 
                                          tauBURR(),
                                          lambdaBURR())) +
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = plot_choice_BURR_SERVER(),
                    args = list(alphaBURR(), 
                                tauBURR(),
                                lambdaBURR()),
                    xlim = xlim_BURR_SERVER(),
                    geom = "area",
                    fill = plot_color_BURR_SERVER(),
                    alpha = 0.7
                )
        })
        
        
        # output$FxBURR <- renderPlotly({
        #     ggplot(data = data.frame(x = c(
        #         0, 5 * tauBURR())
        #     ),
        #     aes(x)) +
        #         stat_function(fun = dburr,
        #                       args = list(alphaBURR(), 
        #                                   tauBURR(),
        #                                   lambdaBURR())) +
        #         ylab("f(x)") +
        #         theme_classic() +
        #         stat_function(
        #             fun = dburr,
        #             args = list(alphaBURR(), 
        #                         tauBURR(),
        #                         lambdaBURR()),
        #             xlim = c(0, xBURR()),
        #             geom = "area",
        #             fill = "red",
        #             alpha = 0.7
        #         )
        # })
        # 
        # output$SxBURR <- renderPlotly({
        #     ggplot(data = data.frame(x = c(
        #         0, 5 * tauBURR())
        #     ),
        #     aes(x)) +
        #         stat_function(fun = dburr,
        #                       args = list(alphaBURR(), 
        #                                   tauBURR(),
        #                                   lambdaBURR())) +
        #         ylab("f(x)") +
        #         theme_classic() +
        #         stat_function(
        #             fun = dburr,
        #             args = list(alphaBURR(), 
        #                         tauBURR(),
        #                         lambdaBURR()),
        #             xlim = c(xBURR(), 5 * tauBURR()),
        #             geom = "area",
        #             fill = "red",
        #             alpha = 0.7
        #         )
        # })
        # 
        # output$QxBURR <- renderPlotly({
        #     ggplot(data = data.frame(x = c(0,1)),
        #            aes(x)) +
        #         stat_function(fun = qburr,
        #                       args = list(alphaBURR(), 
        #                                   tauBURR(),
        #                                   lambdaBURR())) + theme_classic()
        # })
        
        
#### Loi Weibull Serveur ####
        
        betaWEIBULL <- reactive({input$betaWEIBULL})
        
        tauWEIBULL <- reactive({input$tauWEIBULL})
        
        dWEIBULL <- reactive({input$dWEIBULL})
        
        kWEIBULL <- reactive({input$kWEIBULL})
        
        xWEIBULL <- reactive({input$xWEIBULL})
        
        plot_choice_WEIBULL_SERVER <- reactive({
            if(input$plot_choice_WEIBULL == "Densité")
                dweibull
            else
                pweibull
        })
        
        plot_choice_WEIBULL_QX_SERVER <- reactive({
            if(input$plot_choice_WEIBULL_QX == "Densité")
                dweibull
            else
                pweibull
        })
        
        plot_color_WEIBULL_SERVER <- reactive({
            if(input$xlim_WEIBULL == T)
                "Dark Green"
            else
                "Royal Blue"
        })
        
        xlim_WEIBULL_SERVER <- reactive({
            if(input$xlim_WEIBULL == T)
                c(0, xWEIBULL())
            else
                c(xWEIBULL(), 2 * betaWEIBULL())
        })
        
        repartsurvieWEIBULL_LATEX <- reactive({
            if(input$xlim_WEIBULL == T)
            {
                "F_{X}"
            }
            else
            {
                "S_{X}"
            }
        })
        
        repartsurvieWEIBULL <- reactive({
            if(input$xlim_WEIBULL == T)
            {
                format(pweibull(q = xWEIBULL(), 
                                tauWEIBULL(),
                                betaWEIBULL()), 
                       nsmall = 6)
            }
            else
            {
                format(pweibull(q = xWEIBULL(), 
                                tauWEIBULL(),
                                betaWEIBULL(),
                                lower.tail = F), 
                       nsmall = 6)
            }
            
        })
        
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
        
        output$repartsurvieWEIBULL <- renderUI({withMathJax(sprintf("$$%s(%s) = %s$$", 
                                                                    repartsurvieWEIBULL_LATEX(),
                                                                    xWEIBULL(),
                                                                    repartsurvieWEIBULL()
        ))})
        
        
        # output$repartWEIBULL <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
        #                                                    xWEIBULL(),
        #                                                    repartWEIBULL()))
        # })
        # 
        # output$survieWEIBULL <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
        #                                                    xWEIBULL(),
        #                                                    survieWEIBULL()))
        # })
        
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
        
        output$QxWEIBULL <- renderPlotly({
            ggplot(data = data.frame(x = c(0,
                                           max(2 * betaWEIBULL(), VaR_weibull(0.9999, tauWEIBULL(), betaWEIBULL()))
            )
            ),
            aes(x)) + 
                stat_function(fun = plot_choice_WEIBULL_QX_SERVER(),
                              args = list(tauWEIBULL(),
                                          betaWEIBULL())) +
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = plot_choice_WEIBULL_QX_SERVER(),
                    args = list(tauWEIBULL(),
                                betaWEIBULL()),
                    xlim = c(VaRWEIBULL(), 
                             max(2 * betaWEIBULL(), VaR_weibull(0.9999, tauWEIBULL(), betaWEIBULL()))),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$FxWEIBULL <- renderPlotly({
            ggplot(data = data.frame(x = c(0,
                                           2 * betaWEIBULL()
            )
            ),
            aes(x)) + 
                stat_function(fun = plot_choice_WEIBULL_SERVER(),
                              args = list(tauWEIBULL(),
                                          betaWEIBULL())) +
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = plot_choice_WEIBULL_SERVER(),
                    args = list(tauWEIBULL(),
                                betaWEIBULL()),
                    xlim = xlim_WEIBULL_SERVER(),
                    geom = "area",
                    fill = plot_color_WEIBULL_SERVER(),
                    alpha = 0.7
                )
        })
        
#### Loi Lognormale Serveur ####
        
        muLNORM <- reactive({input$muLNORM})
        
        sigma2LNORM <- reactive({input$sigmaLNORM})
        
        xLNORM <- reactive({input$xLNORM})
        
        kLNORM <- reactive({input$kLNORM})
        
        dLNORM <- reactive({input$dLNORM})
        
        plot_choice_LNORM_QX_SERVER <- reactive({
            if(input$plot_choice_LNORM_QX == "Densité")
                dlnorm
            else
                plnorm
        })
        
        plot_choice_LNORM_SERVER <- reactive({
            if(input$plot_choice_LNORM == "Densité")
                dlnorm
            else
                plnorm
        })
        
        plot_color_LNORM_SERVER <- reactive({
            if(xLNORM() == T)
                "Dark Green"
            else
                "Royal Blue"
        })
        
        xlim_LNORM_SERVER <- reactive({
            if(input$xlim_LNORM == T)
                c(0, xLNORM())
            else
                c(xLNORM(), VaR_lnorm(kappa = 0.99, muLNORM(), sqrt(sigma2LNORM())))
        })
        
        repartsurvieLNORM_LATEX <- reactive({
            if(input$xlim_LNORM == T)
            {
                "F_{X}"
            }
            else
            {
                "S_{X}"
            }
        })
        
        repartsurvieLNORM <- reactive({
            if(input$xlim_LNORM == T)
            {
                format(plnorm(q = xLNORM(), muLNORM(), sdlog = sqrt(sigma2LNORM())), nsmall = 6, scientific = F)
            }
            else
            {
                format(plnorm(q = xLNORM(), muLNORM(), sqrt(sigma2LNORM()), lower.tail = F), nsmall = 6, scientific = F)
            }
            
        })
        
        # output$range_LNORM_FX_UI <- renderUI({
        #     sliderInput(inputId = 'range_LNORM_FX',
        #                 label = 'Range',
        #                 min = 0,
        #                 max = VaR_lnorm(kappa = 0.99, muLNORM(), sqrt(sigma2LNORM())),
        #                 value = c(0, VaR_lnorm(kappa = 0.99, muLNORM(), sqrt(sigma2LNORM()))),
        #                 dragRange = T,
        #                 width = "100%"
        #     )
        # })
        
        # range_LNORM_FX <- reactive({seq(from = input$range_LNORM_FX[1], to = input$range_LNORM_FX[2])})
        
        densityLNORM <- reactive({format(dlnorm(xLNORM(), 
                                                muLNORM(), 
                                                sqrt(sigma2LNORM())), 
                                         nsmall = 6)
            })
        
        VaRLNORM <- reactive({format(VaR_lnorm(kappa = kLNORM(), 
                                               mu = muLNORM(), 
                                               sig = sqrt(sigma2LNORM())), 
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
        
        output$repartsurvieLNORM <- renderUI({withMathJax(sprintf("$$%s(%s) = %s$$", 
                                                                  repartsurvieLNORM_LATEX(),
                                                                  xLNORM(),
                                                                  repartsurvieLNORM()
        ))})
        
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
        
        output$QxLNORM <- renderPlotly({
            ggplot(data = data.frame(x = c(0,
                                          VaR_lnorm(kappa = 0.99, muLNORM(), sqrt(sigma2LNORM()))
            )
            ),
            aes(x)) + 
                stat_function(fun = plot_choice_LNORM_QX_SERVER(),
                              args = list(meanlog = muLNORM(), sdlog = sqrt(sigma2LNORM()))) + 
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = plot_choice_LNORM_QX_SERVER(),
                    args = list(meanlog = muLNORM(), sdlog = sqrt(sigma2LNORM())),
                    xlim = c(VaRLNORM(), VaR_lnorm(kappa = 0.99, muLNORM(), sqrt(sigma2LNORM()))),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$FxLNORM <- renderPlotly({
            ggplot(data = data.frame(x = c(0,
                                           VaR_lnorm(kappa = 0.99, muLNORM(), sqrt(sigma2LNORM()))
            )
            ),
            aes(x)) + 
                stat_function(fun = plot_choice_LNORM_SERVER(),
                              args = list(meanlog = muLNORM(), sdlog = sqrt(sigma2LNORM()))) + 
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = plot_choice_LNORM_SERVER(),
                    args = list(meanlog = muLNORM(), sdlog = sqrt(sigma2LNORM())),
                    xlim = xlim_LNORM_SERVER(),
                    geom = "area",
                    fill = plot_color_LNORM_SERVER(),
                    alpha = 0.7
                )
        })
        
#### Loi Beta Serveur ####
        
        alphaBETA <- reactive({input$alphaBETA})
        
        betaBETA <- reactive({input$betaBETA})
        
        xBETA <- reactive({input$xBETA})
        
        kBETA <- reactive({input$kBETA})
        
        dBETA <- reactive({input$dBETA})
        
        plot_choice_BETA_SERVER <- reactive({
            if(input$plot_choice_BETA == "Densité")
                dbeta
            else
                pbeta
        })
        
        plot_choice_BETA_QX_SERVER <- reactive({
            if(input$plot_choice_BETA_QX == "Densité")
                dbeta
            else
                pbeta
        })
        
        
        plot_color_BETA_SERVER <- reactive({
            if(input$xlim_BETA == T)
                "Dark Green"
            else
                "Royal Blue"
        })
        
        xlim_BETA_SERVER <- reactive({
            if(input$xlim_BETA == T)
                c(0, xBETA())
            else
                c(xBETA(), 1)
        })
        
        repartsurvieBETA_LATEX <- reactive({
            if(input$xlim_BETA == T)
            {
                "F_{X}"
            }
            else
            {
                "S_{X}"
            }
        })
        
        densityBETA <- reactive({format(dbeta(x = xBETA(), 
                                                  alphaBETA(), 
                                                  betaBETA()), 
                                          nsmall = 6)})
        
        repartsurvieBETA <- reactive({
            if(input$xlim_BETA == T)
            {
                format(pbeta(q = xBETA(), 
                             alphaBETA(), 
                             betaBETA()), 
                       nsmall = 6)
            }
            else
            {
                format(pbeta(q = xBETA(), 
                             alphaBETA(), 
                             betaBETA(),
                             lower.tail = F), 
                       nsmall = 6)
            }
            
        })
        
        # repartBETA <- reactive({format(pbeta(q = xBETA(), 
        #                                          alphaBETA(), 
        #                                          betaBETA()), 
        #                                  nsmall = 6)})
        # 
        # survieBETA <- reactive({format(pbeta(q = xBETA(), 
        #                                      alphaBETA(), 
        #                                      betaBETA(),
        #                                      lower.tail = F), 
        #                                  nsmall = 6)})
        
        VaRBETA <- reactive({format(VaR_beta(k = kBETA(),
                                             alphaBETA(), 
                                             betaBETA()), 
                                      nsmall = 6)
        })
        
        TVaRBETA <- reactive({format(TVaR_beta(k = kBETA(), 
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
        
        output$repartsurvieBETA <- renderUI({withMathJax(sprintf("$$%s(%s) = %s$$", 
                                                                 repartsurvieBETA_LATEX(),
                                                                 xBETA(),
                                                                 repartsurvieBETA()
        ))})
        
        # output$repartBETA <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
        #                                                      xBETA(),
        #                                                      repartBETA()))
        # })
        # 
        # output$survieBETA <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
        #                                                      xBETA(),
        #                                                      survieBETA()))
        # })
        
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
        
        output$QxBETA <- renderPlotly({
            ggplot(data = data.frame(x = c(0, 1)
            ),
            aes(x)) + 
                stat_function(fun = plot_choice_BETA_QX_SERVER(),
                              args = list(alphaBETA(), 
                                          betaBETA())) +
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = plot_choice_BETA_QX_SERVER(),
                    args = list(alphaBETA(), 
                                betaBETA()),
                    xlim = c(VaRBETA(), 1),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$FxBETA <- renderPlotly({
            ggplot(data = data.frame(x = c(0, 1)
            ),
            aes(x)) + 
                stat_function(fun = plot_choice_BETA_SERVER(),
                              args = list(alphaBETA(), 
                                          betaBETA())) +
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = plot_choice_BETA_SERVER(),
                    args = list(alphaBETA(), 
                                betaBETA()),
                    xlim = xlim_BETA_SERVER(),
                    geom = "area",
                    fill = plot_color_BETA_SERVER(),
                    alpha = 0.7
                )
        })
        
        
        # output$FxBETA <- renderPlotly({
        #     ggplot(data = data.frame(x = c(0,1)),
        #     aes(x)) +
        #         stat_function(fun = dbeta,
        #                       args = list(alphaBETA(), 
        #                                   betaBETA())) +
        #         ylab("f(x)") +
        #         theme_classic() +
        #         stat_function(
        #             fun = dbeta,
        #             args = list(alphaBETA(), 
        #                         betaBETA()),
        #             xlim = c(0, xBETA()),
        #             geom = "area",
        #             fill = "red",
        #             alpha = 0.7
        #         )
        # })
        # 
        # output$SxBETA <- renderPlotly({
        #     ggplot(data = data.frame(x = c(0,1)),
        #            aes(x)) +
        #         stat_function(fun = dbeta,
        #                       args = list(alphaBETA(), 
        #                                   betaBETA())) +
        #         ylab("f(x)") +
        #         theme_classic() +
        #         stat_function(
        #             fun = dbeta,
        #             args = list(alphaBETA(), 
        #                         betaBETA()),
        #             xlim = c(xBETA(),1),
        #             geom = "area",
        #             fill = "red",
        #             alpha = 0.7
        #         )
        # })
        # 
        # 
        # output$QxBETA <- renderPlotly({
        #     ggplot(data = data.frame(x = c(0,1)),
        #            aes(x)) +
        #         stat_function(fun = qbeta,
        #                       args = list(alphaBETA(), 
        #                                   betaBETA())) + theme_classic()
        # })
        
        
        
#### Loi Erlang Serveur ####
        
        nERLANG <- reactive({input$nERLANG})
        
        betaERLANG <- reactive({input$betaERLANG})
        
        dERLANG <- reactive({input$dERLANG})
        
        kERLANG <- reactive({input$kERLANG})
        
        xERLANG <- reactive({input$xERLANG})
        
        # plot_choice_ERLANG_SERVER <- reactive({
        #     if(input$plot_choice_ERLANG == "Densité")
        #         derlang
        #     else
        #         perlang
        # })
        
        # plot_choice_ERLANG_QX_SERVER <- reactive({
        #     if(input$plot_choice_ERLANG_QX == "Densité")
        #         derlang
        #     else
        #         perlang
        # })
        
        
        plot_color_ERLANG_SERVER <- reactive({
            if(input$xlim_ERLANG == T)
                "Dark Green"
            else
                "Royal Blue"
        })
        
        xlim_ERLANG_SERVER <- reactive({
            if(input$xlim_ERLANG == T)
                c(0, xERLANG())
            else
                c(xERLANG(), 2 * nERLANG() * betaERLANG())
        })
        
        repartsurvieERLANG_LATEX <- reactive({
            if(input$xlim_ERLANG == T)
            {
                "F_{X}"
            }
            else
            {
                "S_{X}"
            }
        })
        
        repartsurvieERLANG <- reactive({
            if(input$xlim_ERLANG == T)
            {
                format(perlang(x = xERLANG(), 
                               n = nERLANG(), 
                               b = betaERLANG()), 
                       nsmall = 6)
            }
            else
            {
                format(perlang(x = xERLANG(), 
                               n = nERLANG(), 
                               b = betaERLANG(),
                               lower.tail = F), 
                       nsmall = 6)
            }
            
        })
        
        densityERLANG <- reactive({format(derlang(x = xERLANG(),
                                                  nERLANG(),
                                                  betaERLANG()), 
                                        nsmall = 6)})
        
        # repartERLANG <- reactive({format(perlang(x = xERLANG(),
        #                                          nERLANG(),
        #                                          betaERLANG()), 
        #                                nsmall = 6)})
        # 
        # survieERLANG <- reactive({format(perlang(x = xERLANG(),
        #                                          nERLANG(),
        #                                          betaERLANG(),
        #                                          lower.tail = F), 
        #                                nsmall = 6)})
        # 
        # TVaRERLANG <- reactive({format(TVaR_erlang(kappa = input$kERLANG,
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
        
        output$repartsurvieERLANG <- renderUI({withMathJax(sprintf("$$%s(%s) = %s$$", 
                                                                   repartsurvieERLANG_LATEX(),
                                                                   xERLANG(),
                                                                   repartsurvieERLANG()
        ))})
        
        # output$repartERLANG <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
        #                                                      xERLANG(),
        #                                                      repartERLANG()))
        # })
        # 
        # output$survieERLANG <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
        #                                                      xERLANG(),
        #                                                      survieERLANG()))
        # })
        
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
        
        # output$QxERLANG <- renderPlotly({
        #     ggplot(data = data.frame(x = c(0, 2 * nERLANG() * betaERLANG())
        #     ),
        #     aes(x)) + 
        #         stat_function(fun = plot_choice_ERLANG_QX_SERVER(),
        #                       args = list(nERLANG(),
        #                                   betaERLANG())) +
        #         ylab("f(x)") + 
        #         theme_classic() +
        #         stat_function(
        #             fun = plot_choice_ERLANG_QX_SERVER(),
        #             args = list(nERLANG(),
        #                         betaERLANG()),                    
        #             xlim = c(varERLANG(), 
        #                      2 * nERLANG() * betaERLANG()),
        #             geom = "area",
        #             fill = "red",
        #             alpha = 0.7
        #         )
        # })
        
        output$FxERLANG <- renderPlotly({
            ggplot(data = data.frame(x = c(0, 2 * nERLANG() * betaERLANG())
            ),
            aes(x)) + 
                stat_function(
                    # fun = plot_choice_ERLANG_SERVER(),
                    fun = derlang,
                              args = list(n = nERLANG(),
                                          b = betaERLANG())) +
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    # fun = plot_choice_ERLANG_SERVER(),
                    fun = derlang,
                    args = list(n = nERLANG(),
                                b = betaERLANG()),                    
                    xlim = xlim_ERLANG_SERVER(),
                    geom = "area",
                    fill = plot_color_ERLANG_SERVER(),
                    alpha = 0.7
                )
        })
        
        # output$FxERLANG <- renderPlotly({
        #     ggplot(data = data.frame(x = c(0, 2 * nERLANG() * betaERLANG())),
        #            aes(x)) +
        #         stat_function(fun = derlang,
        #                       args = list(nERLANG(),
        #                                   betaERLANG())) +
        #         ylab("f(x)") +
        #         theme_classic() +
        #         stat_function(
        #             fun = derlang,
        #             args = list(nERLANG(),
        #                         betaERLANG()),                    
        #             xlim = c(0, xERLANG()),
        #             geom = "area",
        #             fill = "red",
        #             alpha = 0.7
        #         )
        # })
        # 
        # output$SxERLANG <- renderPlotly({
        #     ggplot(data = data.frame(x = c(0, 2 * nERLANG() * betaERLANG())),
        #            aes(x)) +
        #         stat_function(fun = derlang,
        #                       args = list(nERLANG(),
        #                                   betaERLANG())) +
        #         ylab("f(x)") +
        #         theme_classic() +
        #         stat_function(
        #             fun = derlang,
        #             args = list(nERLANG(),
        #                         betaERLANG()),                    
        #             xlim = c(xERLANG(), 2 * nERLANG() * betaERLANG()),
        #             geom = "area",
        #             fill = "red",
        #             alpha = 0.7
        #         )
        # })        
        # 
        # output$QxERLANG <- renderPlotly({
        #     ggplot(data = data.frame(x = c(0,1)),
        #            aes(x)) +
        #         stat_function(fun = qerlang,
        #                       args = list(nERLANG(),
        #                                   betaERLANG())) + theme_classic()
        # })
        # 
        
        
#### Loi Log-logistique Serveur ####
        
        lambdaLOGLOGIS <- reactive({input$lambdaLOGLOGIS})
        
        tauLOGLOGIS <- reactive({input$tauLOGLOGIS})
        
        dLOGLOGIS <- reactive({input$dLOGLOGIS})
        
        kLOGLOGIS <- reactive({input$kLOGLOGIS})
        
        xLOGLOGIS <- reactive({input$xLOGLOGIS})
        
        plot_choice_LOGLOGIS_SERVER <- reactive({
            if(input$plot_choice_LOGLOGIS == "Densité")
                dllogis
            else
                pllogis
        })
        
        plot_choice_LOGLOGIS_QX_SERVER <- reactive({
            if(input$plot_choice_LOGLOGIS_QX == "Densité")
                dllogis
            else
                pllogis
        })
        
        
        plot_color_LOGLOGIS_SERVER <- reactive({
            if(input$xlim_LOGLOGIS == T)
                "Dark Green"
            else
                "Royal Blue"
        })
        
        xlim_LOGLOGIS_SERVER <- reactive({
            if(input$xlim_LOGLOGIS == T)
                c(0, xLOGLOGIS())
            else
                c(xLOGLOGIS(), VaR_llogis(kappa = 0.999, lam = lambdaLOGLOGIS(), tau = tauLOGLOGIS()))
        })
        
        repartsurvieLOGLOGIS_LATEX <- reactive({
            if(input$xlim_LOGLOGIS == T)
            {
                "F_{X}"
            }
            else
            {
                "S_{X}"
            }
        })
        
        repartsurvieLOGLOGIS <- reactive({
            if(input$xlim_LOGLOGIS == T)
            {
                format(pllogis(q = xLOGLOGIS(), 
                               shape = tauLOGLOGIS(),
                               scale = lambdaLOGLOGIS()
                ), 
                nsmall = 6)
            }
            else
            {
                format(pllogis(q = xLOGLOGIS(), 
                               shape = tauLOGLOGIS(),
                               scale = lambdaLOGLOGIS(),
                               lower.tail = F), 
                       nsmall = 6)
            }
            
        })
        
        densityLOGLOGIS <- reactive({format(dllogis(x = xLOGLOGIS(), 
                                                    shape = tauLOGLOGIS(),
                                                    scale = lambdaLOGLOGIS()
                                                    ), 
                                            nsmall = 6)})
       
        VaRLOGLOGIS <- reactive({format(VaR_llogis(kappa = kLOGLOGIS(), 
                                                   lambdaLOGLOGIS(), 
                                                   tauLOGLOGIS()), 
                                        nsmall = 6)
        })
        
        TVaRLOGLOGIS <- reactive({format(TVaR_llogis(kappa = kLOGLOGIS(),
                                                     lam = lambdaLOGLOGIS(),
                                                     tau = tauLOGLOGIS()
                                                     ), 
                                         nsmall = 6)
        })
        
        EspTronqLOGLOGIS <- reactive({Etronq_llogis(d = dLOGLOGIS(),
                                                    lam = lambdaLOGLOGIS(),
                                                    tau = tauLOGLOGIS()
                                                    )
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
        
        output$repartsurvieLOGLOGIS <- renderUI({withMathJax(sprintf("$$%s(%s) = %s$$", 
                                                                     repartsurvieLOGLOGIS_LATEX(),
                                                                     xLOGLOGIS(),
                                                                     repartsurvieLOGLOGIS()
        ))})
        
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
        
        output$QxLOGLOGIS <- renderPlotly({
            ggplot(data = data.frame(x = c(0, VaR_llogis(kappa = 0.999, lam = lambdaLOGLOGIS(), tau = tauLOGLOGIS()))
            ),
            aes(x)) + 
                stat_function(fun = plot_choice_LOGLOGIS_QX_SERVER(),
                              args = list(shape = tauLOGLOGIS(), 
                                          scale = lambdaLOGLOGIS())) +
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = plot_choice_LOGLOGIS_QX_SERVER(),
                    args = list(shape = tauLOGLOGIS(), 
                                scale = lambdaLOGLOGIS()),
                    xlim = c(VaRLOGLOGIS(), 
                             VaR_llogis(kappa = 0.999, lam = lambdaLOGLOGIS(), tau = tauLOGLOGIS())),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$FxLOGLOGIS <- renderPlotly({
            ggplot(data = data.frame(x = c(0, VaR_llogis(kappa = 0.999, lam = lambdaLOGLOGIS(), tau = tauLOGLOGIS()))
            ),
            aes(x)) + 
                stat_function(fun = plot_choice_LOGLOGIS_SERVER(),
                              args = list(shape = tauLOGLOGIS(), 
                                          scale = lambdaLOGLOGIS())) +
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = plot_choice_LOGLOGIS_SERVER(),
                    args = list(shape = tauLOGLOGIS(), 
                                scale = lambdaLOGLOGIS()),                    
                    xlim = xlim_LOGLOGIS_SERVER(),
                    geom = "area",
                    fill = plot_color_LOGLOGIS_SERVER(),
                    alpha = 0.7
                )
        })
        
#### Loi inverse gaussienne Serveur ####
        
        betaIG <- reactive({input$betaIG})
        
        muIG <- reactive({input$muIG})
        
        dIG_param <- reactive({input$dIG})
        
        kIG <- reactive({input$kIG})
        
        xIG <- reactive({input$xIG})
        
        plot_choice_IG_SERVER <- reactive({
            if(input$plot_choice_IG == "Densité")
                dIG
            else
                pIG
        })
        
        plot_choice_IG_QX_SERVER <- reactive({
            if(input$plot_choice_IG_QX == "Densité")
                dIG
            else
                pIG
        })
        
        
        plot_color_IG_SERVER <- reactive({
            if(input$xlim_IG == T)
                "Dark Green"
            else
                "Royal Blue"
        })
        
        xlim_IG_SERVER <- reactive({
            if(input$xlim_IG == T)
                c(0, xIG())
            else
                c(xIG(), VaR_IG(kappa = 0.999, mu = muIG(), beta = betaIG()))
            # 1.5 * muIG() + 0.5 *betaIG()
        })
        
        repartsurvieIG_LATEX <- reactive({
            if(input$xlim_IG == T)
            {
                "F_{X}"
            }
            else
            {
                "S_{X}"
            }
        })
        
        repartsurvieIG <- reactive({
            if(input$xlim_IG == T)
            {
                format(pIG(q = xIG(), 
                           muIG(),
                           betaIG()
                ), 
                nsmall = 6)
            }
            else
            {
                format(pIG(q = xIG(), 
                           muIG(),
                           betaIG(),
                           lower.tail = F), 
                       nsmall = 6)
            }
            
        })
        
        densityIG <- reactive({format(dIG(x = xIG(), 
                                           mu = muIG(),
                                           beta = betaIG()), 
                                      nsmall = 6)
        })
        
        # repartIG <- reactive({format(pIG(q = xIG(), 
        #                                   mu = muIG(),
        #                                   beta = betaIG()), 
        #                              nsmall = 6)
        # })
        # 
        # survieIG <- reactive({format(pIG(q = xIG(), 
        #                                  mu = muIG(),
        #                                  beta = betaIG(),
        #                                  lower.tail = F), 
        #                              nsmall = 6)
        # })
        
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
        
        EspTronqIG <- reactive({Etronq_IG(d = dIG_param(),
                                          betaIG(),
                                          muIG())
        })
        
        StopLossIG <- reactive({SL_IG(d = dIG_param(),
                                      betaIG(),
                                      muIG())
        })
        
        EspLimIG <- reactive({Elim_IG(d = dIG_param(),
                                      betaIG(),
                                      muIG())
        })
        
        # ExcesMoyIG <- reactive({Mexcess_IG(d = dIG_param(),
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
        
        output$repartsurvieIG <- renderUI({withMathJax(sprintf("$$%s(%s) = %s$$", 
                                                               repartsurvieIG_LATEX(),
                                                               xIG(),
                                                               repartsurvieIG()
        ))})
        
        # output$repartIG <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$",
        #                                                       xIG(),
        #                                                       repartIG()))
        # })
        # 
        # output$survieIG <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
        #                                                       xIG(),
        #                                                       survieIG()))
        # })
        
        output$VaRIG <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                           kIG(),
                                                           VaRIG()))
        })
        
        output$TVaRIG <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
                                                            kIG(),
                                                            TVaRIG()))
        })
        
        output$EspTronqIG <- renderUI({withMathJax(sprintf("$$E[X \\times 1_{\\{X \\leqslant %s\\}}] = %.4f$$",
                                                                dIG_param(),
                                                                EspTronqIG()))
        })
        
        output$StopLossIG <- renderUI({withMathJax(sprintf("$$ \\pi_{%s}(X) = %.4f$$",
                                                                dIG_param(),
                                                                StopLossIG()))
        })
        
        output$EspLimIG <- renderUI({withMathJax(sprintf("$$E[\\text{min}(X;{%s})] = %.4f$$",
                                                              dIG_param(),
                                                              EspLimIG()))
        })
        
        # output$ExcesMoyIG <- renderUI({withMathJax(sprintf("$$e_{%s}(X) = %.4f$$",
        #                                                         dIG_param(),
        #                                                         ExcesMoyIG()))
        # })
        
        output$QxIG <- renderPlotly({
            ggplot(data = data.frame(x = c(0, VaR_IG(kappa = 0.999, mu = muIG(), beta = betaIG()))
            ),
            aes(x)) + 
                stat_function(fun = plot_choice_IG_QX_SERVER(),
                              args = list(muIG(),
                                          betaIG())) +
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = plot_choice_IG_QX_SERVER(),
                    args = list(muIG(),
                                betaIG()),                    
                    xlim = c(VaRIG(), 
                             VaR_IG(kappa = 0.999, mu = muIG(), beta = betaIG())),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$FxIG <- renderPlotly({
            ggplot(data = data.frame(x = c(0, VaR_IG(kappa = 0.999, mu = muIG(), beta = betaIG()))
            ),
            aes(x)) + 
                stat_function(fun = plot_choice_IG_SERVER(),
                              args = list(muIG(),
                                          betaIG())) +
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = plot_choice_IG_SERVER(),
                    args = list(muIG(),
                                betaIG()),                    
                    xlim = xlim_IG_SERVER(),
                    geom = "area",
                    fill = plot_color_IG_SERVER(),
                    alpha = 0.7
                )
        })
        
        # output$FxIG <- renderPlotly({
        #     ggplot(data = data.frame(x = c(0, 1.5 * muIG() + 0.5 *betaIG())),
        #            aes(x)) +
        #         stat_function(fun = dIG,
        #                       args = list(muIG(),
        #                                   betaIG())) +
        #         ylab("f(x)") +
        #         theme_classic() +
        #         stat_function(
        #             fun = dIG,
        #             args = list(muIG(),
        #                         betaIG()),                    
        #             xlim = c(0, xERLANG()),
        #             geom = "area",
        #             fill = "red",
        #             alpha = 0.7
        #         )
        # })
        # 
        # output$SxIG <- renderPlotly({
        #     ggplot(data = data.frame(x = c(0, 1.5 * muIG() + 0.5 *betaIG())),
        #            aes(x)) +
        #         stat_function(fun = dIG,
        #                       args = list(muIG(),
        #                                   betaIG())) +
        #         ylab("f(x)") +
        #         theme_classic() +
        #         stat_function(
        #             fun = dIG,
        #             args = list(muIG(),
        #                         betaIG()),                    
        #             xlim = c(xERLANG(), 1.5 * muIG() + 0.5 *betaIG()),
        #             geom = "area",
        #             fill = "red",
        #             alpha = 0.7
        #         )
        # })        
    
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
        
        output$xUNIC_SERVER <- renderUI({
            numericInput('xUNIC', '$$x$$', min = aUNIC(), value = 0.5, max = bUNIC(), step = 1)
        })
        
        
        plot_choice_UNIC_SERVER <- reactive({
            if(input$plot_choice_UNIC == "Densité")
                dunif
            else
                punif
        })
        
        plot_choice_UNIC_QX_SERVER <- reactive({
            if(input$plot_choice_UNIC_QX == "Densité")
                dunif
            else
                punif
        })
        
        
        plot_color_UNIC_SERVER <- reactive({
            if(input$xlim_UNIC == T)
                "Dark Green"
            else
                "Royal Blue"
        })
        
        xlim_UNIC_SERVER <- reactive({
            if(input$xlim_UNIC == T)
                c(aUNIC(), min(xUNIC(), bUNIC()))
            else
                c(xUNIC(), bUNIC())
        })
        
        repartsurvieUNIC_LATEX <- reactive({
            if(input$xlim_UNIC == T)
            {
                "F_{X}"
            }
            else
            {
                "S_{X}"
            }
        })
        
        repartsurvieUNIC <- reactive({
            if(input$xlim_UNIC == T)
            {
                format(punif(q = xUNIC(), 
                             min = aUNIC(), 
                             max = bUNIC()), 
                       nsmall = 6)
            }
            else
            {
                format(1 - punif(q = xUNIC(), 
                                 min = aUNIC(),
                                 max = bUNIC()), 
                       nsmall = 6)
            }
            
        })
        
        
        meanUNIC <- reactive({E_unif(aUNIC(), bUNIC())
        })
        
        varUNIC <- reactive({
            format(V_unif(aUNIC(), bUNIC()), 
                   nsmall = 6)
        })
        
        densityUNIC <- reactive({format(dunif(x = xUNIC(), min = aUNIC(), max = bUNIC()),
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
        
        output$repartsurvieUNIC <- renderUI({withMathJax(sprintf("$$%s(%s) = %s$$", 
                                                                 repartsurvieUNIC_LATEX(),
                                                                 xUNIC(),
                                                                 repartsurvieUNIC()
        ))})
        
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
        
        output$QxUNIC <- renderPlotly({
            ggplot(data = data.frame(x = c(aUNIC(),
                                           bUNIC()
            )
            ),
            aes(x)) + 
                stat_function(fun = plot_choice_UNIC_QX_SERVER(),
                              args = list(min = aUNIC(), max = bUNIC())) +
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = plot_choice_UNIC_QX_SERVER(),
                    args = list(min = aUNIC(), max = bUNIC()),
                    xlim = c(VaRUNIC(), 
                             bUNIC()),
                    geom = "area",
                    fill = "red",
                    alpha = 0.7
                )
        })
        
        output$FxUNIC <- renderPlotly({
            ggplot(data = data.frame(x = c(aUNIC(),
                                           bUNIC()
            )
            ),
            aes(x)) + 
                stat_function(fun = plot_choice_UNIC_SERVER(),
                              args = list(min = aUNIC(), max = bUNIC())) +
                ylab("f(x)") + 
                theme_classic() +
                stat_function(
                    fun = plot_choice_UNIC_SERVER(),
                    args = list(min = aUNIC(), max = bUNIC()),
                    xlim = xlim_UNIC_SERVER(),
                    geom = "area",
                    fill = plot_color_UNIC_SERVER(),
                    alpha = 0.7
                )
        })
        # 
        # output$FxUNIC <- renderPlotly({
        #     ggplot(data = data.frame(x = c(
        #         aUNIC(), bUNIC())
        #     ),
        #     aes(x)) +
        #         stat_function(fun = dunif,
        #                       args = list(min = aUNIC(), max = bUNIC())) +
        #         ylab("f(x)") +
        #         theme_classic() +
        #         stat_function(
        #             fun = dunif,
        #             args = list(min = aUNIC(), max = bUNIC()),
        #             xlim = c(aUNIC(), xUNIC()),
        #             geom = "area",
        #             fill = "red",
        #             alpha = 0.7
        #         )
        # })
        # 
        # output$SxUNIC <- renderPlotly({
        #     ggplot(data = data.frame(x = c(
        #         aUNIC(), bUNIC())
        #     ),
        #     aes(x)) +
        #         stat_function(fun = dunif,
        #                       args = list(min = aUNIC(), max = bUNIC())) +
        #         ylab("f(x)") +
        #         theme_classic() +
        #         stat_function(
        #             fun = dunif,
        #             args = list(min = aUNIC(), max = bUNIC()),
        #             xlim = c(xUNIC(), bUNIC()),
        #             geom = "area",
        #             fill = "red",
        #             alpha = 0.7
        #         )
        # })        
        # 
        # output$QxUNIC <- renderPlotly({
        #     ggplot(data = data.frame(x = c(0,1)),
        #            aes(x)) +
        #         stat_function(fun = qunif,
        #                       args = list(min = aUNIC(), max = bUNIC())) + theme_classic()
        # })
        # 
        # 
        
        
#### Loi Binomiale Serveur ####
        
        nBIN <- reactive({input$nBIN})
        
        pBIN <- reactive({input$pBIN})
        
        xBIN <- reactive({input$xBIN})
        
        kBIN <- reactive({input$kBIN})
        
        plot_choice_BIN_SERVER <- reactive({
            if(input$plot_choice_BIN == "Fonction de masse")
                dbinom(x = 0:nBIN(), size = nBIN(), prob = pBIN())
            else
                pbinom(q = 0:nBIN(), size = nBIN(), prob = pBIN())
        })
        
        repartsurvieBIN <- reactive({
            if(input$xlim_BIN == T)
            {
                format(pbinom(xBIN(), nBIN(), pBIN()), nsmall = 6, scientific = F)
            }
            else
            {
                format(pbinom(xBIN(), nBIN(), pBIN(), lower.tail = F), nsmall = 6, scientific = F)
            }
            
        })
        
        repartsurvieBIN_LATEX <- reactive({
            if(input$xlim_BIN == T)
            {
                "F_{X}"
            }
            else
            {
                "S_{X}"
            }
        })
        
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
                    shinyjs::hide("nBIN")
                else
                    shinyjs::show("nBIN")
                
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
        
        VaRBIN <- reactive({format(VaR_binom(kappa = kBIN(),
                                             nBIN(),
                                             pBIN()),
                                   nsmall = 6)
            })
        
        TVaRBIN <- reactive({format(TVaR_binom(kappa = kBIN(),
                                               nBIN(),
                                               pBIN()),
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
        
        output$repartsurvieBIN <- renderUI({withMathJax(sprintf("$$%s(%s) = %s$$", 
                                                                repartsurvieBIN_LATEX(),
                                                                xBIN(),
                                                                repartsurvieBIN()
        ))})
        
        output$VaRBIN <- renderUI({withMathJax(sprintf("$$VaR_{%s} = %s$$",
                                                       input$kBIN,
                                                       VaRBIN()
        ))})
        output$TVaRBIN <- renderUI({withMathJax(sprintf("$$TVaR_{%s} = %s$$",
                                                        input$kBIN,
                                                        TVaRBIN()
        ))})
        
        output$FxBIN <- renderPlotly({
        ggplot(
            data.frame(
                x = 0:nBIN(),
                prob = plot_choice_BIN_SERVER(),
                col_fill = ifelse(0:nBIN() <= xBIN(), "Fx", "Sx")
            ),
            aes(x = x, y = prob)
        ) +
            geom_bar(
                stat = "identity",
                aes(fill = col_fill),
                alpha = 0.7,
                width = 0.3
            ) +
            guides(fill = F) +
            theme_classic() +
            ylab("Pr(X = x)")
        })
        
        # Reactive slider
        observeEvent(nBIN(),
                     {
                         updateSliderInput(session = session, 
                                           inputId = "xBIN",
                                           max = nBIN()
                         )
                     }
        )
    
#### Loi Poisson Serveur ####
        
    xPOI <- reactive({input$xPOI})
        
    kPOI <- reactive({input$kPOI})
    
    dPOI <- reactive({input$dPOI})
        
    lamPOI <- reactive({input$lamPOI})
    
    densityPOI <- reactive({format(dpois(xPOI(), lamPOI()), nsmall = 6)})
    
    repartsurviePOI <- reactive({
        if(input$xlim_POI == T)
        {
            format(ppois(xPOI(), lamPOI()), nsmall = 6)
        }
        else
        {
            format(ppois(xPOI(), lamPOI(), lower.tail = F), nsmall = 6)
        }
        
    })
    
    repartsurviePOI_LATEX <- reactive({
        if(input$xlim_POI == T)
        {
            "F_{X}"
        }
        else
        {
            "S_{X}"
        }
    })
    
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
    
    output$repartsurviePOI <- renderUI({withMathJax(sprintf("$$%s(%s) = %s$$", 
                                                            repartsurviePOI_LATEX(),
                                                            xPOI(),
                                                            repartsurviePOI()
    ))})
    
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
                     value = 2, 
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
    
    xlim_BN_SERVER <- reactive({
        if(definitionBN() == T)
            c(rBN(), 0:qnbinom(p = 0.99999, size = rBN(), prob = qBN()))
        else
            c(0:qnbinom(p = 0.99999, size = rBN(), prob = qBN()))
    })
    
    plot_choice_BN_SERVER <- reactive({
        if(input$plot_choice_BN == "Fonction de masse")
            d_negbinom(k = xlim_BN_SERVER(), r = rBN(), p = qBN(), nb_tries = definitionBN())
        else
            p_negbinom(k = xlim_BN_SERVER(), r = rBN(), p = qBN(), nb_tries = definitionBN())
        
    })
    
    repartsurvieBN <- reactive({
        if(input$xlim_BN == T)
        {
            format(p_negbinom(k = xBN(), r = rBN(), p = qBN(), nb_tries = definitionBN(), lower.tail = T), nsmall = 6, scientific = F)
        }
        else
        {
            format(p_negbinom(k = xBN(), r = rBN(), p = qBN(), nb_tries = definitionBN(), lower.tail = F), nsmall = 6, scientific = F)
        }
        
    })
    
    repartsurvieBN_LATEX <- reactive({
        if(input$xlim_BN == T)
        {
            "F_{X}"
        }
        else
        {
            "S_{X}"
        }
    })
    
    output$changingqBN <- renderUI({
        numericInput('qBN', label = '$$q$$', value = 0.5, min = 0, step = 0.1)
    })
    
    output$changingrBN <- renderUI({
        numericInput('rBN', label = '$$r$$', value = 1, min = 0, step = 1)
    })
    
    output$changingxBN <- renderUI({
        numericInput('xBN', '$$x$$', 
                     # min = 0, 
                     min = {
                         if (definitionBN() == T) {
                             rBN()
                         } 
                         else {
                             0
                         }
                     },
                     value = {
                         if (definitionBN() == T) {
                             rBN()
                         } 
                         else {
                             0
                         }
                     }, 
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
            shinyjs::hide("rBN")
        else
            shinyjs::show("rBN")
        
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
    
    output$repartsurvieBN <- renderUI({withMathJax(sprintf("$$%s(%s) = %s$$", 
                                                           repartsurvieBN_LATEX(),
                                                           xBN(),
                                                           repartsurvieBN()
    ))})
    
    output$FxBN <- renderPlotly({
        ggplot(
            data.frame(
                x = xlim_BN_SERVER(),
                prob = plot_choice_BN_SERVER(),
                col_fill = ifelse(xlim_BN_SERVER() <= xBN(), "Fx", "Sx")
            ),
            aes(x = x, y = prob)
        ) +
            geom_bar(
                stat = "identity",
                aes(fill = col_fill),
                alpha = 0.7,
                width = 0.3
            ) +
            guides(fill = F) +
            theme_classic() +
            ylab("Pr(X = x)")
    })
    
#### Loi Uniforme Discrète Serveur ####
    
    aUNID <- reactive({input$aUNID})
    
    bUNID <- reactive({input$bUNID})
    
    xUNID <- reactive({input$xUNID})
    
    kUNID <- reactive({input$kUNID})
    
    dUNID <- reactive({input$dUNID})
    
    repartsurvieUNID <- reactive({
        if(input$xlim_UNID == T)
        {
            format(punifD(k = xUNID(), aUNID(), bUNID()), nsmall = 6)
        }
        else
        {
            format( 1 - punifD(k = xUNID(), aUNID(), bUNID()), nsmall = 6)
        }
        
    })
    
    repartsurvieUNID_LATEX <- reactive({
        if(input$xlim_UNID == T)
        {
            "F_{X}"
        }
        else
        {
            "S_{X}"
        }
    })
    
    output$repartsurvieUNID <- renderUI({withMathJax(sprintf("$$%s(%s) = %s$$", 
                                                           repartsurvieUNID_LATEX(),
                                                           xUNID(),
                                                           repartsurvieUNID()
    ))})
    
    meanUNID <- reactive({E_unifD(aUNID(), bUNID())
    })
    
    varUNID <- reactive({
        format(V_unifD(aUNID(), bUNID()), 
               nsmall = 6)
    })
    
    densityUNID <- reactive({format(dunifD(x = xUNID(), aUNID(), bUNID()),
                                    nsmall = 6)
    })
    # 
    # repartUNID <- reactive({format(punifD(k = xUNID(), aUNID(), bUNID()), 
    #                                nsmall = 6)
    # })
    # 
    # survieUNID <- reactive({format(1 - punifD(k = xUNID(), aUNID(), bUNID()), 
    #                                nsmall = 6)
    # })
    
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
    # 
    # output$repartUNID <- renderUI({withMathJax(sprintf("$$F_{X}(%s) = %s$$", 
    #                                                   xUNID(),
    #                                                   repartUNID()
    # ))})
    # 
    # output$survieUNID <- renderUI({withMathJax(sprintf("$$S_{X}(%s) = %s$$",
    #                                                   xUNID(),
    #                                                   survieUNID()))
    # })

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
    
    # Crée ce input avec UI pour le cacher dans le cas d'une lognormale
    output$koBNCOMPUI <- renderUI({
        if (input$severityBNCOMP == "Gamma") {
            numericInput('koBNCOMP', label = withMathJax('$$k_{0}$$'), value = 300, step = 100, min = 0, max = 1)
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
        # Cacher la fonction de répartition si lognormale
        if(x == "Lognormale")
        {
            hideElement("Repartition-box-BNCOMP")
            hideElement("Quantile-box-BNCOMP")
        }
        else if(x == "Gamma")
        {
            showElement("Repartition-box-BNCOMP")
            showElement("Quantile-box-BNCOMP")
        }
        
        # rend le paramètre impossible à modifier pour l'utilisateur
        if (x == "Lognormale")
        {
            shinyjs::hide("distrchoiceGAMMA")
        }
        else
        {
            shinyjs::show("distrchoiceGAMMA")
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
    
    repartBNCOMP <- reactive({format(p_BNCOMP(x = xBNCOMP(), 
                                              r = rBNCOMP(),
                                              q = qBNCOMP(),
                                              shapeBNCOMP(), 
                                              rateBNCOMP(),
                                              ko = koBNCOMP(),
                                              distr_severity = input$severityBNCOMP), 
                                     nsmall = 6, scientific = F)
    })
    
    survieBNCOMP <- reactive({format(1 - p_BNCOMP(x = xBNCOMP(), 
                                                  r = rBNCOMP(),
                                                  q = qBNCOMP(),
                                                  ko = koBNCOMP(),
                                                  shapeBNCOMP(), 
                                                  rateBNCOMP(),
                                                  distr_severity = input$severityBNCOMP
                                                  ), 
                                     nsmall = 6, scientific = F)})

    VaRBNCOMP <- reactive({format(VaR_BNCOMP(kappa = kBNCOMP(), 
                                             ko = koBNCOMP(),
                                             q     = qBNCOMP(),
                                             r     = rBNCOMP(),
                                             shapeBNCOMP(), 
                                             rateBNCOMP()
    ), nsmall = 6)})
    
    varkBNCOMP <- reactive({VaR_BNCOMP(kappa = kBNCOMP(), 
                                       ko = koBNCOMP(),
                                       r     = rBNCOMP(),
                                       q     = qBNCOMP(),
                                       shapeBNCOMP(), 
                                       rateBNCOMP()
                                       )})
    
    TVaRBNCOMP <- reactive({format(TVaR_BNCOMP(kappa     = kBNCOMP(),
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
    
    meanBNCOMP <- reactive({format(E_BNCOMP(shapeBNCOMP(), 
                                            rateBNCOMP(),
                                            r     = rBNCOMP(),
                                            q     = qBNCOMP(),
                                            distr_severity = input$severityBNCOMP
    ),
    nsmall = 6,
    scientific = F)
    })
    
    varianceBNCOMP <- reactive({format(V_BNCOMP(shapeBNCOMP(), 
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
    
    # Crée ce input avec UI pour le cacher dans le cas d'une lognormale
    output$koPCOMPUI <- renderUI({
        if (input$severityBINCOMP == "Gamma") {
            numericInput('koPCOMP', label = withMathJax('$$k_{0}$$'), value = 200, step = 100, min = 0, max = 1)
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
        
        # Cacher la fonction de répartition si lognormale
        if(x == "Lognormale")
        {
            hideElement("Repartition-box-PCOMP")
            hideElement("Quantile-box-PCOMP")
        }
        else if(x == "Gamma")
        {
            showElement("Repartition-box-PCOMP")
            showElement("Quantile-box-PCOMP")
        }
        
        # rend le paramètre impossible à modifier pour l'utilisateur
        if (x == "Lognormale")
        {
            shinyjs::hide("distrchoiceGAMMA")
        }
        else
        {
            shinyjs::show("distrchoiceGAMMA")
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
    
    repartPCOMP <- reactive({format(p_PCOMP(x = xPCOMP(), 
                                            lambdaPCOMP(),
                                            shapePCOMP(), 
                                            ratePCOMP(),
                                            ko = koPCOMP(),
                                            distr_severity = input$severityPCOMP), 
                                    nsmall = 6, scientific = F)
    })
    
    surviePCOMP <- reactive({format(1 - p_PCOMP(x = xPCOMP(), 
                                            lambdaPCOMP(),
                                            shapePCOMP(), 
                                            ratePCOMP(),
                                            ko = koPCOMP(),
                                            distr_severity = input$severityPCOMP), 
                                    nsmall = 6, scientific = F)
        })
    
    VaRPCOMP <- reactive({format(VaR_PCOMP(kappa = kPCOMP(), 
                                           lambdaPCOMP(),
                                           shapePCOMP(), 
                                           ratePCOMP(),
                                           ko = koPCOMP()
    ), nsmall = 6)})
    
    varkPCOMP <- reactive({VaR_PCOMP(kappa = kPCOMP(), 
                                     lambdaPCOMP(),
                                     shapePCOMP(), 
                                     ratePCOMP(),
                                     ko = koPCOMP()
    )})
    
    TVaRPCOMP <- reactive({format(TVaR_PCOMP(kappa = kPCOMP(),
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
    
    # Crée le paramètres d'échelle et de forme avec UI pour changer le label selon la distribution
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
    
    # Créer ce input avec UI pour le cacher dans le cas d'une lognormale
    output$koBINCOMPUI <- renderUI({
        if (input$severityBINCOMP == "Gamma") {
            numericInput('koBINCOMP', label = withMathJax('$$k_{0}$$'), value = 300, step = 100, min = 0, max = 1)
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
        
        if(x == "Lognormale")
        {
            hideElement("Repartition-box-BINCOMP")
            hideElement("Quantile-box-BINCOMP")
        }
        else if(x == "Gamma")
        {
            showElement("Repartition-box-BINCOMP")
            showElement("Quantile-box-BINCOMP")
        }
        
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
            shinyjs::hide("distrchoiceGAMMA")
        }
        else if (x == "Gamma")
        {
            shinyjs::show("distrchoiceGAMMA")
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
    
    repartBINCOMP <- reactive({format(p_BINCOMP(x = xBINCOMP(), 
                                              n = nBINCOMP(),
                                              q = qBINCOMP(),
                                              shapeBINCOMP(), 
                                              rateBINCOMP(),
                                              ko = koBINCOMP(),
                                              distr_severity = input$severityBINCOMP), 
                                     nsmall = 6, scientific = F)
    })
    
    survieBINCOMP <- reactive({format(1 - p_BINCOMP(x = xBINCOMP(), 
                                                  n = nBINCOMP(),
                                                  q = qBINCOMP(),
                                                  shapeBINCOMP(), 
                                                  rateBINCOMP(),
                                                  ko = koBINCOMP(),
                                                  distr_severity = input$severityBINCOMP), nsmall = 6, scientific = F)})
    
    VaRBINCOMP <- reactive({format(VaR_BINCOMP(kappa = kBINCOMP(), 
                                             n     = nBINCOMP(),
                                             q     = qBINCOMP(),
                                             shapeBINCOMP(), 
                                             rateBINCOMP(),
                                             ko = koBINCOMP()
    ), nsmall = 6)})
    
    varkBINCOMP <- reactive({VaR_BINCOMP(kappa = kBINCOMP(), 
                                       n     = nBINCOMP(),
                                       q     = qBINCOMP(),
                                       rateBINCOMP(),
                                       shapeBINCOMP(), 
                                       ko = koBINCOMP()
    )})
    
    TVaRBINCOMP <- reactive({format(TVaR_BINCOMP(kappa    = kBINCOMP(),
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
    
    meanBINCOMP <- reactive({format(E_BINCOMP(
                                            n     = nBINCOMP(),
                                            q     = qBINCOMP(),
                                            shapeBINCOMP(), 
                                            rateBINCOMP(),
                                            distr_severity = input$severityBINCOMP
    ),
    nsmall = 6,
    scientific = F)
    })
    
    varianceBINCOMP <- reactive({format(V_BINCOMP(
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
