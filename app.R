# Library
{
library(shiny)
library(actuar)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(rsconnect)
library(plotly)
}
source(file = "functions.R")


# Corps
{
ui <- dashboardPage(skin = "blue", dashboardHeader(title = "Lois de probabilité", titleWidth = 200),
  
  # Paneau Latéral                  
  {
  dashboardSidebar(collapsed = F, sidebarMenu(
  menuItem("Loi normale", tabName = "Normale", icon = icon("neos")),
  menuItem("Loi gamma", icon = icon("gofore"), tabName = "gamma"),
  menuItem("À propos", icon = icon("user-tie"), tabName = "about")))
  },
  
      dashboardBody(
          tags$head(
              tags$style(type = "text/css", "label{ display: table-cell; text-align: center; vertical-align: center; width: 50px; font-size: 13pt} 
                         .form-group { display: table-row;}")
              ),
          tabItems(
        
        # LOI NORMALE
        {tabItem(tabName = "Normale",
        fluidPage(
          titlePanel("Loi Normale"), withMathJax(), helpText("\\(X \\sim\\mathcal{N}(\\mu, \\sigma^2)\\)"), align = "center"),
          fluidRow(
    column(width = 2, 
           box(title = "Paramètres", status = "primary", solidHeader = T, width = NULL,
               numericInput('muNORM', withMathJax('$$\\mu$$'), value = 0),
               numericInput('sigmaNORM', '$$\\sigma^2$$', value = 1))

    ),
    
    ## Moments
    column(width = 3,
           tags$style(" * {font-size:20px}"), # grosseur du tezte
           box(
             title = "Moments", width = NULL, solidHeader = TRUE, status = "warning",
             uiOutput("meanNORM"), 
             uiOutput("varNORM"), 
             align = "center"),
           
           box(
             title = "Autres Moments", width = NULL, solidHeader = TRUE, status = "warning", 
             numericInput('dNORM', withMathJax('$$d$$'), value = 0, width = "20px"),
             # radioButtons('equalityNORM', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
             uiOutput("EspTronqNORM"), 
             uiOutput("EspLimNORM"), 
             uiOutput("StopLossNORM"), 
             uiOutput("ExcesMoyNORM")),
           align = "center"
    ),
    
    ## Fonctions
    column(width = 3,
           box(
             title = "Fonctions", width = NULL, solidHeader = TRUE, 
             tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du tezte
             status = "danger", # pour couleur de la boite, diff couleur pour statut
             numericInput('xNORM', '$$x$$', value = 0), 
             uiOutput("densityNORM"), 
             uiOutput("repartNORM"),
             plotlyOutput("FxNORM"),
             align = "center"
           )
           
    ),
    
    column(width =3,
           boxPlus(
             title = "Mesure de risques", width = NULL, solidHeader = TRUE, status = "success",
             tags$style(" * {font-size:20px;}"), # ligne qui augmente la grosseur du texte
             numericInput('kNORM', '$$\\kappa$$', value = 0.99, step = 0.005), 
             uiOutput("VaRNORM"), 
             uiOutput("TVaRNORM"), 
             align = "center"
           )
    )
  )
)
},

        # LOI GAMMA
        {tabItem(tabName = "gamma",
         fluidPage(
           titlePanel("Loi Gamma"), withMathJax(), helpText("\\(X \\sim\\mathcal{G}(\\alpha, \\beta)\\)"), align = "center"),
         
         fluidRow(column(width = 2, 
                         box(title = "Paramètres", status = "primary", solidHeader = T, width = NULL,
                             numericInput('alphaGAMMA', withMathJax('$$\\alpha$$'), value = 1),
                             numericInput('betaGAMMA', '$$\\beta$$', value = 0.1)),
                         
                         align = "center"
         ),
         
         ## Moments
         column(width = 3,
                tags$style(" * {font-size:20px;}"), # grosseur du tezte
                box(
                    title = "Moments", width = NULL, solidHeader = TRUE, status = "warning",
                    uiOutput("meanGAMMA"), 
                    uiOutput("varianceGAMMA"), 
                    align = "center"
                ),
                box(
                    title = "Autres Moments", width = NULL, solidHeader = TRUE, status = "warning", 
                    numericInput('dGAMMA', withMathJax('$$d$$'), value = 0, width = "20px"),
                    # radioButtons('equalityGAMMA', label = "", choices = c("$$\\geq$$", "$$\\leq$$"), inline = T),
                    uiOutput("EspTronqGAMMA"), 
                    uiOutput("EspLimGAMMA"), 
                    uiOutput("StopLossGAMMA"), 
                    uiOutput("ExcesMoyGAMMA"),
                    align = "left", 
                    align = "center")
         ),
         
         ## Fonctions
         column(width = 3,
                box(
                    title = "Fonctions", width = NULL, solidHeader = TRUE, 
                    tags$style(" * {font-size:20px;}"), # grosseur du tezte
                    status = "danger", # pour couleur de la boite, diff couleur selon le statut
                    numericInput('xGAMMA', '$$x$$', value = 10), 
                    uiOutput("densityGAMMA"), 
                    uiOutput("repartGAMMA"),
                    plotlyOutput("FxGAMMA"),
                    align = "center"
                )
                
         ),
         
         column(width = 3,
                boxPlus(
                    title = "Mesure de risques", width = NULL, solidHeader = TRUE, status = "success",
                    tags$style(" * {font-size:20px;}"), # grosseur du tezte
                    numericInput('kGAMMA', '$$\\kappa$$', value = 0.99, step = 0.005), 
                    uiOutput("VaRGAMMA"), 
                    uiOutput("TVaRGAMMA"), 
                    align = "center"
                )
         )
  )
)},
        

        # À PROPOS
        {
  tabItem(tabName = "about", h2("Nous contacter "), br(), widgetUserBox(
  title = "Marc-André Devost",
  subtitle = "marc-andre.devost.1@ulaval.ca",
  type = NULL,
  src = "marc.jpg",
  color = "blue", collapsible = F, ""), 
  
  widgetUserBox(
    title = "Alec James van Rassel",
    subtitle = "alec.van-rassel.1@ulaval.ca",
    type = NULL,
    src = "alec.jpg",
    color = "blue", collapsible = F, "")
  )
      }

)))
}


## Serveur
server <- function(input, output) 
{
    # SERVEUR LOI NORMALE
    {
  muNORM <- reactive({input$muNORM})
  
  sigma2NORM <- reactive({input$sigmaNORM})

  densityNORM <- reactive({format(dnorm(input$xNORM, muNORM(), sqrt(sigma2NORM())), nsmall = 6)})
  
  repartNORM <- reactive({format(pnorm(input$xNORM, muNORM(), sqrt(sigma2NORM())), nsmall = 6)})
  
  VaRNORM <- reactive({format(qnorm(input$kNORM, muNORM(), sqrt(sigma2NORM())), nsmall = 6)})
  
  TVaRNORM <- reactive({format(muNORM() + (1/(1 - input$kNORM)) * sqrt(sigma2NORM()/(2*pi)) * exp(-qnorm(input$kNORM, muNORM(), sqrt(sigma2NORM()))/2), nsmall = 6)})
  
  EspTronqNORM <- reactive({muNORM() * pnorm((input$dNORM - muNORM())/sqrt(sigma2NORM()), 0, 1) - sqrt(sigma2NORM()/ 2 * pi) * exp(-(input$dNORM - muNORM())^2/2 * sigma2NORM())})
  
  StopLossNORM <- reactive({(muNORM() + input$dNORM) * (1 - pnorm((input$dNORM - muNORM())/sqrt(sigma2NORM()), 0, 1)) - sqrt(sigma2NORM()/ 2 * pi) * exp(-(input$dNORM - muNORM())^2/2 * sigma2NORM())})
  
  EspLimNORM <- reactive({muNORM() * pnorm((input$dNORM - muNORM())/sqrt(sigma2NORM()), 0, 1) - sqrt(sigma2NORM()/ 2 * pi)* exp(-(input$dNORM - muNORM())^2/2 * sigma2NORM()) + input$dNORM * (1 - pnorm((input$dNORM - muNORM())/sqrt(sigma2NORM()), 0, 1))})
  
  ExcesMoyNORM <- reactive({muNORM() + input$dNORM - 1/(1 - pnorm((input$dNORM - muNORM())/sqrt(sigma2NORM()), 0, 1)) * sqrt(sigma2NORM()/ 2 * pi)* exp(-(input$dNORM - muNORM())^2/(2 * sigma2NORM()))})
  
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
  
  output$FxNORM <- renderPlotly({ggplot(data = data.frame(x = c(muNORM() - 4 * sqrt(sigma2NORM()),muNORM() + 4 * sqrt(sigma2NORM()))), 
                   aes(x)) + stat_function(fun = dnorm, args = list(mean = muNORM(), sd = sqrt(sigma2NORM()))) + ylab("f(x)") + theme_classic() + 
                  stat_function(fun = dnorm, args = list(mean = muNORM(), sd = sqrt(sigma2NORM())), 
                  xlim = c(muNORM() - 4 * sqrt(sigma2NORM()), input$xNORM), geom = "area", fill = "red", alpha = 0.7)
                  
                  })
                  
  
  }
    
    # SERVEUR LOI GAMMA
    {
        betaGAMMA <- reactive({input$betaGAMMA})
        
        alphaGAMMA <- reactive({input$alphaGAMMA})
        
        densityGAMMA <- reactive({format(dgamma(input$xGAMMA, alphaGAMMA(), betaGAMMA()),scientific = F,  nsmall = 6)})
        
        repartGAMMA <- reactive({format(pgamma(input$xGAMMA, alphaGAMMA(), betaGAMMA()), nsmall = 6, scientific = F)})
        
        VaRGAMMA <- reactive({format(qgamma(input$kGAMMA, alphaGAMMA(), betaGAMMA()), nsmall = 6)})
        VaRTVARGAMMA <- reactive({qgamma(input$kGAMMA, alphaGAMMA(), betaGAMMA())})
        
        TVaRGAMMA <- reactive({format((alphaGAMMA()/betaGAMMA()) * (1/(1 - input$kGAMMA)) * pgamma(VaRTVARGAMMA(), alphaGAMMA() + 1, betaGAMMA(), lower.tail = F), nsmall = 6)})
        
        EspTronqGAMMA <- reactive({(alphaGAMMA()/betaGAMMA()) * pgamma(input$dGAMMA, alphaGAMMA() + 1, betaGAMMA())})
        
        StopLossGAMMA <- reactive({(alphaGAMMA()/betaGAMMA()) * pgamma(input$dGAMMA, alphaGAMMA() + 1, betaGAMMA(), lower.tail = F) - input$dGAMMA * pgamma(input$dGAMMA, alphaGAMMA(), betaGAMMA(), lower.tail = F)})
        
        EspLimGAMMA <- reactive({(alphaGAMMA()/betaGAMMA()) * pgamma(input$dGAMMA, alphaGAMMA() + 1, betaGAMMA()) + input$dGAMMA * pgamma(input$dGAMMA, alphaGAMMA(), betaGAMMA(), lower.tail = F)})
        
        ExcesMoyGAMMA <- reactive({(alphaGAMMA()/betaGAMMA()) * (pgamma(input$dGAMMA, alphaGAMMA() + 1, betaGAMMA(), lower.tail = F))/(pgamma(input$dGAMMA, alphaGAMMA(), betaGAMMA(), lower.tail = F)) - input$dGAMMA})
        
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
        
        output$FxGAMMA <- renderPlotly({ggplot(data = data.frame(x = c(0,meanGAMMA() + 3 * sqrt(varianceGAMMA()))), 
                                              aes(x)) + stat_function(fun = dgamma, args = list(shape = alphaGAMMA(), rate = betaGAMMA())) + ylab("f(x)") + theme_classic() +
          stat_function(fun = dgamma, args = list(shape = alphaGAMMA(), rate = betaGAMMA()), 
                        xlim = c(0, input$xGAMMA), geom = "area", fill = "red", alpha = 0.7)
          
        })
    }
}



shinyApp(ui = ui, server = server)