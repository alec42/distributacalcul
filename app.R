# Library
{
library(shiny)
library(actuar)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(rsconnect)
}


# Corps
{
ui <- dashboardPage(skin = "blue", dashboardHeader(title = "Lois de probabilité", titleWidth = 200),
  
  # Paneau Latéral                  
  {
  dashboardSidebar(collapsed = T, sidebarMenu(
  
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

          fluidRow(column(width = 2, 
            box(title = "Paramètres", status = "primary", solidHeader = T, width = NULL,
               numericInput('muNORM', withMathJax('$$\\mu$$'), value = 0),
               numericInput('sigmaNORM', '$$\\sigma^2$$', value = 1)), 
            align = "center"
    ),
    
    column(width = 2,
           
           box(
             title = "Moments", width = NULL, solidHeader = TRUE, status = "warning",
             uiOutput("meanNORM"), 
             uiOutput("varNORM"), 
             align = "center"
           ),
           box(
             title = "Autres Moments", width = NULL, solidHeader = TRUE, status = "warning", 
               numericInput('dNORM', withMathJax('$$d$$'), value = 0),
               uiOutput("EspTronqNORM")), align = "center"
           
    ),
    
    column(width = 2,
           
           box(
             title = "Fonctions", width = NULL, solidHeader = TRUE, 
             status = "danger", # pour couleur de la boite, diff couleur pour statut
             numericInput('xNORM', '$$x$$', value = 0), 
             br(), uiOutput("densityNORM"), 
             br(), uiOutput("repartNORM") , 
             align = "center"
           )
           
    ),
    
    column(width = 2,
           box(
             title = "Mesure de risques", width = NULL, solidHeader = TRUE, status = "success",
             
             numericInput('kNORM', '$$\\kappa$$', value = 0.99), 
             uiOutput("VaRNORM"), 
             br(), 
             uiOutput("TVaRNORM"), 
             align = "center"
           )
    ),
    
    column(width = 2,
           box(
             title = "Notes supplémentaires", width = NULL, solidHeader = TRUE, status = "info", collapsible = T, collapsed = T,
             "À compléter"
           )
    )
  )
)},

        # LOI GAMMA
        {tabItem(tabName = "gamma",
         fluidPage(
           titlePanel("Loi Gamma"), withMathJax(), helpText("\\(X \\sim\\mathcal{G}(\\alpha, \\beta)\\)"), align = "center"),
         
         fluidRow(column(width = 2, box(
           
           title = "Paramètres", status = "primary", solidHeader = T, width = NULL,
           splitLayout(
             
             numericInput('muGAMMA', '$$\\mu$$', value = 0, width ='75%'),
             numericInput('sigmaGAMMA', '$$\\sigma^2$$', value = 1, width ='75%')), align = "center"
         )
         
         ),
         
         column(width = 2,
                
                box(
                  title = "Moments", width = NULL, solidHeader = TRUE, status = "warning",
                  textOutput("meanGAMMA"), br(), textOutput("varGAMMA"), align = "center"
                )
         ),
         
         column(width = 2,
                
                box(
                  title = "Fonctions", width = NULL, solidHeader = TRUE, status = "danger",
                  numericInput('xGAMMA', '$$x$$', value = 0, width ='50%'), br(), "Fonction de densité :", textOutput("densityGAMMA"), 
                  br(), "Fonction de répartition :", textOutput("repartGAMMA") , align = "center"
                )
                
         ),
         
         column(width = 2,
                box(
                  title = "Mesure de risques", width = NULL, solidHeader = TRUE, status = "success",
                  numericInput('kNORM', '$$\\kappa$$', value = 0.99, width ='50%'), br(), "Value at Risk :", textOutput("VaRGAMMA"), 
                  br(), "Tail Value at Risk :", textOutput("TVaRGAMMA") , align = "center"
                )
         ),
         
         column(width = 2,
                box(
                  title = "Notes supplémentaires", width = NULL, solidHeader = TRUE, status = "info", collapsible = T, collapsed = T,
                  "À compléter"
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
  }
}



shinyApp(ui = ui, server = server)