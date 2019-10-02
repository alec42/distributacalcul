#### UI ####
### Fonctions IG ----
# ...
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
                              "Cumulative"),
                  selected = "Densité",
                  justified = TRUE),
plotlyOutput("FxIG")

### Mesures de risque IG  ----
# ...
uiOutput("TVaRIG"),
radioGroupButtons(inputId = "plot_choice_IG_QX", 
                  choices = c("Densité", 
                              "Cumulative"),
                  selected = "Cumulative",
                  justified = TRUE),
plotlyOutput("QxIG")

#### Server ####

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

# ...

output$repartsurvieIG <- renderUI({withMathJax(sprintf("$$%s(%s) = %s$$", 
                                                       repartsurvieIG_LATEX(),
                                                       xIG(),
                                                       repartsurvieIG()
))
})

# ...

output$QxIG <- renderPlotly({
    ggplot(data = data.frame(x = c(0, VaR_llogis(kappa = 0.999, mu = muIG(), beta = betaIG()))
    ),
    aes(x)) + 
        stat_function(fun = plot_choice_IG_QX_SERVER(),
                      args = list(shape = muIG(), 
                                  scale = betaIG())) +
        ylab("f(x)") + 
        theme_classic() +
        stat_function(
            fun = plot_choice_IG_QX_SERVER(),
            args = list(shape = muIG(), 
                        scale = betaIG()),                  
            xlim = c(VaRIG(), 
                     VaR_llogis(kappa = 0.999, mu = muIG(), beta = betaIG())),
            geom = "area",
            fill = "red",
            alpha = 0.7
        )
})

output$FxIG <- renderPlotly({
    ggplot(data = data.frame(x = c(0, VaR_llogis(kappa = 0.999, mu = muIG(), beta = betaIG()))
    ),
    aes(x)) + 
        stat_function(fun = plot_choice_IG_SERVER(),
                      args = list(shape = muIG(), 
                                  scale = betaIG())) +
        ylab("f(x)") + 
        theme_classic() +
        stat_function(
            fun = plot_choice_IG_SERVER(),
            args = list(shape = muIG(), 
                        scale = betaIG()),                    
            xlim = xlim_IG_SERVER(),
            geom = "area",
            fill = plot_color_IG_SERVER(),
            alpha = 0.7
        )
})
