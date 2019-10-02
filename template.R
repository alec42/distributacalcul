#### UI ####
### Fonctions ERLANG ----
# ...
uiOutput("densityERLANG"),

switchInput(
    inputId = "xlim_ERLANG",
    onStatus = "success",
    onLabel = "Répartition",
    offStatus = "info",
    offLabel = "Survie",
    value = T,
    labelWidth = "10px"
),
uiOutput("repartsurvieERLANG"),
p("Graphique"),
radioGroupButtons(inputId = "plot_choice_ERLANG", 
                  choices = c("Densité", 
                              "Cumulative"),
                  selected = "Densité",
                  justified = TRUE),
plotlyOutput("FxERLANG")

### Mesures de risque ERLANG  ----
# ...
uiOutput("TVaRERLANG"),
radioGroupButtons(inputId = "plot_choice_ERLANG_QX", 
                  choices = c("Densité", 
                              "Cumulative"),
                  selected = "Cumulative",
                  justified = TRUE),
plotlyOutput("QxERLANG")

#### Server ####

plot_choice_ERLANG_SERVER <- reactive({
    if(input$plot_choice_ERLANG == "Densité")
        derlang
    else
        perlang
})

plot_choice_ERLANG_QX_SERVER <- reactive({
    if(input$plot_choice_ERLANG_QX == "Densité")
        derlang
    else
        perlang
})


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
        c(xERLANG(), 1)
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
                       b = bERLANG()
                       ), 
               nsmall = 6)
    }
    else
    {
        format(perlang(x = xERLANG(), 
                       n = nERLANG(), 
                       b = bERLANG(),
                       lower.tail = F), 
               nsmall = 6)
    }
    
})

# ...

output$repartsurvieERLANG <- renderUI({withMathJax(sprintf("$$%s(%s) = %s$$", 
                                                            repartsurvieERLANG_LATEX(),
                                                            xERLANG(),
                                                            repartsurvieERLANG()
))})

# ...

output$QxERLANG <- renderPlotly({
    ggplot(data = data.frame(x = c(0, 2 * nERLANG() * betaERLANG())
    ),
    aes(x)) + 
        stat_function(fun = plot_choice_ERLANG_QX_SERVER(),
                      args = list(nERLANG(),
                                  betaERLANG())) +
        ylab("f(x)") + 
        theme_classic() +
        stat_function(
            fun = plot_choice_ERLANG_QX_SERVER(),
            args = list(nERLANG(),
                        betaERLANG()),                    
            xlim = c(VaRERLANG(), 
                     2 * nERLANG() * betaERLANG()),
            geom = "area",
            fill = "red",
            alpha = 0.7
        )
})

output$FxERLANG <- renderPlotly({
    ggplot(data = data.frame(x = c(0, 2 * nERLANG() * betaERLANG())
    ),
    aes(x)) + 
        stat_function(fun = plot_choice_ERLANG_SERVER(),
                      args = list(nERLANG(),
                                  betaERLANG())) +
        ylab("f(x)") + 
        theme_classic() +
        stat_function(
            fun = plot_choice_ERLANG_SERVER(),
            args = list(nERLANG(),
                        betaERLANG()),                    
            xlim = xlim_ERLANG_SERVER(),
            geom = "area",
            fill = plot_color_ERLANG_SERVER(),
            alpha = 0.7
        )
})
