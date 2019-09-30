#### UI ####
### Fonctions BIN ----
# ...
uiOutput("densityBIN"),

switchInput(
    inputId = "xlim_BIN",
    onStatus = "success",
    onLabel = "Répartition",
    offStatus = "info",
    offLabel = "Survie",
    value = T,
    labelWidth = "10px"
),
uiOutput("repartsurvieBIN"),
p("Graphique"),
radioGroupButtons(inputId = "plot_choice_BIN", 
                  choices = c("Densité", 
                              "Cumulative"),
                  selected = "Densité",
                  justified = TRUE),
plotlyOutput("FxBIN")

### Mesures de risque BIN  ----
# ...
uiOutput("TVaRBIN"),
plotlyOutput("QxBIN")

#### Server ####

plot_choice_BIN_SERVER <- reactive({
    if(input$plot_choice_BIN == "Densité")
        dBIN
    else
        pBIN
})

plot_color_BIN_SERVER <- reactive({
    if(input$xlim_BIN == T)
        "Dark Green"
    else
        "Royal Blue"
})

xlim_BIN_SERVER <- reactive({
    if(input$xlim_BIN == T)
        c(muNORM() - 4 * sqrt(sigma2NORM()), input$xNORM)
    else
        c(input$xNORM, muNORM() + 4 * sqrt(sigma2NORM()))
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

repartsurvieBIN <- reactive({
    if(input$xlim_BIN == T)
    {
        format(pnorm(input$xNORM, muNORM(), sigma2NORM()), nsmall = 6, scientific = F)
    }
    else
    {
        format(pnorm(input$xNORM, muNORM(), sigma2NORM(), lower.tail = F), nsmall = 6, scientific = F)
    }
    
})

# ...

output$repartsurvieBIN <- renderUI({withMathJax(sprintf("$$%s(%s) = %s$$", 
                                                         repartsurvieBIN_LATEX(),
                                                         input$xBIN,
                                                         repartsurvieBIN()
))})

# ...

output$QxBIN <- renderPlotly({
    ggplot(data = data.frame(x = c(muNORM() - 4 * sqrt(sigma2NORM()),
                                   muNORM() + 4 * sqrt(sigma2NORM())
    )
    ),
    aes(x)) + 
        stat_function(fun = dBIN,
                      args = list(muNORM(), sigma2NORM())) + 
        ylab("f(x)") + 
        theme_classic() +
        stat_function(
            fun = dBIN,
            args = list(muNORM(), sigma2NORM()),
            xlim = c(VaRNORM(), muNORM() + 4 * sqrt(sigma2NORM())),
            geom = "area",
            fill = "red",
            alpha = 0.7
        )
})

output$FxBIN <- renderPlotly({
    ggplot(data = data.frame(x = c(muNORM() - 4 * sqrt(sigma2NORM()),
                                   muNORM() + 4 * sqrt(sigma2NORM())
    )
    ),
    aes(x)) + 
        stat_function(fun = plot_choice__SERVER(),
                      args = list(muNORM(), sigma2NORM())) + 
        ylab("f(x)") + 
        theme_classic() +
        stat_function(
            fun = plot_choice__SERVER(),
            args = list(muNORM(), sigma2NORM()),
            xlim = xlim__SERVER(),
            geom = "area",
            fill = plot_color__SERVER(),
            alpha = 0.7
        )
})
