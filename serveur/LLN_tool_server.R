#### Serveur outil LLN ####

distr_select_LLN_tool <- reactive({input$distr_select_LLN_tool})

shapeLLN_tool <- reactive({input$shapeLLN_tool})
rateLLN_tool <- reactive({input$rateLLN_tool})
nb_simul_LLN_tool <- reactive({input$nb_simul_LLN_tool})

transition_speed_LLN_tool <- reactive({input$transition_speed_LLN_tool})
frame_speed_LLN_tool <- reactive({input$frame_speed_LLN_tool})

## Render les input en UI pour être en mesure de changer, entre autre, leurs labels
output$server_shapeLLN_tool <- renderUI({
    numericInput('shapeLLN_tool', withMathJax('$$\\alpha$$'), value = 2, min = 0)
})
output$server_rateLLN_tool <- renderUI({
    numericInput('rateLLN_tool', '$$\\beta$$', value = 1, min = 0)
})
output$server_nb_simul_LLN_tool <- renderUI({
    numericInput('nb_simul_LLN_tool', '$$n$$', value = 100, min = 0, step = 100)
})
## Paramètres pour modifier la vitesse de l'animation
output$server_frame_speed_LLN_tool <- renderUI({
    numericInput('frame_speed_LLN_tool', 'Frame speed', value = 10, min = 0, step = 1)
})
output$server_transition_speed_LLN_tool <- renderUI({
    numericInput('transition_speed_LLN_tool', 'Transition speed', value = 0, min = 0, step = 1)
})
## Titre de la boite contenant la graphique réactif selon la distribution choisie
output$title_plot_LLN_tool <- renderUI({
    paste("Simulation d'une distribution", distr_select_LLN_tool())
})

## Simulations qui sont ensuite pondérées pour obtenir Wn
## Le code
Wn_LLN_tool <- reactive({
    # cumsum(Xn_LLN_tool()) / (1:nb_simul_LLN_tool())
    if (distr_select_LLN_tool() == "Gamma")
    {
        Xn_LLN_tool <- rgamma(n = nb_simul_LLN_tool(), shape = shapeLLN_tool(), rate = rateLLN_tool())
    }
    else if (distr_select_LLN_tool() == "Lognormale")
    {
        Xn_LLN_tool <- rlnorm(n = nb_simul_LLN_tool(), meanlog = shapeLLN_tool(), sdlog = rateLLN_tool())
    }
    else if (distr_select_LLN_tool() == "Pareto")
    {
        Xn_LLN_tool <- rpareto(n = nb_simul_LLN_tool(), shape = shapeLLN_tool(), scale = rateLLN_tool())
    }
    cumsum(Xn_LLN_tool) / (1:nb_simul_LLN_tool())
})

## Espérance réactive selon la distribution.
## Ces fonctions sont codés dans le packetage (présentement nommé) Distributacalcul
E_LLN_tool <- reactive({
    if (distr_select_LLN_tool() == "Gamma")
    {
        E_gamma(shapeLLN_tool(), rateLLN_tool())
    }
    else if (distr_select_LLN_tool() == "Lognormale")
    {
        E_lnorm(shapeLLN_tool(), rateLLN_tool())
    }
    else if (distr_select_LLN_tool() == "Pareto")
    {
        E_pareto(shapeLLN_tool(), rateLLN_tool())
    }
})

output$E_LLN_tool_ui <- renderUI({
    withMathJax(sprintf("$$E[X] = %s$$",
                        E_LLN_tool()))
})

output$plot_LLN_tool <- renderPlotly({
    
    ## crée un data.frame pour plotter de façon réactive.
    df <- data.frame("y" = Wn_LLN_tool())
    
    ## Nous ajoutons une colonne qui va correspondre au nombre de la simulation.
    df$ID <- seq.int(nrow(df))
    
    ## Fonction pour accumuler les points de l'animation dans le data.frame
    accumulate_by <- function(dat, var) {
        var <- lazyeval::f_eval(var, dat)
        lvls <- plotly:::getLevels(var)
        dats <- lapply(seq_along(lvls), function(x) {
            cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
        })
        dplyr::bind_rows(dats)
    }
    
    ## Appliquons la fonction à notre BD
    ## Nous obtenons donc un ID pour chaque observation ainsi que la colonne frame pour plotter le point à chaque frame.
    df <- df %>%
        accumulate_by(~ID)
    
    ## Graphique de base.
    p <- ggplot(df, aes(ID, y, frame = frame)) +
        geom_line() +
        geom_hline(yintercept = E_LLN_tool(), colour = "red")
    
    ## Graphique avec animmations utilisant le package plotly.
    p <- ggplotly(p) %>%
        layout(
            yaxis = list(
                title = distr_select_LLN_tool(),
                zeroline = F,
                tickprefix = "$"
            ),
            xaxis = list(
                title = "Nombre de simulations",
                zeroline = F, 
                showgrid = F
            )
        ) %>% 
        ## Les paramètres sont réactifs pour être en mesure des modifiers selons nos besoins.
        animation_opts(
            frame = frame_speed_LLN_tool(), 
            transition = transition_speed_LLN_tool(), 
            redraw = FALSE
        ) %>%
        animation_slider(
            currentvalue = list(
                prefix = "Simulation "
            )
        )
})

observeEvent({
    input$distr_select_LLN_tool
},
{
    x <- input$distr_select_LLN_tool

    updateNumericInput(session, "shapeLLN_tool",
                       label =
                       {
                           if (x == "Gamma")
                           {
                               '$$\\alpha$$'
                           }
                           else if (x == "Lognormale")
                           {
                               '$$\\mu$$'
                           }
                           else if (x == "Pareto")
                           {
                               '$$\\alpha$$'
                           }
                       }
    )

    updateNumericInput(session, "rateLLN_tool",
                       label =
                       {
                           if (x == "Gamma")
                           {
                               '$$\\beta$$'
                           }
                           else if (x == "Lognormale")
                           {
                               '$$\\sigma^2$$'
                           }
                           else if (x == "Pareto")
                           {
                               '$$\\lambda$$'
                           }
                       }
    )
})
