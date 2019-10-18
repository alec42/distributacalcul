#### Serveur outil LLN ####

shapeLLN_tool <- reactive({input$shapeLLN_tool})
rateLLN_tool <- reactive({input$rateLLN_tool})
nb.simul_LLN_tool <- reactive({input$nb.simul_LLN_tool})

output$server_shapeLLN_tool <- renderUI({
    numericInput('shapeLLN_tool', withMathJax('$$\\alpha$$'), value = 2, min = 0)
})
output$server_rateLLN_tool <- renderUI({
    numericInput('rateLLN_tool', '$$\\beta$$', value = 1, min = 0)
})
output$server_nb.simul_LLN_tool <- renderUI({
    numericInput('nb.simul_LLN_tool', '$$n$$', value = 100, min = 0)
})

distr_select_LLN_tool <- reactive({input$distr_select_LLN_tool})
# Xn_LLN_tool <- reactive({
#     
# })

Wn_LLN_tool <- reactive({
    cumsum(rgamma(n = nb.simul_LLN_tool(), shape = shapeLLN_tool(), rate = rateLLN_tool()))/(1:nb.simul_LLN_tool())
})

plot_LLN_tool <- renderPlotly({
    ggplot(data = data.frame("abscisse" = 1:nb.simul_LLN_tool(),
                             "ordonnée" = Wn_LLN_tool()),
           aes(x = abscisse,
               y = ordonnée)
    ) +
        geom_line() +
        theme_classic()
})

# observeEvent({
#     input$distr_select_LLN_tool
# },
# {
#     x <- input$distr_select_LLN_tool
#     
#     updateNumericInput(session, "shapeLLN_tool",
#                        label = 
#                        {
#                            if (x == "Gamma")
#                            {
#                                '$$\\alpha$$'
#                            }
#                            else if (x == "Bêta")
#                            {
#                                '$$\\alpha$$'
#                            }
#                            else if (x == "Lognormale")
#                            {
#                                '$$\\mu$$'
#                            }
#                            else if (x == "Pareto")
#                            {
#                                '$$\\alpha$$'
#                            }
#                        }
#     )
#     
#     updateNumericInput(session, "rateLLN_tool",
#                        label = 
#                        {
#                            if (x == "Gamma")
#                            {
#                                '$$\\beta$$'
#                            }
#                            else if (x == "Bêta")
#                            {
#                                '$$\\beta$$'
#                            }
#                            else if (x == "Lognormale")
#                            {
#                                '$$\\sigma^2$$'
#                            }
#                            else if (x == "Pareto")
#                            {
#                                '$$\\lambda$$'
#                            }
#                        }
#     )
# })
