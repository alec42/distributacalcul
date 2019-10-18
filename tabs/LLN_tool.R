# set.seed(2019)
# nn<-10000
# 
# Xn<-rgamma(nn,2,1/5)
# Wn<-cumsum(Xn)/(1:nn)
# plot(1:nn,Wn,type="l",xlab="n",ylab="Wn")
# 
# Xn<-rgamma(nn,2,1/5)
# Wn<-cumsum(Xn)/(1:nn)
# points(1:nn,Wn,type="l")
# 
# Xn<-rgamma(nn,2,1/5)
# Wn<-cumsum(Xn)/(1:nn)
# points(1:nn,Wn,type="l")
# 
# Xn<-rgamma(nn,2,1/5)
# Wn<-cumsum(Xn)/(1:nn)
# points(1:nn,Wn,type="l")
# 
# Xn<-rgamma(nn,2,1/5)
# Wn<-cumsum(Xn)/(1:nn)
# points(1:nn,Wn,type="l")


# shape_LLN_tool <- 3
# rate_LLN_tool <- 2
# n.simul_LLN_tool <- 10000
# Xn_LLN_tool <- rgamma(n = n.simul_LLN_tool, shape = shape_LLN_tool, rate = rate_LLN_tool)
# Wn_LLN_tool <- cumsum(Xn_LLN_tool)/(1:n.simul_LLN_tool)
# Wn_LLN_tool <- cumsum(rgamma(n = n.simul_LLN_tool, shape = shape_LLN_tool, rate = rate_LLN_tool))/(1:n.simul_LLN_tool)
# ggplot(data = data.frame("abscisse" = 1:n.simul_LLN_tool,
#                    "ordonnée" = Wn_LLN_tool),
#        aes(x = abscisse,
#            y = ordonnée)
#        ) +
#     geom_line()

tab_LLN_tool <- tabItem(tabName = "LLN_tool",
                        
                        fluidRow(
                            titlePanel("Loi des grands nombres"),
                            # withMathJax(),
                            align = "center"
                        ),
                        fluidRow(
                            column(
                                width = 4,
                                boxPlus(
                                    title = "Paramètres",
                                    status = "primary",
                                    solidHeader = T,
                                    width = NULL,
                                    closable = F,
                                    uiOutput("server_shapeLLN_tool"),
                                    uiOutput("server_rateLLN_tool"),
                                    uiOutput("server_nb_simul_LLN_tool"),
                                    p("Distribution"),
                                    selectInput(inputId = "distr_select_LLN_tool",
                                                label = "",
                                                choices = c("Gamma",
                                                            "Lognormale",
                                                            "Pareto")
                                    ),
                                    align = "center"
                                )
                            ),
                            column(
                                width = 8,
                                boxPlus(
                                    title = "Plot",
                                    status = "primary",
                                    solidHeader = T,
                                    width = NULL,
                                    closable = F,
                                    plotOutput("plot_LLN_tool")
                                )
                            )
                        )
)