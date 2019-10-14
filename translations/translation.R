#### Composantes principales pour la traduction #### 
translator <- Translator$new(translation_json_path = "translations/translation.json")

i18n <- reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% translator$languages) {
        translator$set_translation_language(selected)
    }
    translator
})
# Crée le sélecteur de langage 
output$language_selector_UI <- renderUI({
    selectInput(inputId = 'selected_language',
                # label = i18n()$t("Change language"),
                label = "",
                choices = translator$languages,
                selected = input$selected_language
    )
})

#### Page principale #### 
output$main_title <- renderText({i18n()$t("Lois de probabilité")})
output$sidebar_title_cont <- renderText({i18n()$t("Lois continues")})
output$sidebar_title_disc <- renderText({i18n()$t("Lois discrètes")})
output$sidebar_title_comp <- renderText({i18n()$t("Lois composées")})
output$NORM_title <- renderText({i18n()$t("Normale")})
output$LNORM_title <- renderText({i18n()$t("Lognormale")})
output$expo_fam_title <- renderText({i18n()$t("Gamma, exponentielle et khi-Carré")})
output$WEI_title <- renderText({i18n()$t("Weibull")})
output$PARETO_title <- renderText({i18n()$t("Pareto")})
output$BURR_title <- renderText({i18n()$t("Burr")})
output$UNIC_title <- renderText({i18n()$t("Uniforme")})
output$BETA_title <- renderText({i18n()$t("Bêta")})
output$ERLANG_title <- renderText({i18n()$t("Erlang")})
output$LOGLOGIS_title <- renderText({i18n()$t("Log-logistique")})
output$IG_title <- renderText({i18n()$t("Inverse Gaussienne")})
output$UNID_title <- renderText({i18n()$t("Uniforme")})
output$BIN_title <- renderText({i18n()$t("Binomiale et bernoulli")})
output$BN_title <- renderText({i18n()$t("Binomiale négative et géométrique")})
output$POI_title <- renderText({i18n()$t("Poisson")})
output$HG_title <- renderText({i18n()$t("Hypergéométrique")})
output$LOGARITHMIQUE_title <- renderText({i18n()$t("Logarithmique")})
output$BNCOMP_title <- renderText({i18n()$t("Binomiale négative composée")})
output$BINCOMP_title <- renderText({i18n()$t("Binomiale composée")})
output$POICOMP_title <- renderText({i18n()$t("Poisson composée")})







