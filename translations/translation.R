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
                choices = c("Français" = "fr", 
                            "English" = "en"),
                selected = input$selected_language
    )
})

#### Page principale #### 
output$main_title <- renderText({i18n()$t("Lois de probabilité")})


output$proj_descr_transl <- renderText({i18n()$t("Description du projet")})
output$dev_transl <- renderText({i18n()$t("Développeurs")})
output$wiki_link_transl <- renderText({i18n()$t("Théorie et formules")})
output$proj_site_transl <- renderText({i18n()$t("Site du projet")})
output$git_link_transl <- renderText({i18n()$t("GitHub")})

output$Notation_transl <- renderText({i18n()$t("Notation")})
output$contact_transl <- renderText({i18n()$t("Nous contacter")})

output$title_proj_about_transl <- renderText({i18n()$t("À propos du projet")})
output$proj_about_transl <- renderText({i18n()$t("À propos du projet")})
output$proj_goal_transl <- renderText({i18n()$t("But du projet")})
output$goal_transl <- renderText({i18n()$t("But du projet")})
output$goal_line1_transl <- renderText({i18n()$t("Ce projet a comme but de simplifier la vie des étudiants en actuariat à l'Université Laval et est conçu particulièrement pour les cours d'introduction à l'actuariat 2 et d'analyse probabiliste des risques actuariels.")})
output$goal_line2_transl <- renderText({i18n()$t("Le site inclut une 'calculatrice' de plusieurs fonctions, mesures de risques, moments, etc. pour plusieurs distributions discrètes, continues et composées.")})
output$goal_line3_transl <- renderText({i18n()$t("De plus, plusieurs outils sont présentement en développement dont un outil pour visuellement observer quelques approximations de distributions de probabilité.")})
output$goal_line4_transl <- renderText({i18n()$t("Également, on peut accéder à un wiki qui contient davantage d'information sur les formules et la théorie des lois de probabilité. Entre autres, il contient les formules pour les fonctions de densité, les mesures de risques, différents moments, etc.")})
output$status_line1_transl <- renderText({i18n()$t("Ce projet de calculatrice est encore en développement et le sera sûrement pour toujours. Si vous êtes intéressés à y contribuer, s'il vous plaît nous contacter!")})
output$links_line1_transl <- renderText({i18n()$t("Pour plus de détails sur le code du projet, voir le lien vers le GitHub.")})
output$links_line2_transl <- renderText({i18n()$t("Pour plus d'information sur les packages utilisés pour le projet, voir cette page (À VENIR) du wiki.")})
output$links_line3_transl <- renderText({i18n()$t("Pour nous faire part de tout commentaires, suggestions, questions, etc. s'il vous plaît nous contacter!")})
output$links_transl <- renderText({i18n()$t("Liens")})
output$status_transl <- renderText({i18n()$t("État du projet")})
output$MGF_tool_transl <- renderText({i18n()$t("Fonctions génératrices des moments")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})
# output$ <- renderText({i18n()$t("")})


