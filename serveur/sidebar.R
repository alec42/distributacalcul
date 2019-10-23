## Fichier pour le serveur du sidebar

## Fonction utile trouvée en ligne pour créer un beau dropdown list qui pourrait être réutilisé
dropdownHack <- function (...,badgeStatus = NULL, .list = NULL,menuname=NULL) 
{
    if (!is.null(badgeStatus)){
        shinydashboard:::validateStatus(badgeStatus)
    }
    items <- c(list(...), .list)
    lapply(items, shinydashboard:::tagAssert, type = "li")
    dropdownClass <- paste0("dropdown ", "text-menu")
    numItems <- length(items)
    if (is.null(badgeStatus)) {
        badge <- NULL
    }
    else {
        badge <- span(class = paste0("label label-", badgeStatus), numItems)
    }
    tags$li(class = dropdownClass, a( href="#", class="dropdown-toggle", 
                                      `data-toggle`="dropdown", menuname, badge),
            tags$ul(class = "dropdown-menu",  items  )
    )
}

## Inputs are outputed through a UI element to format the translated text
output$email1_UI <- renderUI({
    a(actionButton(
        inputId = "email1",
        label = i18n()$t("Nous contacter"),
        icon = icon("envelope", lib = "font-awesome")
        # ,style = 'height:10px'
    ), href = "mailto:alec.van-rassel.1@ulaval.ca")
})

output$notation_indicator_UI <- renderUI({
    radioGroupButtons(
        inputId = "notation_indicator",
        # label = i18n()$t("Notation"),
        choices = c("ACT-2001",
                    "ACT-1002")
    )
})

isolate({updateTabItems(session, "tabs", "description")})

## The menubar is also outputed through a UI element to better render the text and format it.
output$sidebar_output_cont <- renderMenu({
    menuItem(
        # textOutput("sidebar_title_cont"),
        i18n()$t("Lois continues"),
        icon = icon("chart-area"),
        menuSubItem(
            # textOutput("NORM_title"),
            # icon = NULL,
            i18n()$t("Normale"),
            icon = icon("neos"),
            tabName = "Normale"
        ),
        menuSubItem(
            # textOutput("LNORM_title"),
            # icon = NULL,
            i18n()$t("Lognormale"),
            icon = icon("ruler-combined"),
            tabName = "Lognormale"
        ),
        menuSubItem(
            # textOutput("expo_fam_title"),
            # icon = NULL,
            i18n()$t("Gamma, exponentielle et khi-Carré"),
            icon = icon("google"),
            tabName = "gamma"
        ),
        menuSubItem(
            # textOutput("WEI_title"),
            # icon = NULL,
            i18n()$t("Weibull"),
            icon = icon("wikipedia-w"),
            tabName = "Weibull"
        ),
        menuSubItem(
            # textOutput("PARETO_title"),
            # icon = NULL,
            i18n()$t("Pareto"),
            icon = icon("product-hunt"),
            tabName = "Pareto"
        ),
        menuSubItem(
            # textOutput("BURR_title"),
            # icon = NULL,
            i18n()$t("Burr"),
            icon = icon("btc"),
            tabName = "Burr"
        ),
        menuSubItem(
            # textOutput("UNIC_title"),
            # icon = NULL,
            i18n()$t("Uniforme"),
            icon = icon("fish"),
            tabName = "UniformeC"
        ),
        menuSubItem(
            # textOutput("BETA_title"),
            # icon = NULL,
            i18n()$t("Bêta"),
            icon = icon("behance"),
            tabName = "Beta"
        ),
        menuSubItem(
            # textOutput("ERLANG_title"),
            # icon = NULL,
            i18n()$t("Erlang"),
            icon = icon("erlang"),
            tabName = "Erlang"
        ),
        menuSubItem(
            # textOutput("LOGLOGIS_title"),
            # icon = NULL,
            i18n()$t("Log-logistique"),
            icon = icon("dolly"),
            tabName = "LOGLOGIS"
        ),
        menuSubItem(
            # textOutput("IG_title"),
            # icon = NULL,
            i18n()$t("Inverse Gaussienne"),
            icon = icon("italic"),
            tabName = "IG"
        )
        
    )
})

output$sidebar_output_disc <- renderMenu({
    menuItem(
        # textOutput("sidebar_title_disc"),
        i18n()$t("Lois discrètes"),
        icon = icon("chart-bar"),
        menuSubItem(
            # textOutput("UNID_title"),
            # icon = NULL,
            i18n()$t("Uniforme"),
            icon = icon("fish"),
            tabName = "UniformeD"
        ),
        menuSubItem(
            # textOutput("BIN_title"),
            # icon = NULL,
            i18n()$t("Binomiale et bernoulli"),
            icon = icon("bold"),
            tabName = "Binomiale"
        ),
        menuSubItem(
            # textOutput("BN_title"),
            # icon = NULL,
            i18n()$t("Binomiale négative et géométrique"),
            icon = icon("minus"),
            tabName = "Binneg"
        ),
        menuSubItem(
            # textOutput("POI_title"),
            # icon = NULL,
            i18n()$t("Poisson"),
            icon = icon("fish"),
            tabName = "Poisson"
        ),
        menuSubItem(
            # textOutput("HG_title"),
            # icon = NULL,
            i18n()$t("Hypergéométrique"),
            icon = icon("hire-a-helper"),
            tabName = "HG"
        ),
        menuSubItem(
            # textOutput("LOGARITHMIQUE_title"),
            # icon = NULL,
            i18n()$t("Logarithmique"),
            icon = icon("yahoo"),
            tabName = "Logarithmique"
        )
    )
})

output$sidebar_output_comp <- renderMenu({
    menuItem(tabName = "compound_sidebar_menu",
             # textOutput("sidebar_title_comp"),
             i18n()$t("Lois composées"),
             icon = icon("chart-line"),
             menuSubItem(
                 # textOutput("BNCOMP_title"),
                 i18n()$t("Binomiale négative composée"),
                 icon = NULL,
                 tabName = "BNCOMP"
             ),
             menuSubItem(
                 # textOutput("BINCOMP_title"),
                 i18n()$t("Binomiale composée"),
                 icon = NULL,
                 tabName = "BINCOMP"
             ),
             menuSubItem(
                 # textOutput("POICOMP_title"),
                 i18n()$t("Poisson composée"),
                 icon = NULL,
                 tabName = "PCOMP"
             )
    )
})

output$sidebar_output_tools <- renderMenu({
    menuItem(
        # textOutput("sidebar_title_tools"),
        i18n()$t("Outils"),
        icon = icon("wrench"),
        menuSubItem(
            # textOutput("mexcess_transl"),
            i18n()$t("Excès-moyen"),
            icon = NULL,
            tabName = "excess_mean"
        ),
        menuSubItem(
            # textOutput("LLN_transl"),
            i18n()$t("Loi des grands nombres"),
            icon = NULL,
            tabName = "LLN_tool"
        ),
        menuSubItem(
            # textOutput("Approximations_transl"),
            i18n()$t("Approximations"),
            icon = NULL,
            tabName = "approx_tool"
        )
        ,menuSubItem(
            # textOutput("MGF_tool_transl"),
            i18n()$t("Fonctions génératrices des moments"),
            icon = NULL,
            tabName = "MGF_tool"
        )
        # ,menuSubItem("Test d'hypothèse T", href = "https://casertamarco.shinyapps.io/power/")
        # ,menuSubItem("Tests statistiques", tabName = "stat_tests")
        # ,menuSubItem("Copules", tabName = "copulas_tool")
        
    )
})

output$sidebar_output_about <- renderMenu({
    menuItem(
        # text = textOutput("about_transl"),
        i18n()$t("À propos"),
        icon = icon("info-circle"),
        menuSubItem(
            # textOutput("proj_descr_transl"),
            i18n()$t("Description du projet"),
            icon = icon("sticky-note"),
            tabName = "description",
            selected = T
        ),
        menuSubItem(
            # textOutput("dev_transl"),
            # icon = NULL,
            i18n()$t("Développeurs"),
            icon = icon("user-tie"),
            tabName = "about"
        ),
        menuSubItem(
            # textOutput("wiki_link_transl"),
            i18n()$t("Théorie et formules"),
            icon = icon("wikipedia-w"),
            href = "https://gitlab.com/alec42/distributacalcul-wiki/wikis/Home"
        ),
        menuSubItem(
            # itextOutput("git_link_transl"),
            i18n()$t("GitHub"),
            icon = icon("github"),
            href = "https://github.com/alec42/distributacalcul.git"
        ),
        menuSubItem(
            # textOutput("proj_site_transl"),
            i18n()$t("Site du projet"),
            icon = icon("compass"),
            href = "https://alec42.github.io/distributacalcul/"
        )
    )
})

