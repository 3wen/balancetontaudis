#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(shinyalert)
library(magrittr)

# load("data/data.rda")
# data_levels <- data
# save(data_levels, file = "data/data_levels.rda")

if(file.exists("data/data_levels.rda")){
  load("data/data_levels.rda")  
}else{
  data_levels <- 
    list(
      details_elements = data.frame(detail_element = character(0), stringsAsFactors = F),
      details_desordres = data.frame(detail_desordre = character(0), stringsAsFactors = F),
      elements = data.frame(element = character(0), stringsAsFactors = F),
      materiaux = data.frame(materiau = character(0), stringsAsFactors = F),
      zones = data.frame(zone = character(0), stringsAsFactors = F),
      etages = data.frame(etage = character(0), stringsAsFactors = F),
      desordres = data.frame(desordre = character(0), stringsAsFactors = F),
      qualifications_desordres = data.frame(qualification_desordre = character(0), stringsAsFactors = F)
    )
  save(data_levels, file = "data/data_levels.rda")
}

nb_enqueteurs <- 5
liste_enqueteurs <- as.list(1:nb_enqueteurs)
names(liste_enqueteurs) <- c("Mathieu M.", "Ewen G.", "Stéphane P.", "Stéphane G.", "Thibaut M.")

couleurs_status <- 
  data.frame(statut = c("traite", "en_attente", "non_traite"),
             color = c("olive", "orange", "maroon"),
             label = c("Terminé", "En cours", "Non traité"))



ui <- dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Connexion", tabName = "connexion", icon = icon("cog")),
      menuItem("Arrêtés", tabName = "arretes", icon = icon("file")),
      menuItem("Catégories", tabName = "categories", icon = icon("th-list")),
      menuItem("Progression", tabName = "progression", icon = icon("hourglass-end"))
    )
  ),
  dashboardBody(
    tags$script(HTML("$('body').addClass('fixed');")),
    tags$link(rel = "stylesheet", href = "style.css"),
    useShinyalert(),
    fluidRow(
      tags$head(
        tags$style(
          HTML(".shiny-notification-warning {
               position: fixed;
               bottom: 10px;
               left: 15px;
               background-color: #66ff66;
               color: black;
               border: 1px solid #00cc66;
               }"),
          HTML(".shiny-notification-message {
               position: fixed;
               bottom: 60px;
               left: 15px;
               background-color: #66ff66;
               color: black;
               border: 1px solid #00cc66;
          }"
          )
        )
      ),
      tabItems(
        # ------------------- #
        # Onglet de connexion #
        # ------------------- #
        tabItem(tabName = "connexion",
                column(12,
                       
                       selectInput("id_enqueteur_choix", h3("Enquêteur"), 
                                   choices = liste_enqueteurs, selected = 1),
                       
                       actionButton("enqueteur_choix_valider", "Valider"),
                       htmlOutput("id_enqueteur_choisi")
                       # )
                )
        ), # Fin de l'element `connexion`
        # ------------------------------ #
        # Onglet d'affichage des arretes #
        # ------------------------------ #
        tabItem(tabName = "arretes",
                column(width=12,
                       id = "navig",
                       box(
                         width = NULL, solidHeader = TRUE, status = "primary",
                         column(5,
                                infoBoxOutput("info_box_annonce", width = 10)
                         ),
                         column(7,
                                actionButton("arrete_precedent", "Arrêté précédent", width = "200px",
                                             style="color: #fff; background-color: #3399ff; border-color: #0033cc"),
                                actionButton("arrete_suivant", "Arrêté suivant", width = "200px",
                                             style="color: #fff; background-color: #3399ff; border-color: #0033cc"),
                                br(),br(),
                                actionButton("premier_arrete_non_traite", "Premier arrêté non traité", width = "200px",
                                             style="color: #fff; background-color: #3399ff; border-color: #0033cc")
                         )
                       )
                ),
                column(width=12,
                       box(
                         title = "Arrêté de mise en péril :", width = NULL, solidHeader = TRUE, status = "primary",
                         h3("Identifiant de l'arrêté affiché :"),
                         verbatimTextOutput("id_arrete"),
                         h3("Contenu total :"),
                         verbatimTextOutput("contenu_arrete")
                       )
                ),
                column(width=12,
                       box(
                         title = "Extrait courant à traiter :", width = NULL, solidHeader = TRUE, status = "primary",
                         h3("Identifiant de l'item affiché :"),
                         verbatimTextOutput("identifiant_arrete_item"),
                         h3("Contenu de l'item :"),
                         verbatimTextOutput("contenu_arrete_item")
                       )
                ),
                column(width=12,
                       box(
                         width = NULL, solidHeader = TRUE, status = "primary",
                         column(12,
                                actionButton("item_precedent", "Item précédent", width = "150px",
                                             style="color: #fff; background-color: #3399ff; border-color: #0033cc"),
                                actionButton("item_suivant", "Item suivant", width = "150px",
                                             style="color: #fff; background-color: #3399ff; border-color: #0033cc")
                         )
                       )
                ),
                column(width=12,
                       box(
                         title = "Premier désordre constaté dans l'item :", width = NULL, solidHeader = TRUE, status = "primary",
                         column(12,
                                radioButtons(inputId = "item_ignorer_1", label = "Ignorer l'item ?",
                                             choices = list("Oui" = "Oui",
                                                            "Non" = "Non"),
                                             selected = "Non",
                                             inline = TRUE)
                         ),
                         column(6,
                                selectizeInput("item_details_elements_1",
                                               label = "Détails élément",
                                               choices = data_levels$details_elements,
                                               selected = NA,
                                               multiple = TRUE,
                                               options = list(create = TRUE))
                         ),
                         column(6,
                                selectizeInput("item_details_desordres_1",
                                               label = "Détails désordre",
                                               choices = data_levels$details_desordres,
                                               selected = NA,
                                               multiple = TRUE,
                                               options = list(create = TRUE))
                                
                         ),
                         column(4,
                                selectizeInput("item_element_1",
                                               label = "Élément",
                                               choices = data_levels$elements,
                                               selected = NA,
                                               multiple = TRUE,
                                               options = list(create = TRUE)),
                                selectizeInput("item_materiau_1",
                                               label = "Matériau",
                                               choices = data_levels$materiaux,
                                               selected = NA,
                                               multiple = TRUE,
                                               options = list(create = TRUE))
                         ),
                         column(4,
                                selectizeInput("item_zone_1",
                                               label = "Zone",
                                               choices = data_levels$zones,
                                               selected = NA,
                                               multiple = TRUE,
                                               options = list(create = TRUE)),
                                selectizeInput("item_etage_1",
                                               label = "Étage",
                                               choices = data_levels$etages,
                                               selected = NA,
                                               multiple = TRUE,
                                               options = list(create = TRUE))
                         ),
                         column(4,
                                selectizeInput("item_desordre_1",
                                               label = "Désordre",
                                               choices = data_levels$desordres,
                                               selected = NA,
                                               multiple = TRUE,
                                               options = list(create = TRUE)),
                                selectizeInput("item_qualification_desordre_1",
                                               label = "Qualification désordre",
                                               choices = data_levels$qualifications_desordres,
                                               selected = NA,
                                               multiple = TRUE,
                                               options = list(create = TRUE))
                         ),
                         column(12,
                                radioButtons(inputId = "bouton_desordre_2", label = "Ajouter un deuxième désordre",
                                             choices = list("Oui" = "Oui",
                                                            "Non" = "Non"),
                                             selected = "Non",
                                             inline = TRUE)
                         )
                       )
                ),
                conditionalPanel(condition = "input.bouton_desordre_2 == 'Oui'",
                                 column(width=12,
                                        box(
                                          title = "Deuxième désordre constaté dans l'item :", width = NULL, solidHeader = TRUE, status = "primary",
                                          column(12,
                                                 h3("Contenu de l'item :"),
                                                 verbatimTextOutput("contenu_arrete_item_2"),
                                                 radioButtons(inputId = "item_ignorer_2", label = "Ignorer l'item ?",
                                                              choices = list("Oui" = "Oui",
                                                                             "Non" = "Non"),
                                                              selected = "Non",
                                                              inline = TRUE)
                                          ),
                                          column(6,
                                                 selectizeInput("item_details_elements_2",
                                                                label = "Détails élément",
                                                                choices = data_levels$details_elements,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE))
                                          ),
                                          column(6,
                                                 selectizeInput("item_details_desordres_2",
                                                                label = "Détails désordre",
                                                                choices = data_levels$details_desordres,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE))
                                                 
                                          ),
                                          column(4,
                                                 selectizeInput("item_element_2",
                                                                label = "Élément",
                                                                choices = data_levels$elements,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE)),
                                                 selectizeInput("item_materiau_2",
                                                                label = "Matériau",
                                                                choices = data_levels$materiaux,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE))
                                          ),
                                          column(4,
                                                 selectizeInput("item_zone_2",
                                                                label = "Zone",
                                                                choices = data_levels$zones,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE)),
                                                 selectizeInput("item_etage_2",
                                                                label = "Étage",
                                                                choices = data_levels$etages,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE))
                                          ),
                                          column(4,
                                                 selectizeInput("item_desordre_2",
                                                                label = "Désordre",
                                                                choices = data_levels$desordres,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE)),
                                                 selectizeInput("item_qualification_desordre_2",
                                                                label = "Qualification désordre",
                                                                choices = data_levels$qualifications_desordres,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE))
                                          ),
                                          column(12,
                                                 radioButtons(inputId = "bouton_desordre_3", label = "Ajouter un troisième désordre",
                                                              choices = list("Oui" = "Oui",
                                                                             "Non" = "Non"),
                                                              selected = "Non",
                                                              inline = TRUE)
                                          )
                                        )
                                 )
                ),
                conditionalPanel(condition = "input.bouton_desordre_3 == 'Oui'",
                                 column(width=12,
                                        box(
                                          title = "Troisième désordre constaté dans l'item :", width = NULL, solidHeader = TRUE, status = "primary",
                                          column(12,
                                                 h3("Contenu de l'item :"),
                                                 verbatimTextOutput("contenu_arrete_item_3"),
                                                 radioButtons(inputId = "item_ignorer_3", label = "Ignorer l'item ?",
                                                              choices = list("Oui" = "Oui",
                                                                             "Non" = "Non"),
                                                              selected = "Non",
                                                              inline = TRUE)
                                          ),
                                          column(6,
                                                 selectizeInput("item_details_elements_3",
                                                                label = "Détails élément",
                                                                choices = data_levels$details_elements,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE))
                                          ),
                                          column(6,
                                                 selectizeInput("item_details_desordres_3",
                                                                label = "Détails désordre",
                                                                choices = data_levels$details_desordres,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE))
                                                 
                                          ),
                                          column(4,
                                                 selectizeInput("item_element_3",
                                                                label = "Élément",
                                                                choices = data_levels$elements,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE)),
                                                 selectizeInput("item_materiau_1",
                                                                label = "Matériau",
                                                                choices = data_levels$materiaux,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE))
                                          ),
                                          column(4,
                                                 selectizeInput("item_zone_3",
                                                                label = "Zone",
                                                                choices = data_levels$zones,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE)),
                                                 selectizeInput("item_etage_3",
                                                                label = "Étage",
                                                                choices = data_levels$etages,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE))
                                          ),
                                          column(4,
                                                 selectizeInput("item_desordre_3",
                                                                label = "Désordre",
                                                                choices = data_levels$desordres,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE)),
                                                 selectizeInput("item_qualification_desordre_1",
                                                                label = "Qualification désordre",
                                                                choices = data_levels$qualifications_desordres,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE))
                                          )
                                        )
                                 )
                ),
                column(width=12,
                       box(
                         width = NULL, solidHeader = TRUE, status = "primary",
                         column(12,
                                actionButton("item_precedent_bas", "Item précédent", width = "150px",
                                             style="color: #fff; background-color: #3399ff; border-color: #0033cc"),
                                actionButton("item_suivant_bas", "Item suivant", width = "150px",
                                             style="color: #fff; background-color: #3399ff; border-color: #0033cc")
                         )
                       )
                )
        ),# Fin de l'element `arretes`
        # ---------------------------- #
        # Onglet d'ajout de catégories #
        # ---------------------------- #
        tabItem(tabName = "categories",
                column(12,
                       h3("Suppression de catégories"),
                       htmlOutput("texte_suppression_categ"),
                       box(
                         title="Détail élément",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         column(12,
                                DT::dataTableOutput("table_details_elements"),
                                actionButton("bouton_supprimer_detail_element", "Supprimer détail élément", width = "150px",
                                             style="color: #fff; background-color: #EE324E; border-color: #0033cc")
                         )),
                       box(
                         title="Détail désordre",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         column(12,
                                DT::dataTableOutput("table_details_desordres"),
                                actionButton("bouton_supprimer_detail_desordre", "Supprimer détail désordre", width = "150px",
                                             style="color: #fff; background-color: #EE324E; border-color: #0033cc")
                         )),
                       box(
                         title="Élément",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         column(12,
                                DT::dataTableOutput("table_elements"),
                                actionButton("bouton_supprimer_element", "Supprimer élément", width = "150px",
                                             style="color: #fff; background-color: #EE324E; border-color: #0033cc")
                         )),
                       box(
                         title="Matériau",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         column(12,
                                DT::dataTableOutput("table_materiaux"),
                                actionButton("bouton_supprimer_materiau", "Supprimer matériau", width = "200px",
                                             style="color: #fff; background-color: #EE324E; border-color: #0033cc")
                         )),
                       box(
                         title="Zone",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         column(12,
                                DT::dataTableOutput("table_zones"),
                                actionButton("bouton_supprimer_zone", "Supprimer zone", width = "150px",
                                             style="color: #fff; background-color: #EE324E; border-color: #0033cc")
                         )),
                       box(
                         title="Étage",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         column(12,
                                DT::dataTableOutput("table_etages"),
                                actionButton("bouton_supprimer_etage", "Supprimer étage", width = "150px",
                                             style="color: #fff; background-color: #EE324E; border-color: #0033cc")
                         )),
                       box(
                         title="Désordre",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         column(12,
                                DT::dataTableOutput("table_desordres"),
                                actionButton("bouton_supprimer_desordre", "Supprimer désordre", width = "200px",
                                             style="color: #fff; background-color: #EE324E; border-color: #0033cc")
                         )),
                       box(
                         title="Qualification désordre",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         column(12,
                                DT::dataTableOutput("table_qualifications_desordres"),
                                actionButton("bouton_supprimer_qualification_desordre", "Supprimer qualif. désordre", width = "150px",
                                             style="color: #fff; background-color: #EE324E; border-color: #0033cc")
                         ))
                )# Fin de l'element `categories`
        ),
        # --------------------- #
        # Onglet de progression #
        # --------------------- #
        tabItem(tabName = "progression",
                column(12,
                       h2("Progression de la transcription"),
                       verbatimTextOutput("progression_val"),
                       DT::dataTableOutput("tableau_avancement")
                )
        )# Fin de tabItems
      )# Fin de tabItems
    )# Fin de fluidRow
  )# Fin de dashboardBody
)# Fin de dashboardPage

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Load the list of levels for each selectizeInput button
  load("data/data_levels.rda")
  # Load the pathos
  load("data/pathos.rda")
  
  data <- reactiveValues(
    infos_current = NULL,
    desordres = NULL,
    records_user = NULL,
    data_levels = data_levels,
    pathos = pathos,
    pathos_user = NULL,
    pathos_user_split = NULL
  )
  
  
  source("functions.R", local = TRUE)
  
  observeEvent(input$enqueteur_choix_valider,{
    init_session()
  })
  
  output$contenu_arrete <- renderText({
    HTML(fetch_arrete())
  })
  
  output$contenu_arrete_item <- 
    output$contenu_arrete_item_2 <- 
    output$contenu_arrete_item_3 <-
    renderText({
      HTML(fetch_item())
    })
  
  
  observeEvent(input$item_suivant,{
    passer_item_suivant()
  })
  observeEvent(input$item_precedent,{
    passer_item_precedent()
  })
  observeEvent(input$item_suivant_bas,{
    passer_item_suivant()
  })
  observeEvent(input$item_precedent_bas,{
    passer_item_precedent()
  })
  observeEvent(input$arrete_suivant,{
    passer_arrete_suivant()
  })
  observeEvent(input$arrete_precedent,{
    passer_arrete_precedent()
  })
  
  output$id_arrete <- renderText({
    HTML(data$infos_current$id_arrete)
  })
  
  
  # Affichage de l'identifiant de l'item courant
  output$identifiant_arrete_item <- renderText({
    HTML(data$infos_current$id_item)
  })
  
  
  # Info box sur l'état de progression de classification des arrêtés
  # Une icone de fichier est affichée en fonction de l'état d'avancement :
  # - rouge : aucun des items de l'arrêté courant n'a été transcrit
  # - orange : une partie des items de l'arrêté courant a été transcrite
  # - vert : tous les items de l'arrêté courant ont été transcrits
  # Une indication numérique de la progression est également affichée :
  # - pour l'indice de l'arrêté courant
  # - pour l'indice de l'item courant
  output$info_box_annonce <- renderInfoBox({
    
    
    if(is.null(data$infos_current$statut_courant)) data$infos_current$statut_courant <- "non_traite"
    
    statut_courant <- as.character(data$infos_current$statut_courant)
    
    # Couleur et label associes au statut
    couleurs_status_cour <- 
      couleurs_status %>% 
      filter(statut == statut_courant)
    couleur_cour <- couleurs_status_cour$color
    label_cour <- couleurs_status_cour$label
    
    infoBox(
      "Arrêté",
      paste(paste0(data$infos_current$id_arrete, "/", nrow(data$pathos)),
            paste0("Item ", data$infos_current$id_item, "/", length(data$infos_current$nb_items))
      ),
      paste0(label_cour), icon = icon("file"),
      color = couleur_cour
    )
  })
  
  #########################################
  ## SUPPRESSION DE VALEURS DE REFERENCE ##
  #########################################
  
  # Texte à afficher pour expliquer le fonctionnement de la suppression de catégories
  output$texte_suppression_categ <- 
    renderText({
      HTML(str_c("Pour supprimer une catégorie, il suffit de cliquer sur la ligne correspondante dans le tableau ",
                 "(on peut effectuer une recherche à l'aide du champ en haut à droite de chaque tableau). ",
                 "La ligne devient en sur-brillance. En cliquant sur le bouton de suppression, ",
                 "la catégorie sélectionnée est retirée de la base de connaissance.",
                 "<br/>",
                 "Note : cela ne supprime pas les entrées vérifiées par l'enquêteur qui contiennent la valeur supprimée. ",
                 "Prendre toutefois garde à vérifier le dernier élément affiché dans l'onglet des arrêtés.",
                 "<br/><br/>"))
    })
  
  
  # Tables dynamiques
  
  output$table_details_elements = DT::renderDataTable({
    data$data_levels$details_elements
  }, rownames = TRUE, server = TRUE
  )
  
  output$table_details_desordres = DT::renderDataTable({
    data$data_levels$details_desordres
  }, rownames = TRUE, server = TRUE
  )
  
  output$table_elements = DT::renderDataTable({
    data$data_levels$elements
  }, rownames = TRUE, server = TRUE
  )
  
  
  output$table_materiaux = DT::renderDataTable({
    data$data_levels$materiaux
  }, rownames = TRUE, server = TRUE
  )
  
  # Table dynamique des zones
  output$table_zones = DT::renderDataTable({
    data$data_levels$zones
  }, rownames = TRUE, server = TRUE
  )
  
  output$table_etages = DT::renderDataTable({
    data$data_levels$etages
  }, rownames = TRUE, server = TRUE
  )
  
  
  output$table_desordres = DT::renderDataTable({
    data$data_levels$desordres
  }, rownames = TRUE, server = TRUE
  )
  
  # Qualifications de désordres
  output$table_qualifications_desordres = DT::renderDataTable({
    data$data_levels$qualifications_desordres
  }, rownames = TRUE, server = TRUE
  )
  
  
  # Boutons de suppression
  observeEvent(input$bouton_supprimer_detail_element,{
    remove_category(button_remove_detail_element)
  })
  
  observeEvent(input$bouton_supprimer_detail_desordre,{
    remove_category(button_remove_detail_desordre)
  })
  
  
  observeEvent(input$bouton_supprimer_element,{
    remove_category(button_remove_element)
  })
  
  observeEvent(input$bouton_supprimer_materiau,{
    remove_category(button_remove_materiau)
  })
  
  # Suppression d'une zone si selectionnée et bouton de suppression cliqué
  observeEvent(input$bouton_supprimer_zone,{
    remove_category(button_remove_zone)
  })
  
  observeEvent(input$bouton_supprimer_etage,{
    remove_category(button_remove_etage)
  })
  
  
  observeEvent(input$bouton_supprimer_desordre,{
    remove_category(button_remove_desordre)
  })
  
  observeEvent(input$bouton_supprimer_qualification_desordre,{
    remove_category(button_remove_qualification_desordre)
  })
  
  
  ###########################
  ## ONGLET DE PROGRESSION ##
  ###########################
  
  # Affichage de la table des valeurs renseignées par l'utilisateur
  output$tableau_avancement <- DT::renderDataTable(DT::datatable({
    data$records_user
  }))
  
  # Message d'information sur l'état d'avancement des transcriptions de l'enquêteur
  output$progression_val <- renderText({
    if(is.null(data$records_user)){
      # Aucune valeurs pré-existantes renseignées pour l'enquêteur
      str_c("Aucune transcription.")
    }else{
      # Il existe des valeurs pré-existantes, on les dénombre
      nb_arretes_transcrits <- 
        data$records_user %>% 
        select(id_arrete) %>% 
        unique() %>% 
        nrow()
      
      str_c("Nombre de transcriptions : ", nb_arretes_transcrits, "/", nrow(data$pathos_user), ".")
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

