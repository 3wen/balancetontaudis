library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(shinyalert)
library(beepr)

nb_enqueteurs <- 5
liste_enqueteurs <- as.list(1:nb_enqueteurs)
names(liste_enqueteurs) <- c("Mathieu M.", "Ewen G.", "Stéphane P.", "Stéphane G.", "Thibaut M.")

couleurs_status <- 
  data.frame(statut = c("traite", "en_attente", "non_traite"),
             color = c("olive", "orange", "maroon"),
             label = c("Terminé", "En cours", "Non traité"))

# Chargement des references
desordres <- read_csv("data/desordres.csv") %>% arrange(desordre)
elements <- read_csv("data/elements.csv") %>% arrange(element)
descriptions_elements <- read_csv("data/descriptions_elements.csv") %>% arrange(description_element)
materiaux <- read_csv("data/materiaux.csv") %>% arrange(materiau)
etages <- read_csv("data/etages.csv") %>% arrange(etage)
qualification <- read_csv("data/qualification.csv") %>% arrange(qualification)
zones <- read_csv("data/zones.csv") %>% arrange(zone)

# Chargement des donnees d'arretes de mise en peril
pathos <- read_csv("data/pathos.csv")

# Assigner enqueteur pour chaque arrete
pathos$id_enqueteur <- rep(1, (nrow(pathos))) + 0:5

# Ajout d'un identifiant unique
pathos$id_arrete <- 1:nrow(pathos)



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
                         h3("Identifiant de l'arrêté affiché :"),
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
                                radioButtons(inputId = "item_ignorer", label = "Ignorer l'item ?",
                                             choices = list("Oui" = "Oui",
                                                            "Non" = "Non"),
                                             selected = "Non",
                                             inline = TRUE)
                         ),
                         column(4,
                                selectizeInput("item_zone",
                                               label = "Zone",
                                               choices = zones,
                                               selected = NA,
                                               multiple = FALSE,
                                               options = list(create = TRUE)),
                                selectizeInput("item_etage",
                                               label = "Étage",
                                               choices = etages,
                                               selected = NA,
                                               multiple = TRUE,
                                               options = list(create = TRUE))
                         ),
                         column(4,
                                selectizeInput("item_element",
                                               label = "Élément",
                                               choices = elements,
                                               selected = NA,
                                               multiple = TRUE,
                                               options = list(create = TRUE)),
                                selectizeInput("item_description_element",
                                               label = "Description élément",
                                               choices = descriptions_elements,
                                               selected = NA,
                                               multiple = TRUE,
                                               options = list(create = TRUE)),
                                selectizeInput("item_materiau",
                                               label = "Matériau",
                                               choices = materiaux,
                                               selected = NA,
                                               multiple = TRUE,
                                               options = list(create = TRUE))
                         ),
                         column(4,
                                selectizeInput("item_desordre",
                                               label = "Désordre",
                                               choices = desordres,
                                               selected = NA,
                                               multiple = TRUE,
                                               options = list(create = TRUE)),
                                selectizeInput("item_qualification_desordre",
                                               label = "Qualification désordre",
                                               choices = qualification,
                                               selected = NA,
                                               multiple = TRUE,
                                               options = list(create = TRUE))
                         ),
                         column(12,
                                radioButtons(inputId = "bouton_deuxieme_desordre", label = "Ajouter un deuxième désordre",
                                             choices = list("Oui" = "Oui",
                                                            "Non" = "Non"),
                                             selected = "Non",
                                             inline = TRUE)
                         )
                       )
                ),
                conditionalPanel(condition = "input.bouton_deuxieme_desordre == 'Oui'",
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
                                          column(4,
                                                 selectizeInput("item_zone_2",
                                                                label = "Zone",
                                                                choices = zones,
                                                                selected = NA,
                                                                multiple = FALSE,
                                                                options = list(create = TRUE)),
                                                 selectizeInput("item_etage_2",
                                                                label = "Étage",
                                                                choices = etages,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE))
                                          ),
                                          column(4,
                                                 selectizeInput("item_element_2",
                                                                label = "Élément",
                                                                choices = elements,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE)),
                                                 selectizeInput("item_description_element_2",
                                                                label = "Description élément",
                                                                choices = descriptions_elements,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE)),
                                                 selectizeInput("item_materiau_2",
                                                                label = "Matériau",
                                                                choices = materiaux,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE))
                                          ),
                                          column(4,
                                                 selectizeInput("item_desordre_2",
                                                                label = "Désordre",
                                                                choices = desordres,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE)),
                                                 selectizeInput("item_qualification_desordre_2",
                                                                label = "Qualification désordre",
                                                                choices = qualification,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE))
                                          ),
                                          column(12,
                                                 radioButtons(inputId = "bouton_troisieme_desordre", label = "Ajouter un troisième désordre",
                                                              choices = list("Oui" = "Oui",
                                                                             "Non" = "Non"),
                                                              selected = "Non",
                                                              inline = TRUE)
                                          )
                                        )
                                 )
                ),
                conditionalPanel(condition = "input.bouton_troisieme_desordre == 'Oui'",
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
                                          column(4,
                                                 selectizeInput("item_zone_3",
                                                                label = "Zone",
                                                                choices = zones,
                                                                selected = NA,
                                                                multiple = FALSE,
                                                                options = list(create = TRUE)),
                                                 selectizeInput("item_etage_3",
                                                                label = "Étage",
                                                                choices = etages,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE))
                                          ),
                                          column(4,
                                                 selectizeInput("item_element_3",
                                                                label = "Élément",
                                                                choices = elements,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE)),
                                                 selectizeInput("item_description_element_3",
                                                                label = "Description élément",
                                                                choices = descriptions_elements,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE)),
                                                 selectizeInput("item_materiau_3",
                                                                label = "Matériau",
                                                                choices = materiaux,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE))
                                          ),
                                          column(4,
                                                 selectizeInput("item_desordre_3",
                                                                label = "Désordre",
                                                                choices = desordres,
                                                                selected = NA,
                                                                multiple = TRUE,
                                                                options = list(create = TRUE)),
                                                 selectizeInput("item_qualification_desordre_3",
                                                                label = "Qualification désordre",
                                                                choices = qualification,
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
                         title="Zones",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         column(12,
                                DT::dataTableOutput("table_zones"),
                                actionButton("bouton_supprimer_zone", "Supprimer zone", width = "150px",
                                             style="color: #fff; background-color: #EE324E; border-color: #0033cc")
                         )),
                       box(
                         title="Étages",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         column(12,
                                DT::dataTableOutput("table_etages"),
                                actionButton("bouton_supprimer_etage", "Supprimer étage", width = "150px",
                                             style="color: #fff; background-color: #EE324E; border-color: #0033cc")
                         )),
                       box(
                         title="Éléments",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         column(12,
                                DT::dataTableOutput("table_elements"),
                                actionButton("bouton_supprimer_element", "Supprimer élément", width = "150px",
                                             style="color: #fff; background-color: #EE324E; border-color: #0033cc")
                         )),
                       box(
                         title="Descriptions d'éléments",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         column(12,
                                DT::dataTableOutput("table_descriptions_elements"),
                                actionButton("bouton_supprimer_element", "Supprimer desc. élément", width = "200px",
                                             style="color: #fff; background-color: #EE324E; border-color: #0033cc")
                         )),
                       box(
                         title="Matériaux",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         column(12,
                                DT::dataTableOutput("table_materiaux"),
                                actionButton("bouton_supprimer_materiau", "Supprimer matériau", width = "150px",
                                             style="color: #fff; background-color: #EE324E; border-color: #0033cc")
                         )),
                       box(
                         title="Désordres",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         column(12,
                                DT::dataTableOutput("table_desordres"),
                                actionButton("bouton_supprimer_desordre", "Supprimer désordre", width = "150px",
                                             style="color: #fff; background-color: #EE324E; border-color: #0033cc")
                         )),
                       box(
                         title="Qualifications",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         column(12,
                                DT::dataTableOutput("table_qualification"),
                                actionButton("bouton_supprimer_qualification", "Supprimer qualification", width = "200px",
                                             style="color: #fff; background-color: #EE324E; border-color: #0033cc")
                         ))
                )
        ), # Fin de l'element `categories`
        # --------------------- #
        # Onglet de progression #
        # --------------------- #
        tabItem(tabName = "progression",
                column(12,
                       h2("Progression de la transcription"),
                       verbatimTextOutput("progression_val"),
                       DT::dataTableOutput("tableau_avancement")
                )
        ) # Fin de l'element `progression`
      )# Fin de tabItems
    )# Fin de fluidRow
  )# Fin de dashboardBody
)# Fin de dashboardPage

server = function(input, output,session) {
  
  # Valeurs temporaires
  data <- reactiveValues(id_enqueteur=NULL,
                         zones = zones,
                         etages = etages,
                         elements = elements,
                         descriptions_elements = descriptions_elements,
                         materiaux = materiaux,
                         desordres = desordres,
                         qualification = qualification,
                         pathos_courant=NULL,
                         pathos_enqueteur_split = NULL,
                         pathos_courant_items = NULL,
                         id_arrete = NULL,
                         id_item = NULL,
                         statut_courant = NULL,
                         pathos_enqueteur = NULL,
                         data_verification_tmp = NULL,
                         zone_courante = NA,
                         zone_courante_2 = NA,
                         zone_courante_3 = NA,
                         etage_courant = NA,
                         etage_courant_2 = NA,
                         etage_courant_3 = NA,
                         element_courant = NA,
                         element_courant_2 = NA,
                         element_courant_3 = NA,
                         description_element_courant = NA,
                         description_element_courant_2 = NA,
                         description_element_courant_3 = NA,
                         materiau_courant = NA,
                         materiau_courant_2 = NA,
                         materiau_courant_3 = NA,
                         desordre_courant = NA,
                         desordre_courant_2 = NA,
                         desordre_courant_3 = NA,
                         qualification_desordre_courante = NA,
                         qualification_desordre_courante_2 = NA,
                         qualification_desordre_courante_3 = NA,
                         ignorer_courante = NULL,
                         ignorer_courante_2 = NULL,
                         ignorer_courante_3 = NULL,
                         indices_arretes_items = NULL,
                         deuxieme_desordre = NULL,
                         troisieme_desordre = NULL)
  
  
  
  
  #########################
  ## ONGLET DE CONNEXION ##
  #########################
  
  
  # Affichage de texte sur la page de connexion
  # L'utilisateur choisit son identité dans le menu déroulant
  # Puis clique sur le bouton de validation
  # On lui indique alors qu'il est bien authentifié et on lui affiche un gif kikoo
  output$id_enqueteur_choisi <- renderUI({
    
    # Lister les fichiers dans le repertoire `www/img/`
    pick_img <- list.files("www/img/")
    if(!is.null(data$id_enqueteur))
      HTML(str_c("<br/>Vous êtes authentifié comme enquêteur : ", names(liste_enqueteurs)[as.numeric(data$id_enqueteur)],
                 "<br/><br/>",
                 '<img src="img/', sample(pick_img,1), '" height = "200px"/>'))
    
  })
  
  # Bouton de validation de l'enquêteur
  # Au clic, l'enquêteur est définin
  # en fonction du choix effectué dans le menu déroulant `id_enqueteur_choix`
  observeEvent(input$enqueteur_choix_valider,{
    
    # Placer l'identifiant de l'enquêteur dans une variable temporaire
    data$id_enqueteur <- input$id_enqueteur_choix
    id_enqueteur <- input$id_enqueteur_choix
    
    # Filtrer les arrêtes dans `pathos` qui sont assignés à l'enquêteur
    pathos_enqueteur <- pathos %>% filter(id_enqueteur == data$id_enqueteur)
    # Placer ces arrêtés dans une variable temporaire
    data$pathos_enqueteur <- pathos_enqueteur
    
    # Séparation des items à l'intérieur de l'arrêté
    # en fonction du motif "\r\n\r\n"
    pathos_enqueteur_split <- 
      pathos_enqueteur$pathologies %>% 
      str_split(., "\r\n\r\n")
    
    # Placer les items de l'ensemble des arrêtés assignés à l'enquêteur
    # dans une variable temporaire
    data$pathos_enqueteur_split <- pathos_enqueteur_split
    
    # Création d'un tableau de données avec l'ensemble des arrêtés assignés à l'enquêteur
    # indiquant un identifiant pour chaque item
    # L'identifiant retennu :
    # - est propre à chacun des i arrêtés
    # - est numérique, va de 1 à n_i, avec n_i le nombre d'items de l'arrêté i
    indices_arretes_items <- 
      lapply(1:length(pathos_enqueteur_split), function(x){
        data.frame(id_arrete = pathos_enqueteur$id_arrete[x], id_item = seq_len(length(pathos_enqueteur_split[[x]])))
      }) %>% 
      bind_rows() %>% 
      tbl_df()
    
    # Placer ce tableau dans une variable temporaire
    data$indices_arretes_items <- indices_arretes_items
    
    
    # Chargement des donnees de verification des enqueteurs
    if(file.exists(str_c("data/enqueteurs/", data$id_enqueteur, "/arretes_verif_", data$id_enqueteur, ".rda"))){
      # S'il existe des données pré-existantes (si l'enquêteur a déjà effectué des retranscriptions)
      load(str_c("data/enqueteurs/", data$id_enqueteur, "/arretes_verif_", data$id_enqueteur, ".rda"))
      data$data_verification_tmp <- arretes_verifiees_enqueteur
      
      # Tableau indiquant les indices i et i_n des arrêtés et items non traités
      reste_a_parcourir <- 
        indices_arretes_items %>% 
        anti_join(arretes_verifiees_enqueteur) %>% 
        arrange(id_arrete, id_item)
      
      if(nrow(reste_a_parcourir)>0){
        # S'il reste des arrêtés à transcrire
        # Les variables temporaires indiquant l'id de l'arrêté et l'id de l'item
        # courants sont initialisées en prenant la première valeur des non traités
        data$id_arrete <- reste_a_parcourir$id_arrete[1]
        data$id_item <- reste_a_parcourir$id_item[1]
      }else{
        # L'enquêteur a terminé sa retranscription
        # Les variables temporaires indiquant l'id de l'arrêté et l'id de l'item
        # courants sont initialiées à 1
        data$id_arrete <- data$pathos_enqueteur$id_arrete[1]
        data$id_item <- 1
      }
      
    }else{
      # L'enquêteur commence son travail, il n'a pas encore retranscrit d'arrêté
      # Les variables temporaires indiquant l'id de l'arrêté et l'id de l'item
      # courants sont initialiées à 1
      data$data_verification_tmp <- NULL
      data$id_arrete <- data$pathos_enqueteur$id_arrete[1]
      data$id_item <- 1
    }
    
    # L'arrêté courant
    pathos_courant <- pathos_enqueteur %>% filter(id_arrete == data$id_arrete)
    # Que l'on place dans une variable temporaire
    data$pathos_courant <- pathos_courant
    
    # L'item courant de l'arrêté courant
    ind_element <- which(data$pathos_enqueteur$id_arrete == data$id_arrete)
    # Que l'on place dans une variable temporaire
    data$pathos_courant_items <- data$pathos_enqueteur_split[[ind_element]]
    
    # On récupère les éventuelles valeurs pré-remplies pour l'arrêté et l'item courants
    # (elles ne sont pas vides si l'enquêteur a terminé son travail et que l'on affiche
    # le premier arrêté et le premier item)
    recuperer_valeurs_verifications()
    
  })
  
  # Affichage de l'identifiant de l'arrêté
  output$id_arrete <- renderText({
    HTML(data$pathos_courant$id_arrete)
  })
  
  # Affichage du contenu de l'arrêté
  output$contenu_arrete <- renderText({
    HTML(data$pathos_courant$pathologies)
  })
  
  # Affichage de l'identifiant de l'item courant
  output$identifiant_arrete_item <- renderText({
    HTML(data$id_item)
  })
  
  # Affichage (ou reaffichage) de l'item courant
  # Le réaffichage s'effectue, au besoin, pour les désordres 2 et/ou 3
  output$contenu_arrete_item <- 
    output$contenu_arrete_item_2 <- 
    output$contenu_arrete_item_3 <-
    renderText({
      HTML(data$pathos_courant_items[data$id_item])
    })
  
  
  ######################
  ## ONGLET D'ARRETES ##
  ######################
  
  # Info box sur l'état de progression de classification des arrêtés
  # Une icone de fichier est affichée en fonction de l'état d'avancement :
  # - rouge : aucun des items de l'arrêté courant n'a été transcrit
  # - orange : une partie des items de l'arrêté courant a été transcrite
  # - vert : tous les items de l'arrêté courant ont été transcrits
  # Une indication numérique de la progression est également affichée :
  # - pour l'indice de l'arrêté courant
  # - pour l'indice de l'item courant
  output$info_box_annonce <- renderInfoBox({
    
    # Recuperation du statut de l'annonce
    statut_courant <- as.character(data$statut_courant)
    
    
    if(is.null(data$statut_courant)) data$statut_courant <- "non_traite"
    
    # Couleur et label associes au statut
    couleurs_status_cour <- 
      couleurs_status %>% 
      filter(statut == statut_courant)
    
    couleur_cour <- couleurs_status_cour$color
    label_cour <- couleurs_status_cour$label

    infoBox(
      "Arrêté",
      paste(paste0(data$pathos_courant$id_arrete, "/", nrow(pathos)),
            paste0("Item ", data$id_item, "/", length(data$pathos_courant_items))
      ),
      paste0(label_cour), icon = icon("file"),
      color = couleur_cour
    )
  })
  
  
  #' boutons_navigation
  #' Fonction d'aide pour les boutons suivants, precedents, ...
  #' @param type type du bouton : arrete ou item
  #' @param direction direction du bouton ("suivant", "precedent", "premier_arrete_non_traite")
  boutons_navigation <- function(type, direction){
    if(type == "arrete"){
      if(direction == "suivant"){
        
        if(data$id_arrete == tail(data$pathos_enqueteur$id_arrete,1)){
          # On etait avec le dernier arrete
          shinyalert("Attention !", str_c("Il s'agit du dernier arrêté"),
                     type = "warning")
          # On sauvegarde la progression au besoin
          enregistrer_valeurs()
        }else{
          # On sauvegarde la progression au besoin
          enregistrer_valeurs()
          # On peut passer à l'arrêté suivant
          data$id_arrete <- data$pathos_enqueteur$id_arrete[which(data$pathos_enqueteur$id_arrete == data$id_arrete)+1]
          pathos_courant <- data$pathos_enqueteur %>% filter(id_arrete == data$id_arrete)
          data$pathos_courant <- pathos_courant
          ind_element <- which(data$pathos_enqueteur$id_arrete == data$id_arrete)
          data$pathos_courant_items <- data$pathos_enqueteur_split[[ind_element]]
          # data$pathos_courant_items <- str_split(data$pathos_courant$pathologies, "\r\n\r\n")[[1]]
          data$id_item <- 1
          
          # Récupérer les valeurs pré-existantes, si besoin
          recuperer_valeurs_verifications()
        }
        
      }else if(direction == "precedent"){
        if(which(data$pathos_enqueteur$id_arrete == data$id_arrete) == 1){
          # On était avec le premier arrete
          shinyalert("Attention !", str_c("Il s'agit du premier arrêté"),
                     type = "warning")
          # On sauvegarde la progression au besoin
          enregistrer_valeurs()
        }else{
          # On sauvegarde la progression
          enregistrer_valeurs()
          # On peut passer à l'item précédent
          data$id_arrete <- data$pathos_enqueteur$id_arrete[which(data$pathos_enqueteur$id_arrete == data$id_arrete)-1]
          pathos_courant <- data$pathos_enqueteur %>% filter(id_arrete == data$id_arrete)
          data$pathos_courant <- pathos_courant
          ind_element <- which(data$pathos_enqueteur$id_arrete == data$id_arrete)
          data$pathos_courant_items <- data$pathos_enqueteur_split[[ind_element]]
          data$id_item <- 1
          recuperer_valeurs_verifications()
        }
        
      }
    }else if(type == "item"){
      if(direction == "suivant"){
        if(data$id_item == length(data$pathos_courant_items)){
          # On etait avec le dernier item
          shinyalert("Attention !", str_c("Il s'agit du dernier item"),
                     type = "warning")
          # On sauvegarde la progression au besoin
          enregistrer_valeurs()
        }else{
          # On sauvegarde la progression
          enregistrer_valeurs()
          # On peut passer à l'item suivant
          data$id_item <- data$id_item + 1
          recuperer_valeurs_verifications()
        }
        
      }else if(direction == "precedent"){
        if(data$id_item == 1){
          # On était avec le premier item
          shinyalert("Attention !", str_c("Il s'agit du premier item"),
                     type = "warning")
          # On sauvegarde la progression au besoin
          enregistrer_valeurs()
        }else{
          # On sauvegarde la progression
          enregistrer_valeurs()
          # On peut passer à l'item précédent
          data$id_item <- data$id_item - 1
          recuperer_valeurs_verifications()
        }
        
      } 
    }else if(type == "premier_arrete_non_traite"){
      # On sauvegarde la progression
      enregistrer_valeurs()
      if(!is.null(data$data_verification_tmp)){
        reste_a_parcourir <- 
          data$indices_arretes_items %>% 
          anti_join(data$data_verification_tmp) %>% 
          arrange(id_arrete, id_item)
      }else{
        reste_a_parcourir <- 
          data$indices_arretes_items %>% 
          arrange(id_arrete, id_item)
      }
      
      if(nrow(reste_a_parcourir)>0){
        data$id_arrete <- reste_a_parcourir$id_arrete[1]
        pathos_courant <- data$pathos_enqueteur %>% filter(id_arrete == data$id_arrete)
        data$pathos_courant <- pathos_courant
        ind_element <- which(data$pathos_enqueteur$id_arrete == data$id_arrete)
        data$pathos_courant_items <- data$pathos_enqueteur_split[[ind_element]]
        data$id_item <- reste_a_parcourir$id_item[1]
      }else{
        shinyalert("Attention !", str_c("C'est fini !"),
                   type = "warning")
      }
      
      # On récupère les valeurs pré-existantes, si besoin
      recuperer_valeurs_verifications()
    }# Fin de condition sur les types
  }# Fin de boutons_navigation()
  
  
  # Bouton "suivant" arrete
  observeEvent(input$arrete_suivant,{
    boutons_navigation(direction = "suivant", type = "arrete")
  })
  
  # Bouton "précédent" arrete
  observeEvent(input$arrete_precedent,{
    boutons_navigation(direction = "precedent", type = "arrete")
  })
  
  # Bouton "suivant" item
  observeEvent(input$item_suivant,{
    boutons_navigation(direction = "suivant", type = "item")
  })
  
  # Bouton "précédent" item
  observeEvent(input$item_precedent,{
    boutons_navigation(direction = "precedent", type = "item")
  })
  
  # Bouton "suivant" item (bas de page)
  observeEvent(input$item_suivant_bas,{
    boutons_navigation(direction = "suivant", type = "item")
  })
  
  # Bouton "précédent" item (bas de page)
  observeEvent(input$item_precedent_bas,{
    boutons_navigation(direction = "precedent", type = "item")
  })
  
  # Bouton "suivant" arrete
  observeEvent(input$premier_arrete_non_traite,{
    boutons_navigation(direction = NULL, type = "premier_arrete_non_traite")
  })
  
  # Zones #
  # ----- #
  
  #' ajout_zone
  #' Fonction pour ajouter une zone à la liste de zones. Retourne la zone modifée.
  #' @param x (string ou vecteur de strings) zone(s) à ajouter à la liste
  ajout_zone <- function(x){
    # On charge la liste des zones
    # (si jamais un autre utilisateur a deja fait un ajout entre-temps)
    zones <- read_csv("data/zones.csv") %>% arrange(zone)
    # On ajoute la nouvelle proposition à la liste pour une future utilisation
    zones <- 
      zones %>% 
      bind_rows(data.frame(zone = x)) %>% 
      unique() %>% 
      arrange(zone)
    write_csv(zones, path = "data/zones.csv")
    message_enregistrer <<- showNotification(paste("La zone \"", input$item_zone, "\" a été ajoutée"),
                                             duration = 5, type = "message", closeButton = TRUE)
    zones
  }# Fin de ajout_zone()
  
  # Cadre de sélection de la zone pour le désordre 1
  # Lorsque l'utilisateur définit la zone, elle est stockée dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter une nouvelle zone, la fonction `ajout_zone()`
  # est appelée.
  observeEvent(input$item_zone,{
    data$zone_courante <- input$item_zone
    ind <- which(!input$item_zone %in% c("", data$zones$zone))
    if(length(ind)){
      data$zones <- ajout_zone(x = input$item_zone[ind])
    }
  })
  
  # Cadre de sélection de la zone pour le désordre 2
  # Lorsque l'utilisateur définit la zone, elle est stockée dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter une nouvelle zone, la fonction `ajout_zone()`
  # est appelée.
  observeEvent(input$item_zone_2,{
    data$zone_courante_2 <- input$item_zone_2
    ind <- which(!input$item_zone_2 %in% c("", data$zones$zone))
    if(length(ind)){
      data$zones <- ajout_zone(x = input$item_zone_2[ind])
    }
  })
  
  # Cadre de sélection de la zone pour le désordre 3
  # Lorsque l'utilisateur définit la zone, elle est stockée dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter une nouvelle zone, la fonction `ajout_zone()`
  # est appelée.
  observeEvent(input$item_zone_3,{
    data$zone_courante_3 <- input$item_zone_3
    ind <- which(!input$item_zone_3 %in% c("", data$zones$zone))
    if(length(ind)){
      data$zones <- ajout_zone(x = input$item_zone_3[ind])
    }
  })
  
  # Etages #
  # ------ #
  
  #' ajout_etage
  #' Fonction pour ajouter un étage à la liste d'étages. Retourne les nouvelles valeurs d'étages.
  #' @param x (string ou vecteur de strings) étage(s) à ajouter à la liste
  ajout_etage <- function(x){
    # On charge la liste des etages
    # (si jamais un autre utilisateur a deja fait un ajout entre-temps)
    etages <- read_csv("data/etages.csv") %>% arrange(etage)
    # On ajoute la nouvelle proposition à la liste pour une future utilisation
    etages <- 
      etages %>% 
      bind_rows(data.frame(etage = x)) %>% 
      unique() %>% 
      arrange(etage)
    write_csv(etages, path = "data/etages.csv")
    message_enregistrer <<- showNotification(paste("L'étage \"", input$item_etage, "\" a été ajoutée"),
                                             duration = 5, type = "message", closeButton = TRUE)
    etages
  }# Fin de ajout_etage()
  
  # Cadre de sélection de l'étage pour le désordre 1
  # Lorsque l'utilisateur définit l'étage, ce dernier est stocké dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter un nouvel étage, la fonction `ajout_etage()` est appelée.
  observeEvent(input$item_etage,{
    # S'il y a plusieurs étages d'indiqués, on les concatène en utilisant un point virgule
    # avant de les stocker dans la variable temporaire d'étage(s) courant(s)
    data$etage_courant <- str_c(input$item_etage, collapse = ";")
    ind <- which(!input$item_etage %in% c("", data$etages$etage))
    if(length(ind)){
      # Si une nouvelle valeur d'étage apparaît, on modifie la liste d'étages
      data$etages <- ajout_etage(input$item_etage[ind])
    }
  })
  
  # Cadre de sélection de l'étage pour le désordre 2
  # Lorsque l'utilisateur définit l'étage, ce dernier est stocké dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter un nouvel étage, la fonction `ajout_etage()` est appelée.
  observeEvent(input$item_etage_2,{
    data$etage_courant_2 <- str_c(input$item_etage_2, collapse = ";")
    ind <- which(!input$item_etage_2 %in% c("", data$etages$etage))
    if(length(ind)){
      data$etages <- ajout_etage(input$item_etage_2[ind])
    }
  })
  
  # Cadre de sélection de l'étage pour le désordre 3
  # Lorsque l'utilisateur définit l'étage, ce dernier est stocké dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter un nouvel étage, la fonction `ajout_etage()` est appelée.
  observeEvent(input$item_etage_3,{
    data$etage_courant_3 <- str_c(input$item_etage_3, collapse = ";")
    ind <- which(!input$item_etage_3 %in% c("", data$etages$etage))
    if(length(ind)){
      data$etages <- ajout_etage(input$item_etage_3[ind])
    }
  })
  
  # Elements #
  # -------- #
  
  #' ajout_element
  #' Fonction pour ajouter un élément à la liste d'éléments. Retourne les nouvelles valeurs d'éléments.
  #' @param x (string ou vecteur de strings) élément(s) à ajouter à la liste
  ajout_element <- function(x){
    # On charge la liste des elements
    # (si jamais un autre utilisateur a deja fait un ajout entre-temps)
    elements <- read_csv("data/elements.csv") %>% arrange(element)
    # On ajoute la nouvelle proposition à la liste pour une future utilisation
    elements <- 
      elements %>% 
      bind_rows(data.frame(element = x)) %>% 
      unique() %>% 
      arrange(element)
    write_csv(elements, path = "data/elements.csv")
    message_enregistrer <<- showNotification(paste("L'élément \"", input$item_element, "\" a été ajoutée"),
                                             duration = 5, type = "message", closeButton = TRUE)
    elements
  }# Fin de ajout_element()
  
  
  # Cadre de sélection de l'élément pour le désordre 1
  # Lorsque l'utilisateur définit l'élément, ce dernier est stocké dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter un nouvel élément, la fonction `ajout_element()` est appelée.
  observeEvent(input$item_element,{
    # S'il y a plusieurs éléments d'indiqués, on les concatène en utilisant un point virgule
    # avant de les stocker dans la variable temporaire d'élément(s) courant(s)
    data$element_courant <- str_c(input$item_element, collapse = ";")
    ind <- which(!input$item_element %in% c("", data$elements$element))
    if(length(ind)){
      # Si une nouvelle valeur d'élément apparaît, on modifie la liste d'éléments
      data$elements <- ajout_element(input$item_element[ind])
    }
  })
  
  # Cadre de sélection de l'élément pour le désordre 2
  # Lorsque l'utilisateur définit l'élément, ce dernier est stocké dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter un nouvel élément, la fonction `ajout_element()` est appelée.
  observeEvent(input$item_element_2,{
    data$element_courant_2 <- str_c(input$item_element_2, collapse = ";")
    ind <- which(!input$item_element_2 %in% c("", data$elements$element))
    if(length(ind)){
      data$elements <- ajout_element(x = input$item_element_2[ind])
    }
  })
  
  # Cadre de sélection de l'élément pour le désordre 3
  # Lorsque l'utilisateur définit l'élément, ce dernier est stocké dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter un nouvel élément, la fonction `ajout_element()` est appelée.
  observeEvent(input$item_element_3,{
    data$element_courant_3 <- str_c(input$item_element_3, collapse = ";")
    ind <- which(!input$item_element_3 %in% c("", data$elements$element))
    if(length(ind)){
      data$elements <- ajout_element(x = input$item_element_3[ind])
    }
  })
  
  # Descriptions Elements #
  # --------------------- #
  
  #' ajout_description_element
  #' Fonction pour ajouter une description d'éléments à la liste de descriptions d'éléments.
  #' Retourne les nouvelles valeurs de descriptions d'éléments.
  #' @param x (string ou vecteur de strings) desc. d'élément(s) à ajouter à la liste
  ajout_description_element <- function(x){
    # On charge la liste des descriptions elements
    # (si jamais un autre utilisateur a deja fait un ajout entre-temps)
    descriptions_elements <- read_csv("data/descriptions_elements.csv") %>% arrange(description_element)
    # On ajoute la nouvelle proposition à la liste pour une future utilisation
    descriptions_elements <- 
      descriptions_elements %>% 
      bind_rows(data.frame(description_element = x)) %>% 
      unique() %>% 
      arrange(description_element)
    write_csv(descriptions_elements, path = "data/descriptions_elements.csv")
    message_enregistrer <<- showNotification(paste("La desc. élément \"", input$item_description_element, "\" a été ajoutée"),
                                             duration = 5, type = "message", closeButton = TRUE)
    descriptions_elements
  }# Fin de ajout_description_element()
  
  # Cadre de sélection de la description d'éléments pour le désordre 1
  # Lorsque l'utilisateur définit la desc. d'éléments, cette dernière est stockée dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter une nouvelle desc. d'éléments, la fonction `ajout_description_element()` est appelée.
  observeEvent(input$item_description_element,{
    # S'il y a plusieurs desc. d'éléments d'indiqués, on les concatène en utilisant un point virgule
    # avant de les stocker dans la variable temporaire de desc. d'élément(s) courant(s)
    data$description_element_courant <- str_c(input$item_description_element, collapse = ";")
    ind <- which(!input$item_description_element %in% c("", data$descriptions_elements$description_element))
    if(length(ind)){
      # Si une nouvelle valeur de desc. d'éléments apparaît, on modifie la liste
      data$descriptions_elements <- ajout_description_element(x = input$item_description_element[ind])
    }
  })
  
  # Cadre de sélection de la description d'éléments pour le désordre 2
  # Lorsque l'utilisateur définit la desc. d'éléments, cette dernière est stockée dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter une nouvelle desc. d'éléments, la fonction `ajout_description_element()` est appelée.
  observeEvent(input$item_description_element_2,{
    data$description_element_courant_2 <- str_c(input$item_description_element_2, collapse = ";")
    ind <- which(!input$item_description_element_2 %in% c("", data$descriptions_elements$description_element))
    if(length(ind)){
      data$descriptions_elements <- ajout_description_element(x = input$item_description_element_2[ind])
    }
  })
  
  # Cadre de sélection de la description d'éléments pour le désordre 3
  # Lorsque l'utilisateur définit la desc. d'éléments, cette dernière est stockée dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter une nouvelle desc. d'éléments, la fonction `ajout_description_element()` est appelée.
  observeEvent(input$item_description_element_3,{
    data$description_element_courant_3 <- str_c(input$item_description_element_3, collapse = ";")
    ind <- which(!input$item_description_element_3 %in% c("", data$descriptions_elements$description_element))
    if(length(ind)){
      data$descriptions_elements <- ajout_description_element(x = input$item_description_element_3[ind])
    }
  })
  
  # Matériaux #
  # --------- #
  
  #' ajout_materiau
  #' Fonction pour ajouter un matériau à la liste de matériaux. Retourne les nouvelles valeurs de matériaux.
  #' @param x (string ou vecteur de strings) matériau(x) à ajouter à la liste
  ajout_materiau <- function(x){
    # On charge la liste des matériaux
    # (si jamais un autre utilisateur a deja fait un ajout entre-temps)
    materiaux <- read_csv("data/materiaux.csv") %>% arrange(materiau)
    # On ajoute la nouvelle proposition à la liste pour une future utilisation
    materiaux <- 
      materiaux %>% 
      bind_rows(data.frame(materiau = x)) %>% 
      unique() %>% 
      arrange(materiau)
    write_csv(materiaux, path = "data/materiaux.csv")
    message_enregistrer <<- showNotification(paste("Le matériau \"", input$item_materiau, "\" a été ajouté"),
                                             duration = 5, type = "message", closeButton = TRUE)
    materiaux
  }# Fin de ajout_materiau()
  
  # Cadre de sélection de matériau pour le désordre 1
  # Lorsque l'utilisateur définit le matériau, ce dernier est stocké dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter un nouveau matériau, la fonction `ajout_materiau()` est appelée.
  observeEvent(input$item_materiau,{
    # S'il y a plusieurs matériaux d'indiqués, on les concatène en utilisant un point virgule
    # avant de les stocker dans la variable temporaire de matériau(x) courant(s)
    data$materiau_courant <- str_c(input$item_materiau, collapse = ";")
    ind <- which(!input$item_materiau %in% c("", data$materiaux$materiau))
    if(length(ind)){
      # Si une nouvelle valeur de matériau apparaît, on modifie la liste
      data$materiaux <- ajout_materiau(x = input$item_materiau[ind])
    }
  })
  
  # Cadre de sélection de matériau pour le désordre 2
  # Lorsque l'utilisateur définit le matériau, ce dernier est stocké dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter un nouveau matériau, la fonction `ajout_materiau()` est appelée.
  observeEvent(input$item_materiau_2,{
    data$materiau_courant_2 <- str_c(input$item_materiau_2, collapse = ";")
    ind <- which(!input$item_materiau_2 %in% c("", data$materiaux$materiau))
    if(length(ind)){
      data$materiaux <- ajout_materiau(x = input$item_materiau_2[ind])
    }
  })
  
  # Cadre de sélection de matériau pour le désordre 3
  # Lorsque l'utilisateur définit le matériau, ce dernier est stocké dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter un nouveau matériau, la fonction `ajout_materiau()` est appelée.
  observeEvent(input$item_materiau_3,{
    data$materiau_courant_3 <- str_c(input$item_materiau_3, collapse = ";")
    ind <- which(!input$item_materiau_3 %in% c("", data$materiaux$materiau))
    if(length(ind)){
      data$materiaux <- ajout_materiau(x = input$item_materiau_3[ind])
    }
  })
  
  # Desordres #
  # --------- #
  
  #' ajout_desordre
  #' Fonction pour ajouter un désordre à la liste de désordres. Retourne les nouvelles valeurs de désordres.
  #' @param x (string ou vecteur de strings) désordre(s) à ajouter à la liste
  ajout_desordre <- function(x){
    # On charge la liste des desordres
    # (si jamais un autre utilisateur a deja fait un ajout entre-temps)
    desordres <- read_csv("data/desordres.csv") %>% arrange(desordre)
    # On ajoute la nouvelle proposition à la liste pour une future utilisation
    desordres <- 
      desordres %>% 
      bind_rows(data.frame(desordre = x)) %>% 
      unique() %>% 
      arrange(desordre)
    write_csv(desordres, path = "data/desordres.csv")
    message_enregistrer <<- showNotification(paste("Le désordre \"", input$item_desordre, "\" a été ajouté"),
                                             duration = 5, type = "message", closeButton = TRUE)
    desordres
  }# Fin de ajout_desordre()
  
  # Cadre de sélection de désordre pour le désordre 1
  # Lorsque l'utilisateur définit le désordre, ce dernier est stocké dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter un nouveau désordre, la fonction `ajout_desordre()` est appelée.
  observeEvent(input$item_desordre,{
    # S'il y a plusieurs désordres d'indiqués, on les concatène en utilisant un point virgule
    # avant de les stocker dans la variable temporaire de désordre(s) courant(s)
    data$desordre_courant <- str_c(input$item_desordre, collapse = ";")
    ind <- which(!input$item_desordre %in% c("", data$desordres$desordre))
    if(length(ind)){
      # Si une nouvelle valeur de désordre apparaît, on modifie la liste
      data$desordres <- ajout_desordre(x = input$item_desordre[ind])
    }
  })
  
  # Cadre de sélection de désordre pour le désordre 2
  # Lorsque l'utilisateur définit le désordre, ce dernier est stocké dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter un nouveau désordre, la fonction `ajout_desordre()` est appelée.
  observeEvent(input$item_desordre_2,{
    data$desordre_courant_2 <- str_c(input$item_desordre_2, collapse = ";")
    ind <- which(!input$item_desordre_2 %in% c("", data$desordres$desordre))
    if(length(ind)){
      data$desordres <- ajout_desordre(x = input$item_desordre_2[ind])
    }
  })
  
  # Cadre de sélection de désordre pour le désordre 3
  # Lorsque l'utilisateur définit le désordre, ce dernier est stocké dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter un nouveau désordre, la fonction `ajout_desordre()` est appelée.
  observeEvent(input$item_desordre_3,{
    data$desordre_courant_3 <- str_c(input$item_desordre_3, collapse = ";")
    ind <- which(!input$item_desordre_3 %in% c("", data$desordres$desordre))
    if(length(ind)){
      data$desordres <- ajout_desordre(x = input$item_desordre_3[ind])
    }
  })
  
  
  # Qualification des désordres #
  # --------------------------- #
  
  #' ajout_qualif_desordre
  #' Fonction pour ajouter une qualif. de désordre à la liste de qualif. de désordres.
  #' Retourne les nouvelles valeurs de qualif. de désordres.
  #' @param x (string ou vecteur de strings) qualif. de désordre(s) à ajouter à la liste
  ajout_qualif_desordre <- function(x){
    # On charge la liste des qualif. de desordres
    # (si jamais un autre utilisateur a deja fait un ajout entre-temps)
    qualification <- read_csv("data/qualification.csv") %>% arrange(qualification)
    # On ajoute la nouvelle proposition à la liste pour une future utilisation
    qualification <- 
      qualification %>% 
      bind_rows(data.frame(qualification = x)) %>% 
      unique() %>% 
      arrange(qualification)
    write_csv(qualification, path = "data/qualification.csv")
    message_enregistrer <<- showNotification(paste("La qualif. \"", input$item_qualification_desordre, "\" a été ajoutée"),
                                             duration = 5, type = "message", closeButton = TRUE)
    qualification
  }# Fin de ajout_qualif_desordre()
  
  # Cadre de sélection de qualif. de désordre pour le désordre 1
  # Lorsque l'utilisateur définit la qualif. de désordre, cette dernière est stockée dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter une nouvelle qualif. de désordre, la fonction `ajout_qualif_desordre()` est appelée.
  observeEvent(input$item_qualification_desordre,{
    # S'il y a plusieurs de qualif. de désordres d'indiqués, on les concatène en utilisant un point virgule
    # avant de les stocker dans la variable temporaire de qualif. de désordre(s) courant(s)
    data$qualification_desordre_courante <- str_c(input$item_qualification_desordre, collapse = ";")
    ind <- which(!input$item_qualification_desordre %in% c("", data$qualification$qualification))
    if(length(ind)){
      # Si une nouvelle valeur de qualif. des désordres apparaît, on modifie la liste
      data$qualification <- ajout_qualif_desordre(x = input$item_qualification_desordre[ind])
    }
  })
  
  # Cadre de sélection de qualif. de désordre pour le désordre 2
  # Lorsque l'utilisateur définit la qualif. de désordre, cette dernière est stockée dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter une nouvelle qualif. de désordre, la fonction `ajout_qualif_desordre()` est appelée.
  observeEvent(input$item_qualification_desordre_2,{
    data$qualification_desordre_courante_2 <- str_c(input$item_qualification_desordre_2, collapse = ";")
    ind <- which(!input$item_qualification_desordre_2 %in% c("", data$qualification$qualification))
    if(length(ind)){
      data$qualification <- ajout_qualif_desordre(x = input$item_qualification_desordre_2[ind])
    }
  })
  
  # Cadre de sélection de qualif. de désordre pour le désordre 3
  # Lorsque l'utilisateur définit la qualif. de désordre, cette dernière est stockée dans une variable temporaire.
  # Si l'utilisateur souhaite ajouter une nouvelle qualif. de désordre, la fonction `ajout_qualif_desordre()` est appelée.
  observeEvent(input$item_qualification_desordre_3,{
    data$qualification_desordre_courante_3 <- str_c(input$item_qualification_desordre_3, collapse = ";")
    ind <- which(!input$item_qualification_desordre_3 %in% c("", data$qualification$qualification))
    if(length(ind)){
      data$qualification <- ajout_qualif_desordre(x = input$item_qualification_desordre_3[ind])
    }
  })
  
  
  # Bouton permettant d'ignorer l'item (désordre 1)
  # On stock la valeurs du bouton dans une variable temporaire
  observeEvent(input$item_ignorer,{
    data$ignorer_courante <- input$item_ignorer
  })
  
  
  # Bouton permettant d'ajouter un deuxième désordre
  # On stock la valeurs du bouton dans une variable temporaire
  observeEvent(input$bouton_deuxieme_desordre,{
    data$deuxieme_desordre <- input$bouton_deuxieme_desordre
  })
  
  # Bouton permettant d'ignorer l'item (désordre 2)
  # On stock la valeurs du bouton dans une variable temporaire
  observeEvent(input$item_ignorer_2,{
    data$ignorer_courante_2 <- input$item_ignorer_2
  })
  
  # Bouton permettant d'ajouter un troisième désordre
  # On stock la valeurs du bouton dans une variable temporaire
  observeEvent(input$bouton_troisieme_desordre,{
    data$troisieme_desordre <- input$bouton_troisieme_desordre
  })
  
  # Bouton permettant d'ignorer l'item (désordre 3)
  # On stock la valeurs du bouton dans une variable temporaire
  observeEvent(input$item_ignorer_3,{
    data$ignorer_courante_3 <- input$item_ignorer_3
  })
  
  
  #' enregistrer_valeurs
  #' Fonction pour sauvegarder les valeurs renseignées par l'enquêteur
  enregistrer_valeurs <- function(){
    # Recuperation des valeurs pour chaque element à sauvegarder
    
    # Création d'un tableau de données à une ligne pour les informations
    # relatives au désordre 1
    nouvelle_ligne <-
      data.frame(id_enqueteur = data$id_enqueteur,
                 id_arrete = data$pathos_courant$id_arrete,
                 id_item = data$id_item,
                 id_desordre = 1,
                 zone = data$zone_courante,
                 etage = data$etage_courant,
                 element = data$element_courant,
                 description_element = data$description_element_courant,
                 materiau = data$materiau_courant,
                 desordre = data$desordre_courant,
                 qualification_desordre = data$qualification_desordre_courante,
                 ignorer = data$ignorer_courante,
                 stringsAsFactors = FALSE)
    
    # print("data$zone_courante")
    # print(data$zone_courante)
    # print("data$etage_courant")
    # print(data$etage_courant)
    # print("data$element_courant")
    # print(data$element_courant)
    # print("data$description_element_courant")
    # print(data$description_element_courant)
    # print("data$desordre_courant")
    # print(data$desordre_courant)
    # print("data$ignorer_courante")
    # print(data$ignorer_courante)
    # print("")
    
    
    # Est-ce que la nouvelle ligne contient des valeurs à enregistrer ?
    nouvelle_ligne_a_sauvegarder <- 
      !(nouvelle_ligne$zone == "" & 
          is.na(nouvelle_ligne$etage) & 
          is.na(nouvelle_ligne$element) & 
          is.na(nouvelle_ligne$description_element) & 
          is.na(nouvelle_ligne$materiau) & 
          is.na(nouvelle_ligne$desordre) & 
          is.na(nouvelle_ligne$qualification_desordre) & 
          nouvelle_ligne$ignorer == "Non")
    
    
    # Idem pour les informations sur un éventuel deuxième désordre
    nouvelle_ligne_2 <- NULL
    nouvelle_ligne_2_a_sauvegarder <- FALSE
    if(data$deuxieme_desordre == "Oui"){
      nouvelle_ligne_2 <- 
        data.frame(id_enqueteur = data$id_enqueteur,
                   id_arrete = data$pathos_courant$id_arrete,
                   id_item = data$id_item,
                   id_desordre = 2,
                   zone = data$zone_courante_2,
                   etage = data$etage_courant_2,
                   element = data$element_courant_2,
                   description_element = data$description_element_courant_2,
                   materiau = data$materiau_courant_2,
                   desordre = data$desordre_courant_2,
                   qualification_desordre = data$qualification_desordre_courante_2,
                   ignorer = data$ignorer_courante_2,
                   stringsAsFactors = FALSE)
      
      # Est-ce que la nouvelle ligne du désordre 2 contient des valeurs à enregistrer ?
      nouvelle_ligne_2_a_sauvegarder <- 
        !(nouvelle_ligne_2$zone == "" & 
            is.na(nouvelle_ligne_2$etage) & 
            is.na(nouvelle_ligne_2$element) & 
            is.na(nouvelle_ligne_2$description_element) & 
            is.na(nouvelle_ligne$materiau) & 
            is.na(nouvelle_ligne_2$desordre) & 
            is.na(nouvelle_ligne_2$qualification_desordre) & 
            nouvelle_ligne_2$ignorer == "Non")
      
    }# Fin data$deuxieme_desordre == "Oui
    
    
    # Idem pour les informations sur un éventuel deuxième désordre
    nouvelle_ligne_3 <- NULL
    nouvelle_ligne_3_a_sauvegarder <- FALSE
    if(data$troisieme_desordre == "Oui"){
      
      nouvelle_ligne_3 <- 
        data.frame(id_enqueteur = data$id_enqueteur,
                   id_arrete = data$pathos_courant$id_arrete,
                   id_item = data$id_item,
                   id_desordre = 3,
                   zone = data$zone_courante_3,
                   etage = data$etage_courant_3,
                   element = data$element_courant_3,
                   description_element = data$description_element_courant_3,
                   materiau = data$materiau_courant_3,
                   desordre = data$desordre_courant_3,
                   qualification_desordre = data$qualification_desordre_courante_3,
                   ignorer = data$ignorer_courante_3,
                   stringsAsFactors = FALSE)
      
      
      # Est-ce que la nouvelle ligne du désordre 3 contient des valeurs à enregistrer ?
      nouvelle_ligne_3_a_sauvegarder <- 
        !(nouvelle_ligne_3$zone == "" & 
            is.na(nouvelle_ligne_3$etage) & 
            is.na(nouvelle_ligne_3$element) & 
            is.na(nouvelle_ligne_3$description_element) & 
            is.na(nouvelle_ligne$materiau) & 
            is.na(nouvelle_ligne_3$desordre) & 
            is.na(nouvelle_ligne_3$qualification_desordre) & 
            nouvelle_ligne_3$ignorer == "Non")
      
    }# Fin data$troisieme_desordre == "Oui
    
    # Création d'une variable, `nouvelles_lignes` contenant, si renseignées,
    # les valeurs de la retranscription de chaque désordre ( devient un tableau de données)
    # Chaque ligne du tableau concerne alors un désordre.
    nouvelles_lignes <- NULL
    if(nouvelle_ligne_a_sauvegarder){
      nouvelles_lignes <- nouvelles_lignes %>% bind_rows(nouvelle_ligne)
    }
    if(nouvelle_ligne_2_a_sauvegarder){
      nouvelles_lignes <- nouvelles_lignes %>% bind_rows(nouvelle_ligne_2)
    }
    if(nouvelle_ligne_3_a_sauvegarder){
      nouvelles_lignes <- nouvelles_lignes %>% bind_rows(nouvelle_ligne_3)
    }
    
    
    # Ajout de ces informations aux informations existantes pour les autres annonces
    if(!is.null(data$data_verification_tmp)){
      # S'il existe deja un fichier avec des valeurs presentes
      # on retire les valeurs anciennes pour les remplacer par les nouvelles
      arretes_verifiees_enqueteur <- 
        data$data_verification_tmp %>% 
        filter(!(id_arrete %in% nouvelles_lignes$id_arrete & id_item %in% nouvelles_lignes$id_item & id_desordre %in% nouvelles_lignes$id_desordre)) %>% 
        bind_rows(nouvelles_lignes) %>% 
        unique()
    }else{
      # Il n'existe pas de valeurs pré-existantes
      arretes_verifiees_enqueteur <- nouvelles_lignes
    }
    
    
    if(nouvelle_ligne_a_sauvegarder | nouvelle_ligne_2_a_sauvegarder | nouvelle_ligne_3_a_sauvegarder){
      # Au moins une des transcriptions de désordres contient des valeurs qui n'étaient pas
      # présentes dans le fichier gardant la trace des transcriptions.
      # Dans ce cas, on sauvegarde.
      save(arretes_verifiees_enqueteur, file = str_c("data/enqueteurs/", data$id_enqueteur,
                                                     "/arretes_verif_", data$id_enqueteur, ".rda"))
      
      # La trace des transcriptions est alors stockée dans une variable temporaire
      data$data_verification_tmp <- arretes_verifiees_enqueteur
      
    }
    
  }# Fin de enregistrer_valeurs()
  
  #' recuperer_valeurs_verifications
  #' Recupere les valeurs (si existantes) des transcriptions.
  #' Lorsque l'enquêteur a déjà effectué des transcriptions pour l'arrêté et l'item courants
  #' il s'agit de récupérer ces valeurs.
  recuperer_valeurs_verifications <- function(){
    
    # Définition du statut de l'arrêté (non traité, en cours, traité)
    statut_arrete <- "non_traite"
    if(!is.null(data$data_verification_tmp)){
      # Il existe des valeurs de vérifications d'arrêtes pré-existantes
      # Regardons les items déjà traités dans l'arrêté courant
      items_traites_arrete_courant <- 
        data$data_verification_tmp %>% 
        filter(id_arrete == data$pathos_courant$id_arrete) %>% 
        select(id_arrete, id_item) %>% 
        unique()
      
      
      if(nrow(items_traites_arrete_courant)){
        # Il existe des valeurs pré-existantes de vérifications pour l'arrêté courant
        if(nrow(items_traites_arrete_courant) == length(data$pathos_courant_items)){
          # L'ensemble des items a été traité pour l'arrêté courant
          statut_arrete <- "traite"
        }else{
          # Il existe des items non transcrits dans l'arrêté courant
          statut_arrete <- "en_attente"
        }
      }
    }
    
    # Mise à jour de la valeur du statut de l'arrêté dans la variable temporaire
    data$statut_courant <- statut_arrete
    
    arretes_verifiees_enqueteur_cour_tmp <- NULL
    if(!is.null(data$data_verification_tmp)){
      # Recuperation des statuts pour les arretes pour lesquels l'enquêteur a déjà indiqué des renseignements  
      arretes_verifiees_enqueteur_cour_tmp <- 
        data$data_verification_tmp %>% 
        filter(id_arrete == data$pathos_courant$id_arrete, id_item == data$id_item)
      
      # Tableau indiquant les indices des arrêtés et items restant à traiter
      reste_a_parcourir <- 
        data$indices_arretes_items %>% 
        filter(id_arrete == data$id_arrete) %>% 
        anti_join(arretes_verifiees_enqueteur_cour_tmp)
      
    }
    
    
    # Pour pouvoir pré-remplir les cases avec les informations déjà renseignées
    # il faut savoir si ces informations concernent le désordre 1, 2 ou 3.
    valeurs_existantes_1 <- valeurs_existantes_2 <- valeurs_existantes_3 <- FALSE
    
    if(!is.null(arretes_verifiees_enqueteur_cour_tmp)){
      
      # Premier désordre
      arretes_verifiees_enqueteur_cour_tmp_1 <- 
        arretes_verifiees_enqueteur_cour_tmp %>% 
        filter(id_desordre == 1)
      
      # Y a-t-il des valeurs pour le premier desordre
      if(nrow(arretes_verifiees_enqueteur_cour_tmp_1)) valeurs_existantes_1 <- TRUE
      
      # Deuxième désordre
      arretes_verifiees_enqueteur_cour_tmp_2 <- 
        arretes_verifiees_enqueteur_cour_tmp %>% 
        filter(id_desordre == 2)
      
      # Y a-t-il des valeurs pour le deuxieme desordre
      if(nrow(arretes_verifiees_enqueteur_cour_tmp_2)) valeurs_existantes_2 <- TRUE
      
      # Troisième désordre
      arretes_verifiees_enqueteur_cour_tmp_3 <- 
        arretes_verifiees_enqueteur_cour_tmp %>% 
        filter(id_desordre == 3)
      
      # Y a-t-il des valeurs pour le troisieme desordre
      if(nrow(arretes_verifiees_enqueteur_cour_tmp_3)) valeurs_existantes_3 <- TRUE
      
    }
    
    if(valeurs_existantes_1){
      # Il y a des valeurs existantes pour le premier desordre
      
      # Mise à jour des variables temporaires pour y placer les valeurs des différents champs
      # qui ont déjà été renseignées par l'enquêteur
      data$zone_courante <- arretes_verifiees_enqueteur_cour_tmp_1$zone
      data$etage_courant <- arretes_verifiees_enqueteur_cour_tmp_1$etage
      data$element_courant <- arretes_verifiees_enqueteur_cour_tmp_1$element
      data$description_element_courant <- arretes_verifiees_enqueteur_cour_tmp_1$description_element
      data$materiau_courant <- arretes_verifiees_enqueteur_cour_tmp_1$materiau
      data$desordre_courant <- arretes_verifiees_enqueteur_cour_tmp_1$desordre
      data$qualification_desordre_courante <- arretes_verifiees_enqueteur_cour_tmp_1$qualification_desordre
      data$ignorer_courante <- arretes_verifiees_enqueteur_cour_tmp_1$ignorer
      
      # Mise à jour des champs
      updateSelectizeInput(session = session, inputId = "item_zone", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_1$zone, ";")[[1]])
      updateSelectizeInput(session = session, inputId = "item_etage", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_1$etage, ";")[[1]])
      updateSelectizeInput(session = session, inputId = "item_element", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_1$element, ";")[[1]])
      updateSelectizeInput(session = session, inputId = "item_description_element", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_1$description_element, ";")[[1]])
      updateSelectizeInput(session = session, inputId = "item_materiau", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_1$materiau, ";")[[1]])
      updateSelectizeInput(session = session, inputId = "item_desordre", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_1$desordre, ";")[[1]])
      updateSelectizeInput(session = session, inputId = "item_qualification_desordre", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_1$qualification_desordre, ";")[[1]])
      updateRadioButtons(session = session, inputId = "item_ignorer", selected = arretes_verifiees_enqueteur_cour_tmp_1$ignorer)
      
    }else{
      # Il n'y a pas de valeurs existantes pour le premier desordre
      
      # Réinitialisation des champs
      updateSelectizeInput(session = session, inputId = "item_zone", selected = NA)
      updateSelectizeInput(session = session, inputId = "item_etage", selected = NA)
      updateSelectizeInput(session = session, inputId = "item_element", selected = NA)
      updateSelectizeInput(session = session, inputId = "item_description_element", selected = NA)
      updateSelectizeInput(session = session, inputId = "item_materiau", selected = NA)
      updateSelectizeInput(session = session, inputId = "item_desordre", selected = NA)
      updateSelectizeInput(session = session, inputId = "item_qualification_desordre", selected = NA)
      updateRadioButtons(session = session, inputId = "item_ignorer", selected = "Non")
      
      # Réinitialisation des variables temporaires
      data$zone_courante <- ""
      data$etage_courant <- NA
      data$element_courant <- NA
      data$description_element_courant <- NA
      data$materiau_courant <- NA
      data$desordre_courant <- NA
      data$qualification_desordre_courante <- NA
      data$ignorer_courante <- "Non"
      
    }
    
    if(valeurs_existantes_2){
      # Il y a des valeurs existantes pour le deuxieme desordre
      
      # Mise à jour des variables temporaires pour y placer les valeurs des différents champs
      # qui ont déjà été renseignées par l'enquêteur
      data$zone_courante_2 <- arretes_verifiees_enqueteur_cour_tmp_2$zone
      data$etage_courant_2 <- arretes_verifiees_enqueteur_cour_tmp_2$etage
      data$element_courant_2 <- arretes_verifiees_enqueteur_cour_tmp_2$element
      data$description_element_courant_2 <- arretes_verifiees_enqueteur_cour_tmp_2$description_element
      data$materiau_courant_2 <- arretes_verifiees_enqueteur_cour_tmp_2$materiau
      data$desordre_courant_2 <- arretes_verifiees_enqueteur_cour_tmp_2$desordre
      data$qualification_desordre_courante_2 <- arretes_verifiees_enqueteur_cour_tmp_2$qualification_desordre
      data$ignorer_courante_2 <- arretes_verifiees_enqueteur_cour_tmp_2$ignorer
      
      # Mise à jour des champs
      updateRadioButtons(session = session, inputId = "bouton_deuxieme_desordre", selected = "Oui")
      updateSelectizeInput(session = session, inputId = "item_zone_2", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_2$zone, ";")[[1]])
      updateSelectizeInput(session = session, inputId = "item_etage_2", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_2$etage, ";")[[1]])
      updateSelectizeInput(session = session, inputId = "item_element_2", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_2$element, ";")[[1]])
      updateSelectizeInput(session = session, inputId = "item_description_element_2", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_2$description_element, ";")[[1]])
      updateSelectizeInput(session = session, inputId = "item_materiau_2", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_2$materiau, ";")[[1]])
      updateSelectizeInput(session = session, inputId = "item_desordre_2", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_2$desordre, ";")[[1]])
      updateSelectizeInput(session = session, inputId = "item_qualification_desordre_2", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_2$qualification_desordre, ";")[[1]])
      updateRadioButtons(session = session, inputId = "item_ignorer_2", selected = arretes_verifiees_enqueteur_cour_tmp_2$ignorer)
    }else{
      # Il n'y a pas de valeurs existantes pour le deuxieme desordre
      
      # Réinitialisation des champs
      updateRadioButtons(session = session, inputId = "bouton_deuxieme_desordre", selected = "Non")
      updateSelectizeInput(session = session, inputId = "item_zone_2", selected = NA)
      updateSelectizeInput(session = session, inputId = "item_etage_2", selected = NA)
      updateSelectizeInput(session = session, inputId = "item_element_2", selected = NA)
      updateSelectizeInput(session = session, inputId = "item_description_element_2", selected = NA)
      updateSelectizeInput(session = session, inputId = "item_materiau_2", selected = NA)
      updateSelectizeInput(session = session, inputId = "item_desordre_2", selected = NA)
      updateSelectizeInput(session = session, inputId = "item_qualification_desordre_2", selected = NA)
      updateRadioButtons(session = session, inputId = "item_ignorer_2", selected = "Non")
      
      # Réinitialisation des variables temporaires
      data$zone_courante_2 <- ""
      data$etage_courant_2 <- NA
      data$element_courant_2 <- NA
      data$description_element_courant_2 <- NA
      data$materiau_courant_2 <- NA
      data$desordre_courant_2 <- NA
      data$qualification_desordre_courante_2 <- NA
      data$ignorer_courante_2 <- "Non"
      data$deuxieme_desordre <- "Non"
      
    }
    
    if(valeurs_existantes_3){
      # Il y a des valeurs existantes pour le troisieme desordre
      
      # Mise à jour des variables temporaires pour y placer les valeurs des différents champs
      # qui ont déjà été renseignées par l'enquêteur
      data$zone_courante_3 <- arretes_verifiees_enqueteur_cour_tmp_3$zone
      data$etage_courant_3 <- arretes_verifiees_enqueteur_cour_tmp_3$etage
      data$element_courant_3 <- arretes_verifiees_enqueteur_cour_tmp_3$element
      data$description_element_courant_3 <- arretes_verifiees_enqueteur_cour_tmp_3$description_element
      data$element_materiau_3 <- arretes_verifiees_enqueteur_cour_tmp_3$materiau
      data$desordre_courant_3 <- arretes_verifiees_enqueteur_cour_tmp_3$desordre
      data$qualification_desordre_courante_3 <- arretes_verifiees_enqueteur_cour_tmp_3$qualification_desordre
      data$ignorer_courante_3 <- arretes_verifiees_enqueteur_cour_tmp_3$ignorer
      
      # Mise à jour des champs
      updateRadioButtons(session = session, inputId = "bouton_troisieme_desordre", selected = "Oui")
      updateSelectizeInput(session = session, inputId = "item_zone_3", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_3$zone, ";")[[1]])
      updateSelectizeInput(session = session, inputId = "item_etage_3", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_3$etage, ";")[[1]])
      updateSelectizeInput(session = session, inputId = "item_element_3", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_3$element, ";")[[1]])
      updateSelectizeInput(session = session, inputId = "item_description_element_3", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_3$description_element, ";")[[1]])
      updateSelectizeInput(session = session, inputId = "item_materiau_3", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_3$materiau, ";")[[1]])
      updateSelectizeInput(session = session, inputId = "item_desordre_3", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_3$desordre, ";")[[1]])
      updateSelectizeInput(session = session, inputId = "item_qualification_desordre_3", selected = str_split(arretes_verifiees_enqueteur_cour_tmp_3$qualification_desordre, ";")[[1]])
      updateRadioButtons(session = session, inputId = "item_ignorer_3", selected = arretes_verifiees_enqueteur_cour_tmp_3$ignorer)
    }else{
      # Il n'y a pas de valeurs existantes pour le troisieme desordre
      
      # Réinitialisation des champs
      updateRadioButtons(session = session, inputId = "bouton_troisieme_desordre", selected = "Non")
      updateSelectizeInput(session = session, inputId = "item_zone_3", selected = NA)
      updateSelectizeInput(session = session, inputId = "item_etage_3", selected = NA)
      updateSelectizeInput(session = session, inputId = "item_element_3", selected = NA)
      updateSelectizeInput(session = session, inputId = "item_description_element_3", selected = NA)
      updateSelectizeInput(session = session, inputId = "item_materiau_3", selected = NA)
      updateSelectizeInput(session = session, inputId = "item_desordre_3", selected = NA)
      updateSelectizeInput(session = session, inputId = "item_qualification_desordre_3", selected = NA)
      updateRadioButtons(session = session, inputId = "item_ignorer_3", selected = "Non")
      
      # Réinitialisation des variables temporaires
      data$zone_courante_3 <- ""
      data$etage_courant_3 <- NA
      data$element_courant_3 <- NA
      data$description_element_courant_3 <- NA
      data$materiau_courant_3 <- NA
      data$desordre_courant_3 <- NA
      data$qualification_desordre_courante_3 <- NA
      data$ignorer_courante_3 <- "Non"
      data$troisieme_desordre <- "Non"
    }
    
  }# Fin de recuperer_valeurs_verifications()
  
  
  # # Ajout de catégories #
  # # ------------------- #
  # observeEvent(input$valider_zone,{
  #   new_zones <- 
  #     zones %>% 
  #     bind_rows(data.frame(zone = input$ajout_zone)) %>% 
  #     unique() %>% 
  #     arrange(zone)
  #   
  #   write_csv(new_zones, path = "data/zones.csv")
  #   
  #   qual_cour <- data$zone_courante
  #   updateSelectInput(session = session, inputId = "item_zone", 
  #                     selected = qual_cour,
  #                     choices = new_zones)
  #   
  #   zones <<- new_zones
  #   
  #   shinyalert("Attention !", str_c("Zone ajoutée"),
  #              type = "success")
  # })
  # 
  # observeEvent(input$valider_etage,{
  #   new_etages <- 
  #     etages %>% 
  #     bind_rows(data.frame(etage = input$ajout_etage)) %>% 
  #     unique() %>% 
  #     arrange(etage)
  #   
  #   write_csv(new_etages, path = "data/etages.csv")
  #   
  #   qual_cour <- data$etage_courant
  #   updateSelectInput(session = session, inputId = "item_etage", 
  #                     selected = qual_cour,
  #                     choices = new_etages)
  #   
  #   etages <<- new_etages
  #   
  #   shinyalert("Attention !", str_c("Etage ajouté"),
  #              type = "success")
  # })
  # 
  # observeEvent(input$valider_element,{
  #   new_elements <- 
  #     elements %>% 
  #     bind_rows(data.frame(element = input$ajout_element)) %>% 
  #     unique() %>% 
  #     arrange(element)
  #   
  #   
  #   
  #   write_csv(new_elements, path = "data/elements.csv")
  #   
  #   qual_cour <- data$element_courant
  #   updateSelectInput(session = session, inputId = "item_element", 
  #                     selected = qual_cour,
  #                     choices = new_elements)
  #   elements <<- new_elements
  #   shinyalert("Attention !", str_c("Élément ajouté"),
  #              type = "success")
  # })
  # 
  # observeEvent(input$valider_desordre,{
  #   new_desordres <- 
  #     desordres %>% 
  #     bind_rows(data.frame(desordre = input$ajout_desordre)) %>% 
  #     unique() %>% 
  #     arrange(desordre)
  #   
  #   write_csv(new_desordres, path = "data/desordres.csv")
  #   
  #   qual_cour <- data$desordre_courant
  #   updateSelectInput(session = session, inputId = "item_desordre",
  #                     selected = qual_cour,
  #                     choices = new_desordres)
  #   
  #   desordres <<- new_desordres
  #   shinyalert("Attention !", str_c("Désordre ajoutée"),
  #              type = "success")
  # })
  # 
  # observeEvent(input$valider_qualification_desordre,{
  #   
  #   
  #   new_qualifications_desordres <- 
  #     qualification %>% 
  #     bind_rows(data.frame(qualification = input$ajout_qualification_desordre)) %>% 
  #     unique() %>% 
  #     arrange(qualification)
  #   
  #   write_csv(new_qualifications_desordres, path = "data/qualification.csv")
  #   
  #   qual_cour <- data$qualification_desordre_courante
  #   updateSelectInput(session = session, inputId = "item_qualification_desordre", 
  #                   choices = new_qualifications_desordres, selected = qual_cour)
  #   
  #   
  #   qualification <<- new_qualifications_desordres
  #   shinyalert("Attention !", str_c("Qualification de désordre ajoutée"),
  #              type = "success")
  # })
  
  # output$table1 <- renderDataTable({
  #   zones
  # })
  
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
  
  
  # Zones #
  # ----- #
  
  # Table dynamique des zones
  output$table_zones = DT::renderDataTable({
    data$zones
  }, rownames = TRUE, server = TRUE
  )
  
  # Valeurs sélectionnées par l'utilisateur dans la table des zones
  # (on fait appel à cette fonction `selection_zone()`)
  selection_zone <- reactive ({
    input$table_zones_rows_selected
  })
  
  # Suppression d'une zone si selectionnée et bouton de suppression cliqué
  observeEvent(input$bouton_supprimer_zone,{
    
    if (!is.null(selection_zone())) {
      # Il y a eu une sélection par l'utilisateur de valeurs à supprimer de la liste
      zones_a_supprimer <- input$table_zones_rows_selected
      zones_a_supprimer <- str_c(data$zones$zone[zones_a_supprimer], collapse = ";")
      data$zones <- data$zones[-as.numeric(input$table_zones_rows_selected),]
      
      # Récupération des valeurs courantes
      zone_cour <- data$zone_courante
      zone_cour_2 <- data$zone_courante_2
      zone_cour_3 <- data$zone_courante_3
      
      # Mise à jour des champs
      updateSelectizeInput(session = session, inputId = "item_zone", choices = data$zones, selected = zone_cour)
      updateSelectizeInput(session = session, inputId = "item_zone_2", choices = data$zones, selected = zone_cour_2)
      updateSelectizeInput(session = session, inputId = "item_zone_3", choices = data$zones, selected = zone_cour_3)
      
      write_csv(data$zones, path = "data/zones.csv")
      message_enregistrer <<- showNotification(paste("La zone \"", zones_a_supprimer, "\" a été retirée"),
                                               duration = 5, type = "message", closeButton = TRUE)
    }
  })
  
  
  # Etages #
  # ------ #
  
  # Table dynamique des zones
  output$table_etages = DT::renderDataTable({
    data$etages
  }, rownames = TRUE, server = TRUE
  )
  
  # Valeurs sélectionnées par l'utilisateur dans la table des étages
  # (on fait appel à cette fonction `selection_etage()`)
  selection_etage <- reactive ({
    input$table_etages_rows_selected
  })
  
  # Suppression d'un étage si selectionné et bouton de suppression cliqué
  observeEvent(input$bouton_supprimer_etage,{
    
    if (!is.null(selection_etage())) {
      # Il y a eu une sélection par l'utilisateur de valeurs à supprimer de la liste
      etages_a_supprimer <- input$table_etages_rows_selected
      etages_a_supprimer <- str_c(data$etages$etage[etages_a_supprimer], collapse = ";")
      data$etages <- data$etages[-as.numeric(input$table_etages_rows_selected),]
      
      # Récupération des valeurs courantes
      etage_cour <- data$etage_courant
      etage_cour_2 <- data$etage_courant_2
      etage_cour_3 <- data$etage_courant_3
      
      # Mise à jour des champs
      updateSelectizeInput(session = session, inputId = "item_etage", choices = data$etages, selected = etage_cour)
      updateSelectizeInput(session = session, inputId = "item_etage_2", choices = data$etages, selected = etage_cour_2)
      updateSelectizeInput(session = session, inputId = "item_etage_3", choices = data$etages, selected = etage_cour_3)
      
      write_csv(data$etages, path = "data/etages.csv")
      message_enregistrer <<- showNotification(paste("L'étage \"", etages_a_supprimer, "\" a été retiré"),
                                               duration = 5, type = "message", closeButton = TRUE)
    }
  })
  
  # Éléments #
  # -------- #
  
  # Table dynamique des éléments
  output$table_elements = DT::renderDataTable({
    data$elements
  }, rownames = TRUE, server = TRUE
  )
  
  # Valeurs sélectionnées par l'utilisateur dans la table des éléments
  # (on fait appel à cette fonction `selection_element()`)
  selection_element <- reactive ({
    input$table_elements_rows_selected
  })
  
  # Suppression d'un élément si selectionné et bouton de suppression cliqué
  observeEvent(input$bouton_supprimer_element,{
    
    if (!is.null(selection_element())) {
      # Il y a eu une sélection par l'utilisateur de valeurs à supprimer de la liste
      elements_a_supprimer <- input$table_elements_rows_selected
      elements_a_supprimer <- str_c(data$elements$element[elements_a_supprimer], collapse = ";")
      data$elements <- data$elements[-as.numeric(input$table_elements_rows_selected),]
      
      # Récupération des valeurs courantes
      element_cour <- data$element_courant
      element_cour_2 <- data$element_courant_2
      element_cour_3 <- data$element_courant_3
      
      # Mise à jour des champs
      updateSelectizeInput(session = session, inputId = "item_element", choices = data$elements, selected = element_cour)
      updateSelectizeInput(session = session, inputId = "item_element_2", choices = data$elements, selected = element_cour_2)
      updateSelectizeInput(session = session, inputId = "item_element_3", choices = data$elements, selected = element_cour_3)
      
      write_csv(data$elements, path = "data/elements.csv")
      message_enregistrer <<- showNotification(paste("L'élément \"", elements_a_supprimer, "\" a été retiré"),
                                               duration = 5, type = "message", closeButton = TRUE)
    }
  })
  
  
  
  # Description élément #
  # ------------------- #
  
  # Table dynamique des desc. d'éléments
  output$table_descriptions_elements = DT::renderDataTable({
    data$descriptions_elements
  }, rownames = TRUE, server = TRUE
  )
  
  # Valeurs sélectionnées par l'utilisateur dans la table des desc. d'éléments
  # (on fait appel à cette fonction `selection_description_element()`)
  selection_description_element <- reactive ({
    input$table_descriptions_elements_rows_selected
  })
  
  # Suppression d'une description d'élément si selectionnée et bouton de suppression cliqué
  observeEvent(input$bouton_supprimer_description_element,{
    
    if (!is.null(selection_description_element())) {
      # Il y a eu une sélection par l'utilisateur de valeurs à supprimer de la liste
      descriptions_elements_a_supprimer <- input$table_descriptions_elements_rows_selected
      descriptions_elements_a_supprimer <- str_c(data$descriptions_elements$description_element[descriptions_elements_a_supprimer], collapse = ";")
      data$descriptions_elements <- data$descriptions_elements[-as.numeric(input$table_descriptions_elements_rows_selected),]
      
      # Récupération des valeurs courantes
      description_element_cour <- data$description_element_courant
      description_element_cour_2 <- data$description_element_courant_2
      description_element_cour_3 <- data$description_element_courant_3
      
      # Mise à jour des champs
      updateSelectizeInput(session = session, inputId = "item_description_element", choices = data$descriptions_elements, selected = description_element_cour)
      updateSelectizeInput(session = session, inputId = "item_description_element_2", choices = data$descriptions_elements, selected = description_element_cour_2)
      updateSelectizeInput(session = session, inputId = "item_description_element_3", choices = data$descriptions_elements, selected = description_element_cour_3)
      
      write_csv(data$descriptions_elements, path = "data/descriptions_elements.csv")
      message_enregistrer <<- showNotification(paste("La desc. d'élément \"", descriptions_elements_a_supprimer, "\" a été retirée"),
                                               duration = 5, type = "message", closeButton = TRUE)
    }
  })
  
  # Matériaux #
  # --------- #
  
  # Table dynamique des matériaux
  output$table_materiaux = DT::renderDataTable({
    data$materiaux
  }, rownames = TRUE, server = TRUE
  )
  
  # Valeurs sélectionnées par l'utilisateur dans la table des matériaux
  # (on fait appel à cette fonction `selection_materiau()`)
  selection_materiau <- reactive ({
    input$table_materiaux_rows_selected
  })
  
  # Suppression d'un matériau si selectionné et bouton de suppression cliqué
  observeEvent(input$bouton_supprimer_materiau,{
    
    if (!is.null(selection_materiau())) {
      # Il y a eu une sélection par l'utilisateur de valeurs à supprimer de la liste
      materiaux_a_supprimer <- input$table_materiaux_rows_selected
      materiaux_a_supprimer <- str_c(data$materiaux$materiau[materiaux_a_supprimer], collapse = ";")
      data$materiaux <- data$materiaux[-as.numeric(input$table_materiaux_rows_selected),]
      
      # Récupération des valeurs courantes
      materiau_cour <- data$materiau_courant
      materiau_cour_2 <- data$materiau_courant_2
      materiau_cour_3 <- data$materiau_courant_3
      
      # Mise à jour des champs
      updateSelectizeInput(session = session, inputId = "item_materiau", choices = data$materiaux, selected = materiau_cour)
      updateSelectizeInput(session = session, inputId = "item_materiau_2", choices = data$materiaux, selected = materiau_cour_2)
      updateSelectizeInput(session = session, inputId = "item_materiau_3", choices = data$materiaux, selected = materiau_cour_3)
      
      write_csv(data$materiaux, path = "data/materiaux.csv")
      message_enregistrer <<- showNotification(paste("Le matériau \"", materiaux_a_supprimer, "\" a été retiré"),
                                               duration = 5, type = "message", closeButton = TRUE)
    }
  })
  
  # Désordre #
  # -------- #
  
  # Table dynamique des désordres
  output$table_desordres = DT::renderDataTable({
    data$desordres
  }, rownames = TRUE, server = TRUE
  )
  
  # Valeurs sélectionnées par l'utilisateur dans la table des désordres
  # (on fait appel à cette fonction `selection_desordre()`)
  selection_desordre <- reactive ({
    input$table_desordres_rows_selected
  })
  
  # Suppression d'un désordre si selectionné et bouton de suppression cliqué
  observeEvent(input$bouton_supprimer_desordre,{
    
    if (!is.null(selection_desordre())) {
      # Il y a eu une sélection par l'utilisateur de valeurs à supprimer de la liste
      desordres_a_supprimer <- input$table_desordres_rows_selected
      desordres_a_supprimer <- str_c(data$desordres$desordre[desordres_a_supprimer], collapse = ";")
      data$desordres <- data$desordres[-as.numeric(input$table_desordres_rows_selected),]
      
      # Récupération des valeurs courantes
      desordre_cour <- data$desordre_courant
      desordre_cour_2 <- data$desordre_courant_2
      desordre_cour_3 <- data$desordre_courant_3
      
      # Mise à jour des champs
      updateSelectizeInput(session = session, inputId = "item_desordre", choices = data$desordres, selected = desordre_cour)
      updateSelectizeInput(session = session, inputId = "item_desordre_2", choices = data$desordres, selected = desordre_cour_2)
      updateSelectizeInput(session = session, inputId = "item_desordre_3", choices = data$desordres, selected = desordre_cour_3)
      
      write_csv(data$desordres, path = "data/desordres.csv")
      message_enregistrer <<- showNotification(paste("Le désordre \"", desordres_a_supprimer, "\" a été retiré"),
                                               duration = 5, type = "message", closeButton = TRUE)
    }
  })
  
  # Qualifications #
  # -------------- #
  
  # Table dynamique des zones
  output$table_qualification = DT::renderDataTable({
    data$qualification
  }, rownames = TRUE, server = TRUE
  )
  
  # Valeurs sélectionnées par l'utilisateur dans la table des qualifications
  # (on fait appel à cette fonction `selection_qualification()`)
  selection_qualification <- reactive ({
    input$table_qualification_rows_selected
  })
  
  # Suppression d'une qualification si selectionnée et bouton de suppression cliqué
  observeEvent(input$bouton_supprimer_qualification,{
    
    if (!is.null(selection_qualification())) {
      # Il y a eu une sélection par l'utilisateur de valeurs à supprimer de la liste
      qualification_a_supprimer <- input$table_qualification_rows_selected
      qualification_a_supprimer <- str_c(data$qualification$qualification[qualification_a_supprimer], collapse = ";")
      data$qualification <- data$qualification[-as.numeric(input$table_qualification_rows_selected),]
      
      # Récupération des valeurs courantes
      qualification_cour <- data$qualification_courante
      qualification_cour_2 <- data$qualification_courante_2
      qualification_cour_3 <- data$qualification_courante_3
      
      # Mise à jour des champs
      updateSelectizeInput(session = session, inputId = "item_qualification", choices = data$qualification, selected = qualification_cour)
      updateSelectizeInput(session = session, inputId = "item_qualification_2", choices = data$qualification, selected = qualification_cour_2)
      updateSelectizeInput(session = session, inputId = "item_qualification_3", choices = data$qualification, selected = qualification_cour_3)
      
      write_csv(data$qualification, path = "data/qualification.csv")
      message_enregistrer <<- showNotification(paste("La qualification \"", qualification_a_supprimer, "\" a été retirée"),
                                               duration = 5, type = "message", closeButton = TRUE)
    }
  })
  
  
  ###########################
  ## ONGLET DE PROGRESSION ##
  ###########################
  
  # Affichage de la table des valeurs renseignées par l'utilisateur
  output$tableau_avancement <- DT::renderDataTable(DT::datatable({
    data$data_verification_tmp
  }))
  
  # Message d'information sur l'état d'avancement des transcriptions de l'enquêteur
  output$progression_val <- renderText({
    if(is.null(data$data_verification_tmp)){
      # Aucune valeurs pré-existantes renseignées pour l'enquêteur
      str_c("Aucune transcription.")
    }else{
      # Il existe des valeurs pré-existantes, on les dénombre
      nb_arretes_transcrits <- 
        data$data_verification_tmp %>% 
        select(id_arrete) %>% 
        unique() %>% 
        nrow()
      
      str_c("Nombre de transcriptions : ", nb_arretes_transcrits, "/", nrow(data$pathos_enqueteur), ".")
    }
  })
  
  
  
  
  
}

shinyApp(ui,server)