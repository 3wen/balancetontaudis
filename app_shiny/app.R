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
  data.frame(statut = c("terminee", "en_attente", "non_traitee"),
             color = c("olive", "orange", "maroon"),
             label = c("Terminée", "En cours", "Non traitée"))

# Chargement des references
desordres <- read_csv("data/desordres.csv") %>% arrange(desordre)
elements <- read_csv("data/elements.csv") %>% arrange(element)
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
      menuItem("Arrêtés", tabName = "arretes", icon = icon("th")),
      # menuItem("Ajout catégories", tabName = "categories", icon = icon("th")),
      menuItem("Progression", tabName = "progression", icon = icon("th"))
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
        ), # Fin du premier element
        # ------------------------------ #
        # Onglet d'affichage des arretes #
        # ------------------------------ #
        tabItem(tabName = "arretes",
                # h2("Annonces"),
                # shinyWidgets::searchInput(
                #   value = NULL,
                #   inputId = "recherche_annonce_indice",
                #   label = "Accéder à une annonce par son identifiant",
                #   placeholder = "Identifiant de l'annonce",
                #   btnSearch = icon("search"),
                #   btnReset = icon("remove"),
                #   width = "350px"
                # ),
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
                                             style="color: #fff; background-color: #3399ff; border-color: #0033cc"),
                                radioButtons(inputId = "item_ignorer", label = "Ignorer l'item ?",
                                             choices = list("Oui" = "Oui",
                                                            "Non" = "Non"),
                                             selected = NULL,
                                             inline = TRUE),
                                selectizeInput("item_zone",
                                            label = "Quelle est la zone",
                                            choices = zones,
                                            selected = NA,
                                            multiple = FALSE,
                                            options = list(create = TRUE)),
                                selectizeInput("item_etage",
                                            label = "Quel est l'étage",
                                            choices = etages,
                                            selected = NA,
                                            multiple = TRUE,
                                            options = list(create = TRUE)),
                                selectizeInput("item_element",
                                            label = "Quel est l'élément",
                                            choices = elements,
                                            selected = NA,
                                            multiple = TRUE,
                                            options = list(create = TRUE)),
                                selectizeInput("item_desordre",
                                            label = "Quel est le désordre",
                                            choices = desordres,
                                            selected = NA,
                                            multiple = TRUE,
                                            options = list(create = TRUE)),
                                selectizeInput("item_qualification_desordre",
                                            label = "Quelle est la qualification du désordre",
                                            choices = qualification,
                                            selected = NA,
                                            multiple = TRUE,
                                            options = list(create = TRUE)),
                                br(),br()
                         )
                       )
                )
        ),# Fin du deuxieme element
        # ---------------------------- #
        # Onglet d'ajout de catégories #
        # ---------------------------- #
        # tabItem(tabName = "categories",
        #         column(12,
        #                box(
        #                  width = NULL, solidHeader = TRUE, status = "primary",
        #                  column(12,
        #                         textInput("ajout_zone", "Ajouter une zone", NULL),
        #                         actionButton("valider_zone", "Ajouter", width = "150px",
        #                                      style="color: #fff; background-color: #3399ff; border-color: #0033cc")
        #                  )),
        #                box(
        #                  width = NULL, solidHeader = TRUE, status = "primary",
        #                  column(12,
        #                         textInput("ajout_etage", "Ajouter un étage", NULL),
        #                         actionButton("valider_etage", "Ajouter", width = "150px",
        #                                      style="color: #fff; background-color: #3399ff; border-color: #0033cc")
        #                  )),
        #                box(
        #                  width = NULL, solidHeader = TRUE, status = "primary",
        #                  column(12,
        #                         textInput("ajout_element", "Ajouter un élément", NULL),
        #                         actionButton("valider_element", "Ajouter", width = "150px",
        #                                      style="color: #fff; background-color: #3399ff; border-color: #0033cc")
        #                  )),
        #                box(
        #                  width = NULL, solidHeader = TRUE, status = "primary",
        #                  column(12,
        #                         textInput("ajout_desordre", "Ajouter un désordre", NULL),
        #                         actionButton("valider_desordre", "Ajouter", width = "150px",
        #                                      style="color: #fff; background-color: #3399ff; border-color: #0033cc")
        #                  )),
        #                box(
        #                  width = NULL, solidHeader = TRUE, status = "primary",
        #                  column(12,
        #                         textInput("ajout_qualification_desordre", "Ajouter une qualificaiton de désordre", NULL),
        #                         actionButton("valider_qualification_desordre", "Ajouter", width = "150px",
        #                                      style="color: #fff; background-color: #3399ff; border-color: #0033cc")
        #                  ))
        #                # )
        #         )
        # ), # Fin du troisieme element
        # --------------------- #
        # Onglet de progression #
        # --------------------- #
        tabItem(tabName = "progression",
                column(12,
                       h2("Progression de la transcription"),
                       verbatimTextOutput("progression_val"),
                       DT::dataTableOutput("tableau_avancement")
                )
        ) # Fin du quatrieme element
      )# Fin de tabItems
          )# Fin de fluidRow
      )# Fin de dashboardBody
    )# Fin de dashboardPage

server = function(input, output,session) {
  
  
  data <- reactiveValues(id_enqueteur=NULL,
                         pathos_courant=NULL,
                         pathos_enqueteur_split = NULL,
                         pathos_courant_items = NULL,
                         id_arrete = NULL,
                         id_item = NULL,
                         statut_courant = NULL,
                         pathos_enqueteur = NULL,
                         data_verification_tmp = NULL,
                         zone_courante = NA,
                         etage_courant = NA,
                         element_courant = NA,
                         desordre_courant = NA,
                         qualification_desordre_courante = NA,
                         ignorer_courante = NULL,
                         indices_arretes_items = NULL)
  
  
  # Affichage de texte sur la page de connexion #
  output$id_enqueteur_choisi <- renderUI({
    
    
    pick_img <- list.files("www/img/")
    
    
    if(!is.null(data$id_enqueteur))
      HTML(str_c("<br/>Vous êtes authentifié comme enquêteur : ", names(liste_enqueteurs)[as.numeric(data$id_enqueteur)],
                 "<br/><br/>",
                 '<img src="img/', sample(pick_img,1), '" height = "200px"/>'))
    
  })
  
  
  output$info_box_annonce <- renderInfoBox({
    
    # Recuperation du statut de l'annonce
    statut_courant <- as.character(data$statut_courant)
    
    if(is.null(data$statut_courant)) data$statut_courant <- "en_attente"
    
    # Couleur et label associes au statut
    couleurs_status_cour <- 
      couleurs_status %>% 
      filter(statut == statut_courant)
    
    couleur_cour <- couleurs_status_cour$color
    label_cour <- couleurs_status_cour$label
    
    # "- (total de ", nrow(data$pathos_enqueteur), " annonces pour l'enquêteur")
    
    
    
    infoBox(
      "Arrêté",
      paste(paste0(data$pathos_courant$id_arrete, "/", nrow(pathos)),
            paste0("Item ", data$id_item, "/", length(data$pathos_enqueteur_split))
      ),
      paste0(label_cour), icon = icon("file"),
      color = couleur_cour
    )
  })
  
  
  
  
  observeEvent(input$enqueteur_choix_valider,{
    
    data$id_enqueteur <- input$id_enqueteur_choix
    
    
    id_enqueteur <- input$id_enqueteur_choix
    
    # Chargement des registres
    # desordres <- read_csv("data/desordres.csv")
    # elements <- read_csv("data/elements.csv")
    # etages <- read_csv("data/etages.csv")
    # qualification <- read_csv("data/qualification.csv")
    # zones <- read_csv("data/zones.csv")
    
    pathos_enqueteur <- pathos %>% filter(id_enqueteur == data$id_enqueteur)
    data$pathos_enqueteur <- pathos_enqueteur
    
    
    pathos_enqueteur_split <- 
      pathos_enqueteur$pathologies %>% 
      str_split(., "\r\n\r\n")
    
    data$pathos_enqueteur_split <- pathos_enqueteur_split
    
    
    indices_arretes_items <- 
      lapply(1:length(pathos_enqueteur_split), function(x){
      data.frame(id_arrete = pathos_enqueteur$id_arrete[x], id_item = seq_len(length(pathos_enqueteur_split[[x]])))
    }) %>% 
      bind_rows() %>% 
      tbl_df()
    
    data$indices_arretes_items <- indices_arretes_items
    
    
    
    # Chargement des donnees de verification des enqueteurs
    if(file.exists(str_c("data/enqueteurs/", data$id_enqueteur, "/arretes_verif_", data$id_enqueteur, ".rda"))){
      load(str_c("data/enqueteurs/", data$id_enqueteur, "/arretes_verif_", data$id_enqueteur, ".rda"))
      data$data_verification_tmp <- arretes_verifiees_enqueteur
      
      
      reste_a_parcourir <- 
        indices_arretes_items %>% 
        anti_join(arretes_verifiees_enqueteur) %>% 
        arrange(id_arrete, id_item)
      
      if(nrow(reste_a_parcourir)>0){
        data$id_arrete <- reste_a_parcourir$id_arrete[1]
        data$id_item <- reste_a_parcourir$id_item[1]
      }else{
        data$id_arrete <- data$pathos_enqueteur$id_arrete[1]
        data$id_item <- 1
      }
      
    }else{
      data$data_verification_tmp <- NULL
      data$id_arrete <- data$pathos_enqueteur$id_arrete[1]
      data$id_item <- 1
    }
    
    
    
    
    pathos_courant <- pathos_enqueteur %>% filter(id_arrete == data$id_arrete)
    data$pathos_courant <- pathos_courant
    
    ind_element <- which(data$pathos_enqueteur$id_arrete == data$id_arrete)
    
    data$pathos_courant_items <- data$pathos_enqueteur_split[[ind_element]]
    
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
  
  # Affichage de l'item courant
  output$contenu_arrete_item <- renderText({
    HTML(data$pathos_courant_items[data$id_item])
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
          
          recuperer_valeurs_verifications()
          
        }
        
      }else if(direction == "precedent"){
        if(which(data$pathos_enqueteur$id_arrete == data$id_arrete) == 1){
          # On était avec le premier arrete
          shinyalert("Attention !", str_c("Il s'agit du premier arrêté"),
                     type = "warning")
        }else{
          # On sauvegarde la progression
          enregistrer_valeurs()
          # On peut passer à l'item précédent
          data$id_arrete <- data$pathos_enqueteur$id_arrete[which(data$pathos_enqueteur$id_arrete == data$id_arrete)-1]
          pathos_courant <- data$pathos_enqueteur %>% filter(id_arrete == data$id_arrete)
          data$pathos_courant <- pathos_courant
          ind_element <- which(data$pathos_enqueteur$id_arrete == data$id_arrete)
          data$pathos_courant_items <- data$pathos_enqueteur_split[[ind_element]]
          # data$pathos_courant_items <- str_split(data$pathos_courant$pathologies, "\r\n\r\n")[[1]]
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
        # data$pathos_courant_items <- str_split(data$pathos_courant$pathologies, "\r\n\r\n")[[1]]
        data$id_item <- reste_a_parcourir$id_item[1]
      }else{
        shinyalert("Attention !", str_c("C'est fini !"),
                   type = "warning")
      }
      
      recuperer_valeurs_verifications()
    }
    
    
    
    
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
  
  # Bouton "suivant" arrete
  observeEvent(input$premier_arrete_non_traite,{
    boutons_navigation(direction = NULL, type = "premier_arrete_non_traite")
  })
  
  observeEvent(input$item_zone,{
    data$zone_courante <- input$item_zone
    if(!input$item_zone %in% c("", zones$zone)){
      # On charge la liste des zones
      # (si jamais un autre utilisateur a deja fait un ajout entre-temps)
      zones <- read_csv("data/zones.csv") %>% arrange(zone)
      # On ajoute la nouvelle proposition à la liste pour une future utilisation
      zones <- 
        zones %>% 
        bind_rows(data.frame(zone = input$item_zone)) %>% 
        unique() %>% 
        arrange(zone)
      
      
      
      write_csv(zones, path = "data/zones.csv")
      message_enregistrer <<- showNotification(paste("La zone \"", input$item_zone, "\" a été ajoutée"),
                                               duration = 5, type = "message", closeButton = TRUE)
    }
  })
  
  observeEvent(input$item_etage,{
    data$etage_courant <- str_c(input$item_etage, collapse = ";")
    if(!input$item_etage %in% c("", etages$etage)){
      # On charge la liste des etages
      # (si jamais un autre utilisateur a deja fait un ajout entre-temps)
      etages <- read_csv("data/etages.csv") %>% arrange(etage)
      # On ajoute la nouvelle proposition à la liste pour une future utilisation
      etages <- 
        etages %>% 
        bind_rows(data.frame(etage = input$item_etage)) %>% 
        unique() %>% 
        arrange(etage)
      
      write_csv(etages, path = "data/etages.csv")
      
      message_enregistrer <<- showNotification(paste("L'étage \"", input$item_etage, "\" a été ajouté"),
                                               duration = 5, type = "message", closeButton = TRUE)
    }
  })
  
  observeEvent(input$item_element,{
    data$element_courant <- input$item_element
    if(!input$item_element %in% c("", elements$element)){
      # On charge la liste des elements
      # (si jamais un autre utilisateur a deja fait un ajout entre-temps)
      elements <- read_csv("data/elements.csv") %>% arrange(element)
      # On ajoute la nouvelle proposition à la liste pour une future utilisation
      elements <- 
        elements %>% 
        bind_rows(data.frame(element = input$item_element)) %>% 
        unique() %>% 
        arrange(element)
      
      write_csv(elements, path = "data/elements.csv")
      
      message_enregistrer <<- showNotification(paste("L'élément \"", input$item_element, "\" a été ajouté"),
                                               duration = 5, type = "message", closeButton = TRUE)
    }
  })
  
  observeEvent(input$item_desordre,{
    data$desordre_courant <- str_c(input$item_desordre, collapse = ";")
    if(!input$item_desordre %in% c("", desordres$desordre)){
      # On charge la liste des desordres
      # (si jamais un autre utilisateur a deja fait un ajout entre-temps)
      desordres <- read_csv("data/desordres.csv") %>% arrange(desordre)
      # On ajoute la nouvelle proposition à la liste pour une future utilisation
      desordres <- 
        desordres %>% 
        bind_rows(data.frame(desordre = input$item_desordre)) %>% 
        unique() %>% 
        arrange(desordre)
      
      write_csv(desordres, path = "data/desordres.csv")
      
      message_enregistrer <<- showNotification(paste("Le désordre \"", input$item_desordre, "\" a été ajouté"),
                                               duration = 5, type = "message", closeButton = TRUE)
    }
  })
  
  observeEvent(input$item_qualification_desordre,{
    data$qualification_desordre_courante <- str_c(input$item_qualification_desordre, collapse = ";")
    if(!input$item_qualification_desordre %in% c("", qualification$qualification)){
      # On charge la liste des qualif. de desordres
      # (si jamais un autre utilisateur a deja fait un ajout entre-temps)
      qualification <- read_csv("data/qualification.csv") %>% arrange(qualification)
      # On ajoute la nouvelle proposition à la liste pour une future utilisation
      qualification <- 
        qualification %>% 
        bind_rows(data.frame(qualification = input$item_qualification_desordre)) %>% 
        unique() %>% 
        arrange(qualification)
      
      write_csv(qualification, path = "data/qualification.csv")
      message_enregistrer <<- showNotification(paste("La qualification \"", input$item_qualification_desordre, "\" a été ajoutée"),
                                               duration = 5, type = "message", closeButton = TRUE)
    }
  })
  
  observeEvent(input$item_ignorer,{
    data$ignorer_courante <- input$item_ignorer
  })
  
  enregistrer_valeurs <- function(){
    # Recuperation des valeurs pour chaque element à sauvegarder

    
    nouvelle_ligne <-
      data.frame(id_enqueteur = data$id_enqueteur,
                 id_arrete = data$pathos_courant$id_arrete,
                 id_item = data$id_item,
                 zone = data$zone_courante,
                 etage = data$etage_courant,
                 element = data$element_courant,
                 desordre = data$desordre_courant,
                 qualification_desordre = data$qualification_desordre_courante,
                 ignorer = data$ignorer_courante,
                 stringsAsFactors = FALSE)
    
    
    
    # Ajout de ces informations aux informations existantes pour les autres annonces
    if(!is.null(data$data_verification_tmp)){
      
      # S'il existe deja un fichier avec des valeurs presentes
      arretes_verifiees_enqueteur <- 
        data$data_verification_tmp %>% 
        filter(!(id_arrete %in% nouvelle_ligne$id_arrete & id_item %in% nouvelle_ligne$id_item)) %>% 
        bind_rows(nouvelle_ligne) %>% 
        unique()
      
    }else{
      arretes_verifiees_enqueteur <- nouvelle_ligne
     
    }
    
    if(!(nouvelle_ligne$zone[1] == "" & 
         is.na(nouvelle_ligne$etage) & 
         is.na(nouvelle_ligne$element) & 
         is.na(nouvelle_ligne$desordre) & 
         is.na(nouvelle_ligne$qualification_desordre) & 
         nouvelle_ligne$ignorer == "Non")){
      
      save(arretes_verifiees_enqueteur, file = str_c("data/enqueteurs/", data$id_enqueteur,
                                                     "/arretes_verif_", data$id_enqueteur, ".rda"))
      data$data_verification_tmp <- arretes_verifiees_enqueteur
      
    }
    
  }# Fin de enregistrer_valeurs()
  
  
  # Recupere les valeurs (si existantes) des verifications
  recuperer_valeurs_verifications <- function(){
    
    arretes_verifiees_enqueteur_cour_tmp <- NULL
    
    if(!is.null(data$data_verification_tmp)){
        # Recuperation des statuts pour les arretes pour lesquels l'enquêteur a déjà indiqué des renseignements  
      arretes_verifiees_enqueteur_cour_tmp <- 
        data$data_verification_tmp %>% 
        filter(id_arrete == data$pathos_courant$id_arrete, id_item == data$id_item)
      
      
      reste_a_parcourir <- 
        data$indices_arretes_items %>% 
        filter(id_arrete == data$id_arrete) %>% 
        anti_join(arretes_verifiees_enqueteur_cour_tmp)
        
      
      if(nrow(reste_a_parcourir)>0){
        data$statut_courant <- "en_attente"
      }else{
        data$statut_courant <- "terminee"
      }
      
      
      
      
    }
    
    
    
    valeurs_existantes <- FALSE
    
    if(!is.null(arretes_verifiees_enqueteur_cour_tmp)){
      if(nrow(arretes_verifiees_enqueteur_cour_tmp)> 0) valeurs_existantes <- TRUE
    }
    
    
    
    if(valeurs_existantes){
      
      
      data$zone_courante <- arretes_verifiees_enqueteur_cour_tmp$zone
      data$etage_courant <- arretes_verifiees_enqueteur_cour_tmp$etage
      data$element_courant <- arretes_verifiees_enqueteur_cour_tmp$element
      data$desordre <- arretes_verifiees_enqueteur_cour_tmp$desordre
      data$qualification_desordre_courante <- arretes_verifiees_enqueteur_cour_tmp$qualification_desordre
      data$ignorer_courante <- arretes_verifiees_enqueteur_cour_tmp$ignorer
      
      
      
      updateSelectInput(session = session, inputId = "item_zone", selected = arretes_verifiees_enqueteur_cour_tmp$zone[1])
      updateSelectInput(session = session, inputId = "item_etage", selected = str_split(arretes_verifiees_enqueteur_cour_tmp$etage[1], ";")[[1]])
      updateSelectInput(session = session, inputId = "item_element", selected = arretes_verifiees_enqueteur_cour_tmp$element[1])
      updateSelectInput(session = session, inputId = "item_desordre", selected = str_split(arretes_verifiees_enqueteur_cour_tmp$desordre[1], ";")[[1]])
      updateSelectInput(session = session, inputId = "item_qualification_desordre", selected = str_split(arretes_verifiees_enqueteur_cour_tmp$qualification_desordre[1], ";")[[1]])
      updateRadioButtons(session = session, inputId = "item_ignorer", selected = arretes_verifiees_enqueteur_cour_tmp$ignorer[1])
    }else{
      # Pas de valeurs pré-existantes
      
      
      
      updateSelectInput(session = session, inputId = "item_zone", selected = NA)
      updateSelectInput(session = session, inputId = "item_etage", selected = NULL)
      updateSelectInput(session = session, inputId = "item_element", selected = NULL)
      updateSelectInput(session = session, inputId = "item_desordre", selected = NULL)
      updateSelectInput(session = session, inputId = "item_qualification_desordre", selected = NULL)
      updateRadioButtons(session = session, inputId = "item_ignorer", selected = "Non")
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
  
  
  output$tableau_avancement <- DT::renderDataTable(DT::datatable({
    data$data_verification_tmp
  }))
  
  
  output$progression_val <- renderText({
    if(is.null(data$data_verification_tmp)){
      str_c("Aucune transcription.")
    }else{
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