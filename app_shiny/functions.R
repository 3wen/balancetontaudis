setClass("input_selectize", representation(name = "character",
                                           name_list_values = "character",
                                           name_list_values_var = "character"))

setClass("ignore_button", representation(name = "character",
                                           name_list_values = "character",
                                           name_list_values_var = "character"))


setClass("desordre", 
         representation(name = "character",
                        id_desordre = "numeric",
                        values = "list"
         ))

setClass("desordres", 
         representation(desordres = "list"
         ))

setClass("navigation_button", 
         representation(
           name = "character",
           type = "character",
           direction = "character"
         ))


setClass("remove_button", 
         representation(
           name = "character",
           name_list_values = "character",
           name_list_values_var = "character"
         ))

selectize_item_detail_element <- 
  new("input_selectize", name = "item_details_elements",
      name_list_values = "details_elements",
      name_list_values_var = "detail_element")

selectize_item_detail_desordre <- 
  new("input_selectize", name = "item_details_desordres",
      name_list_values = "details_desordres",
      name_list_values_var = "detail_desordre")

selectize_item_element <- 
  new("input_selectize", name = "item_element",
      name_list_values = "elements",
      name_list_values_var = "element")

selectize_item_materiau <- 
  new("input_selectize", name = "item_materiau",
      name_list_values = "materiaux",
      name_list_values_var = "materiau")

selectize_item_zone <- 
  new("input_selectize", name = "item_zone",
      name_list_values = "zones",
      name_list_values_var = "zone")

selectize_item_etage <- 
  new("input_selectize", name = "item_etage",
      name_list_values = "etages",
      name_list_values_var = "etage")

selectize_item_desordre <-
  new("input_selectize", name = "item_desordre",
      name_list_values = "desordres",
      name_list_values_var = "desordre")

selectize_item_qualification_desordre <-
  new("input_selectize", name = "item_qualification_desordre",
      name_list_values = "qualifications_desordres",
      name_list_values_var = "qualification_desordre")



desordre_1 <- 
  new("desordre", name = "desordre_1", id_desordre = 1,
      values = list(
        detail_element = selectize_item_detail_element,
        detail_desordre = selectize_item_detail_desordre,
        element = selectize_item_element,
        materiau = selectize_item_materiau,
        zone = selectize_item_zone,
        etage = selectize_item_etage,
        desordre = selectize_item_desordre,
        qualification_desordre = selectize_item_qualification_desordre
      )
  )

desordre_2 <- 
  new("desordre", name = "desordre_2", id_desordre = 2,
      values = list(
        detail_element = selectize_item_detail_element,
        detail_desordre = selectize_item_detail_desordre,
        element = selectize_item_element,
        materiau = selectize_item_materiau,
        zone = selectize_item_zone,
        etage = selectize_item_etage,
        desordre = selectize_item_desordre,
        qualification_desordre = selectize_item_qualification_desordre)
  )

desordre_3 <- 
  new("desordre", name = "desordre_3", id_desordre = 3,
      values = list(
        detail_element = selectize_item_detail_element,
        detail_desordre = selectize_item_detail_desordre,
        element = selectize_item_element,
        materiau = selectize_item_materiau,
        zone = selectize_item_zone,
        etage = selectize_item_etage,
        desordre = selectize_item_desordre,
        qualification_desordre = selectize_item_qualification_desordre
      )
  )

desordres <- new("desordres", desordres = list(desordre_1, desordre_2, desordre_3))

button_item_precedent <- new("navigation_button",
                             name = "item_precedent",
                             type = "item",
                             direction = "previous")

button_item_suivant <- new("navigation_button",
                           name = "item_suivant",
                           type = "item",
                           direction = "next")

button_arrete_precedent <- new("navigation_button",
                               name = "arrete_precedent",
                               type = "arrete",
                               direction = "previous")

button_arrete_suivant <- new("navigation_button",
                             name = "arrete_suivant",
                             type = "arrete",
                             direction = "next")



button_remove_detail_element <- 
  new("remove_button",
      name = "detail_element",
      name_list_values = "details_elements",
      name_list_values_var = "detail_element")

button_remove_detail_desordre <- 
  new("remove_button",
      name = "detail_desordre",
      name_list_values = "details_desordres",
      name_list_values_var = "detail_desordre")

button_remove_element <- 
  new("remove_button",
      name = "element",
      name_list_values = "elements",
      name_list_values_var = "element")

button_remove_materiau <- 
  new("remove_button",
      name = "materiau",
      name_list_values = "materiaux",
      name_list_values_var = "materiau")

button_remove_zone <- 
  new("remove_button",
      name = "zone",
      name_list_values = "zones",
      name_list_values_var = "zone")

button_remove_etage <- 
  new("remove_button",
      name = "etage",
      name_list_values = "etages",
      name_list_values_var = "etage")

button_remove_desordre <- 
  new("remove_button",
      name = "desordre",
      name_list_values = "desordres",
      name_list_values_var = "desordre")

button_remove_qualification_desordre <- 
  new("remove_button",
      name = "qualification_desordre",
      name_list_values = "qualifications_desordres",
      name_list_values_var = "qualification_desordre")


# Get the current values provided by the user #
# ----------------------------------- #
setGeneric("get_name", function(x) standardGeneric("get_name"))
setMethod("get_name", signature("input_selectize"), function(x) x@name)
setMethod("get_name", signature("desordre"), function(x) x@name)
setGeneric("get_values", function(x, id) standardGeneric("get_values"))
setMethod("get_values", signature("input_selectize"), function(x, id){
  name_x <- get_name(x)
  input[[str_c(name_x, "_", id)]]
})
setMethod("get_values", signature("desordre"), function(x){
  lapply(x@values, get_values, id = x@id_desordre) %>% 
    c(., id_desordre = x@id_desordre)
})
setMethod("get_values", signature("desordres"), function(x){
  lapply(x@desordres, get_values)
})

# Save the current values provided by the user #
# -------------------------------------------- #
save_values <- function(){
  
  
  # new_lines <- lapply(data$desordres, function(x) data.frame(x, stringsAsFactors = F)) %>% 
  #   bind_rows()
  
  new_lines <- 
    lapply(data$desordres, function(des){
      lapply(des, function(x) ifelse(is.null(x), "", x)) %>% 
        data.frame(stringsAsFactors = F)
    }) %>% 
    bind_rows()
  
  
  
  new_lines <- 
    new_lines %>% 
    mutate(
      id_enqueteur = data$infos_current$id_enqueteur,
      id_arrete = data$infos_current$id_arrete,
      id_item = data$infos_current$id_item
    )
  
  
  
  # Getting the names of the variables in `data$data_levels` that contain
  # the levels for each selectizeInput
  new_value <- FALSE
  names_selectizeInputs <- sapply(desordres@desordres[[1]]@values, function(x) x@name_list_values)
  for(i in 1:length(names_selectizeInputs)){
    
    pre_existing_values <- 
      data$data_levels %>% 
      extract2(names_selectizeInputs[[i]]) %>%
      extract2(names(names_selectizeInputs)[[i]])
    
    current_values <- 
      new_lines[, names(names_selectizeInputs)[[i]]] %>% 
      unique()
    ind_empty_string <- which(current_values == "")
    if(length(ind_empty_string)) current_values <- current_values[-ind_empty_string]
    
    
    # If there is a new value, put it in the reactive data
    if(any(! current_values %in% pre_existing_values)){
      new_value <- TRUE
      new_values <- c(pre_existing_values, current_values)
      data$data_levels[[names_selectizeInputs[[i]]]] <- 
        data.frame(x = new_values, stringsAsFactors = FALSE) %>% 
        set_colnames(names(names_selectizeInputs)[[i]])
      # Then update the list of available choices in the dropdown menu
      # for each disorder
      for(j in 1:length(desordres@desordres)){
        input_id <- str_c("item_", names(names_selectizeInputs)[[i]], "_", j)
        current_value <- input[[input_id]]
        updateSelectizeInput(session = session,
                             inputId = input_id,
                             choices = data$data_levels[names_selectizeInputs[[i]]],
                             selected = current_value)
      }
      
    }
  }
  
  
  
  # If a new level was entered by the user for one of the selectizeInput: save the data to the drive
  if(new_value){
    data_levels <- data$data_levels
    save(data_levels, file = "data/data_levels.rda")
    message_enregistrer <<- showNotification(paste("Référentiels mis à jour"),
                                             duration = 5, type = "message", closeButton = TRUE)
  }
  
  
  # Checking whether the new lines contain something different from NAs for the buttons
  
  # load("data/new_lines.rda")
  # save(new_lines, file = "data/new_lines.rda")
  
  lines_to_remove <- NULL
  for(j in 1:length(desordres@desordres)){
    if(all(new_lines[j, names(desordres@desordres[[1]]@values)] == "")) lines_to_remove <- c(lines_to_remove, j)
  }
  
  if(length(lines_to_remove)) new_lines <- new_lines[-lines_to_remove,]
  
  if(nrow(new_lines)){
    # There are some lines that need to be saved
    if(!is.null(data$records_user)){
      
      records_user <- 
        data$records_user %>% 
        filter(!(id_desordre %in% new_lines$id_desordre & 
                   id_arrete %in% new_lines$id_arrete & 
                   id_item %in% new_lines$id_item)) %>%
        bind_rows(new_lines) %>% 
        arrange(id_arrete, id_item)
    }else{
      records_user <- new_lines %>% arrange(id_arrete, id_item)
    }
    
    
    save(records_user, file = str_c("data/enqueteurs/", data$infos_current$id_enqueteur, "/arretes_verif_",
                                    data$infos_current$id_enqueteur, ".rda"))
    # Updating the reactive values
    data$records_user <- records_user
  }
  
  
}# End of save_values()

update_bouton_desordre <- function(id, selected){
  updateRadioButtons(session = session, inputId = str_c("bouton_desordre_", id), selected = selected)
}

# Fetch existing values #
# --------------------- #

fetch_values_desordre <- function(id){
  existing_values <- NULL
  if(!is.null(data$records_user)){
    infos_current <- data$infos_current
    existing_values <- 
      data$records_user %>% 
      filter(
        id_enqueteur == infos_current$id_enqueteur,
        id_arrete == infos_current$id_arrete,
        id_item == infos_current$id_item,
        id_item == infos_current$id_item,
        id_desordre == id
      )
  }
  existing_values
}# End of fetch_values_desordre()
    


setGeneric("fetch_values", function(x, id, existing_values) standardGeneric("fetch_values"))
setMethod("fetch_values", signature("input_selectize"), function(x, id, existing_values){
  existing_value <- FALSE
  
  input_id <-str_c(get_name(x), "_", id)
  if(nrow(existing_values)){
    existing_value <- TRUE
    selected_values <- str_split(existing_values[x@name_list_values_var], ";")[[1]]
  }else{
    selected_values <- NA
  }
  
  updateSelectizeInput(session = session,
                       inputId = input_id,
                       selected = selected_values)
  
  existing_value
})
setMethod("fetch_values", signature("desordre"), function(x){
  existing_values <- fetch_values_desordre(x@id_desordre)
  
  selected <- "Non"
  if(!is.null(data$records_user)){
    values <- sapply(x@values, fetch_values, id = x@id_desordre, existing_values = existing_values)
    if(any(values)) selected <- "Oui"
  }
  update_bouton_desordre(id = x@id_desordre, selected = selected)
  
})
setMethod("fetch_values", signature("desordres"), function(x){
  lapply(x@desordres, fetch_values)
})

add_counter <- function(current, limit, type){
  if(current < limit){
    resul <- current +1
  }else{
    resul <- current
    shinyalert("Attention !", str_c("Il s'agit du dernier ", type),
               type = "warning")
  }
  resul
}

substract_counter <- function(current, type){
  if(current > 1){
    resul <- current -1
  }else{
    resul <- current
    shinyalert("Attention !", str_c("Il s'agit du premier ", type),
               type = "warning")
  }
  resul
}

setGeneric("jump", function(x, id) standardGeneric("jump"))
setMethod("jump", signature("navigation_button"), function(x, id){
  name_x <- x@name
  type_x <- x@type
  direction_x <- x@direction
  if(type_x == "arrete"){
    ind_current <- data$infos_current$ind_arrete
    
    data$infos_current$id_item <- 1
    
    if(direction_x == "next"){
      new_val_counter <- 
        add_counter(ind_current, data$infos_current$nb_arretes, type = type_x)
      
      data$infos_current$ind_arrete <- new_val_counter
      data$infos_current$id_arrete <-
        data$pathos_user %>% slice(new_val_counter) %>% 
        extract2("id_arrete")
    }else if(direction_x == "previous"){
      new_val_counter <- 
        substract_counter(ind_current, type = type_x)
      
      data$infos_current$ind_arrete <- new_val_counter
      data$infos_current$id_arrete <-
        data$pathos_user %>% slice(new_val_counter) %>% 
        extract2("id_arrete")
    }
    
    # Mise à jour du nombre d'items
    data$infos_current$nb_items <- 
      data$pathos_user_split[[data$infos_current$ind_arrete]] %>% 
      length()
    
  }else if(type_x == "item"){
    ind_current <- data$infos_current$id_item
    
    if(direction_x == "next"){
      data$infos_current$id_item <- 
        add_counter(ind_current, data$infos_current$nb_items, type = type_x)
    }else if(direction_x == "previous"){
      data$infos_current$id_item <- 
        substract_counter(ind_current, type = type_x)
    }
  }
  
})


fetch_arrete <- function(){
  arrete <- data$pathos_user %>% filter(id_arrete == data$infos_current$id_arrete)
  arrete$pathologies
}

fetch_item <- function(){
  pathos_user_split_courant <- data$pathos_user_split[[data$infos_current$ind_arrete]]
  pathos_user_split_courant[[data$infos_current$id_item]]
}

init_session <- function(){
  
  # Assigner enqueteur pour chaque arrete
  data$pathos$id_enqueteur <- rep(1, (nrow(data$pathos))) + 0:5
  
  # Ajout d'un identifiant unique
  data$pathos$id_arrete <- 1:nrow(data$pathos)
  
  
  # Recuperer identifiants utilisateur courant
  data$infos_current <- list(id_enqueteur = input$id_enqueteur_choix, 
                             id_arrete = 1, id_item = 1)
  
  # Les pathos à traiter par l'utilisateur
  data$pathos_user <- data$pathos %>% 
    filter(id_enqueteur == data$infos_current$id_enqueteur) %>% 
    mutate(num_arrete_enqueteur = row_number())
  
  
  
  # Séparation de ces pathos en items (en utilisant "\n)
  data$pathos_user_split <- 
    data$pathos_user$pathologies %>% 
    str_split(., "\r\n\r\n")
  
  # Retirer les textes de longueur nulle
  data$pathos_user_split <- lapply(data$pathos_user_split, function(x) x[which(str_length(x) > 0)])
  
  
  # Nombre total d'arrêtés à traiter
  data$infos_current$nb_arretes <- length(data$pathos_user_split)
  
  
  
  # Initialiser le registre d'arrêtes à parcourir par l'utilisateur
  # reste_a_parcourir <- 
  #   indices_arretes_items %>% 
  #   anti_join(arretes_verifiees_enqueteur) %>% 
  #   arrange(id_arrete, id_item)
  
  
  file_arretes_verif <- str_c("data/enqueteurs/", 
                              data$infos_current$id_enqueteur, 
                              "/arretes_verif_", 
                              data$infos_current$id_enqueteur, ".rda")
  
  if(file.exists(file_arretes_verif)){
    load(file_arretes_verif)
    records_user %>% 
      arrange(id_arrete, id_item, id_desordre)
    data$records_user <- records_user
    fetch_values(desordres)
  }
  
  
  
  # indice courant de l'arreté pour l'enquêteur
  data$infos_current$ind_arrete <- 
    data$pathos_user %>% 
    filter(id_arrete == data$infos_current$id_arrete) %>% 
    extract2("num_arrete_enqueteur")
  
  # Nombre total d'items à traiter pour l'arrete courant
  data$infos_current$nb_items <- 
    data$pathos_user_split[[data$infos_current$ind_arrete]] %>% 
    length()
  
  
}

passer_item_suivant <- function(){
  data$desordres <- get_values(desordres)
  save_values()
  jump(button_item_suivant)
  fetch_values(desordres)
}

passer_item_precedent <- function(){
  data$desordres <- get_values(desordres)
  save_values()
  jump(button_item_precedent)
  fetch_values(desordres)
}

passer_arrete_suivant <- function(){
  data$desordres <- get_values(desordres)
  save_values()
  jump(button_arrete_suivant)
  fetch_values(desordres)
}

passer_arrete_precedent <- function(){
  data$desordres <- get_values(desordres)
  save_values()
  jump(button_arrete_precedent)
  fetch_values(desordres)
}


setGeneric("remove_category", function(x) standardGeneric("remove_category"))
setMethod("remove_category", signature("remove_button"), function(x){
  
  # Index of selected rows
  values_to_rm_ind <- input[[str_c("table_", x@name_list_values, "_rows_selected")]]
  
  if(!is.null(values_to_rm_ind)){
    # The user has selected values to remove
    
    # Getting the current values for the item in tab `Arrêtés` (to be able to put these back)
    current_values_arretes <- get_values(desordres)
    
    
    # Remove selected values #
    # ---------------------- #
    
    # Corresponding values
    values_to_rm <- str_c(data$data_levels[[x@name_list_values]][[x@name_list_values_var]][values_to_rm_ind], collapse = ";")
    # Removing values from the table in the reactive values
    data$data_levels[[x@name_list_values]] <-
      data$data_levels[[x@name_list_values]][-as.numeric(values_to_rm_ind)]
    
    
    
    # Updating the selectizeInput values
    for(id in 1:3){
      selected_values <- current_values_arretes[[id]][[x@name]]
      input_id <-str_c(x@name, "_", id)
      updateSelectizeInput(session = session,
                           inputId = input_id,
                           selected = selected_values)
    }# End of for loop over disaster numbers
    
    # Save the changes
    data_levels <- data$data_levels
    save(data_levels, file = "data/data_levels.rda")
    message_enregistrer <<- 
      showNotification(str_c(x@name, "(s) supprimé(es) : ", values_to_rm),
                       duration = 5, type = "message", closeButton = TRUE)
    
  }
  
})