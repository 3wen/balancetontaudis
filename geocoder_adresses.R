# Geocoder les adresses de l'enquete citoyenne sur l'habitat indigne

library(tidyverse)
library(magrittr)
library(jsonlite)
library(httr)

df <- read_csv2("data/enquete-citoyenne-logement-indigne-anonymise.csv")


# Obtenir les coordonnees GPS des addresses
# https://adresse.data.gouv.fr/csv#preview
library(ggmap)
df <- df %>% 
  mutate(
    id = row_number(),
    adresse_geo = str_c(Adresse, ", ", `Code postal`))


# Quelques corrections manuelles
df <- 
  df %>% 
  mutate(adresse_geo = ifelse(adresse_geo == "les balustres , 1, place du recteur jules blache, 13013", "1 place du recteur jules blache, 13013", adresse_geo),
         adresse_geo = ifelse(adresse_geo == "78 au 82 rue Bernard du Bois, 13002", "80 rue Bernard du Bois, 13002", adresse_geo),
         adresse_geo = ifelse(adresse_geo == "22 rue François Moisson donnant également sur la rue des Phocéens 13002 Marseille, 13002", "22 rue François Moisson, 13002", adresse_geo),
         adresse_geo = ifelse(adresse_geo %in% c("TOULON REPUBLIQUE CRIMINELLE 394 BLD FENELON 83200 TOULON SIGNALé depuis 6 MOIS au Syndic TRANSACT au Maire de Toulon et au MINISTERE DU LOGEMENT. A ce jour aucune nouvelle...., 83200",
                                                 "394 BLD FENELON, 83200",
                                                 "394 BLD FENELON 83200 TOULON, 83200"), "394 Boulevard Fenelon, 83200", adresse_geo),
         adresse_geo = ifelse(adresse_geo == "80 rue montecristo 13004 dernier local en rentrant sur la droite avant de rentrer dans le parking weldom, 13004", "80 rue monte cristo, 13004", adresse_geo),
         adresse_geo = ifelse(adresse_geo == "300 avenue de La Capelette derrière l’immeuble au fond de la cour, 13010", "300 avenue de La Capelette, 13010", adresse_geo),
         adresse_geo = ifelse(adresse_geo == "84 rue carnot a st omer nous vivons dans le noir rempli d'humidité le bâtiment ce rempli d'eau de champignons et on peut plus vivre même dans respiré, 62500", "84 Rue Carnot 62500 Saint-Omer", adresse_geo)
  )


#' geocoder_adresse
#' Utilise l'API adresse.data.gouv.fr pour geocoder
#' une adresse
#' @param q recherche plein texte
#' q <- df$adresse_geo[i]
geocoder_adresse <- function(q){
  lien_api <- str_c("https://api-adresse.data.gouv.fr/search/")
  reponse <- GET(lien_api, query = list(q = q, limit = "1"))
  response_content <- content(reponse)
  
  variable_names <- 
    c("longitude", "latitude", "result_label", "result_score", "result_type",
    "result_id", "result_housenumber", "result_name", "result_street", "result_postcode",
    "result_city", "result_context", "result_citycode")
  
  if(length(response_content$features)){
    # Recuperer les coordonnees
    longitude <- response_content$features[[1]]$geometry$coordinates[[1]]
    latitude <- response_content$features[[1]]$geometry$coordinates[[2]]
    result_label <- response_content$features[[1]]$properties$label
    result_score <- response_content$features[[1]]$properties$score
    result_type <- response_content$features[[1]]$properties$type
    result_id <- response_content$features[[1]]$properties$id
    result_housenumber <- response_content$features[[1]]$properties$housenumber
    result_name <- response_content$features[[1]]$properties$name
    result_street <- response_content$features[[1]]$properties$street
    result_postcode <- response_content$features[[1]]$properties$postcode
    result_city <- response_content$features[[1]]$properties$city
    result_context <- response_content$features[[1]]$properties$context
    result_citycode <- response_content$features[[1]]$properties$citycode
  }else{
    for(var in variable_names){
      assign(var, NA)
    }
  }
  
  for(var in variable_names){
    if(is.null(get(var))){
      assign(var, NA)
    }
  }
  
  
  data.frame(
    q = q,
    longitude = longitude,
    latitude =latitude,
    result_label = result_label,
    result_score = result_score,
    result_type = result_type,
    result_id = result_id,
    result_housenumber = result_housenumber,
    result_name = result_name,
    result_street = result_street,
    result_postcode = result_postcode,
    result_city = result_city,
    result_context = result_context,
    result_citycode = result_citycode
  )
}# Fin de geocoder_adresse()



adresses_geo <- vector("list", length(df$adresse_geo))
pb <- txtProgressBar(min = 0, max = length(adresses_geo), style = 3)
for(i in 1:length(adresses_geo)){
  adresse_tmp <- df$adresse_geo[i]
  geocode_tmp <- try(geocoder_adresse(adresse_tmp))
  if(!inherits(geocode_tmp, "try-error")){
   # S'il n'y a pas eu d'erreur
    adresses_geo[[i]] <- geocode_tmp %>% mutate(id = df$id[i])
  }else{
    stop("toto")
  }
  setTxtProgressBar(pb, i)
}


# adresses_geo %>% 
#   bind_rows() %>% 
#   tbl_df() %>% 
#   filter(result_score < 0.4) %>% View()

# Ajouter les adresses dans les donnees initiales
df <- df %>% 
  left_join(
    adresses_geo %>%
      bind_rows() %>%
      tbl_df() %>%
      filter(result_score>=0.4),
    by = c("adresse_geo" = "q", "id" = "id")
  )

save(df, file = "data/enquete_geocoded.rda")


