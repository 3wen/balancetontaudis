
# Recuperation des arretes sur http://logement-urbanisme.marseille.fr/

library(tidyverse)
library(httr)
library(rvest)
library(magrittr)

page <- read_html("http://logement-urbanisme.marseille.fr/am%C3%A9lioration-de-lhabitat/arretes-de-peril")


div_arrond <- page %>% html_nodes("div.panel.panel-default")
arrondissements <- div_arrond %>% html_nodes("h4.panel-title a") %>% html_text()
num_arrondissements <- arrondissements %>% str_extract("^[[:digit:]]{1,2}") %>% as.numeric()

#' recuperer_liens_arrond
#' Recupere les liens vers le fichiers d'arretes de peril
#' pour l'arrondissement en position `i` dans `arrondissements`
#' Retourne un tableau de donnee avec le nom d'arrondissement et les liens
#' @param i indice d'arrondissement dans `arrondissements`
#' i <- 1
recuperer_liens_arrond <- function(i){
  
  liens_arretes <- 
    div_arrond %>% 
    extract(i) %>% 
    html_nodes("div.panel-body ul li a")
  
  data.frame(
    arrondissement = arrondissements[i],
    arrondissement_num = num_arrondissements[i],
    url = liens_arretes %>% 
      html_attr("href"),
    text = liens_arretes %>% 
      html_text(),
    stringsAsFactors = FALSE) %>% 
    tbl_df()
  
}# Fin de recuperer_liens_arrond()

df_urls <- 
  lapply(1:length(arrondissements), recuperer_liens_arrond) %>% 
  bind_rows()

# Ajout du type d'arrete
df_urls <- 
  df_urls %>% 
  mutate(type = ifelse(str_detect(url, "Mains_levees|Main lev.e|Mains_Levees"), "Main Levee", NA),
         type = ifelse(str_detect(url, "Arretes-peril"), "Arrete Peril", type),
         type = ifelse(str_detect(url, "Arretes-deconstruction"), "Arrete Deconstruction", type)
  )

df_urls$filename <- 
  df_urls$url %>% str_split("/") %>% 
  sapply(., function(x) x[length(x)])


any(duplicated(df_urls$filename))


#' dl_arrete
#' Telecharge l'arrete de mise en peril de la ligne `i` de df_urls
# (requiert d'avoir deja un dossier data/arretes/pdf/)
dl_arrete <- function(i){
  lien_dl <- str_c("http://logement-urbanisme.marseille.fr", df_urls$url[i])
  download.file(url = lien_dl, destfile = str_c("data/arretes/pdf/", df_urls$filename[i]))
}# Fin de dl_arrete()

# Ne pas telecharger les fichiers deja presents
N <- list.files("data/arretes/pdf", pattern = "\\.pdf$")
val_i <- which(!df_urls$filename %in% N)

# Boucler sur `df_url` pour telecharger les fichiers PDF
pb <- txtProgressBar(min = 0, max = nrow(df_urls), style = 3)
for(i in val_i){
  try(dl_arrete(i))
  setTxtProgressBar(pb, i)
}

# ---------------------- #
# Geocodage des adresses #
# ---------------------- #

N <- list.files("data/arretes/txt/", pattern = "\\.rda$", full.names = TRUE)

df_urls$filename[[225]] %>%  str_sub(1, 24)
df_urls$filename %>% 
  str_replace_all(str_c("^mainlevee-", "^ML-PARTIELLE", "^ml-", 
                    "^ARRETE_MODIFICATIF_DE_DECONSTRUCTION-",
                    "^REINTEGRATION-PARTIELLE-", sep = "|") %>% 
                fixed(ignore_case = TRUE), "")

df_urls$adresse <- 
  df_urls$filename %>% 
  # Retrait de eventuels qualificatifs avant l'adresse
  str_replace_all(str_c("^mainlevee(-|_)?", "^ML-PARTIELLE(-|_)?", "^ml(-|_)?",
                        "^ARRETE_MODIFICATIF_DE_DECONSTRUCTION(-|_)?",
                        "^pgi_modificatif(-|_)?",
                        "^REINTEGRATION-PARTIELLE(-|_)?", sep= "|") %>% 
                    regex(ignore_case = T), "") %>% 
  # Retrait de la fin du nom de fichier
  str_replace("((-|_)130[[:digit:]]{2})?(-|_)?201[[:digit:]](.*?)\\.pdf$", "") %>% 
  str_replace("((-|_)130[[:digit:]]{2})(.*?)$", "") %>% 
  str_replace(regex("-1305_pi", ignore_case = T), "") %>% 
  str_trim() %>% 
  str_replace(regex("(-|_)pi$", ignore_case = T), "") %>% 
  str_replace_all("-|_", " ")
  

df_urls <- 
  df_urls %>% 
  mutate(adresse = str_c(adresse, " 13", str_pad(df_urls$arrondissement_num, width = 3, side = "left", pad = "0")))


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

df_urls <- df_urls %>% mutate(id = row_number())

adresses_geo <- vector("list", length(df_urls$adresse))
pb <- txtProgressBar(min = 0, max = length(adresses_geo), style = 3)
for(i in 1:length(adresses_geo)){
  adresse_tmp <- df_urls$adresse[i]
  geocode_tmp <- try(geocoder_adresse(adresse_tmp))
  if(!inherits(geocode_tmp, "try-error")){
    # S'il n'y a pas eu d'erreur
    adresses_geo[[i]] <- geocode_tmp %>% mutate(id = df_urls$id[i])
  }else{
    stop("toto")
  }
  setTxtProgressBar(pb, i)
}

df_urls <- df_urls %>% 
  left_join(
    adresses_geo %>%
      bind_rows() %>%
      tbl_df(),
      # filter(result_score>=0.4),
    by = c("adresse" = "q", "id" = "id")
  )

save(df_urls, file = "data/arretes_peril_geocoded.rda")
write_csv2(df_urls, path = "data/arretes_peril_geocoded.csv")



df_marseille <- df_urls

p <- 
  ggplot() +
  geom_polygon(data = marseille_df, aes(x = long, y = lat, group = group), fill = "white", colour = "grey80") +
  geom_point(data = df_marseille, aes(x = longitude, y = latitude), colour = "black", fill = "red", shape = 21, size = .5) +
  coord_quickmap() +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggtitle("Arretes (perils, mains levées, etc.)") +
  geom_density_2d(data = df_marseille, aes(x = longitude, y = latitude))

ggsave(p, filename = "test.pdf", width = 6, height = 6)



# ------------------------------------------ #
# Reconnaissance de caracteres dans les PDFs #
# ------------------------------------------ #

# Nous allons utiliser le package tesseract pour recuperer le texte des fichiers PDF.
# via de la reconnaissance de caracteres (ocr)

# install.packages("tesseract")
library(tesseract)
# tesseract_download("fra")
fra <- tesseract("fra")

#' extraire_texte_pdf
#' Extrait le texte d'un PDF, puis sauvegarde le resultat (au format rda) dans le dossier
#' data/arretes/txt/ en conservant le nommage du fichier d'origine
#' @param fichier (string) nom du fichier
extraire_texte_pdf <- function(fichier){
  text <- tesseract::ocr(str_c("data/arretes/pdf/", fichier), engine = fra)
  save(text, file = str_c("data/arretes/txt/", str_replace(fichier, "\\.pdf$", ".rda")))
}# Fin de extraire_texte_pdf()



N <- list.files("data/arretes/pdf", pattern = "\\.pdf$")
pb <- txtProgressBar(min = 0, max = length(N), style = 3)

# install.packages("pbapply")
library(pbapply)
textes <- 
  pbapply::pblapply(N, function(x){
  try(extraire_texte_pdf(x))
})







N <- list.files("data/arretes/txt/", pattern = "\\.rda$", full.names = TRUE)

textes <- lapply(N, function(x) {
  load(x)
  text
})


un_texte <- textes[[1]]
un_texte <- str_c(un_texte, collapse = "\n")



str_extract("Les personnes morales déclarées responsables pénalement, dans les conditions prévues par l'article", "dans(.*?)$")





# install.packages("SnowballC")
library(SnowballC)
# install.packages("tm")
library(tm)

my_stopwords <- c(stopwords('french'))
textes_corpus <- Corpus(VectorSource(textes))

textes_dtm <- 
  TermDocumentMatrix(textes_corpus,
                     control = list(removePunctuation = TRUE,
                                    stopwords = my_stopwords,
                                    # stopwords=FALSE,
                                    tolower = TRUE,
                                    stemming = TRUE,
                                    removeNumbers = FALSE,
                                    bounds = list(global = c(1, Inf))))

ft <- findFreqTerms(textes_dtm, lowfreq = 10, highfreq = Inf)
ft




# A regarder (pistes)
# Clustering en fonction des raisons de l'arrete
# Clustering en fonction des actions mises en oeuvre
# Nombre de mots danes les documents par arrondissement
# Qui est l'expert
