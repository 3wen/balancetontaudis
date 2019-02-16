
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

df_urls$filname <- 
  df_urls$url %>% str_split("/") %>% 
  sapply(., function(x) x[length(x)])


any(duplicated(df_urls$filname))


#' dl_arrete
#' Telecharge l'arrete de mise en peril de la ligne `i` de df_urls
dl_arrete <- function(i){
  lien_dl <- str_c("http://logement-urbanisme.marseille.fr", df_urls$url[i])
  download.file(url = lien_dl, destfile = str_c("data/arretes/pdf/", df_urls$filname[i]))
}# Fin de dl_arrete()

# Boucler sur `df_url` pour telecharger les fichiers PDF
pb <- txtProgressBar(min = 0, max = nrow(df_urls), style = 3)
for(i in 1:nrow(df_urls)){
  try(dl_arrete(i))
  setTxtProgressBar(pb, i)
}

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



N <- list.files("data/arretes/", pattern = "\\.pdf$")
pb <- txtProgressBar(min = 0, max = length(N), style = 3)
for(i in 1:length(N)){
  try(extraire_texte_pdf(N[i]))
  setTxtProgressBar(pb, i)
}



