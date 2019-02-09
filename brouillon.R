library(tidyverse)
library(magrittr)
library(rgdal)
library(maptools)

# Charger les donnees geocodees de l'enquete (cf geocoder_adresses.R)
load("data/enquete_geocoded.rda")

# Carte des quartiers de Marseille
marseille <- readOGR(dsn="data/cartes/quartiersmarseille/", layer="contours_quartiers_Marseille")
# Étape pour changer la projection de la carte
marseille <- spTransform(marseille, CRS("+proj=longlat +ellps=GRS80"))
# Pour permettre la jointure des objets géométriques
marseille@data$id <- rownames(marseille@data)
# Transformer en data frame pour fournir à ggplot()
marseille_points <- fortify(marseille, region="id")
# Permet d'éviter des trous éventuels
marseille_df <- left_join(marseille_points, marseille@data, by="id") %>% 
  tbl_df()

df_marseille <- 
  df %>% 
  filter(result_city %in% c("Marseille"))

df_marseille <- 
  df_marseille %>% 
  rename(peril = `a déjà été frappé d'un arrêté de péril imminent`)

library(knitr)

df_marseille %>% 
  group_by(peril) %>% 
  summarise(Freq = n()) %>% 
  ungroup() %>% 
  mutate(Pourcentage = (Freq / sum(Freq) * 100) %>% round(2))




ggplot() +
  geom_polygon(data = marseille_df, aes(x = long, y = lat, group = group), fill = "white", colour = "grey80") +
  geom_point(data = df_marseille, aes(x = longitude, y = latitude), colour = "black", fill = "red", shape = 21, size = .5) +
  facet_wrap(~peril) +
  coord_quickmap() +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggtitle("A déjà été frappé d'un arrêté de péril imminent")


df <- 
  df %>% 
  rename(etat_logement = `état du logement`)


library(tm)
my_stopwords <- c(stopwords('french'))
etat_logement <- Corpus(VectorSource(unique(df$etat_logement)))

opinions_tdm <- 
  TermDocumentMatrix(etat_logement,
                     control = list(removePunctuation = TRUE,
                                    stopwords = my_stopwords,
                                    # stopwords=FALSE,
                                    tolower = TRUE,
                                    stemming = TRUE,
                                    removeNumbers = FALSE,
                                    bounds = list(global = c(1, Inf))))

5/100*nrow(df)




inspect(opinions_tdm)
ft <- findFreqTerms(opinions_tdm, lowfreq = 10, highfreq = Inf)
ft
library(wordcloud)


opinions_tdm_matrix <- as.matrix(opinions_tdm)
freq_mots <- opinions_tdm_matrix %>% rowSums()
freq_mots_df <- data.frame(mot = names(freq_mots), freq = freq_mots) %>% 
  tbl_df() %>% 
  arrange(desc(freq))


wordcloud(words = freq_mots_df$mot, freq = freq_mots_df$freq, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# Bigrams


bigram_tokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}



BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm.bigram = TermDocumentMatrix(etat_logement,
                                control = list(tokenize = BigramTokenizer))
freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)

opinions_tdm_bigrams <- 
  DocumentTermMatrix(etat_logement,
                     control = list(tokenize = bigram_tokenizer))


freq = sort(rowSums(as.matrix(opinions_tdm_bigrams)),decreasing = TRUE)
freq %>% tail()
opinions_tdm_matrix_bigram <- as.matrix(opinions_tdm_bigrams)
freq_mots_bigrams <- opinions_tdm_matrix_bigram %>% rowSums()
freq_mots_df_bigrams <- data.frame(mot = names(freq_mots_bigrams), freq = freq_mots_bigrams) %>% 
  tbl_df() %>% 
  arrange(desc(freq))


wordcloud(words = freq_mots_df_bigrams$mot, freq = freq_mots_df_bigrams$freq, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

install.packages("RWeka")
library(RWeka)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

commentaires <- df_marseille$`état du logement`
commentaires[which(is.na(commentaires))] <- "commentaires"

docs<-VCorpus(VectorSource(commentaires),
              readerControl = list(language = "fr"))

docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, my_stopwords)
# docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, stripWhitespace)


tdm <- 
  TermDocumentMatrix(docs, control = list(
    tokenize = BigramTokenizer))

opinions_tdm_matrix_bigram <- as.matrix(tdm)
freq_mots_bigrams <- opinions_tdm_matrix_bigram %>% rowSums()
freq_mots_df_bigrams <- data.frame(mot = names(freq_mots_bigrams), freq = freq_mots_bigrams) %>% 
  tbl_df() %>% 
  arrange(desc(freq))

freq_mots_df_bigrams
