library(dplyr)
library(tidytext)
library(proustr)
library(remotes)
library(widyr)
library(mixr)

remotes::install_github("lvaudor/mixr")

setwd("/home/arthur/Documents/nextcloud_sync/Documents/dataPoisson/environnement_et_cancers-main/data/ds/")

data <- read.csv("discours_scientifique_plus.csv", sep = ";", header = TRUE)

#garder seuelement display_name ; abstract ; top_concepts

data_work <- data %>%
  select(display_name, abstract, top_concepts,publication_year)


#dans data_work des que l'on voit le mot radon ont garde la ligne sinon ont supprime

data_radon_filter <- data_work %>%
  filter(grepl("radon", abstract, ignore.case = TRUE) | grepl("radon", top_concepts, ignore.case = TRUE))


#tokenisation des mots dans les abstracts
radon_abstract_token<- data_radon_filter %>% 
  tidytext::unnest_tokens(output="mot_abstract",
                          input=abstract,
                          token="ngrams",
                          n=1) 

radon_abstract_token_trier <- radon_abstract_token %>%
  anti_join(tidytext::stop_words,by=c("mot_abstract"="word"))

radon_abstract_token_english<- mixr::get_lexicon("en")

freq_lemmes <- radon_abstract_token_trier %>%
  group_by(mot_abstract, publication_year) %>% 
  summarise(freq=n()) %>% 
  arrange(desc(freq)) %>% 
  na.omit()
head(freq_lemmes)





