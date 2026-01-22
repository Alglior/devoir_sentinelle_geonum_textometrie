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

data_work <- subset(data, select = c(
  display_name,
  abstract,
  top_concepts,
  publication_year
))

data_work <- data_work %>%
  mutate(periode = case_when(
    publication_year >= 1950 & publication_year <= 1990 ~ "1950-1990",
    publication_year >= 1991 & publication_year <= 2005 ~ "1991-2005",
    publication_year >= 2006 & publication_year <= 2015 ~ "2006-2015",
    publication_year >= 2016 & publication_year <= 2026 ~ "2016-2026",
    TRUE ~ "Autre"
  ))

#dans data_work des que l'on voit le mot radon ont garde la ligne sinon ont supprime
data_radon_filter <- data_work %>%
  filter(grepl("radon", abstract, ignore.case = TRUE) | grepl("radon", top_concepts, ignore.case = TRUE))


radon_abstract_token <- data_radon_filter %>% tidytext::unnest_tokens(output="mot_abstract",
                                                                      input=abstract,
                                                                      token="ngrams",
                                                                      n=1) 

radon_abstract_token_trie <- radon_abstract_token %>%
  anti_join(tidytext::stop_words,by=c("mot_abstract"="word"))

english_lexicon <- get_lexicon(language = "en")

merge_radon_lexicon <- merge(radon_abstract_token_trie, english_lexicon,by.x = "mot_abstract",
                   by.y = "word")

radon_abstract_token_lemma <- subset(merge_radon_lexicon, select = c(
  display_name,
  top_concepts,
  publication_year,
  periode,
  lemma
))

freq_lemmes <- radon_abstract_token_lemma %>%
  group_by(lemma,periode) %>% 
  summarise(freq=n()) %>% 
  arrange(desc(freq)) %>% 
  na.omit()
head(freq_lemmes)


#correlation en fonction des année de publication
#mots_corelation <- radon_abstract_token_lemma %>% 
 # widyr::pairwise_count(lemma,feature=publication_year,sort=TRUE)

radon_specificities <- mixr::tidy_specificities(radon_abstract_token_lemma,
                                                lemma,
                                                periode)
