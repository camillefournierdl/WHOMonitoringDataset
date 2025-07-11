library(tidyverse)
library(stringi)
library(fuzzyjoin)

datasetHand <- read.csv("exampleCode/data/classificationByHand3.csv", sep = ";")

clean_title <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%  # turn “é” → “e”
    tolower() %>%                          # lowercase
    gsub("[^[:alnum:] ]+", "", .) %>%      # drop punctuation
    trimws()                               # trim whitespace
}

datasetGPTForID <- read.csv("exampleCode/data/datasetToClassifyGPT.csv") %>%
  select(ID, Title) %>% 
  rename(IDgpt = ID) %>% 
  mutate(title_clean = clean_title(Title))

datasetHandForID <- datasetHand %>% 
  group_by(ID) %>% 
  summarize(Title = first(Title)) %>% 
  rename(IDhand = ID) %>% 
  mutate(title_clean = clean_title(Title))

df_merged2 <- stringdist_left_join(
  datasetHandForID, datasetGPTForID,
  by = "title_clean",
  method = "lv",        # Levenshtein
  max_dist = 2,         # tweak this
  distance_col = "dist" # keep the distance
) %>%
  arrange(-dist) %>%          # smallest distances first
  group_by(IDhand) %>% 
  summarize(IDgpt = first(IDgpt),
            Title = first(Title.y))

write.csv(df_merged2, "exampleCode/output/tableIDMatching.csv", row.names = F)
