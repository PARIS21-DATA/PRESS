rm(list = ls())
source("code/00.1 functions.R")
crs_path_new <- "./Data/intermediate/crs03.rds"
crs_path <- "./Data/intermediate/crs02.rds"
df_crs <- readRDS(crs_path)

# we used to make the project with same description as 1 as long as one of the same description is marked as 1
# it is wrong because some projects with the same name will have different purpose codes



## 1.c. split projects by language delimiters "." and " / "

# df_crs$description_comb = iconv(df_crs$description_comb,"WINDOWS-1252","UTF-8")

# crs$description_comb = NULL
# names(crs)
# crs <- cSplit(crs, "toDetect", ".", "long")
# crs <- cSplit(crs, "toDetect", " / ", "long")
# save(crs, file = "crs_2020_clean1_splitToDetect.RData")


df_crs <- df_crs %>%
  filter(language == "en") %>% ##??? to solve later
  mutate(projecttitle_lower = tolower(projecttitle)) %>%
  mutate(title_id = as.numeric(as.factor(projecttitle_lower))) 

df_crs_backup <- df_crs


df_crs <- df_crs_backup %>%
  select(title_id, projecttitle_lower, longdescription) %>%
  filter(!duplicated(title_id)) 

# beep(4)
list_keywords_stat <- readLines("data/statistics_reduced_en.txt")  %>%
  trimws()
list_keywords_gender <- readLines("data/gender_en.txt")  %>%
  trimws()

# Libraries for lemmatization
library(text2vec)
library(tm)
library(textstem)
library(textcat)
library(textclean)
library(lexicon)
library(quanteda) 

# Function to clean strings and lemmatize
clean_and_lemmatize <- function (string){
  string <- string %>% 
    tolower %>% 
    removeNumbers() %>%
    removePunctuation(preserve_intra_word_dashes = TRUE) %>%
    stripWhitespace %>%
    removeWords(c(stopwords('english'))) %>% 
    removeWords(c(stopwords(source = "smart"))) %>%
    lemmatize_strings()
  return(string)
}

# Lemmatize or stem keywords 
list_keywords_stat_stem <- stem_and_concatenate(list_keywords_stat)
list_keywords_gender_stem <- stem_and_concatenate(list_keywords_gender)
list_keywords_stat <- clean_and_lemmatize(list_keywords_stat)
list_keywords_gender <- clean_and_lemmatize(list_keywords_gender)


df_crs <- df_crs %>%
  # select(db_ref, projecttitle, scb) %>%
  # mutate(projecttitle = tolower(projecttitle)) %>%
  mutate(projecttitle_clean = clean_and_lemmatize(projecttitle_lower),
         longdescription_clean = clean_and_lemmatize(longdescription)) %>%
  mutate(match_stat_lemma = str_detect(projecttitle_clean, paste(list_keywords_stat, collapse = "|")),
         match_gender_lemma = str_detect(projecttitle_clean, paste(list_keywords_gender, collapse = "|"))) %>%
  mutate(match_stat_lemma_long = str_detect(longdescription_clean, paste(list_keywords_stat, collapse = "|")),
         match_gender_lemma_long = str_detect(longdescription_clean, paste(list_keywords_gender, collapse = "|"))) %>%
  mutate(projecttitle_stem = stem_and_concatenate(projecttitle_lower)) %>%
  mutate(match_stat_stem = str_detect(projecttitle_stem, paste(list_keywords_stat_stem, collapse = "|")))  %>%
  mutate(match_gender_stem =str_detect(projecttitle_stem, paste(list_keywords_gender_stem, collapse = "|")) )
# 
# tagged.results <- treetag(list_keywords, 
#                           treetagger="manual", format="obj",
#                           TT.tknz=FALSE , lang="en"
#                           # ,
#                           # TT.options=list(path="./TreeTagger", preset="en")
#                           )

#library(openxlsx)
#openxlsx::write.xlsx(df_crs, file = paste0(getwd(), "/Tmp/crs_text_detection_comp.xlsx"), rowNames = FALSE)

df_crs$mining = grepl("land mine|small arm|demining|demine|landmine", df_crs$projecttitle_lower, ignore.case = T)

list_acronyms <- readLines("data/statistics_reduced_acronyms_en.txt")  %>%
  trimws()

df_crs <- df_crs %>%
  mutate(match_stat_stem = str_detect(projecttitle_lower, paste(list_acronyms, collapse = "|"))  | match_stat_stem)

# Number of detected projects 
sum(df_crs$match_stat_lemma)
sum(df_crs$match_stat_lemma_long)
sum(df_crs$match_gender_lemma)
sum(df_crs$match_gender_stem)


df_crs <- df_crs %>%
  mutate(text_detection_wo_mining = match_stat_stem & !mining) %>%
  select(-projecttitle_lower, -projecttitle_stem)

df_crs <- left_join(df_crs_backup, df_crs)

rm(df_crs_backup)

langues <- c("en","fr","es")
df_crs <- df_crs %>%
  select(-projecttitle_lower) %>%
  mutate(language = ifelse(language %in% langues, language, "other"))

df_crs <- df_crs %>%
  mutate( text_detection_wo_mining_w_scb = text_detection_wo_mining | scb)
table(df_crs$text_detection_wo_mining) %>% print
table(df_crs$text_detection_wo_mining_w_scb) %>% print
which(is.na(df_crs$text_detection_wo_mining_w_scb))

#??? PROBLEM: no gen_donor and gen_marker found 
#df_crs <- df_crs %>%
#  mutate(text_filter_gender = gen_donor|gen_ppcode|gen_marker|text_detection_gender)

a = df_crs %>% select(text_id, text_detection_wo_mining_w_scb, match_gender_stem) %>% unique %>% nrow
b = df_crs %>% select(text_id) %>% unique %>% nrow


print(paste0("There are ", a-b, " projects with same names but different purpose code"))

names(df_crs)

saveRDS(df_crs,file = crs_path_new)

write.csv(df_crs, file = "data/intermediate/crs_filter_results.csv", row.names = F)



####################################
#!!! Lemmatization test

library(text2vec)
library(tm)
library(textstem)
library(textcat)
library(textclean)
library(lexicon)
library(quanteda) # for stopwords(source = "smart")

# Define function to create corpus, employs way through corpus whereas clean_and_lemmatize()
# does not create a corpus first
create_df_corpus <- function (data){
  source <- VectorSource(data$projecttitle_lower)
  corpus <- VCorpus(source) 
  corpus <- tm_map(corpus, content_transformer(tolower)) # lower case
  corpus <- tm_map(corpus, removeNumbers)# remove numbers
  corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE) # remove punctuation. We want to keep dashes inside words such as in high-level
  corpus <- tm_map(corpus, stripWhitespace) # remove remaining white space
  corpus <- tm_map(corpus, removeWords, c(stopwords('english'))) # remove stopwords for English
  corpus <- tm_map(corpus, removeWords, c(stopwords(source = "smart"))) # remove some extra stopwords not captured by the previous list
  #corpus <- tm_map(corpus, removeWords, c("iii")) # remove roman number 3 that is very common
  #corpus <- tm_map(corpus, removeWords, c(stopwords("fr"))) # remove french stopwords
  #corpus <- tm_map(corpus, replace_contraction) # extra step to catch exceptions such as "aren't" - might not be necessary
  corpus <- tm_map(corpus, lemmatize_strings) # lemmatize words - this might not be the best choice in particular with a English-French language mix
  corpus <- tm_map(corpus, PlainTextDocument) # transform into a format that can be used more easily later on
  text_df <- t(data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE))
  rownames(text_df) <- NULL
  return(text_df)
}


crs_corpus <- create_df_corpus(df_crs)
beep(2)

df_crs_tmp <- df_crs %>%
  select(title_id, projecttitle_lower, projecttitle_clean, projecttitle_stem)

df_crs_tmp$projettitle_corpus <- crs_corpus[,1]
