rm(list = ls())
source("code/00.1 functions.R")
crs_path_new <- "./Data/intermediate/crs03.rds"
crs_path <- "./Data/intermediate/crs02.rds"
df_crs <- readRDS(crs_path)

# we used to make the project with same description as 1 as long as one of the same description is marked as 1
# it is wrong because some projects with the same name will have different purpose codes

# Switch to switch from lemmatization to stemming 
stemming <- FALSE 
lang <- "de"

## 1.c. split projects by language delimiters "." and " / "

df_crs <- df_crs %>%
  filter(language == lang) %>% ##??? to solve later
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

# Try German 
library(udpipe)
ud_model <- udpipe_download_model("german")
ud_model <- udpipe_load_model(ud_model)

german_lemma <- df_crs %>%
  mutate(longdescription_clean = clean_and_lemmatize(longdescription, language =  "de")) 

clean_and_lemmatize(tolower(df_crs$longdescription[12]), language = "de")

# Function to clean strings and lemmatize
clean_and_lemmatize <- function (string, language = "en"){
  string <- string %>% 
    tolower %>% 
    removeWords("'s") %>% # remove possesive s so that plural nouns get lemmatized correctly, e.g. "women's"
    removeNumbers() %>%
    removePunctuation(preserve_intra_word_dashes = TRUE) %>%
    stripWhitespace 
  
  if(language == "en") {
    string <- string %>%  
      removeWords(c(stopwords('english'))) %>% 
      removeWords(c(stopwords(source = "smart")[!stopwords(source = "smart") %in% "use"])) %>% # exclude "use" from smart stopwords 
      lemmatize_strings()
  } else if (language == "de") {
    string <- string %>%  
      removeWords(c(stopwords('german'))) %>%
      enc2utf8() %>%
      udpipe_annotate(ud_model, .) %>%
      as.data.frame() %>%
      pull(lemma) %>% 
      paste(collapse = " ")
  }
  
  return(string)
}



# Lemmatize or stem keywords 
if (stemming) {
  list_keywords_stat <- stem_and_concatenate(list_keywords_stat)
  list_keywords_gender <- stem_and_concatenate(list_keywords_gender)
} else {
  list_keywords_stat <- clean_and_lemmatize(list_keywords_stat, language = lang)
  list_keywords_gender <- clean_and_lemmatize(list_keywords_gender, language = lang)
  if(lang == "en") list_keywords_gender <- list_keywords_gender %>% append("women") # due to many spelling mistakes like women_s, women?s..
}

if (stemming) {
  df_crs <- df_crs %>%
    mutate(projecttitle_clean = stem_and_concatenate(projecttitle_lower),
           longdescription_clean = stem_and_concatenate(longdescription)) %>%
    mutate(match_stat = str_detect(projecttitle_clean, paste(list_keywords_stat, collapse = "|")),
           match_gender = str_detect(projecttitle_clean, paste(list_keywords_gender, collapse = "|")))
} else {
  df_crs <- df_crs %>%
    mutate(projecttitle_clean = clean_and_lemmatize(projecttitle_lower, language = lang),
           longdescription_clean = clean_and_lemmatize(longdescription, language = lang)) %>%
    mutate(match_stat = str_detect(projecttitle_clean, paste(list_keywords_stat, collapse = "|")),
           match_gender = str_detect(projecttitle_clean, paste(list_keywords_gender, collapse = "|")))
}

# Exclude mining projects, since they contain survey -> not statistical project
df_crs$mining = grepl("land mine|small arm|demining|demine|landmine", df_crs$projecttitle_lower, ignore.case = T)

list_acronyms <- readLines("data/statistics_reduced_acronyms_en.txt")  %>%
  trimws()

df_crs <- df_crs %>%
  mutate(match_stat = str_detect(projecttitle_clean, paste(list_acronyms, collapse = "|"))  | match_stat)

# Number of detected projects 
sum(df_crs$match_stat)
sum(df_crs$match_gender)


df_crs <- df_crs %>%
  mutate(text_detection_wo_mining = match_stat & !mining) %>%
  select(-projecttitle_lower)

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

a = df_crs %>% select(text_id, text_detection_wo_mining_w_scb, match_gender) %>% unique %>% nrow
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
