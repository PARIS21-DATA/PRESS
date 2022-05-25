################################################################################
#
# Title detection for CRS data 
# Author: Yu Tian, Johannes Abele
# Date: 05/10/2022
#
# Objective: 
#            
#            
# 
# input files: - Data/paradata.RData
#              - 
#              - 
#              - 
#              
#
# output file: - 
#
#
################################################################################


# ------------------------------- Preparation ----------------------------------

rm(list = ls())

# Load packages
source("./Code/00. boot.R")

# Load stemming and lemmatization functions
source("./Code/00.1 text_preparation_functions.R")

# Set paths
crs_path_new <- "./Data/intermediate/crs03.rds"
crs_path <- "./Data/intermediate/crs02_original.rds"
df_crs <- readRDS(crs_path)

# we used to make the project with same description as 1 as long as one of the same description is marked as 1
# it is wrong because some projects with the same name will have different purpose codes


#---------------------------- Set parameters -----------------------------------

# All languages to include in classification
languages <- c("en", "fr", "es", "de")


#------------------------- Data frame preparation ------------------------------

# Add unique title id and detect language of title
df_crs <- df_crs %>%
  mutate(projecttitle_lower = tolower(projecttitle)) %>%
  mutate(title_id = as.numeric(as.factor(projecttitle_lower))) %>% 
  mutate(title_language = cld2::detect_language(projecttitle)) 

# Save raw data for later
df_crs_backup <- df_crs

# Select necessary columns and drop projects with duplicated title ids, later merged 
# with backup according to title id to avoid unecessary computation 
df_crs <- df_crs_backup %>%
  select(title_id, projecttitle, projecttitle_lower, longdescription, title_language, long_language = language) %>%
  filter(!duplicated(title_id))

# Use only projects in en, fr, es and de
df_crs <- df_crs %>%
  filter(title_language %in% c(languages, NA) & long_language %in% c(languages,NA))

# Inspect language distribution of long descriptions
table(df_crs$long_language)



#------------------------ Translation of German --------------------------------

# Auth key for free account 
deepl_auth_key <- "fcec1af3-2663-3f39-7f16-dcd07b334f30:fx"

# Translate German long descriptions (takes very long)
df_crs <- df_crs %>%
  mutate(longdescription = ifelse(long_language == "de", 
                                  deeplr::toEnglish2(longdescription, auth_key = deepl_auth_key),
                                  longdescription))




#--------------------------- Title detection -----------------------------------

# Set languages for stemming and lemmatization
stem_languages <- c("de", "fr", "es")
lemma_languages <- c("en")

# Add match_stat, match_gender so that it will be found later on
df_crs$match_stat <- NA
df_crs$match_gender <- NA
df_crs$mining <- NA


# Go through every language, load keywords, clean keywords and detect stat and gender
#!!! TO DO: projecttitle_clean is overwritten by stemming languages 
for (lang in languages){
  list_keywords_stat <- readLines(paste0("./Data/statistics_reduced_", lang, ".txt"))  %>% trimws()
  list_keywords_gender <- readLines(paste0("./Data/gender_", lang, ".txt"))  %>% trimws()
  demining_small_arms <- readLines(paste0("./Data/demining_small_arms_", lang, ".txt"))  %>% trimws()
  list_acronyms <- readLines(paste0("./Data/statistics_reduced_acronyms_", lang, ".txt"))  %>% trimws()
  
  if (lang %in% lemma_languages) {
    list_keywords_stat <- clean_and_lemmatize(list_keywords_stat, language = lang)
    list_keywords_gender <- clean_and_lemmatize(list_keywords_gender, language = lang)
    list_keywords_gender <- list_keywords_gender %>% append("women") # due to many spelling mistakes like women_s, women?s..
    demining_small_arms <- clean_and_lemmatize(demining_small_arms, language = lang)
    
    # Clean projcettitle and detect stat, gender and mining 
    df_crs <- df_crs %>%
      mutate(projecttitle_clean = ifelse(title_language == lang | is.na(title_language), 
                                         clean_and_lemmatize(projecttitle_lower, language = lang),
                                         projecttitle_lower)) %>%
      mutate(match_stat = ifelse(title_language == lang | is.na(title_language), 
                                 str_detect(projecttitle_clean, paste(list_keywords_stat, collapse = "|")),
                                 match_stat),
             match_gender = ifelse(title_language == lang | is.na(title_language),
                                   str_detect(projecttitle_clean, paste(list_keywords_gender, collapse = "|")),
                                   match_gender)) %>%
      mutate(mining = ifelse(title_language == lang | is.na(title_language),
                             str_detect(projecttitle_clean, paste(demining_small_arms, collapse = "|")),
                             mining)) %>% 
      mutate(match_stat = ifelse(title_language == lang | is.na(title_language),
                                 str_detect(projecttitle_lower, paste(list_acronyms, collapse = " | ")) | match_stat,
                                 match_stat))
    print(paste0(lang, " finished"))
    
  }
  else if (lang %in% stem_languages) {
    list_keywords_stat <- stem_and_concatenate(list_keywords_stat, language = lang) 
    list_keywords_gender <- stem_and_concatenate(list_keywords_gender, language = lang)
    demining_small_arms <- stem_and_concatenate(demining_small_arms, language = lang)
    demining_small_arms <- stem_and_concatenate(demining_small_arms, language = lang)
    
    # Clean projcettitle and detect stat, gender and mining 
    df_crs <- df_crs %>%
      mutate(projecttitle_clean = ifelse(title_language == lang, 
                                         stem_and_concatenate(projecttitle_lower, language = lang),
                                         projecttitle_clean)) %>%
      mutate(match_stat = ifelse(title_language == lang, 
                                 str_detect(projecttitle_clean, paste(list_keywords_stat, collapse = "|")),
                                 match_stat),
             match_gender = ifelse(title_language == lang,
                                   str_detect(projecttitle_clean, paste(list_keywords_gender, collapse = "|")),
                                   match_gender)) %>%
      mutate(mining = ifelse(title_language == lang,
                             str_detect(projecttitle_clean, paste(demining_small_arms, collapse = "|")), 
                             mining)) %>%
      mutate(match_stat = ifelse(title_language == lang,
                                 str_detect(projecttitle_lower, paste(list_acronyms, collapse = " | ")) | match_stat,
                                 match_stat))
    print(paste0(lang, " finished"))
    
    }
}

write.xlsx(df_crs, file = "./Tmp/Text detection/crs_4lang_classified.xlsx", rownames = F)

df_crs <- df_crs %>%
  mutate(text_detection_wo_mining = match_stat & !mining) %>%
  select(-projecttitle_lower)

# Number of detected projects 
sum(df_crs$match_stat)
sum(df_crs$match_gender)

#!!! KEEP for later maybe
# Exclude mining projects, since they contain survey -> not statistical project
#if (lang == "en") {
#  df_crs$mining = grepl("land mine|small arm|demining|demine|landmine", df_crs$projecttitle_lower, ignore.case = T)
#} else if (lang == "de"){
#  df_crs$mining = grepl("Landmine|Handfeuerwaffe|Minenentschärfung|Minenräumung|entmienen", df_crs$projecttitle_lower, ignore.case = T)
#} 


#--------------------------- Merge and check results ---------------------------

# Merge with original data according to title id 
df_crs <- left_join(df_crs_backup, df_crs)

rm(df_crs_backup)

langues <- c("en","fr","es")
df_crs <- df_crs %>%
  select(-projecttitle_lower) %>%
  mutate(language = ifelse(language %in% langues, language, "other"))

df_crs <- df_crs %>%
  mutate(text_detection_wo_mining_w_scb = text_detection_wo_mining | scb)
table(df_crs$text_detection_wo_mining) %>% print
table(df_crs$text_detection_wo_mining_w_scb) %>% print
which(is.na(df_crs$text_detection_wo_mining_w_scb))

#??? PROBLEM: no gen_donor and gen_marker found 
df_crs <- df_crs %>%
  mutate(text_filter_gender = gen_donor|gen_ppcode|gen_marker|text_detection_gender)

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
