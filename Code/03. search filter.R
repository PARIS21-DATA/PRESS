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
# input files: - /Code/00.1 text_preparation_functions.R
#              - /Data/intermediate/crs02_full.rds
#              - /Data/statistics_reduced_*.txt
#              - /Data/gender_*.txt
#              - /Data/demining_small_arms_*.txt
#              - /Data/statistics_reduced_acronyms_*.txt
#              
#
# output file: - /Data/intermediate/crs03_full.rds
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
crs_path_new <- "./Data/intermediate/crs03_sample_en.rds"
crs_path <- "./Data/intermediate/crs02_sample.rds"
df_crs <- readRDS(crs_path)
#df_crs <- sample_n(df_crs, size = nrow(df_crs)/50) # take sample to speed up testing
#saveRDS(df_crs, "./Data/intermediate/crs02_sample.rds")

# we used to make the project with same description as 1 as long as one of the same description is marked as 1
# it is wrong because some projects with the same name will have different purpose codes


#---------------------------- Set parameters -----------------------------------

# All languages to include in classification
languages <- c("en")


#------------------------- Data frame preparation ------------------------------

# Add unique title id and detect language of title and long description
df_crs <- df_crs %>%
  mutate(projecttitle_lower = tolower(projecttitle)) %>%
  mutate(title_id = as.numeric(as.factor(projecttitle_lower))) %>% 
  mutate(title_language = cld2::detect_language(projecttitle)) %>%
  mutate(long_language = cld2::detect_language(longdescription))

# Save raw data for later
df_crs_backup <- df_crs

# Select necessary columns and drop projects with duplicated title ids, later merged 
# with backup according to title id to avoid unecessary computation 
df_crs <- df_crs_backup %>%
  select(title_id, projecttitle, projecttitle_lower, longdescription, title_language, long_language) %>%
  filter(!duplicated(title_id))

# Use only projects in en, fr, es and de
df_crs <- df_crs %>%
  filter(title_language %in% c(languages, NA) & long_language %in% c(languages,NA))

# Inspect language distributions
table(df_crs$long_language)
table(df_crs$title_language)


#------------------------ Translation of German --------------------------------
# Translate German long description since there are not enough detected projects
# to train XGBoost 

# Auth key for free account 
deepl_auth_key <- "fcec1af3-2663-3f39-7f16-dcd07b334f30:fx"

# Translate German long descriptions (takes very long)
df_crs_de <- df_crs %>% filter(long_language == "de") # subset German long language
df_crs_de$longdescription <- deeplr::toEnglish2(df_crs_de$longdescription, auth_key = deepl_auth_key)
saveRDS(df_crs_de, file  = "./Data/Raw/CRS/df_crs_de_translated.rds") # save translation

# Add German long descriptions
df_crs <- df_crs %>%
  filter(long_language != "de" | is.na(long_language)) %>%
  rbind(df_crs_de)
rm(df_crs_de)

#--------------------------- Title detection -----------------------------------

# Set languages for stemming and lemmatization
stem_languages <- c("de", "fr", "es")
lemma_languages <- c("en")

# Add match_stat, match_gender so that it will be found later on
df_crs$match_stat <- NA
df_crs$match_gender <- NA
df_crs$mining <- NA

# Go through every language, load keywords, clean keywords and detect stat, mining and gender
#???: Considerations about computation speed possible, not sure if code below is the fastest one can do
for (lang in languages){
  list_keywords_stat <- read_lines(paste0("./Data/Final keyword lists/statistics_reduced_", lang, ".txt"), skip_empty_rows = TRUE)  %>% trimws()
  list_keywords_gender <- read_lines(paste0("./Data/Final keyword lists/gender_", lang, ".txt"), skip_empty_rows = TRUE)  %>% trimws()
  demining_small_arms <- read_lines(paste0("./Data/Final keyword lists/demining_small_arms_", lang, ".txt"), skip_empty_rows = TRUE)  %>% trimws()
  list_acronyms <- read_lines(paste0("./Data/Final keyword lists/statistics_reduced_acronyms_", lang, ".txt"), skip_empty_rows = TRUE)  %>% trimws()
  
  # Use lemmatization for en
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
                                 str_detect(projecttitle_clean, paste(list_keywords_stat, collapse = " | ")),
                                 match_stat),
             match_gender = ifelse(title_language == lang | is.na(title_language),
                                   str_detect(projecttitle_clean, paste(list_keywords_gender, collapse = " | ")),
                                   match_gender)) %>%
      mutate(mining = ifelse(title_language == lang | is.na(title_language),
                             str_detect(projecttitle_clean, paste(demining_small_arms, collapse = " | ")),
                             mining)) %>% 
      mutate(match_stat = ifelse(title_language == lang | is.na(title_language),
                                 str_detect(projecttitle_lower, paste(list_acronyms, collapse = " | ")) | match_stat,
                                 match_stat))
    print(paste0(lang, " finished"))
    
  }
  # Use stemming for fr, es, de
  else if (lang %in% stem_languages) {
    list_keywords_stat <- stem_and_concatenate(list_keywords_stat, language = lang) 
    list_keywords_gender <- stem_and_concatenate(list_keywords_gender, language = lang)
    if (lang == "de") {
      list_keywords_gender_composed <- list_keywords_gender %>% str_subset("^[:upper:]") # For German: fetch all nouns (start with upper case) to search without whitespaces
      list_keywords_gender_composed <- stem_and_concatenate(list_keywords_gender, language = lang)
    } 
    demining_small_arms <- stem_and_concatenate(demining_small_arms, language = lang)
    
    # Clean projcettitle and detect stat, gender and mining 
    df_crs <- df_crs %>%
      mutate(projecttitle_clean = ifelse(title_language == lang & !is.na(title_language), 
                                         stem_and_concatenate(projecttitle_lower, language = lang),
                                         projecttitle_clean)) %>%
      mutate(match_stat = ifelse(title_language == lang & !is.na(title_language), 
                                 str_detect(projecttitle_clean, paste(list_keywords_stat, collapse = " | ")),
                                 match_stat),
             match_gender = ifelse(title_language == lang & !is.na(title_language),
                                   str_detect(projecttitle_clean, paste(list_keywords_gender, collapse = " | ")),
                                   match_gender)) %>%
      mutate(mining = ifelse(title_language == lang & !is.na(title_language),
                             str_detect(projecttitle_clean, paste(demining_small_arms, collapse = " | ")), 
                             mining)) %>%
      mutate(match_stat = ifelse(title_language == lang & !is.na(title_language),
                                 str_detect(projecttitle_lower, paste(list_acronyms, collapse = " | ")) | match_stat,
                                 match_stat))
    if (lang == "de") {
      # Look for nouns in composed words in German (note collapse = " | " -> collapse = "|")
      df_crs <- df_crs %>% 
        mutate(match_gender = ifelse(title_language == lang & match_gender == FALSE & !is.na(title_language),
                                     str_detect(projecttitle_clean, paste(list_keywords_gender_composed, collapse = "|")),
                                     match_gender))
    }
    
    print(paste0(lang, " finished"))
    }
}

# Uncomment to check result (large file for full CRS)
#write.xlsx(df_crs, file = "./Tmp/Text detection/crs_4lang_classified.xlsx", rownames = F)

# Exclude mining projects, since they contain survey -> not statistical project
df_crs <- df_crs %>%
  mutate(text_detection_wo_mining = match_stat & !mining) %>%
  select(-projecttitle_lower)

# Number of detected projects 
lang <- "en" # select lang
sum(df_crs %>% filter(title_language == lang) %>% pull(match_stat), na.rm = T)
sum(df_crs %>% filter(title_language == lang) %>% pull(match_gender), na.rm = T)


#--------------------------- Merge and check results ---------------------------

# Merge with original data according to title id 
df_crs <- left_join(df_crs_backup, df_crs)

rm(df_crs_backup)

# Include projects with scb purpose code
df_crs <- df_crs %>%
  mutate(text_detection_wo_mining_w_scb = match_stat | scb)
#table(df_crs$text_detection_wo_mining) %>% print
#table(df_crs$text_detection_wo_mining_w_scb) %>% print

# Construct final gender filter out of gen_donor (UN Women), gender purpose code and the text detections
# source("./Code/03.1 gender_preclassification_comp.R") # compile different data sets for comparison of gender markers
#df_crs <- df_crs %>%
#  mutate(text_filter_gender = gen_donor|gen_ppcode|match_gender)

# Create different combinations of gender filters to see differences in classification
gen_combinations <- c("gen_donor", "gen_ppcode", "gen_sdg", "gen_marker",
                      "gen_donor|gen_ppcode", "gen_donor|gen_ppcode|gen_marker|gen_sdg",
                      "(gen_donor&gen_ppcode&gen_marker&gen_sdg)")
#gen_combinations <- gtools::permutations(n = 3, r = 3, v = gen_combinations)
#gen_combinations <- apply(gen_combinations, 1, function(x) paste0(x, collapse = '|'))
gen_combinations <- paste0("match_gender|", gen_combinations)
gen_combinations <- c("match_gender", gen_combinations) 

for (comb in gen_combinations){
  df_crs_tmp <- df_crs %>%
      mutate(text_filter_gender = eval(parse(text = comb)))
  saveRDS(df_crs_tmp, file = str_replace_all(paste0("./Data/Gender permutations/df_crs_", comb, ".rds"), "\\|", "_"))
}

a = df_crs %>% select(text_id, text_detection_wo_mining_w_scb, match_gender) %>% unique %>% nrow
b = df_crs %>% select(text_id) %>% unique %>% nrow


print(paste0("There are ", a-b, " projects with same names but different purpose code"))

names(df_crs)

saveRDS(df_crs,file = crs_path_new)

#write.csv(df_crs, file = "data/intermediate/crs_filter_results.csv", row.names = F)

