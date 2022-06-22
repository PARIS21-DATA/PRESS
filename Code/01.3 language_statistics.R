################################################################################
#
# Analysing language distribution in CRS data
# Author: Yu Tian, Johannes Abele
# Date: 05/10/2022
#
# Objective: 
#            
# 
# input files: - /Data/Raw/CRS/crs_full.rds
#              
#
# output file: - /Data/Intermediate/crs01_1_full.rds"
#
#
################################################################################


# ------------------------------- Preparation ----------------------------------

rm(list = ls())

# Load packages
source("./Code/00. boot.R")


#---------------------------- Language analysis --------------------------------

# If full data available in Data/Raw/, uncomment to load 
df_crs <- readRDS("./Data/Raw/CRS/crs_full.rds")

# Subset crs data and detect languages of title and long description
df_crs_lang <- df_crs %>%
  #select(projecttitle, longdescription) %>%
  mutate(projecttitle_lower = tolower(projecttitle), longdescription_lower = tolower(longdescription)) %>%
  mutate(description_comb = paste(projecttitle_lower, longdescription_lower, sep = ".")) %>%
  mutate(title_id = as.numeric(as.factor(description_comb))) %>% 
  filter(!duplicated(title_id)) %>%
  mutate(title_language = cld2::detect_language(projecttitle)) %>%
  mutate(long_language = cld2::detect_language(longdescription))

# Function to output indicative statistics about a language
print_statistics <- function(df) {
  n_projects <-  df %>% nrow
  n_no_long <- df %>% filter(str_count(longdescription) < 2) %>% nrow
  n_en <- df %>% filter(title_language == "en") %>% nrow
  n_en_w_long <- df %>% filter(title_language != "en" & str_count(longdescription) > 2) %>% nrow
  print(paste0("Total number of projects: ", n_projects))
  print(paste0("Number of projects without long description: ", n_no_long , "/", n_no_long/n_projects))
  print(paste0("Number of projects with title language English: ", n_en, "/", n_en/n_projects))
  print(paste0("Number of projects with title language not English and with long description: ", n_en_w_long, "/", n_en_w_long/n_projects))
}

# Create data frames for 3 minor languages
df_crs_es <- df_crs_lang %>%
  filter(title_language == "es" | long_language == "es")
df_crs_de <- df_crs_lang %>%
  filter(title_language == "de" | long_language == "de")
df_crs_fr <- df_crs_lang %>%
  filter(title_language == "fr" | long_language == "fr")

# Print statistics for 3 minor languages
print_statistics(df_crs_de)
print_statistics(df_crs_es)
print_statistics(df_crs_fr)

# Create data frame for the frequencies of all languages present in the CRS data
languages_full_crs <- as.data.frame(table(df_crs_lang$title_language)) %>%
  rename(language = Var1, title_language = Freq) %>% 
  left_join(as.data.frame(table(df_crs_lang$long_language)) %>% 
              rename(language = Var1, long_language = Freq), by = "language")
saveRDS(languages_full_crs, file = "./data/languages_full_crs.rds")

# Germany sample
crs_germany_sample <- readRDS("./Data/Raw/Crs/crs_germany_sample.rds")
crs_germany_sample <- crs_germany_sample %>%
  select(projecttitle, longdescription) %>%
  mutate(projecttitle_lower = tolower(projecttitle), longdescription_lower = tolower(longdescription)) %>%
  mutate(title_id = as.numeric(as.factor(paste(projecttitle_lower, longdescription_lower, sep = ".")))) %>%
  filter(!duplicated(title_id)) %>%
  mutate(title_language = cld2::detect_language(projecttitle)) %>%
  mutate(long_language = cld2::detect_language(longdescription))

languages_germany_crs <- as.data.frame(table(crs_germany_sample$title_language)) %>%
  rename(language = Var1, title_language = Freq) %>% 
  left_join(as.data.frame(table(crs_germany_sample$long_language)) %>% 
              rename(language = Var1, long_language = Freq), by = "language")

# Save language specific data frames for later 
saveRDS(df_crs_de, file  = "./Data/Raw/CRS/df_crs_de.rds")
saveRDS(df_crs_fr, file  = "./Data/Raw/CRS/df_crs_fr.rds")
saveRDS(df_crs_es, file  = "./Data/Raw/CRS/df_crs_es.rds")

# df_crs_de <- readRDS("./Data/Raw/CRS/df_crs_de.rds")
# df_crs_fr <- readRDS("./Data/Raw/CRS/df_crs_fr.rds")
# df_crs_es <- readRDS("./Data/Raw/CRS/df_crs_es.rds")

# Count characters for language lang (for determining translation costs)
lang <- "de"

n_char_title <- df_crs_de %>% 
  filter(title_language != "en") %>%
  pull(projecttitle) %>%
  paste(collapse = "") %>%
  str_replace_all(fixed(" "), "") %>%
  str_count()
n_char_long <- df_crs_de %>% 
  filter(long_language == lang) %>%
  pull(longdescription) %>%
  paste(collapse = "") %>%
  str_replace_all(fixed(" "), "") %>%
  str_count()
print(paste0("Number of characters in titles to be translated: ", n_char_title))
print(paste0("Title costs with deepL: ", (n_char_title)*0.00002," EUR"))
print(paste0("Number of characters in long descriptions to be translated: ", n_char_long))
print(paste0("Total costs with deepL: ", (n_char_title + n_char_long)*0.00002," EUR"))

sample_trans_lang <- deeplr::toEnglish2(df_crs$longdescription[12], auth_key = deepl_auth_key)
sample_trans_lang  

# Save data frames
write.xlsx(languages_full_crs, file = "./Tmp/languages_full_crs.xlsx", row.names = F)
write.xlsx(languages_germany_crs, file = "./Tmp/languages_germany_crs.xlsx", row.names = F)
