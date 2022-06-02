################################################################################
#
# DeepL package exploration + translation of keyword lists
# Author: Yu Tian, Johannes Abele
# Date: 05/10/2022
#
# Objective: 
#            
#            
# 
# input files: - /Data/intermediate/crs02_full.rds
#              - /Data/"keyword lists"_en.txt
#              - 
#              - 
#              
#
# output files: - /Data/"keyword lists"_lang.txt
#
#
################################################################################


# ------------------------------- Preparation ----------------------------------


################## !!! Added: DeepL attempt ####################################
# try DeepL language detection
# the authentication key comes from a free developer account created here: https://www.deepl.com/pro#developer 
# FYI: language detection benchmark - https://www.r-bloggers.com/2021/05/language-identification-using-the-fasttext-package-a-benchmark/

#install.packages("deeplr")
library(deeplr)
deepl_auth_key <- "fcec1af3-2663-3f39-7f16-dcd07b334f30:fx" # auth key corresponds to deepL account johannes.abele@oecd.org

# Fraction of projects in deepL-able languages 
lang_deepl <- deeplr::available_languages2(auth_key = deepl_auth_key) %>%
  mutate(language = tolower(language))
tab <- table(df_crs$language) %>% as.data.frame
tab_tmp <- tab %>%
  rename(language = Var1) %>%
  mutate(frac_deepl = ifelse(language %in% lang_deepl$language, Freq/sum(Freq), NA))

# Fraction of projects in de and nl (two most detected after en/fr/es)
sum(tab_tmp %>% filter(language == "de" | language == "nl") %>% pull(frac_deepl), na.rm = TRUE)

# Approximate costs of deepL full translation/detection (excluding en/fr/es) from 
# available languages - question: does quota apply to language detection?
nchar(paste(df_crs %>% filter(language %in% lang_deepl$language & !(language %in% c("en", "fr", "es"))) %>% pull(longdescription) , collapse = "")) * 0.00002 * 40

# Translate keyword lists
list_keywords_stat_en <- read_lines("data/statistics_reduced_en.txt")  %>% trimws()
list_keywords_stat_de <- deeplr::toGerman2(list_keywords_stat_en, auth_key = deepl_auth_key) # German
list_keywords_stat_es <- deeplr::toSpanish2(list_keywords_stat_en, auth_key = deepl_auth_key) # Spanish
list_keywords_stat_fr <- deeplr::toFrench2(list_keywords_stat_en, auth_key = deepl_auth_key) # French
write.table(list_keywords_stat_de, file = "./data/statistics_reduced_de.txt", row.names = F, col.names = F, quote = FALSE)
write.table(list_keywords_stat_es, file = "./data/statistics_reduced_es.txt", row.names = F, col.names = F, quote = FALSE)
write.table(list_keywords_stat_fr, file = "./data/statistics_reduced_fr.txt", row.names = F, col.names = F, quote = FALSE)

list_keywords_gender_en <- readLines("data/gender_en.txt")  %>% trimws()
list_keywords_gender_de <- deeplr::toGerman2(list_keywords_gender_en, auth_key = deepl_auth_key) # German
list_keywords_gender_es <- deeplr::toSpanish2(list_keywords_gender_en, auth_key = deepl_auth_key)
list_keywords_gender_fr <- deeplr::toFrench2(list_keywords_gender_en, auth_key = deepl_auth_key)
write.table(list_keywords_gender_de, file = "./data/gender_de.txt", row.names = F, col.names = F, quote = FALSE)
write.table(list_keywords_gender_es, file = "./data/gender_es.txt", row.names = F, col.names = F, quote = FALSE)
write.table(list_keywords_gender_fr, file = "./data/gender_fr.txt", row.names = F, col.names = F, quote = FALSE)
