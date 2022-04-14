### ---------------
# start data cleaning 
### 
rm(list = ls())
source <- "crs"
crs_path <- "./Data/intermediate/crs01_1.rds"
crs_path_new <- "./Data/intermediate/crs02.rds"
df_crs_raw <- readRDS(crs_path)
rm(crs_path)

cols_needed <- c("process_id", 
                 "projecttitle", 
                 "shortdescription", 
                 "longdescription", 
                 "purposecode",
                 "donorname")

# every step, we try to use a subset of the data to make the process quicker

# Dop french part of the canadian project titles/descriptions since they are in 
# the format "Englisch / French", double filtering since format "English/French" is also present
df_crs_CAN <- df_crs_raw %>%
  select(cols_needed) %>%
  filter(donorname == "Canada") %>%
  separate(projecttitle, into = c("projecttitle", "projecttitle_fr"), sep = " / ", fill = "right", extra = "drop") %>%
  separate(shortdescription, into = c("shortdescription", "shortdescription_fr"), sep = " / ", fill = "right", extra = "drop") %>%
  separate(longdescription, into = c("longdescription", "longdescription_fr"), sep = " / ", fill = "right", extra = "drop") %>%
  mutate(longdescription_fr = NULL, shortdescription_fr = NULL, projecttitle_fr = NULL) %>%
  separate(projecttitle, into = c("projecttitle", "projecttitle_fr"), sep = "/", fill = "right", extra = "drop") %>%
  separate(shortdescription, into = c("shortdescription", "shortdescription_fr"), sep = "/", fill = "right", extra = "drop") %>%
  separate(longdescription, into = c("longdescription", "longdescription_fr"), sep = "/", fill = "right", extra = "drop") %>%
  mutate(longdescription_fr = NULL, shortdescription_fr = NULL, projecttitle_fr = NULL)

# Check same number of Canadian projects
nrow(df_crs_CAN)
nrow(df_crs_raw %>% filter(donorname == "Canada"))

# Replace all canadian projects with the ones with English project titles/descriptions
df_crs_raw <- df_crs_raw %>%
  select(cols_needed) %>%
  filter(!(donorname == "Canada")) %>%
  rbind(df_crs_CAN)
rm(df_crs_CAN)

# Create a unique text_id that is made from the combination of the project title, 
# short description and the long description
df_crs <- df_crs_raw %>%
  mutate(description_comb = paste(projecttitle, 
                                  shortdescription, 
                                  longdescription, 
                                  sep = ". "), 
         description_comb = tolower(description_comb)
         ) %>%
  mutate(text_id = as.numeric(as.factor(description_comb))) %>%
  ## add SCB identifier
  mutate(scb = ifelse(purposecode==16062,1,0), 
         pop = ifelse(purposecode==13010,1,0),
         gen_ppcode = ifelse(purposecode %in% c(15170:15180), 1, 0)
         ) %>%
  select(-cols_needed[which(!cols_needed %in% c("process_id", "longdescription"))])

# Detect language of the projects
df_crs_lang <- df_crs %>% 
  select(text_id, longdescription) %>%
  ## 1.b. drop duplicate project descriptions
  filter(!duplicated(text_id)) %>%
  mutate(language = cld2::detect_language(longdescription)) %>%
  select(-longdescription) 

# Add detected languages to the crs data
df_crs <- df_crs %>% 
  select(-longdescription) %>%
  left_join(df_crs_lang) %>%
  right_join(df_crs_raw)

rm(df_crs_lang)
rm(df_crs_raw)

which(is.na(df_crs$description_comb)) %>% print 
which(df_crs$description_comb == "") %>% print
table(df_crs$language) %>% print 

#Output::
saveRDS(df_crs, file=crs_path_new)


################## !!! Added: DeepL attempt ####################################
# try DeepL language detection
# the authentication key comes from a free developer account created here: https://www.deepl.com/pro#developer 
# FYI: language detection benchmark - https://www.r-bloggers.com/2021/05/language-identification-using-the-fasttext-package-a-benchmark/

#install.packages("deeplr")
library(deeplr)
deepl_auth_key <- "fcec1af3-2663-3f39-7f16-dcd07b334f30:fx"

#df_crs_tmp <- df_crs %>% 
#  filter(language == "fr") %>%
#  head(50) %>%
#  mutate(lang_2 = deeplr::detect2(longdescription, auth_key = deepl_auth_key))


# Fraction of projects in deepL-able languages 
lang_deepl <- available_languages2(auth_key = deepl_auth_key) %>%
  mutate(language = tolower(language))
tab <- table(df_crs$language) %>% as.data.frame
tab_tmp <- tab %>%
  rename(language = Var1) %>%
  mutate(frac_deepl = ifelse(language %in% lang_deepl$language, Freq/sum(Freq), NA))

# Fraction of projects in de and nl (two most detected after en/fr/es)
sum(tab_tmp %>% filter(language == "de" | language == "nl") %>% pull(frac_deepl), na.rm = TRUE)

# Approximate costs of deepL full translation/detection (including english and french) - question: does quota apply to language detection?
nchar(paste(df_crs$longdescription, collapse = "")) * 0.00002 * 40


