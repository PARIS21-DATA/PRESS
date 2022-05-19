rm(list = ls())
source("code/00.1 functions.R")
crs_path_new <- "./Data/intermediate/crs03de.rds"
crs_path <- "./Data/intermediate/crs02de.rds"
df_crs <- readRDS(crs_path)

# we used to make the project with same description as 1 as long as one of the same description is marked as 1
# it is wrong because some projects with the same name will have different purpose codes



## 1.c. split projects by language delimiters "." and " / "

# df_crs$desc_2mine = iconv(df_crs$desc_2mine,"WINDOWS-1252","UTF-8")

# crs$desc_2mine = NULL
# names(crs)
# crs <- cSplit(crs, "toDetect", ".", "long")
# crs <- cSplit(crs, "toDetect", " / ", "long")
# save(crs, file = "crs_2020_clean1_splitToDetect.RData")


df_crs <- df_crs %>%
  filter(language == "en") %>% ##??? to solve later
  mutate(projecttitle_lower = tolower(projecttitle)) %>%
  mutate(title_id = as.numeric(as.factor(projecttitle_lower))) 

df_crs_backup <- df_crs


df_crs <- df_crs %>%
  select(title_id, projecttitle_lower) %>%
  filter(!duplicated(title_id)) 

# beep(4)
list_keywords <- readLines("data/statistics_reduced_en.txt")  %>%
  trimws()

# list_keywords_simon <- readLines("data/statistics_simon.txt") %>%
#   trimws()
# list_keywords <- c(list_keywords, list_keywords_simon) %>% unique
# 
# list_keywords <- list_keywords_simon

list_keywords_gender <- readLines("data/gender_en.txt")  %>%
  trimws()


list_keywords_stem <- stem_and_concatenate(list_keywords)
list_keywords_gender_stem <- stem_and_concatenate(list_keywords_gender)


df_crs <- df_crs %>%
  # select(db_ref, projecttitle, scb) %>%
  # mutate(projecttitle = tolower(projecttitle)) %>%
  mutate(projecttitle_stem = stem_and_concatenate(projecttitle_lower)) %>%
  mutate(text_detection = str_detect(projecttitle_stem, paste(list_keywords_stem, collapse = "|")))  %>%
  mutate(text_detection_gender =str_detect(projecttitle_stem, paste(list_keywords_gender_stem, collapse = "|")) )
# 
# tagged.results <- treetag(list_keywords, 
#                           treetagger="manual", format="obj",
#                           TT.tknz=FALSE , lang="en"
#                           # ,
#                           # TT.options=list(path="./TreeTagger", preset="en")
#                           )

list_blacklist <- readLines("data/blacklist_en.txt")  %>%
  trimws()

list_blacklist <- paste0(" ", list_blacklist, " ")


list_acronyms <- readLines("data/statistics_reduced_acronyms_en.txt")  %>%
  trimws()
list_acronyms <- paste0(" ", list_acronyms, " ")

df_crs <- df_crs %>%
  mutate(projecttitle_lower = paste0(" ", projecttitle_lower, " ")) %>%
  mutate(mining = str_detect(projecttitle_lower, paste(list_blacklist, collapse = "|")) ) %>%
  mutate(text_detection = str_detect(projecttitle_lower, paste(list_acronyms, collapse = "|"))  | text_detection) 



df_crs <- df_crs %>%
  mutate(text_detection_wo_mining = text_detection & !mining
         ) %>%
  select(-projecttitle_lower, -projecttitle_stem)

df_crs <- left_join(df_crs_backup, df_crs)

rm(df_crs_backup)

langues <- c("en","fr","es")
df_crs <- df_crs %>%
  select(-projecttitle_lower) %>%
  mutate(language = ifelse(language %in% langues, language, "other") )

df_crs <- df_crs %>%
  mutate( text_detection_wo_mining_w_scb = text_detection_wo_mining | scb)
table(df_crs$text_detection_wo_mining) %>% print
table(df_crs$text_detection_wo_mining_w_scb) %>% print
which(is.na(df_crs$text_detection_wo_mining_w_scb))

df_crs <- df_crs %>%
  mutate(text_filter_gender = gen_donor|gen_ppcode|gen_marker|text_detection_gender)

a = df_crs %>% select(text_id, text_detection_wo_mining_w_scb, text_detection_gender) %>% unique %>% nrow
b = df_crs %>% select(text_id) %>% unique %>% nrow

# list = df_crs %>% 
#   filter(text_detection_wo_mining_w_scb) %>%
#   select(text_id, projecttitle) %>%
#   unique
# saveRDS(list, file = "data/list_by_P21.rds")

print(paste0("There are ", a-b, " projects with same names but different purpose code"))

names(df_crs)

saveRDS(df_crs,file = crs_path_new)

write.csv(df_crs, file = "data/intermediate/crs_filter_results.csv", row.names = F)
