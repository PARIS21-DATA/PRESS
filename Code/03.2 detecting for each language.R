df_crs <- df_crs_full %>%
  filter(language_title == lang2analyse) ##??? to solve later

# df_crs_backup <- df_crs

# df_crs <- df_crs %>%
#   select(title_id, projecttitle_lower) %>%
#   filter(!duplicated(title_id)) 

# beep(4)
list_keywords <- readLines(paste0("data/statistics_reduced_", lang2analyse, ".txt")
                           ,encoding = "UTF-8"
                           )  %>%
  trimws()

# list_keywords_simon <- readLines("data/statistics_simon.txt") %>%
#   trimws()
# list_keywords <- c(list_keywords, list_keywords_simon) %>% unique
# 
# list_keywords <- list_keywords_simon

list_keywords_gender <- readLines(paste0("data/gender_", lang2analyse, ".txt"), encoding = "UTF-8")  %>%
  trimws()


list_keywords_stem <- stem_and_concatenate(list_keywords)
list_keywords_gender_stem <- stem_and_concatenate(list_keywords_gender)
print_time_diff(start)

df_crs <- df_crs %>%
  # select(db_ref, projecttitle, scb) %>%
  # mutate(projecttitle = tolower(projecttitle)) %>%
  mutate(projecttitle_stem = stem_and_concatenate(projecttitle_lower)) %>%
  mutate(text_detection = str_detect(projecttitle_stem, paste(list_keywords_stem, collapse = "|")))  %>%
  mutate(text_detection_gender =str_detect(projecttitle_stem, paste(list_keywords_gender_stem, collapse = "|")) )
print_time_diff(start)


# 
# tagged.results <- treetag(list_keywords, 
#                           treetagger="manual", format="obj",
#                           TT.tknz=FALSE , lang="en"
#                           # ,
#                           # TT.options=list(path="./TreeTagger", preset="en")
#                           )

list_blacklist <- readLines(paste0("data/blacklist_", lang2analyse, ".txt"), encoding = "UTF-8")  %>%
  trimws()

list_blacklist <- paste0(" ", list_blacklist, " ")


list_acronyms <- readLines(paste0("data/statistics_reduced_acronyms_", lang2analyse, ".txt"), encoding = "UTF-8")  %>%
  trimws()
list_acronyms <- paste0(" ", list_acronyms, " ")

df_crs <- df_crs %>%
  mutate(projecttitle_lower = paste0(" ", projecttitle_lower, " ")) %>%
  mutate(mining = str_detect(projecttitle_lower, paste(list_blacklist, collapse = "|")) ) %>%
  mutate(text_detection = str_detect(projecttitle_lower, paste(list_acronyms, collapse = "|"))  | text_detection) 

df_crs <- df_crs %>%
  # mutate(text_detection_wo_mining = text_detection & !mining) %>%
  select(-projecttitle_lower, -projecttitle_stem)

df_crs <- left_join(df_crs_backup, df_crs)

rm(df_crs_backup)

assign(paste0("df_crs_", lang2analyse), df_crs)
