source("code/00. boot.R")
start <- Sys.time()
rm(list = ls())
# pkgload:::unload("tidytext") # the stemmer in tidy text might be problematic for our steps here. 

# source("code/00.2 functions_thilo.R")
# source("code/00.3 functions_yu.R")

job_specific_suffix <- "_full_"
# load("data/intermediate/crs04_lang_utf8_full.rdata")

crs_path <- paste0("./Data/intermediate/crs03",
                   job_specific_suffix, year(Sys.Date()), ".rds")
crs_path_ids <- paste0("./Data/intermediate/crs04_positive_id",
                       job_specific_suffix, year(Sys.Date()), ".rds")
crs_path_new <- paste0("Data/Intermediate/crs05", 
                       job_specific_suffix, 
                       year(Sys.Date()), 
                       ".rds")
# crs_path_new_1 <- paste0("./Data/intermediate/crs04.0_crs1_", lang,job_specific_suffix, ".rds")
# crs_path_new_0 <- paste0("./Data/intermediate/crs04.0_crs0_", lang,job_specific_suffix, ".rds")
# start <- Sys.time()
df_crs <- read_rds(crs_path)
ls_ids <- read_rds(crs_path_ids)
# print_time_diff(start)


df_crs <- df_crs %>% 
  mutate(stats_filter_desc = ifelse(text_id %in% ls_ids, T, F)) %>% 
  mutate(stats_filter  = replace_na(stats_filter, F),
         stats_filter_desc = replace_na(stats_filter_desc, F)) %>% 
  mutate(stats  = stats_filter|stats_filter_desc)

# write_rds(df_crs, file = "Data/Intermediate/crs05_utf8_full.rds")

# df_crs <- read_rds("Data/Intermediate/crs05_utf8_full.rds")

job_specific_suffix <- "_gen_full_"
# load("data/intermediate/crs04_lang_utf8_full.rdata")

crs_path_ids <- paste0("./Data/intermediate/crs04_positive_id",
                       job_specific_suffix, year(Sys.Date()), ".rds")

ls_ids <- read_rds(crs_path_ids)

df_crs <- df_crs %>% 
  mutate(gender_filter_desc = ifelse(text_id %in% ls_ids, T, F)) %>% 
  mutate(gender_filter_desc = replace_na(gender_filter_desc, F), 
         text_filter_gender = replace_na(text_filter_gender, F)) %>% 
  mutate(gender_filter_both  = gender_filter_desc|text_filter_gender) 


write_rds(df_crs, file = crs_path_new)

df_crs$gender_filter_both %>% table 
beep()
# print_time_diff(start)