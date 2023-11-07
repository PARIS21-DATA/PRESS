source("code/00. boot.R")
start <- Sys.time()
rm(list = ls())
# pkgload:::unload("tidytext") # the stemmer in tidy text might be problematic for our steps here. 

# source("code/00.2 functions_thilo.R")
# source("code/00.3 functions_yu.R")

job_specific_suffix <- "_full_"
# load("data/intermediate/crs04_lang_utf8_full.rdata")

path_input_crs <- paste0("./Data/intermediate/crs03",
                   job_specific_suffix, year(Sys.Date()), ".feather")
path_input_ids_stat <- paste0("./Data/intermediate/crs04_positive_id",
                       job_specific_suffix, year(Sys.Date()), ".rds")

path_intermediate <- paste0("Data/Intermediate/crs05", 
                            "_intermediate_just_stat_", 
                            job_specific_suffix, 
                            year(Sys.Date()), 
                            ".feather")
path_output <- paste0("Data/Intermediate/crs05", 
                       job_specific_suffix, 
                       year(Sys.Date()), 
                       ".feather")

path_input_black_list <- paste0("data/intermediate/04.5b d4d manual blacklist ",
                          year(Sys.Date()), 
                          ".feather")
path_input_white_list <- paste0("data/intermediate/04.5a d4d manual additions ",
                          year(Sys.Date()), 
                          ".feather")

path_input_db_ref <- paste0("./Data/intermediate/crs02.4 db_ref and process id list", job_specific_suffix, year(Sys.Date()), ".feather")

# crs_path_new_1 <- paste0("./Data/intermediate/crs04.0_crs1_", lang,job_specific_suffix, ".rds")
# crs_path_new_0 <- paste0("./Data/intermediate/crs04.0_crs0_", lang,job_specific_suffix, ".rds")
# start <- Sys.time()
df_crs <- read_feather(path_input_crs)
ls_ids <- read_rds(path_input_ids_stat)
df_db_ref <- read_feather(path_input_db_ref)
df_white_list <- read_feather(path_input_white_list) %>% 
  select(db_ref) %>% 
  mutate(stat_d4d = T) 
df_black_list <- read_feather(path_input_black_list) %>% 
  select(db_ref) %>% 
  mutate(stat_blacklist_d4d = T)

df_d4d <- df_db_ref %>% 
  left_join(df_white_list) %>% 
  left_join(df_black_list) %>% 
  mutate(across(stat_d4d:stat_blacklist_d4d , ~ replace_na(.x, F))) 


# print_time_diff(start)

df_crs <- df_crs %>% 
  inner_join(df_d4d)
rm(df_d4d, 
   df_black_list, 
   df_white_list)
df_crs <- df_crs %>% 
  mutate(stat_desc = ifelse(desc_2mine_id %in% ls_ids, T, F)) 


df_crs <- df_crs %>% 
  mutate(stat_title_ppcode = replace_na(stat_title_ppcode, F),
         stat_desc = replace_na(stat_desc, F)) %>% 
  mutate(stat  = stat_title_ppcode|stat_desc|stat_d4d) %>% 
  mutate(stat = stat &(!stat_blacklist_d4d))

# legacy columns

df_crs <- df_crs %>% 
  mutate(stats_filter_desc = stat_desc, 
         stats = stat, 
         d4d_addition_search = stat_d4d)

# write_feather(df_crs, path_intermediate)


rm(list = ls())

job_specific_suffix = "_full_"
path_intermediate <- paste0("Data/Intermediate/crs05", 
                            "_intermediate_just_stat_", 
                            job_specific_suffix, 
                            year(Sys.Date()), 
                            ".feather")
df_crs <- read_feather(path_intermediate)



job_specific_suffix <- "_gen_full_"

path_input_ids_gender <- paste0("./Data/intermediate/crs04_positive_id",
                       job_specific_suffix, year(Sys.Date()), ".rds")

ls_ids <- read_rds(path_input_ids_gender)

df_crs <- df_crs %>% 
  mutate(gen_desc = ifelse(desc_2mine_id %in% ls_ids, T, F)) %>% 
  mutate(gen_desc = replace_na(gen_desc, F), 
         gen_markers_title = replace_na(gen_markers_title, F)) %>% 
  mutate(gen = gen_desc|gen_markers_title) 



# legacy column
df_crs <- df_crs %>% 
  mutate(gender_filter_desc = gen_desc, 
         gender_filter_both  = gen)


write_feather(df_crs, path_output)

# df_crs$gender_filter_both %>% table 
beep()
# print_time_diff(start)