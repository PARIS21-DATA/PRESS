rm(list = ls())
source("code/00. boot.R")
start <- Sys.time()
### start merging filtered results to data
### load filtered results
job_specific_suffix <- "_full_"
crs_path <- paste0("Data/Intermediate/crs05.2", 
                   job_specific_suffix, 
                   year(Sys.Date()), 
                   ".rds")
d4d_whitelist_path <- "data/Intermediate/07.3a d4d manual additions.feather"
d4d_blacklist_path <- "data/Intermediate/07.3b d4d manual blacklist.feather"

crs_path_new <- paste0("data/intermediate/crs05.3_onlystats", 
                       job_specific_suffix, 
                       year(Sys.Date()), 
                       "_temp_",
                       ".rds")


df_crs <- read_rds(crs_path)
print_time_diff(start)
gc()

df_white_list <- read_feather(d4d_whitelist_path)
df_black_list <- read_feather(d4d_blacklist_path)

df_crs_whitelist <- df_white_list %>% 
  select(db_ref) %>%
  distinct %>% 
  # distinct %>% 
  inner_join(df_crs) 

df_crs_stats <- df_crs %>% 
  filter(stats) %>%
  mutate(d4d_addition_search = F)

rm(df_crs)

df_crs_whitelist <- df_crs_whitelist %>% 
  anti_join(select(df_crs_stats, db_ref)) %>% 
  mutate(d4d_addition_search = T) 

df_crs_stats <- df_crs_whitelist %>%
  rbind(df_crs_stats) %>% 
  anti_join(select(df_black_list, db_ref))

df_crs_stats$d4d_addition_search %>% table

saveRDS(df_crs_stats, crs_path_new)

gc()
print_time_diff(start)
