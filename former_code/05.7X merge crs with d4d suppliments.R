rm(list = ls())
gc()
source("code/00. boot.R")
start_time <- Sys.time()
job_specific_suffix <- "_full_"
crs_path_stats_input <- paste0("data/intermediate/crs05.3_onlystats", 
                         job_specific_suffix, 
                         year(Sys.Date()),  
                         "_temp_",
                         ".rds")

crs_path_addition_input <- paste0("Data/Intermediate/crs05.5 crs mathced", 
                                   job_specific_suffix, 
                                   year(Sys.Date()), 
                                   ".rds")

crs_path_output <- paste0("data/intermediate/crs05.3_onlystats", 
                          job_specific_suffix, 
                          year(Sys.Date()),  
                          ".rds")

df_crs_filtered <- readRDS(crs_path_stats_input)
df_crs_filtered$d4d_addition_search %>% table
df_crs_filtered <- df_crs_filtered %>% 
  mutate(d4d_addition_match = F)
df_crs_addition_d4d <- readRDS(crs_path_addition_input)
df_crs_addition_d4d <- df_crs_addition_d4d %>% 
  mutate(d4d_addition_match = T, d4d_addition_search = F)
df_crs <- rbind(df_crs_filtered, df_crs_addition_d4d) 
df_crs$d4d_addition_search %>% table
df_crs <- df_crs %>% 
  filter(!duplicated(db_ref))

saveRDS(df_crs, crs_path_output)

print_time_diff(start_time)

