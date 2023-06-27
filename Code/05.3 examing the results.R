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
crs_path_new <- paste0("data/intermediate/crs05.3_onlystats", 
                       job_specific_suffix, 
                       year(Sys.Date()), 
                       ".rds")


df_crs <- read_rds(crs_path)
print_time_diff(start)
gc()


df_crs_stats <- df_crs %>% 
  filter(stats)

# df_crs_o <- df_crs

saveRDS(df_crs_stats, crs_path_new)
rm(df_crs)
gc()
print_time_diff(start)
