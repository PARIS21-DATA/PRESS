rm(list = ls())
gc()
# Sys.sleep(10)
source("code/00. boot.R")
# source("code/00.1a functions_stem_and_concat.R")

job_specific_suffix <- "_full_"
path_input_crs_full <- paste0("./Data/intermediate/crs01_1", job_specific_suffix, year(Sys.Date()), ".feather")
# path_input_crs <- paste0("./Data/intermediate/crs03", job_specific_suffix, year(Sys.Date()), ".feather")

path_output <- paste0("./Data/intermediate/crs02.4 db_ref and process id list", job_specific_suffix, year(Sys.Date()), ".feather")

df_crs_raw <- read_feather(path_input_crs_full)
df_crs_raw <- df_crs_raw %>% 
  select(process_id, db_ref) 

write_feather(df_crs_raw, path_output)
