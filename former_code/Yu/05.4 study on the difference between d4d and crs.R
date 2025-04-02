df_crs_old <- read_feather("data/intermediate/06.3 crs with donor code_2023_withoutD4D.feather")


df_crs_old <- df_crs_old %>% 
  filter(!duplicated(db_ref))


vec_db_ref_original <- df_crs_old$db_ref 
head(vec_db_ref_original)
saveRDS(vec_db_ref_original, file = "data/auxiliary/List of projects before d4d merge_2023.rds")


