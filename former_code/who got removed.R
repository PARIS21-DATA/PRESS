df_db_ref <- read_rds("Data/Analysis/db_db_ref.rds")
vec_db_ref_crs <- df_db_ref %>% 
  filter(source == "crs") %>% 
  .$db_ref
# df_press %>% 
#   filter(!db_ref %in% vec_db_ref_crs)
which(!vec_db_ref_crs %in% x$db_ref)

vec_db_ref_survey <- df_db_ref %>% 
  filter(source == "survey") %>% 
  .$db_ref
# df_press %>% 
#   filter(!db_ref %in% vec_db_ref_crs)
which(!vec_db_ref_survey %in% df_press$db_ref)
vec_db_ref_survey[which(!vec_db_ref_survey %in% x$db_ref)] %>% print
