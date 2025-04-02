rm(list =ls())

path_output <- paste0("data/intermediate/09.1 sdg markers ", 
                      year(Sys.Date()), 
                      ".feather")

path_output_unique_goals <- paste0("data/intermediate/09.1 sdg markers UNIQUE goals ", 
                      year(Sys.Date()), 
                      ".feather")

df_crs <- read_feather("output/ch/2023-09-15 PRESS 2023 data.feather")

df_sdgs <- df_crs %>% 
  select(db_ref, sdgfocus) %>% 
  filter(!is.na(sdgfocus)) %>% 
  filter(sdgfocus != "") %>% 
  filter(sdgfocus != "0") %>% 
  distinct

rm(df_crs)

ls_sdgs <- df_sdgs$sdgfocus %>% 
  sapply(FUN = function(x) str_split(x, ";")) 
vec_sdgs <- unlist(ls_sdgs)
head(ls_sdgs)
vec_db_ref <- rep(df_sdgs$db_ref, sapply(ls_sdgs, length))
df_sdgs_extended <- tibble(db_ref = vec_db_ref, 
                           sdgfocus = vec_sdgs)

rm(ls_sdgs, vec_sdgs, vec_db_ref)

df_sdgs_extended <- df_sdgs_extended %>% 
  mutate(sdg_goal = str_extract(sdgfocus, "^\\d+")) %>% 
  mutate(sdg_goal = as.numeric(sdg_goal))

df_sdgs_extended$sdg_goal %>% table

df_sdgs_extended %>% 
  write_feather(path_output)


tmp_df_goal_per_proj <- df_sdgs_extended %>% 
  select(db_ref, sdg_goal) %>% 
  distinct 

df_unique_goal <- tmp_df_goal_per_proj %>% 
  group_by(db_ref) %>% 
  summarise(cnt = n()) %>% 
  filter(cnt == 1) %>% 
  select(db_ref) %>% 
  inner_join(tmp_df_goal_per_proj)


rm(tmp_df_goal_per_proj)
df_unique_goal %>% 
  write_feather(path_output_unique_goals)

  