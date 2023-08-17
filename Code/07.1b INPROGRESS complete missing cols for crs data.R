


df_crs <- df_crs %>% 
  mutate(year = as.numeric(year))


df_crs_commitment_year <- df_crs %>% 
  ungroup %>% 
  select(commitmentdate, year) %>%
  unique %>% 
  mutate(commitment_year =  substr(commitmentdate, 1, 4))  %>% 
  mutate(commitment_year = as.numeric(commitment_year)) %>% 
  mutate(commitment_year = ifelse(is.na(commitment_year), year, commitment_year)) %>% 
  unique

df_crs <- df_crs %>% 
  inner_join(df_crs_commitment_year)

rm(df_crs_commitment_year)



df_crs <- df_crs  %>% 
  mutate(idenfied_by_stat = ifelse(scb, 
                                    "PP", 
                                    ifelse(text_detection, 
                                           "title", 
                                           ifelse(stats_filter_desc, 
                                                  "desc", 
                                                  "somethingelse"))))
df_crs$idenfied_by_stat %>% table
