source("code/06.2 add unified rec and region code to CRS.R")
source("code/06.3 add donor codes to crs and survey.R")
rm(list = ls())
source("Code/00. boot.R")
path_crs_4ref <- "data/Intermediate/06.3 crs with donor code_2023.feather"


df_crs <- read_feather(path_crs_4ref)

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
                                                  ifelse(d4d_addition_search|d4d_addition_match, 
                                                         "d4d", 
                                                         "something else")))))
df_crs$idenfied_by_stat %>% table
