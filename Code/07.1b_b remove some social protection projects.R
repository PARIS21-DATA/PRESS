rm(list =ls())
source("code/00. boot.R")
start_time <- Sys.time()
# 1. set up parameters and constant variables
path_crs_in <- paste0("Output/CH/", 
                          "2023-09-15", 
                          " PRESS 2023 data.feather")
path_output <- paste0("data/intermediate/07.1b_b social protection projects 2 remove ",
                      year(Sys.Date()),
                      ".rds")

df_crs <- read_feather(path_crs_in)


df_crs %>% 
  filter(ch_name == "The World Bank") %>% 
  filter(year == 2021) %>% 
  filter(grepl("protection", projecttitle, ignore.case = T)) %>% 
  select(projecttitle, db_ref, usd_disbursement_defl, recipientname, idenfied_by_stat) 

df_crs %>% 
  filter(ch_name == "The World Bank") %>% 
  filter(year == 2021) %>% 
  filter(grepl("protection", projecttitle, ignore.case = T)) %>% 
  select(projecttitle, db_ref, usd_disbursement_defl) %>% 
  select(usd_disbursement_defl) %>% 
  sum
56.98926 * 0.31

df_crs %>% 
  filter(ch_name == "The World Bank") %>% 
  filter(year == 2021) %>% 
  filter(grepl("protection", projecttitle, ignore.case = T)) %>% 
  select(db_ref, usd_disbursement_defl)


vec_social_protection_2remove <- c("df_2987696", 
                                   "df_2987968", 
                                   "df_2987972"
                                   )

vec_social_protection_2remove %>% 
  saveRDS(path_output)
