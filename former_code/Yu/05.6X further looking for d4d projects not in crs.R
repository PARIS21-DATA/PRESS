rm(list = ls())
gc()
source("code/00. boot.R")

d4d_path_input <- paste0("Data/Intermediate/crs05.5 still missing", 
                                        job_specific_suffix, 
                                        year(Sys.Date()), 
                                        ".rds")
crs_raw_path_input <- paste0("Data/Intermediate/crs05.2", 
                             job_specific_suffix, 
                             year(Sys.Date()), 
                             ".rds")


df_d4d <- readRDS(d4d_path_input)
df_crs <- readRDS(crs_raw_path_input)


df_crs <- df_crs %>% 
  mutate(projecttitle = tolower(projecttitle))

df_d4d <- df_d4d %>%
  mutate(projecttitle = tolower(projecttitle)) 

df_matched <- df_d4d %>% 
  select(year, 
         # usd_disbursement,
         donorname
         ,projecttitle) %>% 
  unique %>% 
  inner_join(df_crs)

df_matched %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T), 
            count = n())

df_d4d %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T), 
            count = n())
  


df_d4d %>% 
  filter(!crsid %in% df_matched$crsid) %>% 
  select(#crsid, 
    #projecttitle, 
    donorname) %>% 
  unique



