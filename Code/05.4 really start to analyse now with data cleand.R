rm(list = ls())
source("code/X3. search filter.R")
rm(list = ls())
source("Code/00. boot.R")


start <- Sys.time()
df_crs <- read_rds("data/intermediate/crs05.2.1_utf8_full.rds")
print_time_diff(start)
gc()


df_crs <- df_crs %>% 
  filter(stats)

# df_crs_o <- df_crs

# write_rds(df_crs, "data/intermediate/crs05.3_onlystats_utf8_full.rds")
# gc()

# df_crs <- read_rds("data/intermediate/crs05.3_onlystats_utf8_full.rds")

df_crs %>% filter(year > 2009)%>% .$commitmentdate %>% substr(1, 4) %>% unique

df_crs <- df_crs %>% 
  mutate(usd_disbursement_defl = replace_na(usd_disbursement_defl, 0),  
         usd_commitment_defl = replace_na(usd_commitment_defl, 0)) %>% 
  mutate(gen_rmnch = replace_na(rmnch, 0), 
         gen_rmnch_narrow = gen_rmnch %in% c(3,4), 
         gen_rmnch_broader = gen_rmnch %in% c(2:4)) %>% 
  mutate(gender_filter_both_rmnch = gender_filter_both|gen_rmnch_narrow) %>% 
  mutate(commitmentdate = as.Date(commitmentdate, "%Y-%m-%d")) %>% 
  mutate(commitmentyear = year(commitmentdate))


df_crs <- df_crs %>% 
  mutate(identified_by = ifelse(scb, "Purpose code", 
                                ifelse(text_detection, "Project title", 
                                       ifelse(stats_filter_desc, "Description","unknown"))))

df_crs <- df_crs %>% 
  mutate(identified_by_gender = ifelse(gen_ppcode, "Purpose code", 
                                       ifelse(gen_donor, "UN Women as channel", 
                                              ifelse(text_detection_gender, "Project title", 
                                                     ifelse(gen_marker2, "Gender equality marker", 
                                                            ifelse(gender_filter_desc, "Project description", 
                                                                   ifelse(gen_rmnch_narrow, "RMNCH marker", 
                                                                          ifelse(!gender_filter_both_rmnch, "Not a gender project", "Unknown"))))))))


table(df_crs$identified_by)
table(df_crs$identified_by_gender)


df_crs_wo_infoSys <- read_rds("data/intermediate/crsX3_utf8_full.rds") 

df_crs_wo_infoSys <- df_crs_wo_infoSys %>% 
  filter(title_id %in% df_crs$title_id) %>%
  select(-stats_filter_wo_infoSys) %>% 
  unique

df_crs_merged <- inner_join(df_crs, df_crs_wo_infoSys) 

df_crs_merged %>% 
  filter(text_detection, !text_detection_wo_infoSys) %>% 
  nrow 


df_crs_merged <- df_crs_merged %>% 
  mutate(stats_wo_infoSys = (text_detection_wo_infoSys|scb|stats_filter_desc)&(!mining))

df_crs_merged %>% 
  filter(stats, !stats_wo_infoSys) %>% 
  nrow 


df_crs <- df_crs_merged

write_rds(df_crs, "data/intermediate/crs05.3_onlystats_utf8_full.rds")

