rm(list =ls())
source("code/00. boot.R")
start_time <- Sys.time()
# 1. set up parameters and constant variables
job_specific_suffix <- "_full_"
path_input_sdg_markers <- paste0("data/intermediate/09.1 sdg markers unique goals ", 
                                 year(Sys.Date()), 
                                 ".feather")


path_input_crs <- paste0("Data/Intermediate/crs05.2", 
                         job_specific_suffix, 
                         year(Sys.Date()), 
                         ".rds")
rm(job_specific_suffix)
# 2. load data
df_crs_data <- read_feather("output/ch/2023-09-15 PRESS 2023 data.feather")

df_crs <- readRDS(path_input_crs)
df_crs <- df_crs %>% 
  filter(year != '\032')
names(df_crs)
df_descriptions <- df_crs %>% 
  filter(!is.na(longdescription), 
         longdescription!="") %>% 
  select(db_ref, longdescription) 

df_descriptions %>% 
  filter(!duplicated(longdescription)) %>% 

df_crs_simplified <- df_crs %>% 
  # mutate(gender_marker = gender == 2)  %>% 
  # mutate(gender_marker = ifelse(is.na(gender_marker),
  #                               F,
  #                               gender_marker)) %>%
  mutate(year = as.numeric(year)) %>% 
  select(gender, 
         db_ref, 
         donorname, 
         projecttitle, 
         longdescription, 
         usd_disbursement_defl,
         bi_multi, 
         year) 

df_crs$year %>% 
  table

df_crs %>% 
  filter(is.na(text_id))


df_crs_simplified <- df_crs_simplified  %>% 
  filter(!is.na(longdescription), 
         longdescription!="") %>% 
  filter(nchar(longdescription) > 100) 
df_crs_simplified <- df_crs_simplified  %>% 
  mutate(mention_data = grepl("data|dato|indicator|indicador|survey|indicateur|statist|estadíst", 
               longdescription, 
               ignore.case = T)) 

df_crs_simplified <- df_crs_simplified %>% 
  filter(!is.na(year))

df_crs_simplified %>%
  filter(bi_multi != 4, bi_multi != 6) %>% 
  mutate(gender = ifelse(is.na(gender), 0, gender)) %>% 
  group_by(gender, mention_data, 
           group =  (year - 2010) %/% 3) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  mutate(group = group*3, 
         group = group+2012, 
         group = paste0(group-2, "-", group)) %>% 
  spread(key = mention_data, value = total) %>% 
  mutate(share = `TRUE`/(`TRUE`+`FALSE`)*100) %>% 
  select(group, gender, share) %>% 
  spread(key = gender, value = share) 




df_crs_simplified %>%
  mutate(gender = is.na(gender)) %>% 
  mutate(bi_multi = ifelse(bi_multi %in% c(1, 3, 7,8), 1, bi_multi)) %>% 
  group_by( year, gender, bi_multi) %>% 
  summarise(cnt = n()) %>% 
  spread(key = gender, value =  cnt) %>% 
  filter(year > 2018)
  


df_crs_simplified %>% 
  mutate(gender_reported = !is.na(gender)) %>% 
  group_by(year, gender_reported) %>% 
  summarise(#cnt = n()
            # , 
            total = sum(usd_disbursement_defl, na.rm = T)
            ) %>%
  spread(key = gender_reported, value = total)
  

print_time_diff(start_time)

