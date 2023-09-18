rm(list =ls())
gc()
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
                         ".feather")
path_output_summary <- paste0("Data/Output/10.2 Frequency of data mentioned in gender projects ", 
                              year(Sys.Date()), 
                              ".xlsx")

rm(job_specific_suffix)
# 2. load data
df_crs_data <- read_feather("output/ch/2023-09-15 PRESS 2023 data.feather")

df_crs <- read_feather(path_input_crs)
print_time_diff(start_time)

# df_crs <- df_crs %>% 
#   filter(year != '\032')
# df_crs <- df_crs %>% 
#   mutate(year = as.numeric(year))
# df_descriptions <- df_crs %>% 
  # filter(!is.na(longdescription),
  #        longdescription!="") %>%
#   select(db_ref, longdescription) %>% 
#   mutate(hash_longdesc = digest(longdescription, algo = "sha256"))
# print_time_diff(start_time)
# 
# df_crs <- df_descriptions %>% 
#   select(db_ref, hash_longdesc) %>% 
#   inner_join(df_crs)
# write_rds(df_crs, file = path_input_crs)

df_descriptions <- df_crs %>% 
  select(hash_longdesc, longdescription) %>% 
  # filter(!is.na(longdescription),
  #        longdescription!="") %>%
  distinct 

df_descriptions$longdescription %>% unique %>% length

# "data|dato|indicator|indicador|indicateur|statist|estadíst|estadist|survey"
vec_keywords <- "data|datos|statist|estadíst|estadist|données|donnees|indicator|indicador|indicateur"

source("code/10.2a produce summary table.R")

df_summary_by_3year %>% 
  write.xlsx(path_output_summary)


df_crs_simplified %>%
  mutate(gender_reported = !is.na(gender)) %>% 
  mutate(bi_multi = ifelse(bi_multi %in% c(1, 3, 7,8), 1, bi_multi)) %>% 
  group_by( year, gender_reported, bi_multi) %>% 
  summarise(cnt = n()) %>% 
  spread(key = gender_reported, value =  cnt) %>% 
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

