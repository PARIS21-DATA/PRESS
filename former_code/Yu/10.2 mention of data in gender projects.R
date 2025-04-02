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
path_output_summary <- paste0("Output/press/charts PRESS 2023/10.2 Frequency of data mentioned in gender projects ", 
                              year(Sys.Date()), 
                              ".xlsx")

rm(job_specific_suffix)
# 2. load data
df_crs_data <- read_feather("output/ch/2023-09-15 PRESS 2023 data.feather")
df_crs <- df_crs_data

df_crs_raw <- read_feather(path_input_crs)
print_time_diff(start_time)

## Rows below only ran once. To create hash values for the project descriptions. 

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

df_descriptions <- df_crs_raw %>% 
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
  

print_time_diff(start_time)

vec_gender <- c("gender|sex|women|woman|femm|girl|femal|reproduc|FGM|time use|time-use|unpaid|marriag|gbv|femini|vaw|matern|reproduct|srhr|mother|disagg")

source("code/10.2b data projects mentioning gender.R")


## by share would even work better
df_crs %>% 
  filter(!is.na(longdescription)) %>% 
  filter(nchar(longdescription) > 100) %>%
  filter(!gender_filter_both_desc, 
         !gen_rmnch2) %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl,na.rm = T))

## by total amount
df_crs %>% 
  filter(!gender_filter_both_desc, 
         !gen_rmnch2) %>% 
  mutate(mention_gender = F) %>% 
  filter(!is.na(longdescription)) %>%
  filter(nchar(longdescription) > 100) %>%
  mutate(mention_gender = grepl(vec_gender,
                                longdescription,
                                ignore.case = T)) %>%
  # mutate(mention_gender = ifelse(gen_marker1, 
  #                                T, 
  #                                mention_gender)) %>% 
  fun_summarise_ts("mention_gender") %>% 
  # mutate(share = `TRUE`/(`TRUE`+`FALSE`)) %>%
  # mutate(usd_disbursement_defl = share) %>%
  mutate(usd_disbursement_defl = `TRUE`) %>%
  mutate(by_var = "total") %>%
  fun_summarise_ts("by_var",rolling = T, 3)  

