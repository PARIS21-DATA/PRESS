rm(list = ls())
gc()
source("code/00. boot.R")
start_time <- Sys.time()
job_specific_suffix <- "_full_"
crs_path_input <- paste0("data/intermediate/crs05.3_onlystats", 
                         job_specific_suffix, 
                         year(Sys.Date()),  
                         "_temp_",
                         ".rds")


crs_raw_path_input <- paste0("Data/Intermediate/crs05.2", 
                             job_specific_suffix, 
                             year(Sys.Date()), 
                             ".rds")

d4d_path_still_missing_output <- paste0("Data/Intermediate/crs05.5 still missing", 
                                   job_specific_suffix, 
                                   year(Sys.Date()), 
                                   ".rds")

# crs_path_matching_output <- paste0("Data/Intermediate/crs05.5 crs matched", 
#                                    job_specific_suffix, 
#                                    year(Sys.Date()), 
#                                    ".rds")

crs_path_output <- paste0("data/intermediate/crs05.3_onlystats", 
                          job_specific_suffix, 
                          year(Sys.Date()),  
                          ".rds")



df_d4d <- read_xlsx("data/D4D/D4D Profiles data_2022_c.xlsx")
names(df_d4d) <- tolower(names(df_d4d))

df_crs_stats <- readRDS(crs_path_input)

df_d4d_not_in_crs <- df_d4d %>% 
  filter(!(crsid %in% df_crs_stats$crsid)) 

df_d4d_not_in_crs %>%
  group_by(year) %>%
  summarise(total = sum(usd_disbursement_defl, na.rm=T))

rm(df_d4d)

df_crs <- readRDS(crs_raw_path_input)

table(df_crs$year)
df_crs <- df_crs %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(year))

df_crs$projecttitle %>% head
df_d4d_not_in_crs_4matching <- df_d4d_not_in_crs %>% 
  select(crsid, year, projectnumber) %>% 
  unique 

df_crs_missingD4D_match <- df_crs %>% 
  inner_join(df_d4d_not_in_crs_4matching )

rm(df_crs)

df_crs_missingD4D_match %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T), 
            count = n())

df_d4d_not_in_crs %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T), 
            count = n())


df_d4d_not_in_crs %>% 
  filter(!crsid %in% df_crs_missingD4D_match$crsid) %>% 
  select(crsid, 
         projecttitle, 
         donorname, 
         usd_disbursement_defl, 
         year) %>% 
  slice(1:20) %>% 
  arrange(desc(usd_disbursement_defl))
  
# only about 20 projects not found, most of them don't have an crs id

df_d4d_2search <- df_d4d_not_in_crs %>% 
  filter(!crsid %in% df_crs_missingD4D_match$crsid)

df_crs_stats$d4d_addition_search %>% table
df_crs_stats <- df_crs_stats %>% 
  mutate(d4d_addition_match = F)

df_crs_missingD4D_match <- df_crs_missingD4D_match %>% 
  mutate(d4d_addition_match = T, d4d_addition_search = F)

df_crs_stats <- df_crs_stats %>% 
  mutate(year = as.numeric(year))

df_crs <- rbind(df_crs_stats, df_crs_missingD4D_match) 
df_crs$d4d_addition_search %>% table

df_crs <- df_crs %>% 
  filter(!duplicated(db_ref))
# is.numeric(df_crs_missingD4D_match$year)

df_crs$year %>% table

saveRDS(df_crs, crs_path_output)
saveRDS(df_d4d_2search, file =  d4d_path_still_missing_output)

print_time_diff(start_time)

# saveRDS(df_crs_missingD4D_match, file = crs_path_matching_output)

beepr::beep()

