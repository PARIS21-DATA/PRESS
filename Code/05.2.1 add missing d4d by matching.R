rm(list = ls())
gc()
source("code/00. boot.R")
start_time <- Sys.time()
job_specific_suffix <- "_full_"
# crs_path_input <- paste0("data/intermediate/crs05.3_onlystats", 
#                          job_specific_suffix, 
#                          year(Sys.Date()),  
#                          "_temp_",
#                          ".rds")


path_input <- paste0("Data/Intermediate/crs05.2", 
                             job_specific_suffix, 
                             year(Sys.Date()), 
                             ".feather")

path_output_still_missing <- paste0("Data/Intermediate/crs05.5 still missing", 
                                   job_specific_suffix, 
                                   year(Sys.Date()), 
                                   ".feather")

# crs_path_matching_output <- paste0("Data/Intermediate/crs05.5 crs matched", 
#                                    job_specific_suffix, 
#                                    year(Sys.Date()), 
#                                    ".rds")

path_output <- paste0("data/intermediate/crs05.3_onlystats", 
                          job_specific_suffix, 
                          year(Sys.Date()),  
                          ".feather")



df_d4d <- read_xlsx("data/D4D/D4D Profiles data_2022_c.xlsx")
names(df_d4d) <- tolower(names(df_d4d))

# df_crs <- df_crs_01_filtred
# rm(df_crs_01_filtered)

df_crs <- read_feather(path_input)

df_crs_stats <- df_crs %>% 
  filter(stat)

df_d4d_not_in_crs <- df_d4d %>% 
  filter(!(crsid %in% df_crs_stats$crsid)) 

df_d4d_not_in_crs %>%
  group_by(year) %>%
  summarise(total = sum(usd_disbursement_defl, na.rm=T))

rm(df_d4d)

# df_crs <- read_feather(crs_raw_path_input)

table(df_crs$year)
# df_crs1 <- df_crs %>% 
  # mutate(year = as.numeric(year)) %>% 
  # filter(!is.na(year))

# df_crs$projecttitle %>% head
df_d4d_not_in_crs_4matching <- df_d4d_not_in_crs %>% 
  select(crsid, year, projectnumber) %>% 
  distinct

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

# which one haven't we found?
df_d4d_not_in_crs %>% 
  filter(!crsid %in% df_crs_missingD4D_match$crsid) %>% 
  select(crsid, 
         projecttitle, 
         donorname, 
         usd_disbursement_defl, 
         year) %>% 
  # slice(1:20) %>% 
  arrange(desc(usd_disbursement_defl))
  
# only about 20 projects not found, most of them don't have an crs id

df_d4d_2search <- df_d4d_not_in_crs %>% 
  filter(!crsid %in% df_crs_missingD4D_match$crsid)

df_crs_stats$stat_d4d %>% table
df_crs_stats <- df_crs_stats %>% 
  mutate(stat_d4d_matching = F)

df_crs_missingD4D_match <- df_crs_missingD4D_match %>% 
  mutate(stat_d4d_matching = T, stat_d4d = F, stat = T)

# df_crs_stats <- df_crs_stats %>% 
#   mutate(year = as.numeric(year))

df_crs <- rbind(df_crs_stats, df_crs_missingD4D_match) 

df_crs <- df_crs %>% 
  mutate(stat_d4d_searching = stat_d4d) %>% 
  mutate(stat_d4d = stat_d4d_matching|stat_d4d_searching)

df_crs$stat_d4d %>% table

df_crs <- df_crs %>% 
  filter(!duplicated(db_ref))
# is.numeric(df_crs_missingD4D_match$year)


# legacy columns
df_crs <- df_crs %>% 
  mutate(d4d_additional_matching = stat_d4d_matching)

df_crs$year %>% table

write_feather(df_crs, path_output)
write_feather(df_d4d_2search, path_output_still_missing)

print_time_diff(start_time)

# saveRDS(df_crs_missingD4D_match, file = crs_path_matching_output)

beepr::beep()

