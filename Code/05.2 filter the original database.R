rm(list = ls())
source("code/00. boot.R")
start <- Sys.time()
### start merging filtered results to data
### load filtered results
job_specific_suffix <- "_full_"
path_input_crs_filters <- paste0("Data/Intermediate/crs05", 
                                   job_specific_suffix, 
                                   year(Sys.Date()), 
                                   ".feather")
path_output <- paste0("data/intermediate/crs05.2", 
                                   job_specific_suffix, 
                                   year(Sys.Date()), 
                                   ".rds")

path_output_feather <- paste0("data/intermediate/crs05.2", 
                       job_specific_suffix, 
                       year(Sys.Date()), 
                       ".feather")

path_input_crs_full_data <- paste0("data/intermediate/crs01_1", 
                      job_specific_suffix, 
                      year(Sys.Date()), 
                      ".feather")

path_output_stat_ids <- paste0("data/intermediate/05.2 List of ids before d4d examine ",
                               year(Sys.Date()),
                               ".rds")

# path_finance_type <- "data/auxiliary/finance types 2023.xlsx"

# df_crs_wFilters <- df_crs
# rm(df_crs)
df_crs_wFilters <- read_feather(path_input_crs_filters)
### load data with process ids 
# df_crs02 <- read_rds("data/Intermediate/crs02_utf8_full.rds")

names(df_crs_wFilters)
# names(df_crs02)


# all of df_crs columsn are actually within df_crs_wFilters
# but are all the rows there too?
# previously because a mistake in merging data in step 3. Our output data was shorter, leading to crs_wFilters shorter. 
# df_crs02 contains text_id and process_id, which can be vital to latere stages, but it does not contain title ids. 
# we should assume that crs01 and crs02 should have consistent process id. 
# with the txt data added in late may. we should also assume that it has the most recent data. 
# 
# names(df_crs_wFilters)[!names(df_crs_wFilters) %in% names(df_crs02)]
# names(df_crs02)[!names(df_crs02) %in% names(df_crs_wFilters)]
# 
# unique(df_crs02$process_id) %>% length
# unique(df_crs_wFilters$process_id) %>% length
# 
# unique(df_crs02$text_id) %>% length
# unique(df_crs_wFilters$text_id) %>% length
# beep()

# df_crs02_reduced <- df_crs02 %>% 
#   select(text_id, process_id) 

# df_crs02_filtered <- inner_join(df_crs02_reduced, df_crs_wFilters)
# use innerjoin to see if it was successful
# results: df_crs02 is a subset of filtered


# rm(df_crs02)
# rm(df_crs02_reduced, df_crs_wFilters)
gc()



# now merging with crs01. The very raw data

df_crs01 <- read_feather(path_input_crs_full_data)

names(df_crs_wFilters)
names(df_crs01)


names(df_crs_wFilters)[!names(df_crs_wFilters) %in% names(df_crs01)]
names(df_crs01)[!names(df_crs01) %in% names(df_crs_wFilters)]

names(df_crs_wFilters)[names(df_crs_wFilters) %in% names(df_crs01)]
names(df_crs01)[names(df_crs01) %in% names(df_crs_wFilters)]

unique(df_crs_wFilters$process_id) %>% length
unique(df_crs01$process_id) %>% length

df_crs_01_filtered <- df_crs_wFilters %>% 
  # only keep the filters you got
  select(-projecttitle, 
         -year, 
         -bi_multi, 
         -finance_t, 
         -rmnch,
         -commitmentdate,
         -shortdescription, 
         -longdescription, 
         -purposecode, 
         -donorname, 
         -gender, 
         -channelcode) %>% 
  # use innerjoin to check the consistency
  inner_join(df_crs01)

unique(df_crs_wFilters$process_id) %>% length
unique(df_crs01$process_id) %>% length

rm(df_crs_wFilters, df_crs01)
gc()

# df_finance_t <- read_xlsx(path_finance_type)
# 
# df_finance_t <- df_finance_t %>% 
#   rename(finance_t = sub_category_code, 
#          finance_t_father_code = category_code, 
#          finance_t_name_original = sub_category, 
#          finance_t_name_father = category)
# 
# df_crs_01_filtered <- df_crs_01_filtered %>% 
#   left_join(df_finance_t)

# 
# df_crs_01_filtered <- df_crs_01_filtered %>% 
#   mutate(donor_type = ifelse(bi_multi == 4, 
#                              "Multilateral", 
#                              ifelse(bi_multi == 6, 
#                                     "Private", 
#                                     "Bilateral")))

# rm(df_finance_t, path_finance_type)





## !!! this section above will be moved to a much earlier stage

# write_rds(df_crs_01_filtered, file = path_output)
write_feather(df_crs_01_filtered, path_output_feather)
# vec_stat_ids <- df_crs_01_filtered %>% 
#   filter(stat) %>% 
#   .$process_id
# vec_stat_ids %>% 
#   saveRDS(path_output_stat_ids)
beep()
print_time_diff(start)


