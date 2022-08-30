rm(list = ls())
source("code/00. boot.R")


### start merging filtered results to data
### load filtered results
df_crs_wFilters <- read_rds("data/Intermediate/crs05_utf8_full.rds")
### load data with process ids 
df_crs02 <- read_rds("data/Intermediate/crs02_utf8_full.rds")

names(df_crs_wFilters)
names(df_crs02)


# all of df_crs columsn are actually within df_crs_wFilters
# but are all the rows there too?
# previously because a mistake in merging data in step 3. Our output data was shorter, leading to crs_wFilters shorter. 
# df_crs02 contains text_id and process_id, which can be vital to latere stages, but it does not contain title ids. 
# we should assume that crs01 and crs02 should have consistent process id. 
# with the txt data added in late may. we should also assume that it has the most recent data. 

names(df_crs_wFilters)[!names(df_crs_wFilters) %in% names(df_crs02)]
names(df_crs02)[!names(df_crs02) %in% names(df_crs_wFilters)]

unique(df_crs02$process_id) %>% length
unique(df_crs_wFilters$process_id) %>% length

unique(df_crs02$text_id) %>% length
unique(df_crs_wFilters$text_id) %>% length


df_crs02_reduced <- df_crs02 %>% 
  select(text_id, process_id) 

df_crs02_filtered <- innerjoin(df_crs02, df_crs_wFilters)
# use innerjoin to see if it was successful


rm(df_crs02)
rm(df_crs02_reduced, df_crs_wFilters)
gc()



# now merging with crs01. The very raw data

df_crs01 <- read_rds("data/Intermediate/crs01_1_utf8_full.rds")

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
  select(-projecttitle, -shortdescription, -longdescription, -purposecode, -donorname, -gender, -channelcode) %>% 
  # use innerjoin to check the consistency
  inner_join(df_crs01)

unique(df_crs_wFilters$process_id) %>% length
unique(df_crs01$process_id) %>% length

