rm(list =ls())
source("code/00. boot.R")
start_time <- Sys.time()
# 1. set up parameters and constant variables
path_input_sdg_markers <- paste0("data/intermediate/09.1 sdg markers UNIQUE goals ", 
                                 year(Sys.Date()), 
                                 ".feather")
path_output_just_climate_xlsx <- paste0("output/press/", 
                                        Sys.Date(), 
                                        " climate data projects 2018-2021",
                                        # year(Sys.Date()), 
                                        ".xlsx")

path_output_just_climate_all_xlsx <- paste0("output/press/", 
                                        Sys.Date(), 
                                        " climate data projects 2010-2021",
                                        # year(Sys.Date()), 
                                        ".xlsx")

path_output_press_xlsx <- paste0("output/press/", 
                                        Sys.Date(), 
                                        " press 2023 data full",
                                        # year(Sys.Date()), 
                                        ".xlsx")

path_output_feather <- paste0("output/press/", 
                                        Sys.Date(),
                                        " PRESS data with climate markers",
                                        year(Sys.Date()),
                                        ".feather")


path_output_2share_RDS <- paste0("output/press/", 
                                 Sys.Date(),
                                 " PRESS data sharing P21-DI ",
                                 year(Sys.Date()),
                                 ".RDS")

# 2. load data
df_crs <- read_feather("output/ch/current PRESS 2023 data.feather")

df_climate_filters_agency <- read.xlsx("data/auxiliary/Climate filters.xlsx", 
                                       sheet = 1) %>% 
  filter(!is.na(Climate)) %>% 
  filter(Climate ==1 |Climate ==2)

df_climate_filters_donor <- read.xlsx("data/auxiliary/Climate filters.xlsx", 
                                      sheet = 2) 

df_climate_filters_channel <- read.xlsx("data/auxiliary/Climate filters.xlsx", 
                                      sheet = 3) %>% 
  filter(!is.na(Climate)) %>% 
  filter(Climate ==1 |Climate ==2)

df_sdgs <- read_feather(path_input_sdg_markers)
rm(path_input_sdg_markers)

# 2.1 climate keywords
source("code/10.1b keywords list for climate.R")

# 3. adapt and implify data to be ready for analysis 
names(df_climate_filters_agency) <- tolower(names(df_climate_filters_agency))
names(df_climate_filters_channel) <- tolower(names(df_climate_filters_channel))
names(df_climate_filters_donor) <- tolower(names(df_climate_filters_donor))

df_climate_filters_agency <- df_climate_filters_agency %>% 
  select(dac_donorcode = donorcode, 
         # dac_donorname = donornamee, 
         agencycode, 
         # agencynamee, 
         climate_agency = climate) %>% 
  mutate(climate_agency_2nd = climate_agency == 2) %>% 
  mutate(climate_agency = (climate_agency == 1)) 

df_climate_filters_channel <- df_climate_filters_channel %>% 
  select(channelcode = channel.id, 
         climate_channel = climate) %>% 
  mutate(climate_channel_2nd = climate_channel == 2) %>% 
  mutate(climate_channel = (climate_channel == 1)) 

df_climate_filters_donor <- df_climate_filters_donor %>% 
  select(dac_donorcode = donor.code, 
         climate
         # ,dac_donorname = `donor.name.(en)`
         ) %>% 
  mutate(climate_donor = climate == 1)  %>% 
  mutate(climate_donor_2nd = climate == 2) %>% 
  select(-climate)


# 4 analysis 
# 4.1 merge filter data with the main dataset

df_crs <- df_crs %>% 
  left_join(df_climate_filters_agency) %>% 
  left_join(df_climate_filters_channel) %>% 
  left_join(df_climate_filters_donor)

rm(df_climate_filters_agency, 
   df_climate_filters_channel, 
   df_climate_filters_donor)

# 4.2 adding other filters

### 4.2.1 Rio markers

df_crs <- df_crs %>% 
  mutate(climate_marker = climateadaptation == 2|climatemitigation==2) 

### 4.2.2 SDGs

vec_only_sdg13 <- df_sdgs %>% 
  filter(sdg_goal == 13) %>% 
  .$db_ref 

rm(df_sdgs)


df_crs <- df_crs %>% 
  mutate(climate_sdg_marker = db_ref %in% vec_only_sdg13)
rm(vec_only_sdg13)

### 4.2.3 title 
#### 4.2.3.1 title positive
df_crs <- df_crs %>% 
  mutate(climate_title_whitelist = ifelse(is.na(projecttitle), 
                                          F, 
                                          grepl(x = projecttitle,
                                                pattern = vec_climate_all, 
                                                ignore.case = T)))

#### 4.2.3.2 blacklist 

df_crs <- df_crs %>%
  mutate(climate_title_blacklist = ifelse(is.na(projecttitle), 
                                          F, 
                                          str_detect(projecttitle, 
                                                     regex("population climate|business climate", ignore_case = TRUE))))


#### 4.2.3.3 merge postive and blacklist 

df_crs <- df_crs %>% 
  mutate(climate_title = climate_title_whitelist &(!climate_title_blacklist))

df_crs %>% select(climate_title_blacklist, 
                  climate_title_whitelist) %>% 
  table

# 4.3 fill the NAs in the filter columns
df_crs <- df_crs %>% 
  mutate(across(c("climate_donor", 
                  "climate_agency",
                  "climate_channel", 
                  "climate_marker", 
                  "climate_sdg_marker", 
                  "climate_title"), 
                ~ifelse(is.na(.x), F, .x)))

## 4.4 create the unified filter and add identified_by column

### 4.4.1 merge all filters
df_crs <- df_crs %>% 
  mutate(climate = climate_donor|climate_agency|climate_channel|climate_marker|
           climate_sdg_marker|climate_title)

### 4.4.2 add a variable on how the climate dimension was identified

#### 4.4.2.1 create the ordering
df_climate_filter_order <- tibble(markers = c("climate_donor", 
                                              "climate_marker",
                                              "climate_sdg_marker", 
                                              "climate_channel", 
                                              "climate_agency",
                                              "climate_title"), 
                                  order = c(1:6))

#### 4.4.2.2 get a long list of all climate filters
tmp_df_climate_key_filter <- df_crs %>% 
  filter(climate) %>% 
  select(db_ref, starts_with("climate_")) %>% 
  select(-climate_title_whitelist, -climate_title_blacklist) %>% 
  select(-climate_donor_2nd, -climate_agency_2nd, -climate_channel_2nd) %>% 
  gather(-db_ref, key  = "markers", value = "value") 

#### 4.4.2.3
tmp_df_climate_key_filter <- tmp_df_climate_key_filter %>% 
  # only keep the ones that has the filter positive
  filter(value) %>% 
  # join with the ordering list and order it
  left_join(df_climate_filter_order) %>% 
  arrange(db_ref, order) %>% 
  # keep the prioritised filter
  group_by(db_ref) %>% 
  filter(row_number() == 1) %>% 
  # remove the "climate_" prefix
  mutate(identified_by_climate = str_replace(markers, "climate_", "")) %>% 
  select(db_ref, identified_by_climate)

#### 4.4.2.4 merge with the full dataset
df_crs <- df_crs %>% 
  left_join(tmp_df_climate_key_filter) %>% 
  mutate(identified_by_climate = replace_na(identified_by_climate, "not climate"))
rm(df_climate_filter_order, tmp_df_climate_key_filter)

## 4.5 add questionable markers based on discussion

source("code/10.1c add climate questionable markers.R")

## 4.6 save the data
df_crs %>% 
  write_feather(path_output_feather)

df_crs %>% 
  filter(climate) %>% 
  select(identified_by_climate) %>% 
  table



# 5 share data with DI

df_crs <- read_feather(path_output_feather)

## 5.1 clean up the workspace and load data
rm(# path_output_RDS,
   path_output_just_climate_all_xlsx, 
   path_output_press_xlsx, 
   path_output_just_climate_xlsx)

# df_crs <- readRDS(path_output_RDS)
# df_crs_raw <- read_feather("data/intermediate/crs05.2_full_2023.feather")
# df_crs_raw %>% 
#   slice(1:100) %>% 
#   write_feather("data/intermediate/crs05.2a_onlyStructure_2023.feather")

df_crs_raw_simplified <- read_feather("data/intermediate/crs05.2a_onlyStructure_2023.feather")

## 5.2 compare columns
setdiff(names(df_crs_raw_simplified), names(df_crs))

setdiff(names(df_crs), names(df_crs_raw_simplified))

## 5.3 reduce and add columns
### 5.3.1 remove dac_ before some names
df_crs_2share <- df_crs %>% 
  select(-regionname) %>%
  rename_with(~ gsub("^dac_", "", .), starts_with("dac_"))

df_crs_2share <- df_crs_2share %>% 
  rename(regioncode_press = regioncode) %>% 
  rename(regioncode = regionncode) %>% 
  select(-regioncode_press)

setdiff(names(df_crs_2share), names(df_crs_raw_simplified))
setdiff(names(df_crs_raw_simplified), names(df_crs_2share))

### 5.3.2 revert name of crsid
df_crs_2share <- df_crs_2share %>%
  rename(crsid = db_original_id ) 
setdiff(names(df_crs_2share), names(df_crs_raw_simplified))
setdiff(names(df_crs_raw_simplified), names(df_crs_2share))

### 5.3.3 remove additional cols one by one
#### 5.3.3.1 step 1
vec_names2_remove <- setdiff(names(df_crs_2share), names(df_crs_raw_simplified))[1:4]
vec_names2_remove
# 1] "gen_title"           "gen_desc"            "d4d_addition_search"
# [4] "d4d_addition_match" 
df_crs_2share <- df_crs_2share %>%
  select(-all_of(vec_names2_remove))
setdiff(names(df_crs_2share), names(df_crs_raw_simplified))
setdiff(names(df_crs_raw_simplified), names(df_crs_2share))
#### 5.3.3.2 step 2
vec_names2_remove <- setdiff(names(df_crs_2share), names(df_crs_raw_simplified))[3:4]
vec_names2_remove 
# [1] "ch_name"    "ReporterId"
df_crs_2share <- df_crs_2share %>%
  select(-all_of(vec_names2_remove)) %>% 
  rename(infosys_title = infosys) 

setdiff(names(df_crs_2share), names(df_crs_raw_simplified))
setdiff(names(df_crs_raw_simplified), names(df_crs_2share))

#### 5.3.3.3 step 3
vec_names2_remove <- setdiff(names(df_crs_2share), names(df_crs_raw_simplified))[5:10]
vec_names2_remove 
# "infosys_title"        "finance_t_name"       "gen_rmnch2_uncertain"  "gen_agency"           "gen_channel"          "gen_sdg" 
df_crs_2share <- df_crs_2share %>%
  select(-all_of(vec_names2_remove)) 

setdiff(names(df_crs_2share), names(df_crs_raw_simplified))
setdiff(names(df_crs_raw_simplified), names(df_crs_2share))

#### 5.3.3.4 step 4
vec_names2_remove <- setdiff(names(df_crs_2share), names(df_crs_raw_simplified))[8:13]
vec_names2_remove
# "ict_title"      "digit_title"    "database_title" "innov_title"    "geo_title"     "ict_digit_info"
df_crs_2share <- df_crs_2share %>%
  select(-all_of(vec_names2_remove)) 

setdiff(names(df_crs_2share), names(df_crs_raw_simplified))
setdiff(names(df_crs_raw_simplified), names(df_crs_2share))

### 5.3.4 remove some cols that we created in earlier stage, so they are already in CRS raw 

#### 5.3.4.1 step 1
names(df_crs_2share)
vec_names2_remove <- names(df_crs_2share)[2:17]
vec_names2_remove 
# "hash_longdesc"     "hash_longdesc_num" "process_id"        "desc_2mine"       
# "text_id"           "scb"               "pop"               "gen_ppcode"       
# "gen_donor"         "gen_marker"        "gen_marker1"       "gen_marker2"      
# "gen_rmnch"         "gen_rmnch1"        "gen_rmnch2"        "language"   
df_crs_2share <- df_crs_2share %>% 
  select(-all_of(vec_names2_remove))


#### 5.3.4.2 step 2
names(df_crs_2share)

vec_names2_remove <- names(df_crs_2share)[3:11]
vec_names2_remove
# [1] "language_title"              "title_id"                   
# [3] "text_detection"              "mining"                     
# [5] "text_detection_wo_mining"    "stats_filter"               
# [7] "text_filter_gender"          "text_filter_gender_narrower"
# [9] "stats_filter_desc"   
df_crs_2share <- df_crs_2share %>% 
  select(-all_of(vec_names2_remove))


names(df_crs_2share)

setdiff(names(df_crs_2share), names(df_crs_raw_simplified))
setdiff(names(df_crs_raw_simplified), names(df_crs_2share))

## 5.4 save to RDS
df_crs_2share %>% 
  saveRDS(path_output_2share_RDS)

rm(vec_names2_remove)
rm(df_crs_raw_simplified)
rm(df_crs_2share)
gc()

print_time_diff(start_time)
