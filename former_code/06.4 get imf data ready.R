rm(list = ls())
source("code/00. boot.R")
source("code/00.1 functions.R")

path_input_imf <- "Data/Raw/Survey/imf_2021_2023correction.feather"
path_output_imf <- paste0("Data/Intermediate/06.4 imf with regions and rec code_",
                                           year(Sys.Date())
                                           ,".feather")
path_survey_4ref <- "data/Intermediate/06.3 survey with donor code_2023.feather"
path_crs_4ref <- "data/Intermediate/06.3 crs with donor code_2023.feather"

path_aux_rec_codes <- "data/auxiliary/new_regions_2023.feather"

# 1. loading data
df_imf <- read_feather(path_input_imf)
df_crs <- read_feather(path_crs_4ref)
df_survey <- read_feather(path_survey_4ref)
df_recipient_code <- read_feather(path_aux_rec_codes)

setdiff(names(df_imf), names(df_survey))

# 2. merge with region codes and recipient codes
df_imf <- df_recipient_code %>% 
  select(recipientname, dac_recipientcode, regioncode, regionname) %>% 
  right_join(df_imf) 
rm(df_recipient_code)


## 2.1 check issues with the old recipient codes in the dataframe and remove the old column
df_imf %>%
  filter(dac_recipientcode != recipientcode) %>%
  select(recipientname, dac_recipientcode,
         recipientcode) %>%
  unique %>%
  as.data.frame()

df_imf %>%
  filter(regionid != regioncode) %>%
  select(recipientname, regionid,
         regioncode) %>%
  unique %>%
  as.data.frame()

df_imf <- df_imf %>% 
  select(-regionid, -recipientcode)


# 3. complete df_imf with more columns so it will be compatible with df_survey
setdiff(names(df_survey), names(df_imf))
names(df_imf)
## 3.1 db_original_id
df_imf <- df_imf %>% 
  mutate(db_original_id = str_replace(db_ref, 
                                      "imf_", 
                                      ""))
df_imf$db_original_id %>% head

## 3.2 reported year
df_imf <- df_imf %>% 
  mutate(reported_year = commitmentdate)

## 3.3 gender filter, not existing for imf
df_imf <- df_imf %>% 
  mutate(gender_filter_both_rmnch = F)  %>% 
  mutate(identified_by = "survey") %>% 
  mutate(source = "imf")


## 3.4 donor codes
df_donors <- readRDS("data/auxiliary/reporters_final_2022.rds")

df_imf <- df_imf %>% 
  select(-donorname_survey) %>% 
  mutate(ReporterName = "IMF - International Monetary Fund") %>% 
  left_join(df_donors )

setdiff(names(df_survey), names(df_imf))

## 3.5 add columns to be more similar to survey data
## !! there are some assumptions made here
df_imf <- df_imf %>% 
  mutate(uniqueIdentifier = db_original_id) %>% 
  mutate(startyear = reported_year,
         endyear = reported_year) %>% 
  mutate(grant_loan = "Grant") # %>% 
  # mutate(genmul = NA, genmul_per = NA, genmul_topic = NA)

# 4. save imf data

df_imf %>% 
  write_feather(path = path_output_imf)

