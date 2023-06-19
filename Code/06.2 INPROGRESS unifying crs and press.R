rm(list = ls())
source("code/00. boot.R")
source("code/00.1 functions.R")

# reading in the data from the survey 
####################################
## --- PRESS Data Preparation --- ##
####################################

# 0 Input::

path_input_survey <- paste0("Data/Intermediate/06.1b survey merged with corrected recipient and regions_", 
                     year(Sys.Date())
                     ,".rds")

path_input_crs <- paste0("Data/Intermediate/crs05.3_onlystats_full_", 
                         year(Sys.Date())
                         ,".rds")

path_output <- paste0("Data/Intermediate/06.2 crs and press with region and country code_", 
                                  year(Sys.Date())
                                  ,".data")


# 5 merge with CRS data
df_crs <- read_rds(path_crs_filtered) 

df_crs %>% 
  select(regionname, regioncode) %>% 
  unique

<- df_regions %>% 
  select(regionname, regioncode) %>% 
  unique

df_crs = df_crs %>%
  dplyr::rename(db_original_id = crsid) %>% 
  rename(dac_regionname = regionname, 
         dac_regionncode = regioncode, 
         dac_recipientcode = recipientcode
  )

df_crs <- df_regions %>% 
  select(-regioncode_larger, -regionname_larger, -recipientname) %>% 
  right_join(df_crs) 


df_crs %>% filter(is.na(regionname)) %>% nrow()

df_survey <- df_survey[df_survey$usd_commitment!=0 | df_survey$usd_costestimate!=0,]

#just for this one time
df_survey <- df_survey %>%
  select(-cost_estimate, -commitment, -financial_or_non, -financing_mechanism, -commitment_type, -currency, -genmul_topic)


save(df_survey, df_crs, file = path_output)

