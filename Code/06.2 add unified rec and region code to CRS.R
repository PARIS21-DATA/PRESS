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
                                  ,".Rdata")

path_aux_region <- "data/auxiliary/regions.rds"

# 5 merge with CRS data
df_region <- readRDS(path_aux_region)
df_crs <- read_rds(path_input_crs) 
df_survey <- readRDS(path_input_survey)

df_crs %>% 
  select(regionname, regioncode) %>% 
  unique

df_crs <- df_crs %>%
  dplyr::rename(db_original_id = crsid) %>% 
  rename(dac_regionname = regionname, 
         dac_regionncode = regioncode, 
         dac_recipientcode = recipientcode
  )


names(df_survey)

df_region %>% 
  select(dac_recipientcode, regioncode, regionname, 
         regionname_larger, regionname_larger, recipientname) %>% 
  filter(!is.na(dac_recipientcode)) %>% 
  distinct %>% 
  filter(duplicated(recipientname))

df_region_simplified <- df_region %>% 
  select(-regioncode_larger, -regionname_larger, -recipientname) %>% 
  distinct

df_crs <- df_crs %>% 
  left_join(df_region_simplified)

df_crs %>% filter(is.na(regionname)) %>% nrow()
# fix an isssue of no matching.
# 20230627 adding another row on Bilateral, unspecified
# df_crs %>% filter(is.na(regionname)) %>% select(recipientname, dac_recipientcode) %>% unique
# df_region <- df_region %>% 
#   filter(recipientname == "Bilateral, unspecified") %>%
#   mutate(dac_recipientcode = 9998) %>% 
#   bind_rows(df_region) 
# # 20230627 also decided to remove the one with old code to mainly uniqueness
# df_region <- df_region %>% 
#   filter(dac_recipientcode != 998)
# saveRDS(df_region, path_aux_region)

# df_survey <- df_survey[df_survey$usd_commitment!=0 | df_survey$usd_costestimate!=0,]

# df_survey <- df_survey %>%
#   select(-cost_estimate, -commitment, -financial_or_non, -financing_mechanism, -commitment_type, -currency, -genmul_topic)

df_survey <- df_survey %>% 
  filter(draft_or_final == "Final")

save(df_survey, df_crs, file = path_output)
setdiff(names(df_survey), names(df_crs))


df_survey %>% select(ignore) %>% table
df_survey %>% select(draft_or_final) %>% table
