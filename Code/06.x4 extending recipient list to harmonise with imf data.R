rm(list = ls())
source("code/00. boot.R")
source("code/00.1 functions.R")

path_output_survey_w_db_and_recs <- paste0("Data/Intermediate/06.1b survey merged with corrected recipient and regions_", 
                                           year(Sys.Date())
                                           ,".rds")

df_imf <- read_xlsx("data/Intermediate/06.4 intermediate imf_fixed.xlsx")
# df_recipient_code <- read_rds("data/auxiliary/regions.rds")
# df_recipient_code %>% 
#   write.xlsx(file = "data/auxiliary/regions_2fix.xlsx")
df_recipient_code <- read_xlsx("data/auxiliary/regions_fixed.xlsx")

setdiff(names(df_recipient_code), names(df_imf))
names(df_recipient_code)
df_recipient_not_in_recnames <- df_imf %>% 
  select(recipientname) %>% 
  unique %>% 
  left_join(df_recipient_code) %>% 
  filter(is.na(dac_recipientcode)) %>% 
  select(regionname = recipientname) 

df_recipient_code %>% 
  select(regionname, regioncode) %>% 
  unique %>% 
  right_join(df_recipient_not_in_recnames) %>% 
  filter(is.na(regioncode))

df_recipient_code %>% 
  filter(duplicated(dac_recipientcode))

df_recipient_code <- read_xlsx("data/auxiliary/regions_fixed.xlsx")

df_recipient_code <- df_recipient_code %>%
  mutate(Need_iso = ifelse(is.na(Need_iso), F, T)) %>%
  mutate(isocode = ifelse(Need_iso,
                          countrycode(recipientname,
                                      "country.name",
                                      "iso3c"),
                          isocode))  %>%
  mutate(iso3n = ifelse(Need_iso,
                                  countrycode(recipientname,
                                              "country.name",
                                              "iso3n"),
                                  iso3n))

df_recipient_code %>%
  filter(is.na(isocode)|isocode == "") %>%
  as.data.frame()
df_recipient_code <- df_recipient_code %>% 
  select(-Need_iso)
write_feather(df_recipient_code, path = "data/auxiliary/new_regions_2023.feather")
