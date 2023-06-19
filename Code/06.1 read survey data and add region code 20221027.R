# rm(list = ls())
# source("code/00. boot.R")
# source("code/00.1 functions.R")
# 
# # reading in the data from the survey 
# ####################################
# ## --- PRESS Data Preparation --- ##
# ####################################
# 
# #Input::
# 
# df_survey <- read_rds("data/Raw/Survey/PRESS_survey_2021.rds")
# 
# df_survey$disbursement %>% head
# ## 1. clean up
# # df_survey$usd_commitment = df_survey$commitment
# # is.numeric(df_survey$usd_commitment)
# # df_survey$db_ref = paste0("survey_", df_survey$pressid)
# # press <- press[!press$ReporterId %in% c(0,67),] ## empty entries and PARIS21 test entries
# # df_survey$usd_commitment[is.na(df_survey$usd_commitment)] <- 0
# # df_survey$usd_costestimate <- df_survey$cost_estimate
# # df_survey$usd_costestimate[is.na(df_survey$usd_costestimate)] <- 0
# 
# # df_survey = df_survey %>% 
#   # mutate(usd_disbursement = ifelse(is.na(usd_commitment), usd_costestimate, usd_commitment))
# 
# # df_survey <- df_survey[df_survey$usd_commitment!=0 | df_survey$usd_costestimate!=0,]
# 
# # df_survey = df_survey %>%
#   # dplyr::rename(db_original_id = pressid) 
# 
# #juust for this one time
# df_survey <- df_survey %>%
#   select(-cost_estimate, -commitment, -financial_or_non, -financing_mechanism, -commitment_type, -currency, -genmul_topic) %>% 
#   rename(recipientname = recipient)
# # df_survey <- df_survey %>% 
# #   mutate(recipientname = str_replace(recipientname, pattern = ", ", replacement = ": "))
# 
# 
# # create a df with db_ref and every single recipient in the list
# ls_survey_recipients <- lapply(df_survey$recipientname ,FUN = function(x) str_split(x, pattern = ", "))
# ls_survey_n.of.recipients <- lapply(ls_survey_recipients, FUN = function(x)  length(x[[1]])  ) %>% unlist
# ls_survey_db_ref_unlist <- rep(df_survey$db_ref , ls_survey_n.of.recipients)
# ls_survey_recipients_unlist <- unlist(ls_survey_recipients)
# 
# 
# df_survey_recipients <- data.frame(db_ref = ls_survey_db_ref_unlist, 
#                                    recipientname = ls_survey_recipients_unlist)
# 
# rm(ls_survey_recipients, ls_survey_recipients_unlist, 
#    ls_survey_db_ref_unlist, ls_survey_n.of.recipients)
# 
# 
# # fixing the compatibility between survey data and region data
# df_survey_recipients_name_correction <- read_rds("Data/auxiliary/survey_recipient_names_correction.rds")
# df_survey_recipients <- df_survey_recipients %>% 
#   rename(recipientname_old = recipientname) %>% 
#   left_join(df_survey_recipients_name_correction) %>% 
#   mutate(recipientname = ifelse(is.na(recipientname_new), 
#                                 recipientname_old, 
#                                 recipientname_new))  %>% 
#   select(-recipientname_new, 
#          -recipientname_old)
# 
# rm(df_survey_recipients_name_correction)
# 
# 
# 
# # df_survey_recipients <- df_survey_recipients %>% 
# #   select(recipientname) %>% 
# #   unique() %>% 
# #   mutate(isocode =  countrycode(recipientname, "country.name", "iso3c")) %>% 
# #   right_join(df_survey_recipients)
# 
# df_regions <- read_rds("./data/auxiliary/regions.rds")
# names(df_regions)
# 
# df_survey_recipients_regions <- df_survey_recipients %>% 
#   # select(-isocode) %>% 
#   left_join(df_regions)
# 
# 
# df_regions %>% 
#   select(regioncode, regionname) %>% 
#   unique
# 
# # df_survey_recipients_regions[1069,]
# df_survey_recipients_regions %>% 
#   filter(is.na(regioncode)) %>% 
#   select(recipientname) %>% 
#   unique
# 
# df_survey_recipients_regions %>% names()
# 
# df_survey_tab_by_reg <- df_survey_recipients_regions %>% 
#   group_by(db_ref, regionname, regioncode) %>% 
#   summarise(cnt = n()) %>% 
#   group_by(db_ref) %>% 
#   mutate(cnt_regions = n())
# 
# df_survey_tab_single_reg <- df_survey_tab_by_reg %>% 
#   filter(cnt_regions <= 1) %>% 
#   select(-cnt, -cnt_regions)
# 
# df_survey_tab_multiple_reg <- df_survey_tab_by_reg %>% 
#   filter(cnt_regions > 1) %>% 
#   select(db_ref) %>% 
#   left_join(df_survey_recipients_regions) %>% 
#   select(db_ref, 
#          regionname = regionname_larger, 
#          regioncode = regioncode_larger) %>% 
#   unique %>% 
#   group_by(db_ref) %>% 
#   mutate(cnt = n()) %>% 
#   mutate(regionname = ifelse(cnt >1, "Multilateral unallocated", regionname), 
#          regioncode = ifelse(cnt>1, 1500, regioncode)) %>% 
#   select(db_ref, regionname, regioncode) %>% 
#   unique
# 
# df_survey_reg <- rbind(df_survey_tab_single_reg, df_survey_tab_multiple_reg) 
# df_survey_regions <- left_join(df_survey, df_survey_reg)
# rm(df_survey_reg, 
#    df_survey_tab_by_reg, 
#    df_survey_tab_multiple_reg, 
#    df_survey_tab_single_reg)
# 
# df_survey_recipients_regions_unique.country <- df_survey_recipients_regions %>% 
#   select(db_ref, isocode, iso3n) %>%
#   unique %>% 
#   group_by(db_ref) %>% 
#   mutate(cnt_isos = n()) %>% 
#   filter(cnt_isos <= 1) %>% 
#   select(-cnt_isos)
# 
# df_survey_recipients_regions_unique.dac.code <- df_survey_recipients_regions %>% 
#   select(db_ref, dac_recipientcode) %>%
#   unique %>% 
#   group_by(db_ref) %>% 
#   mutate(cnt_isos = n()) %>% 
#   filter(cnt_isos <= 1) %>% 
#   select(-cnt_isos)
# 
# df_survey_reg_rec_code <- df_survey_regions %>% 
#   left_join(df_survey_recipients_regions_unique.country) %>% 
#   left_join(df_survey_recipients_regions_unique.dac.code)
# 
# rm(df_survey_recipients_regions_unique.country, 
#    df_survey_recipients_regions_unique.dac.code, 
#    df_survey_recipients_regions, 
#    df_survey_regions)
# 
# 
# 
# df_survey_reg_rec_code <-  df_survey_reg_rec_code  %>%
#   rename(donorname = donor) %>% 
#   filter(donorname!="",!is.na(donorname))
# 
# df_survey <- df_survey_reg_rec_code

names(df_survey)

df_crs <- read_rds("Data/Intermediate/crs05.3_onlystats_utf8_full.rds") 

df_crs %>% 
  select(regionname, regioncode) %>% 
  unique

df_regions %>% 
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


# save(press, df_crs, regions, reporters, regions_notes, file = "./analysis/CRS_PRESS_before_merging_2021.RData")


save(df_survey, df_crs, file = "data/Intermediate/06.1 crs and press with region and country code.rdata")

