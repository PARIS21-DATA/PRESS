rm(list =ls())
source("code/00. boot.R")
gc()
# 1. set up parameters and constant variables
path_togo_filters <- "Documents/Togo filters.xlsx"
path_output_xlsx <- paste0("output/press/", 
                           Sys.Date(), 
                           " togo projects for CRESS.xlsx")
# 2. read data
df_crs <- read_feather("output/ch/2023-09-15 PRESS 2023 data.feather")

df_togo_filter_channels <- read.xlsx(path_togo_filters, sheet = 1)
df_togo_filter_donors <- read.xlsx(path_togo_filters, sheet = 2)

# 3. simplify data
df_togo_filter_channels <- df_togo_filter_channels %>% 
  select(channelcode = Channel.ID, 
         channelname_fr = `Full.Name.(French)`, 
         channel_abbreviation_fr = `Acronym.(FR)`) %>% 
  mutate(togo_channel = T)

df_togo_filter_donors <- df_togo_filter_donors %>% 
  select(dac_donorcode = Donor.code, 
         donorname_fr = `Donor.name.(FR)`) %>% 
  mutate(togo_donor = T)


df_crs %>% 
  select(dac_regionname, dac_regionncode) %>% 
  distinct()

df_crs %>%
  select(dac_recipientcode,recipientname) %>% 
  distinct %>% 
  filter(grepl("africa", recipientname, ignore.case= T) )

# 4. filter data 
df_crs <- df_crs %>% 
  mutate(togo_recipient = recipientname == "Togo") %>% 
  mutate(africa_recipient = dac_regionncode %in% c(10003, #sub-saharan
                                                   10001 # africa
                                                   )) %>% 
  mutate(africa_recipient = dac_recipientcode %in% c(298, # africa regional
                                                     1030 # western africa regional
                                                     ))  %>% 
  mutate(across(c("togo_recipient", "africa_recipient"), ~ifelse(is.na(.x), 
                                                                 F, 
                                                                 .x)))

df_crs <- df_crs %>% 
  filter(togo_recipient|africa_recipient)

df_crs %>% 
  left_join(df_togo_filter_channels) %>% 
  left_join(df_togo_filter_donors) %>% 
  mutate(across(c("togo_donor","togo_channel"), ~ifelse(is.na(.x), 
                                                        F, 
                                                        .x))) %>% 
  filter(togo_donor|togo_channel) %>% 
  select(dac_donorname, 
         agencyname, 
         donorname_fr, 
         channelname,
         channelname_fr, 
         db_ref, 
         db_original_id, 
         projectnumber, 
         projecttitle, 
         longdescription, 
         usd_disbursement_defl, 
         recipientname, 
         commitment_year, 
         expectedstartdate, 
         completiondate, 
         commitmentdate, 
         year, 
         usd_commitment_defl, 
         togo_donor,
         finance_t, 
         gender, 
         gender_filter_both,
         aid_t,
         disbursement_national, 
         currencycode, 
         usd_commitment, 
         usd_disbursement) %>% 
  filter(year > 2016)  %>% 
  write.xlsx(path_output_xlsx) %>% 
  system.time

df_crs$aid_t %>% unique


system.time(gc())
all.equal()