
# setwd("~/Dropbox/PARIS21/PRESS/2021/")
rm(list = ls())
load("./data/Intermediate/06.1 crs and press with region and country code.rdata")
df_reporters2 <- read_rds("data/auxiliary/reporters.rds")
names(df_reporters2)
df_reporters <- read_csv("data/auxiliary/donor_codes_and_classifications.csv")

df_reporters1 <- df_reporters %>% 
  select(ReporterId, ReporterName, 
         crs_name_en, crs_code, 
         name_modification, ch_name,
         dac_member, dac_bi_member, mult_donor, mult_donor_and_eu, private_donor, non_dac_donor)  %>% 
  unique

df_reporters1 <- df_reporters1 %>% 
  mutate(total = rowSums( df_reporters1[,7:12], na.rm = T)) %>% 
  filter(total  != 0)

df_crs <- df_crs %>% 
  filter(stats_wo_infoSys)
df_crs$usd_commitment = df_crs$usd_commitment *1000000
df_crs$usd_disbursement = df_crs$usd_disbursement *1000000
df_crs$usd_commitment_defl = df_crs$usd_commitment_defl *1000000
df_crs$usd_disbursement_defl = df_crs$usd_disbursement_defl *1000000
df_crs$commitmentdate = df_crs$commitmentyear

df_survey$year = df_survey$startyear



df_crs <-  df_reporters1  %>%
  # filter(crs == 1) %>% # do not forget this step
  select(ch_name, donorcode = crs_code) %>%
  # select(donorcode,donorname = donorname_unified) %>%
  unique %>%
  right_join(df_crs) 

df_crs$ch_name %>% is.na %>% which


a <-  df_crs$ch_name %>% is.na %>% which
df_crs$donorname[a]


df_reporters_press = reporters %>%
  filter(!is.na(ReporterName)) %>%
  select(donorname = ReporterName, donorname_unified, donorcode) %>%
  unique

#there is duplicates for WB, AfDB and IMF, so before merging:
df_reporters_press = df_reporters_press %>%
  group_by(donorname, donorname_unified) %>%
  dplyr::summarise(donorcode = min(donorcode))


# which(reporters_press$donorname %>% duplicated )



df_survey = df_survey %>%
  left_join(df_reporters_press)   %>%
  mutate(donorname = donorname_unified) %>%
  select(-donorname_unified) 


# which(is.na(press$donorcode))
# which(is.na(crspress$donorcode))
# which(is.na(press$donorname))
# which(is.na(crspress$donorname))



df_crs$source = "CRS"
df_survey$source = "survey"

# crspress$usd_commitment %>% is.numeric()
# press$usd_commitment %>% is.numeric()
# press$usd_costestimate %>% is.numeric()

# not working if doing conversion directly
# press$usd_commitment1  = as.numeric(press$usd_commitment)

press_commitments = df_survey %>%
  select(db_ref, usd_commitment, usd_costestimate) 

press_commitments$usd_commitment = gsub("[[:space:]]","", press_commitments$usd_commitment)

press_commitments$usd_commitment = gsub(",", "",press_commitments$usd_commitment)

press_commitments$usd_commitment = as.numeric(press_commitments$usd_commitment)

press_commitments = press_commitments %>%
  mutate(usd_commitment = ifelse(is.na(usd_commitment), 0, usd_commitment)) %>%
  mutate(usd_commitment = ifelse(usd_commitment == 0, usd_costestimate, usd_commitment))



df_survey  = df_survey %>% 
  select(-usd_commitment, -usd_costestimate) %>%
  left_join(press_commitments, by = "db_ref")


df_survey = df_survey %>% 
  mutate(usd_disbursement = ifelse(is.na(usd_commitment), usd_costestimate, usd_commitment))


merged_press_CRS = plyr::rbind.fill(df_survey, df_crs)

write_rds(merged_press_CRS, file = "./data/intermediate/99.01 merged_press_crs_2021_v2.Rds")


# sum(merged_press_CRS$usd_disbursement, na.rm= T)
# sum(merged_press_CRS$usd_commitment, na.rm= T)
# v2 removed the fuzzy part of japan





