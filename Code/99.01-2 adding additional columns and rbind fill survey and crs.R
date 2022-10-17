


df_crs <- df_crs %>% 
  filter(stats_wo_infoSys)
df_crs$usd_commitment = df_crs$usd_commitment *1000000
df_crs$usd_disbursement = df_crs$usd_disbursement *1000000
df_crs$usd_commitment_defl = df_crs$usd_commitment_defl *1000000
df_crs$usd_disbursement_defl = df_crs$usd_disbursement_defl *1000000
df_crs$commitmentdate = df_crs$commitmentyear

df_survey$year = df_survey$startyear

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





