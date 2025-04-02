df_jp <- read_feather("data/intermediate/crs05.5 still missing_full_2023.feather")

df_jp <- df_jp %>% 
  filter(year == 2018)


setdiff(names(df_jp), names(df_crs))
names(df_donors)


setdiff(names(df_jp), names(df_crs_2018_joined))
setdiff(names(df_crs_2018_joined), names(df_jp))

df_jp <- df_jp %>% 
  select(-donorcode) %>% 
  left_join(select(df_donors,donorname = crs_name_en
                   , donorcode = crs_code
                   , ch_name 
                   , ReporterId = ReporterId_chlist))





df_jp <- df_jp %>% 
  mutate(db_ref = paste0("df_survey_", 
                         row_number())) %>% 
  mutate(crsid = paste0("d4d_", 
                        row_number())) 

df_jp %>% 
  select(recipientcode, 
         recipientname, 
         regioncode, 
         regionname)
df_jp$commitmentdate
df_jp <- df_jp %>% 
  rename(db_original_id = crsid) 
df_jp <- df_jp %>% 
  mutate(dac_donorcode= donorcode, 
         dac_donorname = donorname,
         process_id = NA, 
         dac_recipientcode = 9998, 
         dac_regionncode = 15006, 
         dac_regioncode = 15006, 
         dac_regionname = regionname, 
         commitment_year = year, 
         donor_type = "Bilateral", 
         finance_t_name = "Standard grant", 
         isocode = NA
         )


setdiff(names(df_jp), names(df_crs_2018_joined))
setdiff(names(df_crs_2018_joined), names(df_jp))

df_jp <- df_jp %>% 
  select(all_of(names(df_crs_2018_joined)))


df_crs_new_2 <- rbind(df_jp, df_crs_2018_joined) %>% 
  mutate(commitment_year = as.numeric(commitment_year)) %>% 
  plyr::rbind.fill(df_crs)
df_jp$usd_disbursement_defl %>% sum


df_crs_new_2 %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T))
