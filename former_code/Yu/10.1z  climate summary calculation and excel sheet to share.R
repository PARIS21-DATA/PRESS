# 5. calculations

df_crs %>% 
  filter(climate) %>%
  select(identified_by_climate) %>% 
  table


df_crs %>% 
  filter(climate) %>% 
  # filter(year > 2015) %>% 
  group_by(year) %>% 
  summarise(total_disb = sum(usd_disbursement_defl, na.rm = T), 
            total_commit = sum(usd_commitment_defl, na.rm = T),
            cnt = n())




df_crs %>% 
  filter(climate) %>% 
  filter(year > 2017) %>% 
  arrange(desc(year), desc(usd_disbursement_defl)) %>% 
  select(projecttitle,
         longdescription, 
         donorname = ch_name, 
         recipientname, 
         disbursement_year = year, 
         usd_disbursement_defl, 
         identified_as_climate_because = identified_by_climate, 
         channelname, 
         agencyname, 
         sdgfocus, 
         climatemitigation, 
         climateadaptation, 
         usd_commitment, 
         commitmentdate) %>% 
  write.xlsx(path_output_just_climate_xlsx)


df_crs %>% 
  select(reported_year = year, 
         projecttitle,
         longdescription, 
         donorname = ch_name, 
         recipientname, 
         disbursement_year = year, 
         usd_disbursement_defl, 
         identified_as_climate_because = identified_by_climate, 
         channelname, 
         agencyname, 
         sdgfocus, 
         climatemitigation, 
         climateadaptation, 
         usd_commitment, 
         commitmentdate, 
         identified_as_data_because = idenfied_by_stat
  ) %>% 
  write.xlsx(path_output_press_xlsx)


df_crs %>% 
  filter(climate) %>% 
  # filter(year > 2017) %>% 
  arrange(desc(year), desc(usd_disbursement_defl)) %>% 
  select(projecttitle,
         longdescription, 
         donorname = ch_name, 
         recipientname, 
         disbursement_year = year, 
         usd_disbursement_defl, 
         identified_as_climate_because = identified_by_climate, 
         channelname, 
         agencyname, 
         sdgfocus, 
         climatemitigation, 
         climateadaptation, 
         usd_commitment, 
         commitmentdate, 
         db_ref) %>% 
  write.xlsx(path_output_just_climate_all_xlsx)
print_time_diff(start_time )