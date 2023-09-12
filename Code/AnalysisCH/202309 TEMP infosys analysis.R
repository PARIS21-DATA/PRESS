
df_crs_full_cols %>% 
  filter(stats_wo_infoSys) %>% 
  group_by(year) %>% 
  summarise(total_disb = sum(usd_disbursement_defl, na.rm = T))

df_crs_full_cols %>% 
  filter(stats_wo_infoSys) %>%
  group_by(year) %>% 
  summarise(total_disb = sum(usd_disbursement_defl, na.rm = T))


