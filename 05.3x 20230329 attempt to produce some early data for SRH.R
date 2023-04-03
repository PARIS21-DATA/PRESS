
df_crs <- df_crs_01_filtered %>% 
  filter(year>2009)

df_crs %>% 
  filter(stats2) %>% 
  group_by(year) %>% 
  filter(donorname!= "International Development Association") %>% 
  # filter(donorname!= "International Development Association", 
         # donorname != "International Bank for Reconstruction and Development") %>% 
  summarise(sum = sum(usd_disbursement_defl , na.rm = T)) 




df_crs %>% 
  filter(stats) %>% 
  # filter(gender_filter_both) %>% 
  filter(text_filter_gender_narrower|gender_filter_desc) %>% 
  group_by(year) %>% 
  # filter(donorname!= "International Development Association"
         # ,
         # donorname != "International Bank for Reconstruction and Development"
         # ) %>%
  summarise(sum = sum(usd_disbursement_defl , na.rm = T))

df_crs$text_filter_gender %>% attributes

df_crs <- df_crs %>% 
  mutate(gender_filter_desc = replace_na(gender_filter_desc, F), 
         stats_filter_desc = replace_na(stats_filter_desc, F)) %>% 
  mutate(gender_filter_both  = gender_filter_desc|text_filter_gender, 
         stats  = stats_filter|stats_filter_desc)

df_crs %>% 
  filter(stats) %>% 
  filter(gender_filter_both) %>% 
  group_by(year) %>% 
  filter(
         donorname == "International Development Association"
  ) %>%
  summarise(sum = sum(usd_disbursement_defl , na.rm = T))

df_crs <- df_crs %>% 
  mutate(info_sys = grepl("information sys|infosys|info sys", 
                          x = projecttitle, 
                          ignore.case = T))

df_crs <- df_crs %>% 
  mutate(stats2  = stats&(!info_sys) ) %>% 
  mutate(stats2 = stats2|scb )


df_crs_woIDA <- df_crs %>% 
  filter(stats) %>% 
  group_by(year) %>% 
  filter(donorname!= "International Development Association") %>% 
  # filter(donorname!= "International Development Association", 
  # donorname != "International Bank for Reconstruction and Development") %>% 
  summarise(sum = sum(usd_disbursement_defl , na.rm = T)) 
df_crs_summary <- df_crs %>% 
  filter(stats) %>% 
  group_by(year) %>% 
  # filter(donorname!= "International Development Association") %>% 
  # filter(donorname!= "International Development Association", 
  # donorname != "International Bank for Reconstruction and Development") %>% 
  summarise(sum_withIDA = sum(usd_disbursement_defl , na.rm = T)) %>% 
  inner_join(df_crs_woIDA)

df_gender_woIDA <- df_crs %>%
  filter(stats) %>% 
  # filter(gender_filter_both|gen_rmnch2) %>% 
  filter(gender_filter_both) %>% 
  group_by(year) %>% 
  filter(
    donorname != "International Development Association"
  ) %>%
  summarise(sum_wo_ida = sum(usd_disbursement_defl , na.rm = T))

df_gender <- df_crs %>%
  filter(stats) %>% 
  # filter(gender_filter_both|gen_rmnch2) %>% 
  filter(gender_filter_desc|text_filter_gender_narrower) %>% 
  group_by(year) %>% 
  # filter(
  #   donorname != "International Development Association"
  # ) %>%
  summarise(sum = sum(usd_disbursement_defl , na.rm = T)) %>% 
  inner_join(df_gender_woIDA)


write_csv(df_crs_summary, file = "output/PRESS/20230331_summary.csv")
write_csv(df_gender, file = "Output/press/20230331 gender summary.csv")


df_crs %>%
  filter(stats) %>% 
  # filter(gender_filter_both|gen_rmnch2) %>% 
  # filter(gender_filter_desc|text_filter_gender_narrower) %>% 
  # group_by(year) %>% 
  filter(
    donorname == "International Development Association"
  ) %>%
  filter(year == 2021) %>% 
  group_by(projecttitle) %>% 
  # arrange(desc(usd_disbursement_defl)) %>% 
  summarise(sum = sum(usd_disbursement_defl, na.rm = T)) %>% 
  arrange(desc(sum)) %>% 
  select(projecttitle, sum) %>% 
  head(10) %>% 
  write_csv(file = "output/press/wb2023.csv")


df_crs %>%
  filter(stats) %>% 
  filter(gender_filter_both|gen_rmnch2) %>%
  # filter(gender_filter_desc|text_filter_gender_narrower) %>% 
  # group_by(year) %>% 
  filter(
    donorname == "International Development Association" | 
      donorname == "International Bank for Reconstruction and Development"
  ) %>%
  filter(year == 2021) %>% 
  group_by(projecttitle) %>% 
  # arrange(desc(usd_disbursement_defl)) %>% 
  summarise(sum = sum(usd_disbursement_defl, na.rm = T)) %>% 
  arrange(desc(sum)) %>% 
  select(projecttitle, sum) %>% 
  head(10) %>% 
  write_csv(file = "output/press/wb2023 gender.csv")

