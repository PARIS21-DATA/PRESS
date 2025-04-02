
df_crs %>% 
  # filter(gender_filter_both) %>% 
  filter(gender_filter_both_desc_rmnch) %>% 
  select(identified_by_gender) %>% 
  table

df_crs %>% 
  # filter(gender_filter_both) %>% 
  # filter(gender_filter_both_desc_rmnch) %>%
  filter(gender_filter_both_desc) %>%
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T))

df_crs %>% 
  filter(identified_by_gender == "rmnch2") %>% 
  filter(year ==2021) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(ch_name, projecttitle, usd_disbursement_defl) %>% 
  slice(1:20)

df_crs %>% 
  filter(identified_by_gender == "rmnch2") %>% 
  select(rmnch) %>% 
  table


df_crs %>% 
  # filter(gender_filter_both) %>% 
  filter(gender_filter_both_desc_rmnch) %>% 
  filter(year > 2017) %>% 
  filter(identified_by_gender == "title") %>% 
  filter(grepl("social protection|data disaggregation|intersectional", 
               projecttitle, 
               ignore.case = T)) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(ch_name, projecttitle, usd_disbursement_defl, year, identified_by_stat) 


df_crs %>% 
  # filter(gender_filter_both) %>% 
  filter(gender_filter_both_desc_rmnch) %>% 
  filter(year > 2017) %>% 
  filter(identified_by_gender == "title") %>% 
  filter(grepl("social protection|data disaggregation|intersectional", 
               projecttitle, 
               ignore.case = T)) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(ch_name, projecttitle, usd_disbursement_defl, year) %>% 
  group_by(projecttitle, year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>%
  arrange(year, desc(total)) %>% 
  group_by(year) %>% 
  filter(row_number() < 6)

df_crs %>% 
  filter(projecttitle == "Strengthening Systems for Social Protection and Civil Registration Project") %>% 
  select(gen_marker2, gen_rmnch2, gen_desc, 
         gen_title, gen_ppcode, gen_donor)





df_crs %>% 
  # filter(gender_filter_both) %>% 
  filter(gender_filter_both_desc_rmnch) %>% 
  # filter(year > 2017) %>% 
  # filter(identified_by_gender == "title") %>% 
  filter(grepl("intersec", 
               projecttitle, 
               ignore.case = T)) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(ch_name, projecttitle, usd_disbursement_defl, year)
