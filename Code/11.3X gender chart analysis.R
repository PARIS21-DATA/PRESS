
# what are the large gender projects 
df_crs %>% 
  filter(gender_filter_both_desc_rmnch) %>%
  filter(year > 2020) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(#ch_name, 
    usd_disbursement_defl, 
    # identified_by_gender,
    identified_by_stat,
    projecttitle)

df_crs %>% 
  filter(gender_filter_both_desc|gen_socpro|gen_rmnch2_uncertain) %>%
  # filter(!gender_filter_both_desc|is.na(gender_filter_both_desc)) %>%
  fun_summarise_ts("finance_t_name")  

df_crs %>% 
  filter(gen_rmnch2_uncertain, !gen_rmnch2, !gender_filter_both_desc) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(#ch_name, 
    # identified_by_gender, 
    # gender, 
    rmnch, 
    projecttitle) 

df_crs %>% 
  filter(gender_filter_both_desc_rmnch) %>%
  # filter(gen_rmnch2) %>% 
  # filter(!gender_filter_both_desc) %>% 
  filter(year == 2015) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(#ch_name, 
    # usd_disbursement_defl, 
    # identified_by_gender,
    # identified_by_stat,
    projecttitle) 



df_crs %>% 
  ungroup %>% 
  filter(gender_filter_both_desc_rmnch) %>%
  filter(!gender_filter_both_desc) %>% 
  filter(year > 2019) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(#ch_name, 
    # usd_disbursement_defl, 
    # identified_by_gender,
    # identified_by_stat,
    projecttitle) %>% 
  distinct %>% 
  left_join(df_crs) %>% 
  filter(usd_disbursement_defl > 0, 
         !is.na(usd_disbursement_defl)) %>% 
  filter(rmnch > 2) %>% 
  select(ch_name,dac_donorname,year, projecttitle, rmnch ) %>%
  distinct


df_crs %>% 
  mutate(gen_rmnch2 =ifelse(gender == 0# & (is.na(gender))
                            , 
                            F, 
                            gen_rmnch2_uncertain)) %>% 
  filter(gender_filter_both_desc|gen_rmnch2|gen_socpro) %>%
  # filter(!gender_filter_both_desc|is.na(gender_filter_both_desc)) %>%
  fun_summarise_ts("finance_t_name")  


df_crs %>% 
  mutate(gen_rmnch2 =ifelse(gender == 0 & (is.na(gender)), 
                            F, 
                            gen_rmnch2_uncertain)) %>% 
  filter(gender_filter_both_desc|gen_rmnch2|gen_socpro) %>%
  # filter(!gender_filter_both_desc|is.na(gender_filter_both_desc)) %>%
  mutate(usd_disbursement_defl = usd_commitment_defl) %>% 
  mutate(year = commitment_year) %>% 
  filter(year > 2010) %>% 
  mutate(var_by = "total") %>% 
  fun_summarise_ts("var_by")  


df_crs %>% 
  mutate(usd_disbursement_defl = usd_commitment_defl) %>% 
  mutate(year = commitment_year) %>% 
  filter(year > 2010) %>% 
  mutate(var_by = "total") %>% 
  fun_summarise_ts("var_by")  


df_crs %>% 
  filter(ch_name == "United States", 
         year> 2018, 
         is.na(gender)) 

df_crs %>% 
  filter(year > 2016, 
         is.na(gender),
         !is.na(rmnch),
         rmnch == 4 | rmnch==2) %>% 
  select(ch_name, 
         projecttitle,
         identified_by_stat, identified_by_gender, 
         rmnch, 
         gen_rmnch2, 
         year) %>% 
  distinct




df_crs %>% 
  filter(year > 2016, 
         is.na(gender),
         !is.na(rmnch),
         rmnch == 4 | rmnch==2) %>% 
  select(ch_name, 
         projecttitle,
         identified_by_stat, identified_by_gender, 
         rmnch, 
         gen_rmnch2, 
         year, 
         gender) %>% 
  distinct %>% 
  select(projecttitle) %>% 
  unique %>% 
  inner_join(df_crs_raw) %>% 
  filter(grepl("bill",
               donorname, 
               ignore.case = T)) %>% 
  filter(year == 2018) %>% 
  # filter(!is.na(gender)) %>%
  filter(!is.na(rmnch))
select(rmnch) %>% 
  table

df_crs %>% 
  filter(gender_filter_both_desc|gen_rmnch|gen_socpro) %>% 
  filter(!gender_filter_both_desc|is.na(gender_filter_both_desc)) %>%
  arrange(desc(usd_disbursement_defl)) %>% 
  select(#ch_name, 
    usd_disbursement_defl, 
    # identified_by_gender,
    identified_by_stat,
    projecttitle)

# this project does not look even like a data project
df_crs %>% 
  filter(projecttitle == "WHO Immunization Information System") %>% 
  select(longdescription) %>% 
  unique %>% 
  .$longdescription
# thge long description proved it to be data project

df_crs_raw %>% 
  filter(projecttitle == "WHO Immunization Information System")  %>% 
  select(gender, rmnch)
# the marker for rmnch is carefully done


