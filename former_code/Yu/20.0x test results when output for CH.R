df_crs_best <- read_feather("output/press/2023-10-17 PRESS data with climate markers2023.feather")
df_crs_output <- read_feather("output/CH/2023-09-15 PRESS 2023 data.feather")

df_crs %>% 
  fun_summarise_ts("finance_t_name")  


df_crs_best %>%
  filter(gen_ppcode|gen_donor|gen_marker2|gen_channel|gen_agency|gen_title|gen_desc|gen_rmnch2_narrow) %>%
  # filter(!gen_black_list) %>% 
  fun_summarise_ts("finance_t_name")  


df_crs_best <- df_crs_best %>% 
  mutate(gen_rmnch2_narrow = gen_rmnch2&(gender == 1)) %>%
  mutate(gen_final = gen_ppcode|gen_donor|gen_marker2|gen_channel|gen_agency|gen_title|gen_desc|gen_rmnch2_narrow) %>% 
  select(-gen_rmnch2_narrow) %>%
  ungroup %>% 
  filter(gen_final) %>% 
  mutate(total = paste(db_original_id, 
                       year, 
                       dac_donorname, 
                       recipientname, 
                       projectnumber, 
                       purposecode, 
                       sep = ";;")) %>% 
  select(gen_final, total)

df_crs_output <- df_crs_output %>%
  ungroup %>% 
  mutate(total = paste(db_original_id, 
                       year, 
                       dac_donorname, 
                       recipientname, 
                       projectnumber, 
                       purposecode, 
                       sep = ";;")) 


df_crs_best %>%
  ungroup %>% 
  filter(gen_final) %>% 
  mutate(total = paste(db_original_id, 
                       year, 
                       dac_donorname, 
                       recipientname, 
                       projectnumber, 
                       purposecode, 
                       sep = ";;")) %>% 
  select(total, gen_final)  %>% 
  right_join(df_crs_output)%>% 
  filter(gen_final) %>% 
  fun_summarise_ts("finance_t")  

df_crs_output %>% 
  fun_summarise_ts("finance_t")  
  

df_crs_best$db_ref %>% head



df_crs_best %>% 
  fun_summarise_ts("finance_t")  

df_crs_output %>% 
  fun_summarise_ts("finance_t")  


df_crs_output %>% 
  mutate(gen_socpro = grepl("social protec", 
                            projecttitle, 
                            ignore.case = T)) %>% 
  # filter(gender_filter_both|gender_filter_both_desc|gen_rmnch2|gen_socpro) %>% 
  # filter(gender_filter_both_desc_rmnch) %>%
  # filter(gender_filter_both_desc_rmnch|gen_rmnch2|gen_socpro) %>%
  filter(gender_filter_both|gender_filter_both_desc_rmnch|gen_socpro) %>% 
  fun_summarise_ts("finance_t") 

df_crs_output 




df_crs_output %>% 
  filter(gender_filter_both_desc_rmnch|gen_rmnch2) %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T))


df_crs %>% 
  fun_summarise_ts("finance_t_name")  

  