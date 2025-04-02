df_crs %>% 
  filter(finance_t_name == "Standard loan") %>% 
  # filter(ch_name == "The World Bank") %>% 
  mutate(usd_disbursement_defl = grantequiv ) %>% 
  fun_summarise_ts("donor_type")


df_crs %>% 
  # filter(finance_t_name == "Standard loan") %>% 
  # filter(ch_name == "The World Bank") %>% 
  mutate(usd_disbursement_defl = grantequiv ) %>%
  fun_summarise_ts("donor_type")


df_crs %>% 
  # filter(finance_t_name == "Standard loan") %>% 
  # filter(ch_name == "The World Bank") %>% 
  # mutate(usd_disbursement_defl = grantequiv ) %>%
  fun_summarise_ts("finance_t_name")


df_crs %>% 
  filter(finance_t_name == "Standard loan") %>% 
  filter(year > 2018) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  group_by(ch_name, projecttitle) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  arrange(desc(total))
distinct



df_crs %>% 
  filter(grepl("social protect", projecttitle, T)) %>% 
  filter(ch_name == "The World Bank") %>% 
  filter(recipientname == "Nepal") %>% 
  select(projecttitle
         , year
         , usd_disbursement_defl
         , usd_arrears_interest
         , usd_interest
         , interest1
         , interest2)  %>% 
  slice(1:20)
