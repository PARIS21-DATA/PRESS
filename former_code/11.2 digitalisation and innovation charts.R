

# fig 3: innovation
df_crs %>%
  mutate(group = ict_digit_info) %>% 
  group_by(year, group) %>% 
  summarise(total = sum(usd_disbursement_defl,na.rm = T)) %>% 
  spread(key =  group, value = total)


df_crs %>% 
  # filter(ict_digit_info) %>% 
  filter(database_title) %>% 
  filter(year > 2019) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(ch_name, projecttitle, usd_disbursement_defl) %>% 
  select(projecttitle) %>% 
  distinct %>% 
  slice(1:20) %>% 
  .$projecttitle 


df_crs %>% 
  filter(ict_digit_info) %>% 
  filter(!d4d_addition_match, 
         !d4d_addition_search, 
         identified_by_stat == "title") %>% 
  filter(year > 2015) %>% 
  group_by(projecttitle, ch_name) %>% 
  summarise(total = sum(usd_disbursement_defl, 
                        na.rm = T)) %>% 
  arrange(desc(total)) %>% 
  select(ch_name, projecttitle, total) %>% 
  write.xlsx("output/press/temp_infosys_projects.xlsx")

## 3.1 funding for digitalisation
df_crs %>% 
  filter(ict_digit_info) %>% 
  mutate(group = finance_t_name) %>% 
  group_by(year, group)%>% 
  summarise(total = sum(usd_disbursement_defl,na.rm = T)) %>% 
  spread(key =  group, value = total)

df_crs %>% 
  filter(finance_t_name == "Standard loan") %>% 
  filter(year > 2018) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(ch_name, projecttitle, usd_disbursement_defl)


