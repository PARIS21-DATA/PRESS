df_merged %>% 
  group_by(ch_name, disbursementdate) %>%  
  filter(disbursementdate > 2017) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  arrange(desc(total))  %>% 
  spread(key = disbursementdate, value = total , fill = 0)  %>% 
  mutate(drop = `2020`*2/(`2019`+`2018`), 
         sum = `2019`+`2019`+`2020`) %>% 
  arrange(desc(sum))

df_merged %>% 
  filter(ch_name =="International Labour Organisation") %>% 
  filter(disbursementdate > 2017) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(projecttitle, usd_disbursement_defl, disbursementdate) %>% 
  head(10)


df_merged %>% 
  filter(ch_name =="The World Bank") %>% 
  filter(commitmentdate > 2019) %>% 
  arrange(desc(usd_commitment_defl)) %>% 
  select(projecttitle, usd_commitment_defl, disbursementdate)  %>% 
  group_by(projecttitle) %>% 
  summarise(total = usd_commitment_defl) %>% 
  arrange(desc(total)) %>% 
  head(100) %>% 
  as.data.frame()

