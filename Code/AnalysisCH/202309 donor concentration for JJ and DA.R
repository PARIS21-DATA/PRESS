

df_crs %>% 
  filter(year > 2018) %>% 
  group_by(donorname_dac) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  arrange(desc(total)) %>% 
  ungroup %>% 
  mutate(all = sum(total, na.rm = T)) %>% 
  mutate(share = total/all) %>% 
  slice(1:6) %>%
  select(share) %>% 
  sum

df_crs %>% 
  filter(year %in% c(2019:2021)) %>% 
  group_by(donorname_dac) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  arrange(desc(total)) %>% 
  ungroup %>% 
  # mutate(all = sum(total, na.rm = T)) %>% 
  # mutate(share = total/all) %>% 
  # slice(1:5) %>% 
  filter(total > 1 ) %>% 
  # select(total) %>% 
  nrow


df_crs_original %>% 
  filter(year %in% c(2019:2021)) %>% 
  group_by(donorname) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  arrange(desc(total)) %>% 
  ungroup %>% 
  # mutate(all = sum(total, na.rm = T)) %>% 
  # mutate(share = total/all) %>% 
  # slice(1:5) %>% 
  filter(total > 1000 ) %>% 
  # select(total) %>% 
  nrow

df_crs %>% 
  filter(year %in% c(2016:2018)) %>% 
  group_by(ch_name) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  arrange(desc(total)) %>% 
  ungroup %>% 
  # mutate(all = sum(total, na.rm = T)) %>%
  # mutate(share = total/all) %>%
  # slice(1:5) %>% 
  filter(total > 30) %>% 
  # select(total) %>% 
  nrow



df_crs_original <- readRDS("data/Intermediate/crs01_1_full_2023.rds")

# df_crs_original %>% names
# names(df_crs_original) <- tolower(names(df_crs_original))

df_crs_original %>% 
  filter(year > 2018) %>% 
  group_by(donorname) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  arrange(desc(total)) %>% 
  ungroup %>% 
  mutate(all = sum(total, na.rm = T)) %>% 
  mutate(share = total/all) %>% 
  slice(1:6) %>%
  select(share) %>% 
  sum


df_crs %>% 
  filter(year > 2018) %>% 
  group_by(ch_name) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  arrange(desc(total)) %>% 
  ungroup %>% 
  mutate(all = sum(total, na.rm = T)) %>% 
  mutate(share = total/all) %>% 
  slice(1:5) %>%
  select(share) %>% 
  sum


df_crs %>% 
  filter(year %in% c(2016:2018)) %>% 
  group_by(ch_name) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  arrange(desc(total)) %>% 
  ungroup %>% 
  mutate(all = sum(total, na.rm = T)) %>% 
  mutate(share = total/all) %>% 
  slice(1:5) %>%
  select(share) %>% 
  sum



