rm(list = ls())
df_crs <- read_rds("data/Intermediate/crs01_1_utf8_full.rds")
df_crs_o <- df_crs 
names(df_crs)


df_crs <- df_crs_o %>% 
  select(year, usd_disbursement_defl, gender) %>% 
  group_by(year, gender) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  group_by(year) %>% 
  mutate(sum = sum(total, na.rm = T)) %>% 
  spread(key = gender, value = total)  %>% 
  mutate(share_gen = (`2`+`1`)/sum) %>% 
  select(year, share_gen, `2`, `1`)

write_csv(df_crs, file = "output/gen_equality_oda.csv")

