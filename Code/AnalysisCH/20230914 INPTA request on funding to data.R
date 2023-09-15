rm(list =ls())
df_crs <- read_feather("output/ch/2023-09-13 PRESS 2023 data.feather")
df_ict <- read_feather("output/press/ict and digitalisation.feather")

df_crs <- df_ict %>% 
  select(db_ref, ict) %>% 
  right_join(df_crs) %>% 
  mutate(ict = ifelse(is.na(ict), F, ict)) 

df_crs %>% 
  filter(gender_filter_both) %>% 
  filter(year> 2020) %>% 
  filter(grepl("Strengthening Systems for Social", projecttitle)) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(db_ref, projecttitle, usd_disbursement_defl, year, finance_t) %>% 
  select(usd_disbursement_defl) %>% 
  sum()

df_crs %>% 
  # filter(africa) %>% 
  # filter(ict) %>% 
  mutate(bi_multi_merged = ifelse(bi_multi ==3, 1,
                                  ifelse(bi_multi > 6, 1, bi_multi))) %>% 
  group_by(bi_multi_merged,
           year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  # mutate(group = group*3, 
  #        group = group+2012, 
  #        group = paste0(group-2, "-", group)) %>% 
  mutate(bi_multi_merged = ifelse(bi_multi_merged ==1, 
                                  "Bilateral donors",
                                  ifelse(bi_multi_merged == 4, 
                                         "Multilateral donors", 
                                         "Private foundations"))) %>% 
  spread(key = bi_multi_merged, value = total) %>% 
  # rename(Year = group) %>%
  write.xlsx("output/press/20230914 funding to data.xlsx")


df_crs %>% 
  # filter(africa) %>% 
  # filter(ict) %>%
  # mutate(bi_multi_merged = ifelse(bi_multi ==3, 1,
  #                                 ifelse(bi_multi > 6, 1, bi_multi))) %>% 
  group_by(ict,
           year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  # mutate(group = group*3, 
  #        group = group+2012, 
  #        group = paste0(group-2, "-", group)) %>% 
  # mutate(bi_multi_merged = ifelse(bi_multi_merged ==1, 
  #                                 "Bilateral donors",
  #                                 ifelse(bi_multi_merged == 4, 
  #                                        "Multilateral donors", 
  #                                        "Private foundations"))) %>% 
  spread(key = ict, value = total) %>% 
  # rename(Year = group) %>%
  write.xlsx("output/press/20230914 funding to data_ict.xlsx")


df_crs %>% 
  # filter(africa) %>% 
  # filter(ict) %>%
  # mutate(bi_multi_merged = ifelse(bi_multi ==3, 1,
  #                                 ifelse(bi_multi > 6, 1, bi_multi))) %>% 
  mutate(grant = finance_t == 110) %>% 
  group_by(grant,
           year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  # mutate(group = group*3, 
  #        group = group+2012, 
  #        group = paste0(group-2, "-", group)) %>% 
  # mutate(bi_multi_merged = ifelse(bi_multi_merged ==1, 
  #                                 "Bilateral donors",
  #                                 ifelse(bi_multi_merged == 4, 
  #                                        "Multilateral donors", 
  #                                        "Private foundations"))) %>% 
  spread(key = grant, value = total) %>% 
  # rename(Year = group) %>%
  write.xlsx("output/press/20230914 funding to data_grantLoans.xlsx")


