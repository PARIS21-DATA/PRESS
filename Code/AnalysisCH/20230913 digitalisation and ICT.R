rm(list = ls())
gc()
job_specific_suffix <- "_full_"

crs_path <- paste0("Data/Intermediate/crs05.2", 
                   job_specific_suffix, 
                   year(Sys.Date()), 
                   ".rds")
crs_data_path <- paste0("Output/CH/", 
                          Sys.Date(), 
                          " PRESS 2023 data.feather")
df_crs <- readRDS(crs_path)
df_crs_data <- read_feather(crs_data_path)

df_crs_ict <- df_crs %>% 
  mutate(ict_ppcode = purposecode == 22040) 

rm(df_crs)
gc()

df_crs_ict <- df_crs_ict %>% 
  mutate(digital_title = grepl(pattern = "digitali", 
                               projecttitle, 
                               ignore.case = T))

df_crs_ict <- df_crs_ict %>% 
  mutate(ict_title = grepl(pattern = "\\bict\\b", 
                               projecttitle, 
                               ignore.case = T))

df_crs_ict <- df_crs_ict %>% 
  mutate(ict = ict_ppcode|digital_title|ict_title)

df_crs_ict %>% 
  filter(digital_title) %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T))

df_crs_ict %>% 
  filter(africa) %>% 
  filter(digital_title) %>% 

df_crs_ict %>% 
  filter(digital_title) %>% 
  # filter(africa) %>% 
  filter(year> 2018) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(projecttitle
         ,usd_disbursement_defl) 

# digitalisation
df_crs_ict %>% 
  filter(digital_title) %>% 
  mutate(bi_multi_merged = ifelse(bi_multi ==3, 1,
                                  ifelse(bi_multi > 6, 1, bi_multi))) %>% 
  group_by(year, bi_multi_merged) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  spread(key = bi_multi_merged, value = total)

# ict infrastructure
df_crs_ict %>% 
  filter(ict_title|ict_ppcode) %>% 
  mutate(bi_multi_merged = ifelse(bi_multi ==3, 1,
                                  ifelse(bi_multi > 6, 1, bi_multi))) %>% 
  group_by(year, bi_multi_merged) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  spread(key = bi_multi_merged, value = total)

df_crs_data <- df_crs_ict %>% 
  filter(ict) %>% 
  select(db_ref, ict, ict_ppcode, ict_title, digital_title) %>% 
  right_join(df_crs_data)

# data
df_crs_data %>% 
  mutate(bi_multi_merged = ifelse(bi_multi ==3, 1,
                                  ifelse(bi_multi > 6, 1, bi_multi))) %>% 
  group_by(year, bi_multi_merged) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  spread(key = bi_multi_merged, value = total) %>% 
  write.xlsx("output/press/20230913 funding to data.xlsx")

# ict & digitalisation
df_crs_ict %>% 
  filter(ict) %>% 
  mutate(bi_multi_merged = ifelse(bi_multi ==3, 1,
                                  ifelse(bi_multi > 6, 1, bi_multi))) %>% 
  group_by(year, bi_multi_merged) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  spread(key = bi_multi_merged, value = total) %>% 
  write.xlsx("output/press/20230913 funding to ict.xlsx")



# data
df_crs_data %>% 
  filter(africa) %>% 
  mutate(bi_multi_merged = ifelse(bi_multi ==3, 1,
                                  ifelse(bi_multi > 6, 1, bi_multi))) %>% 
  group_by(bi_multi_merged,
           group = (year - 2010) %/% 3) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  mutate(group = group*3, 
         group = group+2012, 
         group = paste0(group-2, "-", group)) %>% 
  mutate(bi_multi_merged = ifelse(bi_multi_merged ==1, 
                                  "Bilateral donors",
                                  ifelse(bi_multi_merged == 4, 
                                         "Multilateral donors", 
                                         "Private foundations"))) %>% 
  spread(key = bi_multi_merged, value = total) %>% 
  rename(Year = group) %>%
  write.xlsx("output/press/20230913 funding to data.xlsx")

# data
df_crs_data %>% 
  filter(africa) %>% 
  filter(ict) %>% 
  mutate(bi_multi_merged = ifelse(bi_multi ==3, 1,
                                  ifelse(bi_multi > 6, 1, bi_multi))) %>% 
  group_by(bi_multi_merged,
           group = (year - 2010) %/% 3) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  mutate(group = group*3, 
         group = group+2012, 
         group = paste0(group-2, "-", group)) %>% 
  mutate(bi_multi_merged = ifelse(bi_multi_merged ==1, 
                                  "Bilateral donors",
                                  ifelse(bi_multi_merged == 4, 
                                         "Multilateral donors", 
                                         "Private foundations"))) %>% 
  spread(key = bi_multi_merged, value = total) %>% 
  rename(Year = group) %>%
  write.xlsx("output/press/20230913 funding to data_ict.xlsx")

# ict & digitalisation
df_crs_ict %>% 
  filter(ict) %>% 
  filter(africa) %>% 
  mutate(bi_multi_merged = ifelse(bi_multi ==3, 1,
                                  ifelse(bi_multi > 6, 1, bi_multi))) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(bi_multi_merged,
           ,  group = (year - 2010) %/% 3) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  mutate(group = group*3, 
         group = group+2012, 
         group = paste0(group-2, "-", group)) %>% 
  mutate(bi_multi_merged = ifelse(bi_multi_merged ==1, 
                                  "Bilateral donors",
                                  ifelse(bi_multi_merged == 4, 
                                         "Multilateral donors", 
                                         "Private foundations"))) %>% 
  spread(key = bi_multi_merged, value = total) %>% 
  rename(Year = group) %>%
  write.xlsx("output/press/20230913 funding to ict.xlsx")


df_crs_ict %>% select(regionname, regioncode) %>% 
  unique
vec_africa_names <- c("North of Sahara" , "South of Sahara" , "Africa, regional"  ,
  "Africa, regional")

vec_africa_codes <- c(10003, 10002, 10001, 298)
vec_africa_reccode <- c(1027,
                        1028,
                        1029,
                        1030, 
                        289, 
                        298,
                        189)

df_crs_ict <- df_crs_ict %>% 
  filter(ict)


df_crs_ict <- df_crs_ict %>%
  mutate(africa = F) %>% 
  mutate(africa = ifelse((!is.na(regioncode))&(regioncode %in% vec_africa_codes), T, africa)) %>% 
  mutate(africa = ifelse(recipientcode %in% vec_africa_reccode, 
                         T, 
                         africa))

df_crs_data <- df_crs_data %>%
  mutate(africa = F) %>% 
  mutate(africa = ifelse((!is.na(dac_regionncode))&(dac_regionncode %in% vec_africa_codes), T, africa)) %>% 
  mutate(africa = ifelse(dac_recipientcode %in% vec_africa_reccode, 
                         T, 
                         africa))

df_crs_ict %>% 
  filter(is.na(recipientcode))



# ict & digitalisation
df_crs_ict %>% 
  filter(ict) %>% 
  filter(africa) %>% 
  filter(finance_t == 110) %>% 
  mutate(bi_multi_merged = ifelse(bi_multi ==3, 1,
                                  ifelse(bi_multi > 6, 1, bi_multi))) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(bi_multi_merged,
            group = (year - 2010) %/% 3) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  mutate(group = group*3, 
         group = group+2012, 
         group = paste0(group-2, "-", group)) %>% 
  mutate(bi_multi_merged = ifelse(bi_multi_merged ==1, 
                                  "Bilateral donors",
                                  ifelse(bi_multi_merged == 4, 
                                         "Multilateral donors", 
                                         "Private foundations"))) %>% 
  spread(key = bi_multi_merged, value = total) %>% 
  rename(Year = group) %>%
  write.xlsx("output/press/20230913 funding to ict grants only.xlsx")

write_feather(df_crs_ict, "output/press/ict and digitalisation.feather")
