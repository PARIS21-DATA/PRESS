source("Code/00. boot.R")

start <- Sys.time()
df_crs <- read_rds("data/intermediate/crs05.2.1_utf8_full.rds")
print_time_diff(start)
gc()


df_crs_stats <- df_crs %>% 
  filter(stats)

df_crs_o <- df_crs

write_rds(df_crs_stats, "data/intermediate/crs05.3_onlystats_utf8_full.rds")
rm(df_crs)
gc()

df_crs_stats <- read_rds("data/intermediate/crs05.3_onlystats_utf8_full.rds")
df_crs_stats$rmnch %>% unique()


df_crs <- df_crs_stats %>% 
  mutate(usd_disbursement_defl = replace_na(usd_disbursement_defl, 0),  
         usd_commitment_defl = replace_na(usd_commitment_defl, 0)) %>% 
  mutate(gen_rmnch = replace_na(rmnch, 0), 
         gen_rmnch_narrow = gen_rmnch %in% c(3,4), 
         gen_rmnch_broader = gen_rmnch %in% c(2:4)) %>% 
  mutate(gender_filter_both_rmnch = gender_filter_both|gen_rmnch_narrow)

# examine each colums of gender-related filters, see which one is significantly more frequent
# a = df_crs %>% 
#   select(gen_donor, gen_ppcode, gen_marker2, gender_filter_desc, text_filter_gender, text_detection_gender)
# lapply(a, table)
# rm(a)


df_crs %>% 
  filter(stats_filter) %>% 
  select(text_detection_gender) %>% 
  table


# funding to gender data over the years
# NOT FINAL Filter

df_crs %>% group_by(year, text_filter_gender) %>% 
  filter(year > 2016) %>% 
  # filter(text_filter_gender) %>% 
  summarise(total = sum(usd_disbursement_defl), 
            cnt = n()) 


# Funding to gender data
# FULL filter ex rmnch
df_crs %>% group_by(year
                    # , text_filter_gender
) %>% 
  # filter(year > 2016) %>% 
  filter(gender_filter_both) %>%
  summarise(total = sum(usd_disbursement_defl), 
            cnt = n()) 

# check donornames
df_crs %>% arrange(donorname) %>% select(donorname) %>%  unique()

# UK and sweden's contribution
df_crs %>% group_by(year
                    # , text_filter_gender
) %>% 
  filter(donorname %in% c("United Kingdom", "Sweden") )%>% 
  # filter(year > 2016) %>% 
  filter(gender_filter_both) %>%
  summarise(total_disb = sum(usd_disbursement_defl), 
            total_commit = sum(usd_commitment_defl), 
            cnt = n()) 


# douuble check and check what if we remove gender marker 
df_crs <- df_crs %>% 
  mutate(gender_filter_both2 = text_detection_gender|gender_filter_desc|gen_ppcode|gen_donor|gen_marker2, 
         gender_filter_both_narrower = text_detection_gender|gender_filter_desc|gen_ppcode|gen_donor) 

# double check 
df_crs %>% group_by(year
                    # , text_filter_gender
) %>% 
  filter(donorname %in% c("United Kingdom", "Sweden") )%>% 
  # filter(year > 2016) %>% 
  filter(gender_filter_both2) %>%
  summarise(total_disb = sum(usd_disbursement_defl), 
            total_commit = sum(usd_commitment_defl), 
            cnt = n()) 

# check when remove gender marker
df_crs %>% group_by(year
                    # , text_filter_gender
) %>% 
  filter(donorname %in% c("United Kingdom", "Sweden") )%>% 
  # filter(year > 2016) %>% 
  filter(gender_filter_both_narrower) %>%
  summarise(total_disb = sum(usd_disbursement_defl), 
            total_commit = sum(usd_commitment_defl), 
            cnt = n()) 

# check all gender projects - not only statistics
df_crs_o %>% group_by(year
                      # , text_filter_gender
) %>% 
  filter(donorname %in% c("United Kingdom", "Sweden") )%>% 
  # filter(year > 2016) %>% 
  filter(gender_filter_both) %>%
  summarise(total_disb = sum(usd_disbursement_defl, na.rm = T), 
            total_commit = sum(usd_commitment_defl, na.rm = T), 
            cnt = n()) 

# check gender markers to validate against the OECD findings
df_crs_o$gender %>% table

df_crs_o %>% group_by(year
                      # , text_filter_gender
) %>% 
  filter(donorname %in% c("United Kingdom", "Sweden") )%>% 
  # filter(year > 2016) %>% 
  # filter(gender_filter_both) %>%
  filter(gender == 1 | gender == 2) %>% 
  summarise(total_disb = sum(usd_disbursement_defl, na.rm = T), 
            total_commit = sum(usd_commitment_defl, na.rm = T), 
            cnt = n()) 


# overall funding to gender increased in 2020
# but UK and Sweden decreased
df_crs_o %>% group_by(year
                      # , text_filter_gender
) %>% 
  # filter(donorname %in% c("United Kingdom", "Sweden") )%>%
  # filter(year > 2016) %>% 
  # filter(gender_filter_both) %>%
  filter(gender == 1 | gender == 2) %>% 
  summarise(total_disb = sum(usd_disbursement_defl, na.rm = T), 
            total_commit = sum(usd_commitment_defl, na.rm = T), 
            cnt = n()) 

# redownload the data to double check
df_2022 <- read.csv("data/Raw/CRS/other CRS/CRS 2020 data_20220503.txt",sep = "|", header = T, stringsAsFactors = F, encoding = "utf-8")

names(df_2022) = tolower(names(df_2022))
df_2022 %>% group_by(year
                     # , text_filter_gender
) %>% 
  # filter(donorname %in% c("United Kingdom", "Sweden") )%>% 
  # filter(year > 2016) %>% 
  # filter(gender_filter_both) %>%
  filter(gender == 1 | gender == 2) %>% 
  summarise(total_disb = sum(usd_disbursement_defl, na.rm = T), 
            total_commit = sum(usd_commitment_defl, na.rm = T), 
            cnt = n()) 

# we can also check only DAC donors later
# dac_codes <- read_csv("Documents/DAC_donor_code.csv")

names(df_2022)
# df_2022$category %>% unique

df_2022 %>% group_by(year
                     # , text_filter_gender
) %>% 
  filter(donorname %in% c("United Kingdom", "Sweden") )%>% 
  # filter(year > 2016) %>% 
  # filter(gender_filter_both) %>%
  filter(gender == 1 | gender == 2) %>% 
  summarise(total_disb = sum(usd_disbursement_defl, na.rm = T), 
            total_commit = sum(usd_commitment_defl, na.rm = T), 
            cnt = n()) 

## previously we checked for how gender projects were identified in each language. 
# df_crs %>% 
#   filter(language_title %in% c("en","fr", "es", "de")) %>% 
#   group_by(language_title, text_detection_gender) %>% 
#   summarise(total  = sum(usd_disbursement_defl)) %>% 
#   spread(key = language_title, value = total)
# 
# df_crs %>% filter(!text_detection_gender) %>% 
#   select(projecttitle) %>% 
#   head(10)
