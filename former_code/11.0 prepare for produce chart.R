rm(list = ls())
source("Code/00. boot.R")
time_start <- Sys.time()
# 0. load data
df_crs <- read_feather("output/CH/current PRESS 2023 data.feather")
df_crs_raw <- read_feather("data/intermediate/crs05.2_full_2023.feather")
print_time_diff(time_start)
# 0.1 basic tabulation
df_crs %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl,na.rm = T)) 

df_crs %>% 
  filter(gender_filter_both_desc_rmnch) %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl,na.rm = T)) 

df_crs %>%
  filter(grepl("social protect",
               projecttitle,
               ignore.case = T)) %>%
  filter(year > 2020) %>%
  arrange(desc(usd_disbursement_defl)) %>%
  select(projecttitle, gender_filter_both_desc_rmnch)

# 0.2 prepare a new env for chart data
e_fig <- new.env()

# 0.3 tmp adding variables to crs_raw
df_crs_raw <- df_crs_raw %>% 
  mutate(donor_type = ifelse(bi_multi == 4, 
                             "Multilateral", 
                             ifelse(bi_multi == 6, 
                                    "Private", 
                                    "Bilateral")))

# 0.4 tmp adding variables to crs

df_crs <- df_crs %>% 
  mutate(gen_socpro = grepl("social protec", 
                            projecttitle, 
                            ignore.case = T))


df_crs <- df_crs %>% 
  mutate(country_specific = !is.na(isocode))


df_fragile_states <- read_feather("data/auxiliary/fragile states 2023.feather")

df_crs <- df_crs %>% 
  left_join(df_fragile_states)

df_crs <- df_crs %>% 
  mutate(fs = !is.na(fs))

rm(df_fragile_states)

df_sids <- read_csv("data/auxiliary/sids.csv")

df_crs <- df_crs %>% 
  left_join(df_sids) %>% 
  mutate(sids = !is.na(sids))

df_crs <- df_crs %>% 
  mutate(sids_region = regionname %in% c("Oceania", "Caribbean")) %>% 
  mutate(sids_region_recipient = recipientname %in% c("Caribbean, regional",
                                                      "Oceania, regional",
                                                      "Melanesia, regional")) %>% 
  mutate(sids_region_recipient = ifelse(is.na(recipientname), F, 
                                        sids_region_recipient), 
         sids_region = ifelse(is.na(regionname), F, sids_region)) %>% 
  mutate(sids_region = sids_region|sids_region_recipient) %>% 
  select(-sids_region_recipient) %>% 
  rename(sids_recipient = sids) %>% 
  mutate(sids = sids_recipient|sids_region)


df_crs <- df_crs %>% 
  mutate(finance_t_name_w_equiv = ifelse(grantequiv>0&!is.na(grantequiv)&finance_t_name == "Standard loan", 
                                         "Grant equivalent of loan", 
                                         finance_t_name)) %>% 
  mutate(usd_disbursement_defl_equiv = ifelse(finance_t_name_w_equiv == "Grant equivalent of loan", 
                                              grantequiv, 
                                              usd_disbursement_defl)) 
df_crs <- df_crs %>% 
  filter(!grepl("Camp expenses in", 
                projecttitle, 
                T)) 

rm(df_sids)

df_names_change <- read_xlsx("data/auxiliary/new_names_for_output.xlsx")
