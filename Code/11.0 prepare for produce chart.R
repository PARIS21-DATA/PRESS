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
