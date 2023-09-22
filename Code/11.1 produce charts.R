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

# fig 1: time series by donor type
e_fig$total_by_donorType <- df_crs %>% 
  mutate(donor_type = ifelse(bi_multi ==2, 
                             "Multi-bi", 
                             donor_type)) %>% 
  group_by(year, donor_type) %>% 
  summarise(total = sum(usd_disbursement_defl,na.rm = T)) %>% 
  spread(key = donor_type, value = total)

e_fig$total_by_donorType

e_fig$total_by_donorType %>% 
  write.xlsx("output/press/charts PRESS 2023/fig 1 time series by donor types.xlsx")

# fig 2: time series by grants and loans
e_fig$total_by_financeType <- df_crs %>% 
  filter(finance_t != 520) %>% 
  group_by(year, finance_t_name) %>% 
  summarise(total = sum(usd_disbursement_defl,na.rm = T)) %>% 
  spread(key =  finance_t_name, value = total) 

e_fig$total_by_financeType 

e_fig$total_by_financeType %>% 
  write.xlsx("output/press/charts PRESS 2023/fig 1 time series by grant and loans.xlsx")


e_fig$multilat_by_financeType <- df_crs %>% 
  filter(finance_t != 520) %>% 
  filter(donor_type == "Multilateral") %>% 
  group_by(year, finance_t_name) %>% 
  summarise(total = sum(usd_disbursement_defl,na.rm = T)) %>% 
  spread(key =  finance_t_name, value = total) 

e_fig$multilat_by_financeType 



# 2.2 oda overall?
df_crs_raw %>% 
  filter(bi_multi != 4, bi_multi != 6) %>% 
  fun_summarise_ts(group_var = "incomegroupname",
                   rolling = T, 
                   roll_period = 3)

df_crs_raw %>% 
  filter(bi_multi == 4) %>% 
  fun_summarise_ts(group_var = "incomegroupname",
                   rolling = T, 
                   roll_period = 3)



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



# fig 4 wb 
df_crs %>%
  mutate(group = ch_name == "The World Bank") %>% 
  group_by(year, group) %>% 
  summarise(total = sum(usd_disbursement_defl,na.rm = T)) %>% 
  spread(key =  group, value = total)



# fig 5 gender

df_crs %>% 
  filter(gender_filter_both_desc_rmnch) %>%
  filter(year > 2020) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(#ch_name, 
    usd_disbursement_defl, 
    # identified_by_gender,
    identified_by_stat,
    projecttitle)

df_crs %>% 
  filter(gender_filter_both_desc|gen_socpro) %>%
  # filter(!gender_filter_both_desc|is.na(gender_filter_both_desc)) %>%
  fun_summarise_ts("finance_t_name")  

df_crs %>% 
  filter(gender_filter_both_desc_rmnch) %>%
  filter(!gender_filter_both_desc) %>% 
  filter(year > 2019) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(#ch_name, 
    usd_disbursement_defl, 
    # identified_by_gender,
    identified_by_stat,
    projecttitle) 


df_crs %>% 
  ungroup %>% 
  filter(gender_filter_both_desc_rmnch) %>%
  filter(!gender_filter_both_desc) %>% 
  filter(year > 2019) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(#ch_name, 
    # usd_disbursement_defl, 
    # identified_by_gender,
    # identified_by_stat,
    projecttitle) %>% 
  distinct %>% 
  left_join(df_crs) %>% 
  filter(usd_disbursement_defl > 0, 
         !is.na(usd_disbursement_defl)) %>% 
  filter(rmnch > 2) %>% 
  select(ch_name,dac_donorname,year, projecttitle, rmnch ) %>%
  distinct

  

e_fig$gender_by_fintype <- df_crs %>% 
  filter(gender_filter_both_desc|gen_rmnch_strict|gen_socpro) %>%
  # filter(!gender_filter_both_desc|is.na(gender_filter_both_desc)) %>%
  fun_summarise_ts("finance_t_name")  

e_fig$gender_by_fintype

e_fig$gender_by_fintype %>% 
  write.xlsx("output/press/charts PRESS 2023/5 gender by year by loans.xlsx")


df_crs %>% 
  filter(gender_filter_both_desc|gen_rmnch|gen_socpro) %>% 
  filter(!gender_filter_both_desc|is.na(gender_filter_both_desc)) %>%
  arrange(desc(usd_disbursement_defl)) %>% 
  select(#ch_name, 
         usd_disbursement_defl, 
         # identified_by_gender,
         identified_by_stat,
         projecttitle)

# this project does not look even like a data project
df_crs %>% 
  filter(projecttitle == "WHO Immunization Information System") %>% 
  select(longdescription) %>% 
  unique %>% 
  .$longdescription
# thge long description proved it to be data project

df_crs_raw %>% 
  filter(projecttitle == "WHO Immunization Information System")  %>% 
  select(gender, rmnch)
# the marker for rmnch is carefully done


