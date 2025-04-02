rm(list = ls())

source("code/00. boot.R")
source("code/00.1 functions.R")

path_input_imf <- "Data/intermediate/06.4 imf with regions and rec code_2023.feather"
path_survey_4ref <- "data/Intermediate/06.3 survey with donor code_2023.feather"
path_crs_4ref <- "data/Intermediate/06.3 crs with donor code_2023.feather"
path_aux_rec_codes <- "data/auxiliary/new_regions_2023.feather"
path_aux_donors <- "data/auxiliary/reporters_final_2022.rds"
path_survey_disb <- "data/Intermediate/06.1a survey data disbursement info_2023.rds"
path_interim_output_duplicat_surveyCRS_commit <- "data/intermediate/07.2 duplicate reportings commit 2fix_2023.xlsx"
path_interim_output_duplicat_surveyCRS_disb <- "data/intermediate/07.2 duplicate reportings disb 2fix_2023.xlsx"
path_interim_input_dupoicat_surveyCRS_disb <- "data/Intermediate/07.2 duplicate reportings disb fixed_2023.xlsx"

# 1. loading data
df_imf <- read_feather(path_input_imf)
df_crs <- read_feather(path_crs_4ref)
df_survey <- read_feather(path_survey_4ref)
df_recipient_code <- read_feather(path_aux_rec_codes)
df_reporters <- readRDS(path_aux_donors)
df_disbursements_survey <- readRDS(path_survey_disb)
rm(path_input_imf
   , path_crs_4ref
   , path_survey_4ref
   , path_aux_rec_codes
   , path_aux_donors
   , path_survey_disb)

# 2 find the duplicates between crs and imf
# 2.1 duplications in commitments

df_survey$role %>% table

df_donor_commit_survey <- df_survey %>% 
  filter(role =="Donor") %>% 
  group_by(commitmentdate, ReporterId) %>% 
  summarise(total_commit = sum(usd_commitment, na.rm = T)) %>% 
  filter(total_commit > 0)

df_donor_commit_crs <- df_crs %>% 
  # rename(reported_year = year) %>% 
  group_by(commitment_year, ReporterId) %>% 
  summarise(total_commit = sum(usd_commitment, na.rm = T) ) %>% 
  ungroup() %>% 
  rename(commitmentdate = commitment_year) %>% 
  filter(total_commit > 0)


df_donor_commit_crs %>% 
  rename(total_commit_crs = total_commit) %>% 
  mutate(total_commit_crs = total_commit_crs * 1000000) %>% 
  ungroup %>%
  inner_join(df_donor_commit_survey)  %>% 
  inner_join(df_reporters %>% select(ReporterId, ch_name)) %>% 
  write.xlsx(path_interim_output_duplicat_surveyCRS_commit)
rm(path_interim_output_duplicat_surveyCRS_commit)

# 2.2 duplications in disbursements
df_disb_survey <- df_disbursements_survey %>% 
  ungroup %>% 
  select(db_original_id, disbursment_date, value) %>% 
  left_join(df_survey %>%  select(db_original_id, ReporterId, role)) %>% 
  ungroup %>% 
  filter(role == "Donor") %>% 
  filter(value != 0, !is.na(value)) %>% 
  select(-role) %>% 
  group_by(disbursment_date, ReporterId) %>% 
  summarise(total_disb_survey = sum(value, na.rm = T))


df_disb_crs <- df_crs %>% 
  group_by(year, ReporterId) %>% 
  summarise(total_disb_crs = sum(usd_disbursement, na.rm = T)) %>% 
  mutate(total_disb_crs = total_disb_crs *1000000)


df_disb_survey %>% 
  rename(year = disbursment_date) %>% 
  inner_join(df_disb_crs) %>% 
  left_join(df_reporters %>% select(ReporterId, ch_name)) %>% 
  write.xlsx(path_interim_output_duplicat_surveyCRS_disb)
rm(path_interim_output_duplicat_surveyCRS_disb)


# 3. remove duplicated lines in CRS and survey

df_crs %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  mutate(total = total * 1000000)


df_survey %>% 
  filter(role == "Donor") %>% 
  select(db_original_id) %>% 
  distinct %>% 
  left_join(df_disbursements_survey) %>% 
  filter(disbursement_currency == "US Dollars") %>% 
  group_by(reported_year) %>% 
  summarise(total = sum(value, na.rm = T)) 

df_disb_fixed <- read.xlsx(path_interim_input_dupoicat_surveyCRS_disb)
rm(path_interim_input_dupoicat_surveyCRS_disb)


df_2remove_crs <- df_disb_fixed %>% 
  filter(choice == "survey") %>% 
  filter(ReporterId != 932) %>% 
  select(year, ReporterId) 

df_crs_reduced <- df_crs %>% 
  anti_join(df_2remove_crs)

rm(df_2remove_crs, )


df_crs_reduced %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  mutate(total = total * 1000000)


df_crs %>% 
  mutate(text_infosys = grepl("information sys|infosys|info sys", projecttitle, ignore.case = T)) %>% 
  filter(!text_infosys) %>%
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  mutate(total = total * 1000000) %>% 
  write_csv("Output/PRESS/20230810 for discussion.csv")



df_crs %>% 
  mutate(text_infosys = grepl("information sys", projecttitle, ignore.case = T)) %>% 
  filter(text_infosys) %>% 
  select(projecttitle, idenfied_by_stat, usd_disbursement_defl) %>% 
  arrange(desc(usd_disbursement_defl))
