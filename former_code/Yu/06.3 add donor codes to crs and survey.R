rm(list = ls())
source("code/00. boot.R")
# source("code/00.1 functions.R")

# reading in the data from the survey 
####################################
## --- PRESS Data Preparation --- ##
####################################

# 0 Input::

path_input <- paste0("Data/Intermediate/06.2 crs and press with region and country code_", 
                      year(Sys.Date())
                      ,".Rdata")

path_output_crs <- paste0("Data/Intermediate/06.3 crs with donor code_", 
                     year(Sys.Date())
                     ,".feather")

path_output_survey <- paste0("Data/Intermediate/06.3 survey with donor code_", 
                          year(Sys.Date())
                          ,".feather")

path_donors_crs <- "data/auxiliary/reporters_crs_2023.feather"
path_donors_survey <- "data/auxiliary/reporters_survey_2022.rds"


# 1 load data::
load(path_input)
df_reporters_crs <- read_feather(path_donors_crs)
df_reporters_survey <- read_rds(file = path_donors_survey)

# df_reporters_crs_addition <- read_xlsx("data/auxiliary/reporters_d4d_2023_202310.xlsx")
# df_reporters_crs <- rbind(df_reporters_crs, 
#                           df_reporters_crs_addition)
# write_feather(df_reporters_crs, "data/auxiliary/reporters_crs_2023.feather")

# 2 work on CRS data:
names(df_reporters_crs)
df_reporters_crs_4merge <- df_reporters_crs %>% 
  select(donorname = crs_name_en, 
         donorcode = crs_code, 
         ch_name, 
         ReporterId = ReporterId_chlist) %>% 
  unique
# test if we can get same number of rows
df_crs_new <- df_crs %>% 
  inner_join(df_reporters_crs_4merge)
df_crs %>% 
  left_join(df_reporters_crs_4merge) %>% 
  filter(is.na(ch_name)) %>% 
  select(donorname, donorcode) %>% 
  distinct 

# df_crs_donors_missing <- df_crs %>% 
#   left_join(df_reporters_crs_4merge) %>% 
#   filter(is.na(ch_name)) %>% 
#   select(donorname, donorcode) %>% 
#   distinct 

# df_crs_donors_missing$donorname

# df_crs_donors_missing %>% 
#   filter(donorcode %in% df_reporters_crs_4merge)

# after testing merge only by donorcode
df_reporters_crs_4merge <- df_reporters_crs %>% 
  select(#donorname = crs_name_en, 
         donorcode = crs_code, 
         ch_name, 
         ReporterId = ReporterId_chlist) %>% 
  unique

df_crs <- df_crs %>% 
  inner_join(df_reporters_crs_4merge)

df_crs <- df_crs %>% 
  rename(dac_donorname = donorname, 
         dac_donorcode = donorcode)
rm(df_crs_new)

# # add Liechtenstein in 2023
# df_reporters_crs <- df_reporters_crs %>% 
#   add_row(ReporterName_chlist = "Liechtenstein", 
#           ReporterId_chlist = 70, 
#           crs_name_en = "Liechtenstein", 
#           crs_code = 70, 
#           name_modification = "Liechtenstein", 
#           ch_name = "Liechtenstein",
#           dac_bi_member = NA, 
#           dac_member = NA, 
#           mult_donor = NA, 
#           mult_donor_and_eu = NA, 
#           private_donor = NA, 
#           non_dac_donor = 1)
# saveRDS(df_reporters_crs, file = "data/auxiliary/reporters_crs_2022.rds")


# 3 work on survey data:

names(df_reporters_survey)
names(df_survey)

df_reporters_survey_4merge <- df_reporters_survey %>% 
  select(donorname = donorname_survey, 
         ch_name, 
         ReporterId = ReporterId_chlist) %>% 
  unique

# test if there are columns dropped during this process
df_survey_new <- df_survey %>% 
  inner_join(df_reporters_survey_4merge)


rm(df_survey_new)
df_survey <- df_survey %>% 
  inner_join(df_reporters_survey_4merge)

df_survey <- df_survey %>% 
  rename(donorname_survey = donorname) 

# 4 output
write_feather(df_crs, path_output_crs)
write_feather(df_survey, path_output_survey)
