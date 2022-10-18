rm(list = ls())

##### 
# 1 loading data
#@ load project data
load("./data/Intermediate/06.1 crs and press with region and country code.rdata")
## load reporter data
# df_reporters_press <- read_rds("data/auxiliary/reporters.rds")
df_reporters_survey <- read_rds("data/auxiliary/reporters_survey_2022.rds")
df_reporters_crs <- read_rds("data/auxiliary/reporters_crs_2022.rds")

df_reporters_survey <- df_reporters_survey %>% 
  select(donorname_survey , ReporterId = ReporterId_survey, ch_name)

df_reporters_crs <- df_reporters_crs %>% 
  select(crs_code, ReporterId = ReporterId_chlist, ch_name)


df_survey <- df_survey %>% 
  rename(donorname_survey = donorname)

df_survey <- df_survey %>%
  left_join(df_reporters_survey)   


# check if there are unsuccessful merge
var_crs_merge_quality <- df_survey %>% select(ReporterId) %>% is.na %>% which

# df_survey$survey_donorname[var_crs_merge_quality] %>% unique
if(length(var_crs_merge_quality) == 0) {
  print("Great merge! All survey project have corresponding donor code in the donor list!")
} else {
  "Problematic merge! Some survey projects' donor are not in your donor list!!"
}
rm(var_crs_merge_quality)


# source("code/X99.01-1a examine merged survey data.R")

df_reporters_crs_4merge <- df_reporters_crs %>%
  # select(ch_name, donorcode = crs_code) %>%
  rename(donorcode = crs_code) %>% 
  filter(!is.na(donorcode)) %>%
  # select(donorcode,donorname = donorname_unified) %>%
  unique


# merge the reporter with CRS project list by CRS donor code
df_crs <-  df_reporters_crs_4merge %>%
  right_join(df_crs) 
# now df_crs has both crs donor name and "ch donor name"


######
# 2.2. examine which donors are not in the list
var_crs_merge_quality <-  df_crs$ch_name %>% is.na %>% which
if(length(var_crs_merge_quality) == 0) {
  print("Great merge! All CRS project have corresponding donor code in the donor list!")
} else {
  "Problematic merge! Some CRS projects' donor are not in your donor list!!"
}
rm(var_crs_merge_quality)

# it also means that all CRS codes are used

# otherwise FIX: 
# a <-  df_crs$ch_name %>% is.na %>% which
# df_crs %>% select(donorname, donorcode) %>% slice(a) %>% unique %>% write_csv("Data/Analysis/reporters_not_in_list.csv")
# fixed by adding the additional ones




