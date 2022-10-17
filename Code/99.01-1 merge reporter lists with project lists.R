

source("code/99.01 merge crs and survey.R")
#####
# 2.0. clean PRESS donor list 
df_reporters_press$ReporterName[df_reporters_press$ReporterName %>% duplicated]

#there is duplicates for WB, AfDB and IMF, so before merging:

df_reporters_press_4merge <- df_reporters_press %>%
  filter(!is.na(ReporterName)) %>% #!! not sure if this is the best way. But working so far in the script 
  group_by(#ReporterName, 
    donorname_unified) %>%
  mutate(donorcode_unified = min(donorcode))

# remerging the simplified list with the original list
# df_reporters_press_4merge <- df_reporters_press %>% 
#   select(donorcode, ReporterId, donorname, donor_type,ReporterName) %>% 
#   unique %>% 
#   right_join(df_reporters_press_4merge)

names(df_reporters_press_4merge)
names(df_reporters_press)


######
# 2.5 create a donor list from PRESS donors

df_survey <- df_survey %>% 
  rename(donorname_survey = donorname)

df_survey <- df_survey %>%
  left_join(df_reporters_press_4merge, 
            by = c("donorname_survey" = "ReporterName"))   

# %>%
#   mutate(donorname = donorname_unified) %>%
#   select(-donorname_unified) 

# check if there are unsuccessful merge
var_crs_merge_quality <- df_survey %>% select(donorcode) %>% is.na %>% which

# df_survey$survey_donorname[var_crs_merge_quality] %>% unique
if(length(var_crs_merge_quality) == 0) {
  print("Great merge! All survey project have corresponding donor code in the donor list!")
} else {
  "Problematic merge! Some survey projects' donor are not in your donor list!!"
}
rm(var_crs_merge_quality)


df_survey_reporters <- df_survey %>% 
  select(donorcode, donorname, donorname_unified, donorcode_unified ,
         ReporterId, donorname_survey) %>% 
  unique

source("code/X99.01-1a examine merged survey data.R")

df_reporters_crs_4merge <- df_reporters %>%
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



