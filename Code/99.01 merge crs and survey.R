rm(list = ls())

##### 
# 1 loading data
#@ load project data
load("./data/Intermediate/06.1 crs and press with region and country code.rdata")
## load reporter data
df_reporters_press <- read_rds("data/auxiliary/reporters.rds")
names(df_reporters_press)
df_reporters <- read_csv("data/auxiliary/donor_codes_and_classifications.csv")

df_reporters_concat_names <- read_csv(file = "Data/auxiliary/reporter_names_concatenation.csv")
df_reporters_concat_names <- df_reporters_concat_names %>% 
  filter(!is.na(short_names))

#####
# 2. reporter data reshape and reforme

#####
# 2.1. reduce the CH reporter data
df_reporters <- df_reporters %>% 
  select(ReporterId, ReporterName, 
         crs_name_en, crs_code, 
         name_modification, ch_name,
         dac_member, dac_bi_member, mult_donor, mult_donor_and_eu, private_donor, non_dac_donor)  %>% 
  unique
# remove the countries that does not belong to any group
df_reporters <- df_reporters %>% 
  mutate(total = rowSums( df_reporters[,7:12], na.rm = T)) %>% 
  filter((total  != 0)|(!is.na(ReporterId)))  %>% 
  select(-total)


# which(is.na(press$donorcode))
# which(is.na(crspress$donorcode))
# which(is.na(press$donorname))
# which(is.na(crspress$donorname))



#######
# 2.3 
# reduce the press reporter dataset to only survey ones
names(df_reporters_press)
vec_survey_reporters <- df_survey %>% 
  .$donorname %>% 
  unique
df_reporters_press <- df_reporters_press %>%
  filter(!is.na(ReporterName), 
         ReporterName %in% vec_survey_reporters) %>%
  select(-crs, -press) %>%
  # rename the variables for merging
  rename(donorname_inlist = donorname, 
         donorname = ReporterName) %>%  
  unique

rm(vec_survey_reporters)



#####
# 2.0. clean PRESS donor list 
#there is duplicates for WB, AfDB and IMF, so before merging:
df_reporters_press_4merge <- df_reporters_press %>%
  group_by(donorname, donorname_unified) %>%
  dplyr::summarise(donorcode = min(donorcode))





# now you have ReporterName as "donorname", are they unified with any of the names in ch list? 

######
# 2.5 create a donor list from PRESS donors
df_survey = df_survey %>%
  left_join(df_reporters_press_4merge)   %>%
  mutate(donorname = donorname_unified) %>%
  select(-donorname_unified) 


# merge the reporter with CRS project list by CRS donor code
df_crs <-  df_reporters %>%
  select(ch_name, donorcode = crs_code) %>%
  filter(!is.na(donorcode)) %>% 
  # select(donorcode,donorname = donorname_unified) %>%
  unique %>%
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


# check if there are unsuccessful merge
var_crs_merge_quality <- df_survey %>% select(donorcode) %>% is.na %>% which
if(length(var_crs_merge_quality) == 0) {
  print("Great merge! All survey project have corresponding donor code in the donor list!")
} else {
  "Problematic merge! Some survey projects' donor are not in your donor list!!"
}
rm(var_crs_merge_quality)




df_crs <- df_crs %>% 
  filter(stats_wo_infoSys)
df_crs$usd_commitment = df_crs$usd_commitment *1000000
df_crs$usd_disbursement = df_crs$usd_disbursement *1000000
df_crs$usd_commitment_defl = df_crs$usd_commitment_defl *1000000
df_crs$usd_disbursement_defl = df_crs$usd_disbursement_defl *1000000
df_crs$commitmentdate = df_crs$commitmentyear

df_survey$year = df_survey$startyear

df_crs$source = "CRS"
df_survey$source = "survey"

# crspress$usd_commitment %>% is.numeric()
# press$usd_commitment %>% is.numeric()
# press$usd_costestimate %>% is.numeric()

# not working if doing conversion directly
# press$usd_commitment1  = as.numeric(press$usd_commitment)

press_commitments = df_survey %>%
  select(db_ref, usd_commitment, usd_costestimate) 

press_commitments$usd_commitment = gsub("[[:space:]]","", press_commitments$usd_commitment)

press_commitments$usd_commitment = gsub(",", "",press_commitments$usd_commitment)

press_commitments$usd_commitment = as.numeric(press_commitments$usd_commitment)

press_commitments = press_commitments %>%
  mutate(usd_commitment = ifelse(is.na(usd_commitment), 0, usd_commitment)) %>%
  mutate(usd_commitment = ifelse(usd_commitment == 0, usd_costestimate, usd_commitment))



df_survey  = df_survey %>% 
  select(-usd_commitment, -usd_costestimate) %>%
  left_join(press_commitments, by = "db_ref")


df_survey = df_survey %>% 
  mutate(usd_disbursement = ifelse(is.na(usd_commitment), usd_costestimate, usd_commitment))


merged_press_CRS = plyr::rbind.fill(df_survey, df_crs)

write_rds(merged_press_CRS, file = "./data/intermediate/99.01 merged_press_crs_2021_v2.Rds")


# sum(merged_press_CRS$usd_disbursement, na.rm= T)
# sum(merged_press_CRS$usd_commitment, na.rm= T)
# v2 removed the fuzzy part of japan





