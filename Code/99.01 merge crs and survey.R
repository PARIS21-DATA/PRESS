rm(list = ls())

##### 
# 1 loading data
#@ load project data
load("./data/Intermediate/06.1 crs and press with region and country code.rdata")
## load reporter data
df_reporters_press <- read_rds("data/auxiliary/reporters.rds")
names(df_reporters_press)
df_reporters_original <- read_csv("data/auxiliary/donor_codes_and_classifications.csv")

df_reporters_unification <- read_csv(file = "Data/auxiliary/reporter_names_concatenation.csv")
df_reporters_concat_names <- df_reporters_unification %>% 
  filter(!is.na(short_names))

#####
# 2. reporter data reshape and reforme

#####
# 2.1. reduce the CH reporter data
df_reporters <- df_reporters_original %>% 
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
  # filter(!is.na(ReporterName)
         # , ReporterName %in% vec_survey_reporters
         # ) %>%
  select(-crs, -press) %>%
  # rename(donorname_inlist = donorname, 
  #        donorname = ReporterName) %>%  
  unique

rm(vec_survey_reporters)
