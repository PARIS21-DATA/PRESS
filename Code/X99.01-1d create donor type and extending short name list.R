rm(list = ls())

##### 
# 1 loading data
#@ load project data
load("./data/Intermediate/06.1 crs and press with region and country code.rdata")
## load reporter data
# df_reporters_press <- read_rds("data/auxiliary/reporters.rds")
df_reporters_survey <- read_rds("data/auxiliary/reporters_survey_2022.rds")
df_reporters_crs <- read_rds("data/auxiliary/reporters_crs_2022.rds")

df_reporters_types <- read_csv("data/Analysis/donortypes_2021.csv")

df_reporters_merged <- rbind(select(df_reporters_crs, ReporterId = ReporterId_chlist, ch_name) , 
                             select(df_reporters_survey, ReporterId = ReporterId_survey, ch_name)) %>% 
  unique

df_reporters_merged_type <- inner_join(df_reporters_merged , df_reporters_types)

df_reporters_merged_type2 <- df_reporters_survey %>% 
  select(donor_type,ReporterId = ReporterId_survey, 
         ch_name) %>% unique %>% 
  filter(!ReporterId %in% df_reporters_merged_type$ReporterId)


df_reporters_unmerged <- df_reporters_merged %>% 
  filter(!ReporterId %in% df_reporters_types$ReporterId) %>% 
  arrange(ReporterId) 

vec_types_adding <- c(rep("non-DAC", 2),
                      rep("multilateral", 2),
                      rep("private", 11),
                      rep("multilateral", 7),
                      rep("private", 2),
                      rep("multilateral", 1),
                      rep("private", 3)
                      )

df_reporters_unmerged <- df_reporters_unmerged %>% 
  mutate(ReporterType = vec_types_adding)
# df_reporters_merged_type$ReporterName == df_reporters_merged_type$ch_name
write_csv(df_reporters_unmerged, file ="data/analysis/reporters_type_unmerged.csv")

df_reporters_unmerged <- read.csv("Data/Analysis/reporters_type_unmerged_fixed.csv", 
                                  encoding = "utf-8")

df_reporters_unmerged2 <- df_reporters_unmerged %>% 
  filter(! ReporterId %in% df_reporters_merged_type2$ReporterId)



names(df_reporters_survey)
names(df_reporters_unmerged)

df_reporters_crs_replace <- df_reporters_unmerged %>% 
  filter(!is.na(short_names) , 
         short_names!="") %>% 
  select(ReporterId_chlist = ReporterId, ch_name_new) %>% 
  inner_join(df_reporters_crs) %>% 
  mutate(ch_name = ch_name_new) %>% 
  select(-ch_name_new) 

df_reporters_crs <- df_reporters_crs %>% 
  filter(!ReporterId_chlist %in% df_reporters_crs_replace$ReporterId_chlist) %>% 
  rbind(df_reporters_crs_replace)

df_reporters_crs$ch_name == "WTO - International Trade Centre"
  
df_reporters_crs$ch_name[93] = "WTO International Trade Centre"

df_reporters_survey_replace <- df_reporters_unmerged %>% 
  filter(!is.na(short_names) , 
         short_names!="") %>% 
  select(ReporterId_survey = ReporterId, ch_name_new) %>% 
  inner_join(df_reporters_survey)%>% 
  mutate(ch_name = ch_name_new) %>% 
  select(-ch_name_new) 


df_reporters_survey <- df_reporters_survey %>% 
  filter(!ReporterId_survey %in% df_reporters_survey_replace$ReporterId_survey) %>% 
  rbind(df_reporters_survey_replace)

write_rds(df_reporters_survey, file = "data/auxiliary/reporters_survey_2022.rds")
write_rds(df_reporters_crs, file = "data/auxiliary/reporters_crs_2022.rds")


df_reporter_types_new <- df_reporters_unmerged2 %>% 
  select(ReporterType, ReporterId, ReporterName = ch_name_new)


df_reporter_types_survey <- df_reporters_survey %>% select(ReporterType = donor_type, 
                               ReporterId = ReporterId_survey, 
                               ReporterName = ch_name)
names(df_reporters_types)

df_reporters_types2 <- rbind(df_reporters_types , 
                             df_reporter_types_new, 
                             df_reporter_types_survey) %>% 
  unique


df_reporters_merged_type2 <- inner_join(df_reporters_merged , df_reporters_types2)

write_rds(df_reporters_types2, file = "data/auxiliary/reporters_types_2022.rds")


df_reporters_shortnames <- read_rds("data/auxiliary/reporters_shorten_2022.rds")

names(df_reporters_shortnames)

df_reporters_shortnames2 <- df_reporters_unmerged %>% 
  filter(short_names!= "") %>% 
  select(ReporterId, short_names) %>% 
  unique %>% 
  rbind(df_reporters_shortnames)

write_rds(df_reporters_shortnames2 , file = "data/auxiliary/reporters_shorten_2022.rds")


