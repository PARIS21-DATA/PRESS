rm(list = ls())

df_reporters_crs <- read_rds(file = "data/auxiliary/reporters_crs_2022.rds")
df_reporters_crs <- df_reporters_crs %>% 
  mutate(ch_name = ifelse(crs_code == 1618, 
                          "David and Lucile Packard Foundation", 
                          ch_name))

df_reporters_survey <- read_rds(file = "data/auxiliary/reporters_survey_2022.rds")
df_reporters_crs <- df_reporters_crs %>% 
  select(ch_name, ReporterId = ReporterId_chlist) %>% 
  unique
df_reporters_survey <- df_reporters_survey %>% 
  select(ch_name,ReporterId =  ReporterId_survey) %>% 
  unique

df_reporters <- rbind(df_reporters_crs, 
                      df_reporters_survey) %>% 
  unique

df_reporters %>% select(ReporterId) %>% duplicated() %>% which


df_reporters_types <- read_rds("data/auxiliary/reporters_types_2022.rds") %>% unique
df_reporters_short <- read_rds("data/auxiliary/reporters_shorten_2022.rds") %>% unique

df_reporters_short <- df_reporters_short %>% 
  filter(short_names != "UNESCWA", 
         short_names != "UNECLAC")
df_reporters_short <- add_row(df_reporters_short, ReporterId = 909, short_names = "IADB") %>% unique
write_rds(df_reporters_short, file = "data/auxiliary/reporters_shorten_2022.rds")

df_reporters <- df_reporters %>% 
  left_join(select(df_reporters_types, -ReporterName)) %>% 
  left_join(df_reporters_short) %>% 
  unique

df_reporters$ReporterId %>% duplicated() %>% which

df_reporters <- df_reporters %>% 
  mutate(ReporterName = ifelse(is.na(short_names), 
                               ch_name, 
                               paste0(short_names, " - ", ch_name)))

df_reporters <- df_reporters %>% 
  select(-short_names)


write_rds(df_reporters, file = "data/auxiliary/reporters_final_2022.rds")


rm(df_reporters_crs, 
   df_reporters_survey, 
   df_reporters_short, 
   df_reporters_types)



