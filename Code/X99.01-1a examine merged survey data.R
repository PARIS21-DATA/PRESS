# many of the df_survey donor code are in ReportIds in df_reporter
df_survey_reporters_unmateched <- df_survey_reporters %>% 
  filter(!donorcode %in% df_reporters$ReporterId)

df_survey_reporters_unmateched$ReporterName %in% df_reporters_unification$current_names
unique(df_survey$ReporterName) %in% df_reporters_unification$current_names

df_survey_reporters_unmateched$ReporterName %in% df_reporters$ch_name
df_survey_reporters_unmateched$ReporterName %in% df_reporters$ReporterName
df_survey_reporters_unmateched$ReporterName %in% df_reporters$crs_name_en
df_survey_reporters_unmateched$ReporterName %in% df_reporters$name_modification
df_survey_reporters_unmateched$ReporterId %in% df_reporters$ReporterId
df_survey_reporters_unmateched$ReporterId %in% df_reporters$crs_code
df_survey_reporters_unmateched$donorcode %in% df_reporters$ReporterId
df_survey_reporters_unmateched$donorcode %in% df_reporters$crs_code

