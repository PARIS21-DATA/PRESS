
df_reporters_mergeBy_idUnicode$donorname_survey %in% df_reporters_mergeBy_codecode$donorname_survey 

# df_reporters_crs <- df_reporters %>% 
#   filter(!is.na(crs_code)) %>% 
#   unique
# 
# 

df_survey_reporters %>% select(donorcode_unified) %>% unique %>% nrow

vec_donorname_survey_matched <- y %>% select(donorname_survey) %>% unique %>% .$donorname_survey
df_survey_reporters_unmateched <-  df_survey_reporters %>% 
  filter(!donorname_survey %in% vec_donorname_survey_matched)