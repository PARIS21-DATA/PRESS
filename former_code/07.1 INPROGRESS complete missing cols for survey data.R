df_survey <- df_survey %>% 
  mutate(source = "survey")


df_survey_merged <- df_survey_merged %>% 
  mutate(identified_by = "survey", 
         identified_by_gender = ifelse(gender_filter_both_rmnch, "Not a gender project", "survey"), 
         source = "survey")


df_survey_merged <- df_survey_merged %>% 
  mutate(gender_filter_both_rmnch = genmul == "Yes, gender statistics isn’t the main area of this project, but the project has activities related to gender statistics")