df_crs <- df_crs %>% 
  mutate(ict_title = grepl(pattern = "\\bict\\b", 
                           projecttitle, 
                           ignore.case = T))


df_crs <- df_crs %>% 
  mutate(digit_title = grepl(pattern = "digitali", 
                           projecttitle, 
                           ignore.case = T))


df_crs <- df_crs %>% 
  mutate(database_title = grepl(pattern = "database", 
                             projecttitle, 
                             ignore.case = T))

df_crs <- df_crs %>% 
  mutate(innov_title = grepl(pattern = "innovat", 
                                projecttitle, 
                                ignore.case = T))


df_crs <- df_crs %>% 
  mutate(geo_title = grepl(pattern = "geo|cadast", 
                             projecttitle, 
                             ignore.case = T))


df_crs <- df_crs %>% 
  mutate(across(c("ict_title", 
                  "digit_title", 
                  "infosys",
                  "database_title", 
                  "innov_title",
                  "geo_title"), ~ifelse(is.na(.x), 
                                      F, 
                                      .x)))

df_crs <- df_crs %>% 
  mutate(ict_digit_info = infosys|ict_title|digit_title|
           database_title|
           innov_title|geo_title)



# df_crs <- df_crs %>% 
#   mutate(survey_title = grepl(pattern = "survey|enquête|enquete|encuesta|recensement|census|censo", 
#                              projecttitle, 
#                              ignore.case = T))
# 
# df_crs %>%
#   mutate(group = survey_title) %>% 
#   group_by(year, group) %>% 
#   summarise(total = sum(usd_disbursement_defl,na.rm = T)) %>% 
#   spread(key =  group, value = total) %>% 
#   mutate(share = `TRUE`/(`TRUE`+`FALSE`))
