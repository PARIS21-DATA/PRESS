df_crs <- df_crs %>%
  mutate(data = grepl("statis|estadi", df_crs$projecttitle, ignore.case = T))

# df_crs$SCB = ifelse((df_crs$purposecode == 16062 ) & (!is.na(df_crs$purposecode)), 1, 0)
# df_crs$POP = ifelse(( df_crs$purposecode  ==13010 ) & (!is.na(df_crs$purposecode)), 1, 0)
# df_crs$data2 = grepl("data|datos|donne", df_crs$projecttitle, ignore.case = T)

df_crs$DHS = grepl("DHS", df_crs$projecttitle, ignore.case = T)

df_crs$census = grepl("census|Volkszählung|recensement|censo", df_crs$projecttitle, ignore.case = T)
df_crs$survey = grepl("survey|sondage|enquête|encuesta", df_crs$projecttitle, ignore.case = T)