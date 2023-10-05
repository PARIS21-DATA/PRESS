df_crs <- df_crs %>% 
  mutate(climate_title_questionable = ifelse(is.na(projecttitle), 
                                             F, 
                                             grepl(pattern = "Technical quality, statistics and cross cutting themes |Plan de Acción Nacional para la Reducción y Gestión de Riesgos de Desastre en el sector agropecuario y la seguridad alimentaria y nutricional de Paraguay", 
                                                   x = projecttitle ,
                                                   T))) 

# df_crs %>% 
#   filter(climate_title_questionable) %>% 
#   select(projecttitle, ch_name) %>% 
#   distinct