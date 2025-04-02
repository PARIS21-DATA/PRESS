df_crs <- df_crs %>% 
  mutate(stat_manual_additions = grepl("National Agricultural Statistics Service|Integrated Household Survey|National Panel Survey|Living Standards Measurement Study Office|Core Agricultural and Rural Data Survey|Agricultural Integrated Survey", 
                              longdescription, 
                              T)) 