df_crs_output <- df_crs %>% 
  filter(text_detection_wo_mining_w_scb) %>%
  filter((!gen_marker)&(!gen_donor)&(!gen_ppcode) &(!text_detection_gender)) 

df_crs_output$unique_id <- !duplicated(df_crs_output$text_id)
df_crs_output <- df_crs_output  %>% 
  filter(unique_id) %>%
  select(-unique_id) %>% 
  select(-channelcode, -desc_2mine, -text_id, -gen_marker, -gen_marker1, -gen_marker2, -projecttitle_lower, -title_id, -text_detection, -mining, -(text_detection_wo_mining:text_filter_gender), -shortdescription , 
         -pop, -scb, -language_title, -language) 




write_csv(df_crs_output, file = "./Output/Gender markers and analysis.csv")
