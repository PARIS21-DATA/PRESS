start <- Sys.time()
df_crs <- read_rds("data/intermediate/crs05.2_utf8_full.rds")

print_time_diff(start)
df_crs$gender_filter_both %>% table
df_crs1 = df_crs
# remive NAs in each column, it seems that the results for gender identification changed a log while the results for data & stats didn't. 


df_crs <- df_crs1 %>% 
  mutate(text_detection = replace_na(text_detection, F), 
         text_detection_gender = replace_na(text_detection_gender, F), # previously made a mistake here. should be very careful with the suffix. 
         mining = replace_na(mining, F), 
         gen_ppcode = replace_na(gen_ppcode, F),
         gen_donor = replace_na(gen_donor, F),
         gen_marker = replace_na(gen_marker, F), 
         gen_marker1 = replace_na(gen_marker1, F), 
         gen_marker2 = replace_na(gen_marker2, F), 
         scb = replace_na(scb, 0), 
         pop = replace_na(pop, 0)
  ) %>% 
  mutate(scb = (scb == 1), 
         pop = (pop == 1))



df_crs <- df_crs %>%
  mutate(text_detection_wo_mining = text_detection & !mining
  )


# langues <- c("en","fr","es")
# df_crs <- df_crs %>%
#   select(-projecttitle_lower) %>%
#   mutate(language = ifelse(language %in% langues, language, "other") )

df_crs <- df_crs %>%
  mutate(stats_filter = text_detection_wo_mining | scb)
table(df_crs$text_detection_wo_mining) %>% print
table(df_crs$stats_filter) %>% print
table(df_crs$text_detection_gender)
table(df_crs$gender_filter_both)
table(df_crs$gender_filter_desc)
table(df_crs$text_filter_gender)

# which(is.na(df_crs$text_detection_wo_mining_w_scb))
print_time_diff(start)

df_crs <- df_crs %>%
  mutate(text_filter_gender = gen_donor|gen_ppcode|text_detection_gender|gen_marker2
  )  %>%
  mutate(text_filter_gender_narrower = gen_ppcode|text_detection_gender|gen_donor
         # | gen_marker2
  ) %>% 
  mutate(text_filter_gender_narrower = ifelse(is.na(text_filter_gender_narrower), F, text_filter_gender_narrower))


df_crs <- df_crs %>% 
  mutate(gender_filter_desc = replace_na(gender_filter_desc, F), 
         stats_filter_desc = replace_na(stats_filter_desc, F)) %>% 
  mutate(gender_filter_both  = gender_filter_desc|text_filter_gender, 
         stats  = stats_filter|stats_filter_desc)



start <- Sys.time()
write_rds(df_crs, file = "data/intermediate/crs05.2.1_utf8_full.rds")
print_time_diff(start)
