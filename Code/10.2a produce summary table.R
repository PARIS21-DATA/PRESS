start_time <- Sys.time()


df_descriptions <- df_descriptions %>% 
  mutate(mention_data = grepl(vec_keywords, 
                              longdescription, 
                              ignore.case = T))

df_descriptions %>% select(mention_data) %>% table


## only select the relevant columns 
df_crs_simplified <- df_crs_raw %>% 
  filter(!db_ref %in% df_crs$db_ref) %>% 
  # mutate(gender = climatemitigation) %>% 
  # mutate(gender_marker = gender == 2)  %>% 
  # mutate(gender_marker = ifelse(is.na(gender_marker),
  #                               F,
  #                               gender_marker)) %>%
  # mutate(year = as.numeric(year)) %>% 
  select(gender, 
         db_ref, 
         donorname, 
         projecttitle, 
         longdescription, 
         usd_disbursement_defl,
         bi_multi, 
         year, 
         hash_longdesc) 


df_crs_simplified <- df_crs_simplified %>%
  inner_join(df_descriptions)


df_summary_by_3year <- df_crs_simplified %>%
  # only look for bilateral donors
  # filter(bi_multi != 4, bi_multi != 6) %>% 
  # mutate(gender = ifelse(is.na(gender), 0, gender)) %>%
  filter(!is.na(longdescription)) %>% 
  filter(nchar(longdescription) > 100) %>%
  mutate(group = year
         ,
         group =  (year - 2010) %/% 2
         , group = group*2,
         group = group+2011,
         group = paste0(group-1, "-", group)
  ) %>% 
  group_by(gender, mention_data, 
           group) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>%
  spread(key = mention_data, value = total) %>% 
  mutate(share = `TRUE`/(`TRUE`+`FALSE`)*100) %>%
  # mutate(share = `TRUE`) %>% 
  select(group, gender, share) %>% 
  spread(key = gender, value = share)

# find top projects 
e_fig$oda_gender_mention_data_projs <- df_crs_simplified %>%
  # only look for bilateral donors
  # filter(bi_multi != 4, bi_multi != 6) %>% 
  # mutate(gender = ifelse(is.na(gender), 0, gender)) %>%
  filter(!is.na(longdescription)) %>% 
  filter(nchar(longdescription) > 100) %>%
  filter(mention_data) %>% 
  filter(gender == 2) %>% 
  filter(year > 2018) %>% 
  group_by(donorname, projecttitle) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  ungroup %>% 
  # arrange(desc(total)) %>% 
  select(donorname, 
         # usd_disbursement_defl, 
         total,
         projecttitle) %>% 
  mutate(tmp_id = 1:n()) %>% 
  left_join(df_crs_simplified %>% ungroup %>% select(donorname, projecttitle, longdescription)) %>% 
  arrange(tmp_id, desc(nchar(longdescription))) %>% 
  group_by(tmp_id) %>% 
  filter(row_number() ==1) %>% 
  ungroup %>% 
  arrange(desc(total)) %>% 
  slice(1:300)
e_fig$oda_gender_mention_data_projs 

e_fig$oda_gender_mention_data_projs  %>% 
  write.xlsx("output/press/charts PRESS 2023/gender oda mention data.xlsx")




df_summary_by_3year %>% 
  print


df_summary_by_3year <- df_crs_simplified %>%
  # only look for bilateral donors
  # filter(bi_multi != 4, bi_multi != 6) %>% 
  # mutate(gender = ifelse(is.na(gender), 0, gender)) %>%
  filter(!is.na(longdescription)) %>% 
  filter(nchar(longdescription) > 100) %>%
  mutate(group = year
         ,
         group =  (year - 2010) %/% 3
         , group = group*3,
         group = group+2012,
         group = paste0(group-2, "-", group)
  ) %>% 
  group_by(gender, mention_data, 
           group) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>%
  spread(key = mention_data, value = total) %>% 
  mutate(share = `TRUE`/(`TRUE`+`FALSE`)*100) %>%
  # mutate(share = `TRUE`) %>% 
  select(group, gender, share) %>% 
  spread(key = gender, value = share)

df_summary_by_3year %>% 
  print
print_time_diff(start_time)

