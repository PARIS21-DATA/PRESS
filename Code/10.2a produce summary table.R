start_time <- Sys.time()


df_descriptions <- df_descriptions %>% 
  mutate(mention_data = grepl(vec_keywords, 
                              longdescription, 
                              ignore.case = T))

df_descriptions %>% select(mention_data) %>% table



df_crs_simplified <- df_crs %>% 
  # mutate(gender_marker = gender == 2)  %>% 
  # mutate(gender_marker = ifelse(is.na(gender_marker),
  #                               F,
  #                               gender_marker)) %>%
  mutate(year = as.numeric(year)) %>% 
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
  filter(nchar(longdescription) > 100) %>%
  filter(!is.na(longdescription)) %>% 
  mutate(group = year
         ,
         group =  (year - 2013) %/% 2
         , group = group*2,
         group = group+2015,
         group = paste0(group-1, "-", group)
  ) %>% 
  group_by(gender, mention_data, 
           group) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>%
  spread(key = mention_data, value = total) %>% 
  mutate(share = `TRUE`/(`TRUE`+`FALSE`)*100) %>% 
  select(group, gender, share) %>% 
  spread(key = gender, value = share)

df_summary_by_3year %>% 
  print
print_time_diff(start_time)


df_crs_simplified %>%
  # only look for bilateral donors
  # filter(bi_multi != 4, bi_multi != 6) %>% 
  # mutate(gender = ifelse(is.na(gender), 0, gender)) %>%
  mutate(gender = is.na(gender)) %>% 
  # filter(year > 2015) %>% 
  # filter(nchar(longdescription) > 100) %>%
  # filter(!is.na(longdescription)) %>% 
  mutate(group = year
         # ,
         # group =  (year - 2010) %/% 3
         # , group = group*3,
         # group = group+2012,
         # group = paste0(group-2, "-", group)
  ) %>%
  group_by(gender,donorname
           # , group
           ) %>% 
  # summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>%
  summarise(total = n()) %>% 
  spread(key = gender, value = total, fill = 0) %>% 
  filter(!((`TRUE`== 0) & (`FALSE` == 0))) %>% 
  mutate(share = `TRUE`/(`TRUE`+`FALSE`)*100) %>% 
  select(# group, 
         donorname, share) %>%
  # spread(key = group, value = share) 
  arrange((share))  %>% 
  # .$share %>% quantile
  filter(share > 27) %>% 
  .$donorname 
  

