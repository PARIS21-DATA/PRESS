# df_crs_raw <- read_feather("data/intermediate/crs05.2_full_2023.feather")

# find out the donors and the years where rmnch is irregular
df_rmnch_wrong_value_donors <- df_crs %>% 
  select(donorname, rmnch, year) %>% 
  filter(!is.na(rmnch)) %>% 
  filter(rmnch > 2) %>% 
  # select(dac_donorname = donorname, year) %>% 
  distinct() 

# for the ones with irregular data change rmnch marker

df_rmnch_donors_new_gender_filter <- df_crs %>% 
  inner_join(df_rmnch_wrong_value_donors) %>% 
  mutate(gen_rmnch1 = (rmnch > 1 )&(rmnch< 4)&(!is.na(rmnch)), 
         gen_rmnch2 = (rmnch == 4) & (!is.na(rmnch))) %>% 
  mutate(across(c("gen_rmnch1", "gen_rmnch2"), 
                ~ ifelse(is.na(.x), 
                         F, 
                         .x))) %>% 
  mutate(gen_rmnch = gen_rmnch1|gen_rmnch2)
names(df_rmnch_donors_new_gender_filter )
names(df_crs)
# for the ones that are regular, also assign the marker

df_crs <- df_crs %>% 
  filter(!process_id %in% df_rmnch_donors_new_gender_filter$process_id) %>% 
  mutate(gen_rmnch1 = (rmnch == 1 )&(!is.na(rmnch)), 
         gen_rmnch2 = (rmnch == 2) & (!is.na(rmnch))) %>% 
  mutate(across(c("gen_rmnch1", "gen_rmnch2"), 
                ~ ifelse(is.na(.x), 
                         F, 
                         .x))) %>% 
  mutate(gen_rmnch = gen_rmnch1|gen_rmnch2) %>% 
  rbind(df_rmnch_donors_new_gender_filter)


# df_crs <- df_crs %>%
  # mutate(gen_rmnch2 = gen_rmnch2) %>% 
  # mutate(gen_rmnch2_narrow = ifelse(is.na(gender)|gender == 0, 
  #                                  gen_rmnch2, 
  #                                  ))
  
  
rm(df_rmnch_wrong_value_donors,
     df_rmnch_donors_new_gender_filter)
# rm(df_crs_raw)
