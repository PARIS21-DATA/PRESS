# 0 setting up parameters 
path_input_sdg_markers <- paste0("data/intermediate/09.1 sdg markers UNIQUE goals ", 
                                 year(Sys.Date()), 
                                 ".feather")


# 1 merge with more gender filters
df_sdg <- read_feather(path_input_sdg_markers)
df_sdg <- df_sdg %>% 
  filter(sdg_goal == 5) %>% 
  select(db_ref) %>% 
  mutate(gen_sdg = T)



df_gender_agencies <- read.xlsx("data/auxiliary/gender filters.xlsx", sheet = 2)
names(df_gender_agencies) <- tolower(names(df_gender_agencies))
df_gender_channels <- read.xlsx("data/auxiliary/gender filters.xlsx", sheet = 1)
names(df_gender_channels) <- tolower(names(df_gender_channels))

df_gender_agencies <- df_gender_agencies %>% 
  filter(gender == 1) %>% 
  select(dac_donorcode= donorcode, agencycode
         # , agencyname = agencynamee
  )  %>% 
  mutate(gen_agency = T)

df_gender_channels <- df_gender_channels %>% 
  filter(gender == 1) %>% 
  select(channelcode= channel.id) %>% 
  mutate(gen_channel = T)

df_crs <- df_crs %>% 
  left_join(df_gender_agencies) %>% 
  left_join(df_gender_channels) %>% 
  left_join(df_sdg)



rm(df_gender_agencies, df_gender_channels, 
   df_sdg)

# 1.2 aggregate filters


gen_cols <- grep("^gen_", names(df_crs), value = TRUE)

df_crs <- df_crs %>% 
  mutate(across(all_of(gen_cols), ~ ifelse(is.na(.x), 
                                           F, 
                                           .x)))
rm(gen_cols)

df_crs <- df_crs %>% 
  mutate(gen_donor = ifelse(gen_channel|gen_agency, T,gen_donor))

df_crs <- df_crs %>% 
  mutate(gender_filter_both = gen_ppcode|gen_donor|gen_marker2|gen_title|gen_sdg)


df_crs <- df_crs %>% 
  mutate(gender_filter_both_desc = gen_desc|gender_filter_both) %>% 
  mutate(gender_filter_both_desc_rmnch = gen_desc|gender_filter_both|gen_rmnch2)


df_gender <- df_crs %>% 
  filter(gender_filter_both_desc_rmnch) %>% 
  select(gen_donor, 
         gen_ppcode, 
         gen_agency,
         gen_marker2, 
         gen_rmnch2, 
         gen_desc, 
         gen_title, 
         gen_sdg,
         # gender_filter_both, 
         db_ref
  ) %>% 
  gather(key = "filter", value = "value", -db_ref)


# 2. create identified by column

df_gender_filter_order <- tibble(filter = c("gen_donor", 
                                            "gen_ppcode", 
                                            "gen_title" , 
                                            "gen_marker2", 
                                            "gen_desc", 
                                            "gen_rmnch2", 
                                            "gen_sdg")) 
df_gender_filter_order <- df_gender_filter_order %>% 
  mutate(order = 1:n())


df_gender <- df_gender %>% 
  inner_join(df_gender_filter_order) %>% 
  arrange(db_ref, order) %>% 
  filter(value) %>% 
  group_by(db_ref) %>% 
  filter(row_number() == 1) 

df_gender <- df_gender %>% 
  mutate(identified_by_gender = str_remove(pattern = "gen_", 
                                           filter)) %>% 
  select(db_ref, identified_by_gender) 

df_gender <- df_gender %>% 
  distinct

df_crs <- df_crs %>% 
  # select(-identified_by_gender) %>% 
  left_join(df_gender)

rm(df_gender_filter_order, df_gender)
