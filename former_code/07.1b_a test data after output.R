vec_non_d4d <- readRDS("data/auxiliary/List of projects before d4d merge_2023.rds")
df_d4d_original <- read.xlsx("data/d4d/D4D Profiles data_2022_c.xlsx")
names(df_d4d_original) <- tolower(names(df_d4d_original))

df_d4d <- df_crs %>% 
  # filter(year > 2020) %>%
  filter(!db_ref %in% vec_non_d4d) %>% 
  arrange(desc(usd_disbursement_defl)) 

df_crs %>% 
  filter(idenfied_by_stat != "d4d") %>% 
  filter(gender_filter_both) %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) 
  
  

df_crs %>% 
  group_by(year) %>% 
  filter(!is.na(grantequiv)) %>% 
  summarise(total_disb = sum(usd_disbursement_defl, na.rm = T), 
            total_grant = sum(usd_grantequiv, na.rm = T))

df_crs %>% 
  group_by(year, finance_t) %>% 
  summarise(
            # total = sum(usd_disbursement_defl, na.rm = T)
            # , 
            total = sum(usd_grantequiv, na.rm = T)
            ) %>% 
  spread(key = finance_t, value = total) 

df_crs %>% 
  group_by(year, flowname) %>% 
  summarise(
    total = sum(usd_disbursement_defl, na.rm = T)
    # , 
    # total = sum(usd_grantequiv, na.rm = T)
  ) %>% 
  spread(key = flowname, value = total) 
  


df_d4d %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T))

df_crs %>% 
  # filter(year > 2020) %>%
  filter(db_ref %in% vec_non_d4d) %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T))


df_crs %>% 
  # filter(year > 2020) %>%
  # filter(db_ref %in% vec_non_d4d) %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T))

df_d4d %>% 
  filter(idenfied_by_stat != "d4d") %>% 
  mutate(crsid = db_original_id) %>% 
  anti_join(select(df_d4d_original, crsid) %>% unique)

df_d4d  %>% 
  filter(grepl("data center", projecttitle, ignore.case  = T)) %>% 
  # select(usd_disbursement_defl, projecttitle, ch_name, idenfied_by_stat) %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T))

df_d4d  %>% 
  filter(grepl("data-driven", projecttitle, ignore.case  = T)) %>% 
  # select(usd_disbursement_defl, projecttitle, ch_name, idenfied_by_stat) %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T))

df_d4d  %>% 
  filter(grepl("Strengthened country capacity in data and innovation", projecttitle, ignore.case  = T)) %>% 
  # select(usd_disbursement_defl, projecttitle, ch_name, idenfied_by_stat) %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T))

df_d4d  %>% 
  filter(year > 2020) %>%
  filter(idenfied_by_stat != "d4d") %>% 
  # filter(grepl("data center", projecttitle, ignore.case  = T)) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(usd_disbursement_defl, projecttitle, ch_name
         # , idenfied_by_stat
         ) %>% 
  select(projecttitle) 

df_d4d  %>% 
  filter(!grepl("Strengthened country capacity in data and innovation", projecttitle, ignore.case  = T)) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(usd_disbursement_defl, projecttitle, ch_name
         , idenfied_by_stat
  ) # %>% 
  # select(projecttitle) 


df_d4d  %>% 
  filter(year > 2020) %>%
  filter(idenfied_by_stat != "d4d") %>% 
  # filter(grepl("data center", projecttitle, ignore.case  = T)) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  select(usd_disbursement_defl, projecttitle, ch_name
         # , idenfied_by_stat
  ) %>% 
  select(usd_disbursement_defl) %>% 
  sum(na.rm = T) 


df_d4d  %>% 
  filter(year > 2020) %>%
  filter(idenfied_by_stat != "d4d") %>% 
  # filter(grepl("data center", projecttitle, ignore.case  = T)) %>% 
  arrange(desc(usd_disbursement_defl)) %>% 
  group_by(projecttitle) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T)) %>% 
  arrange(desc(total))


df_d4d  %>% 
  filter(year > 2020) %>%
  filter(idenfied_by_stat != "d4d") %>% 
  select(projecttitle) %>% 
  slice(1:20) 




