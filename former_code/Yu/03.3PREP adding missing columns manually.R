df_crs_full %>% 
  filter(grepl("sppm", 
               projecttitle, 
               T)) %>% 
  select(year, donorname
         # ,usd_disbursement_defl
         , longdescription)  %>% 
  select(longdescription, year) %>% 
  distinct %>% 
  .$longdescription  %>% 
  unique


AGRIS
CARDS
NASS # 

National Agricultural Statistics Service
Integrated Household Survey
National Panel Survey
Living Standards Measurement Study Office
Core Agricultural and Rural Data Surveys


df_crs_full %>% 
  filter(grepl("AGRIS", 
               longdescription, 
               F)) %>% 
  select(year, donorname
         # ,usd_disbursement_defl
         , longdescription)  %>% 
  filter(donorname != "United States") %>% 
  # distinct
  select(longdescription, year) %>% 
  distinct %>% 
  .$longdescription  %>% 
  unique

names(df_crs_full)
df_crs <- df_crs %>% 
  # filter(!stats_filter) %>% 
  # filter(donorname != "United States") %>% 
  mutate(stat_missing = grepl("National Agricultural Statistics Service|Integrated Household Survey|National Panel Survey|Living Standards Measurement Study Office|Core Agricultural and Rural Data Survey|Agricultural Integrated Survey", 
                              longdescription, 
                              T)) 
  filter(grepl("National Agricultural Statistics Service|Integrated Household Survey|National Panel Survey|Living Standards Measurement Study Office|Core Agricultural and Rural Data Survey|Agricultural Integrated Survey", 
               longdescription, 
               T)) %>% 
  # nrow
  # select(year, donorname
  #        # ,usd_disbursement_defl
  #        , longdescription)  %>% 
  # # distinct
  select(longdescription, projecttitle) %>%
  # distinct %>% 
  # .$longdescription  %>% 
  unique %>% 
  slice(n()) %>% 
  .$projecttitle
  