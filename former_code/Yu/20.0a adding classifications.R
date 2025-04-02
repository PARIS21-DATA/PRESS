
df_crs_d4d <- read_feather(paste0("data/intermediate/13.3 d4d data ready for charts ", 
                              year(Sys.Date()), 
                              ".feather"))


df_crs_d4d %>% 
  select( sector) %>% 
  table

df_csa_crs <- df_crs_d4d %>% 
  inner_join(tibble(sector = c(1:9), 
                    csa =c(5, 1,1,1,2, 2, 3,3,4))) %>% 
  select(db_ref, csa) %>% 
  right_join(select(df_crs, db_ref)) %>% 
  mutate(csa = replace_na(csa , 3)) %>% 
  mutate(share = 1) %>% 
  mutate(csa = paste0("predictions_domain", csa)) %>% 
  spread(key = csa, value = share, fill = 0)

rm(df_crs_d4d)


df_cofog <- read_csv("data/auxiliary/sectorcode COFOG.csv")

df_cofog_crs <- df_cofog %>% 
  select(sectorcode, COFOG_first_code, COFOG_first_description) %>% 
  unique %>% 
  inner_join(df_crs %>% select(db_ref, sectorcode)) %>% 
  select(db_ref, COFOG_first_code
         # , COFOG_first_description
         )  %>% 
  rename(final_cofog = COFOG_first_code)

rm(df_cofog)


df_sdg <- read_feather("data/intermediate/09.1 sdg markers 2023.feather")
names(df_sdg)

df_sdg_crs <- df_sdg %>% 
  inner_join(df_crs %>% select(db_ref)) %>% 
  select(db_ref, SDG = sdg_goal)

rm(df_sdg)
df_csa_crs %>% write_csv(paste0("output/ch/csa_classification_",
                                Sys.Date(), 
                                ".csv"))

df_csa_crs %>% write_csv(paste0("output/ch/csa_classification_",
                                "current", 
                                ".csv"))

df_sdg_crs %>% write_csv(paste0("output/ch/sdg_classification_",
                                Sys.Date(), 
                                ".csv"))
df_sdg_crs %>% write_csv(paste0("output/ch/sdg_classification_",
                                "current", 
                                ".csv"))

df_cofog_crs %>% write_csv(paste0("output/ch/cofog_classification_",
                                Sys.Date(), 
                                ".csv"))
df_cofog_crs %>% write_csv(paste0("output/ch/cofog_classification_",
                                "current", 
                                ".csv"))
