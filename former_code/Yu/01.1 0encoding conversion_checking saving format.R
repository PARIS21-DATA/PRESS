rm(list= ls())
df_crs <- readRDS("Data/Raw/CRS/crs_utf8_full.rds")


df_crs %>% filter(donorname %in% c("France","Spain", "Switzerland")) %>%
  filter(longdescription != "") %>% 
  pull(longdescription) %>%
  sample() %>%
  head(100)


df_crs2 <- readRDS("Data/Raw/CRS/crs2_utf8_full.rds")


b = df_crs2 %>% filter(donorname %in% c("France","Spain", "Switzerland")) %>%
  filter(longdescription != "") %>% 
  pull(longdescription) %>%
  sample() %>%
  head(100)

