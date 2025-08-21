source("code/00. boot.R")
rm(list = ls)
path_crs <- "data/intermediate/06.3 crs with donor code_2023.feather"

path_blacklist <- "data/intermediate/07.3b d4d manual blacklist.feather"

path_whitelist <- "data/intermediate/07.3a d4d manual additions.feather"

df_crs <- read_feather(path_crs)
df_blacklist <- read_feather(path_blacklist)
df_whitelist <- read_feather(path_whitelist)

df_crs <- df_crs %>% 
  filter(!db_ref %in% df_blacklist$db_ref) 

df_crs %>% 
  group_by(year) %>% 
  summarise(total = sum(usd_disbursement_defl, na.rm = T))

df_whitelist %>% 
  filter(db_ref %in% df_crs$db_ref) %>% 
  nrow


