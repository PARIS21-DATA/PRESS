rm(list = ls())
source("Code/00. boot.R")

start <- Sys.time()
df_crs_stats <- read_rds("data/intermediate/crs05.3_onlystats_utf8_full.rds")
print_time_diff(start)
gc()

df_crs <- df_crs_stats

df_crs %>% 
  select(donorname, donorcode) %>% 
  unique() %>% 
  arrange(donorname) %>% 
  write_csv(file = "data/list_donors_2022.csv")

# IMF was not found through the traditional method! 

# now download the entire database
start <- Sys.time()
df_crs <- read_rds("data/intermediate/crs05_utf8_full.rds")
print_time_diff(start)

df_crs %>% 
  select(donorname) %>% # no donor code in this file 
  unique() %>% 
  arrange(donorname) %>% 
  write_csv(file = "data/list_donors_full.csv")


names(df_crs)

df_crs_imf <- df_crs %>% 
  filter(donorname == "IMF (Concessional Trust Funds)") 

# only 777 obs
df_crs_imf %>% select(projecttitle) %>% unique 
# only 6 unique titles, most debt forgiveness programme
# the Concessional trust fund is not really IMF

rm(list = ls())
gc()
