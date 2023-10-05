rm(list =ls())
source("code/00. boot.R")
start_time <- Sys.time()
# 1. set up parameters and constant variables


path_input_RDS <- paste0("output/press/2023-09-22 PRESS data with climate markers2023.RDS")

job_specific_suffix <- "_full_"
crs_path_input_feather <- paste0("data/intermediate/crs05.2", 
                               job_specific_suffix, 
                               year(Sys.Date()), 
                               ".feather")

df_crs_raw <- read_feather(crs_path_input_feather )
df_crs <- readRDS(path_input_RDS)
df_crs_raw <- df_crs_01_filtered
rm(df_crs_01_filtered)


# 2. ICF problems in PRESS
df_crs %>% 
  filter(grepl("icf", 
               projectnumber, 
               T)) %>% 
  select(projecttitle, ch_name)

df_crs_raw %>% 
  filter(donorname == "United Kingdom") %>% 
  filter(grepl("icf", 
               projectnumber, 
               T)) %>% 
  select(projecttitle,longdescription,  donorname) %>% 
  distinct %>% 
  # filter(grepl("data|survey|info",
  #              projecttitle,
  #              T))
  filter(grepl("data|survey|informa|statist|census",
               longdescription,
               T))

# there is no ICF projects

# 3. "business climate"
df_crs %>% 
  filter(!grepl("climate",
               projecttitle,
               T)) %>%
  filter(climate_title) %>% 
  select(projecttitle,
         # longdescription, 
         dac_donorname) %>% 
  distinct  %>% 
  as.data.frame



df_crs %>% 
  filter(projecttitle == "Survey of the population climate in the Republic of Moldova") %>% 
  filter(climate_title) %>% 
  select(# projecttitle,
         longdescription,
         dac_donorname) %>% 
  as.data.frame


df_crs %>% 
  mutate(climate_blacklist_title = ifelse(is.na(climate_blacklist_title), 
                                          F, 
                                          str_detect(projecttitle, 
                                                     regex("population climate|business climate", ignore_case = TRUE))))

df_crs %>% 
  filter(climate_blacklist_title) %>% 
  select(projecttitle, ch_name)

df_crs %>% 
  filter(ch_name == "The World Bank") %>% 
  select(dac_donorcode, dac_donorname) %>% 
  distinct()


# 4. questionable projects like FAO ones

df_crs %>% 
  filter(climate) %>% 
  filter(identified_by_climate == "title") %>% 
  filter(grepl("food", ch_name, T)) %>% 
  select(projecttitle) %>% 
  distinct %>% 
  as.data.frame


df_crs %>% 
  filter(climate) %>% 
  filter(grepl("paragu", projecttitle, T)) %>%
  select(projecttitle) %>% 
  distinct %>% 
  as.data.frame


# 5. examine difference of outputs  with the previous versions
df_crs %>% 
  filter(!climate, 
         climate_agency_2nd, 
         climate_channel_2nd, 
         climate_donor_2nd) 


df_crs_old <- read_rds("output/press/2023-09-22 PRESS data with climate markers2023.RDS")
df_crs_old %>% 
  select(identified_by_climate) %>% 
  table


df_crs %>% 
  select(identified_by_climate) %>% 
  table

df_crs %>% 
  filter(!(db_ref %in% filter(df_crs_old, identified_by_climate == "title")$db_ref) ) %>% 
  filter(identified_by_climate == "title") %>% 
  select(projecttitle) %>% 
  distinct %>% 
  as.data.frame



# 6. world bank 
# df_crs_raw %>% 

df_crs2021 <- read.csv("~/downloads/CRS 2020 data.txt", 
                       sep = "|",
                       header = T,
                       stringsAsFactors = F
                       , 
                       fileEncoding = "UTF-16")

names(df_crs2021) <- tolower(names(df_crs2021))
df_crs2021 %>% 
  filter(donorcode %in% c(901,905)) %>%
  # select(climatemitigation)  %>% 
  select(climateadaptation, climatemitigation)  %>%
  distinct


df_crs2021 %>% 
  filter(donorcode %in% c(901,905)) %>% 
  # select(climatemitigation)  %>% 
  select(climateadaptation)  %>%
  distinct

  