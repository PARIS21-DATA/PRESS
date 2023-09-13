rm(list = ls())
source("code/00. boot.R")
source("code/00.1 functions.R")

# reading in the data from the survey 
####################################
## --- PRESS Data Preparation --- ##
####################################

# 0 Input::

path_input <- paste0("Data/Intermediate/06.2 crs and press with region and country code_", 
                     year(Sys.Date())
                     ,".Rdata")


path_donors_crs <- "data/auxiliary/reporters_crs_2022.rds"
path_donors_crs_output <- "data/auxiliary/reporters_crs_2023.rds"

# 1 load data::
load(path_input)
df_reporters_crs <- read_rds(file = path_donors_crs)

# 2 work on CRS data:
names(df_reporters_crs)
df_reporters_crs_4merge <- df_reporters_crs %>% 
  select(donorname = crs_name_en, 
         donorcode = crs_code, 
         ch_name, 
         ReporterId = ReporterId_chlist) %>% 
  unique
# test if we can get same number of rows
df_crs_new <- df_crs %>% 
  inner_join(df_reporters_crs_4merge)

df_crs %>% 
  left_join(df_reporters_crs_4merge) %>% 
  filter(is.na(ch_name)) %>% 
  select(donorname, donorcode) %>% 
  distinct 

write.xlsx(df_reporters_crs, file = "data/auxiliary/reporters_d4d_2023.xlsx")

df_reporters_new <- read.xlsx("data/auxiliary/reporters_d4d_2023_fixed.xlsx")

df_reporters_crs <- rbind(df_reporters_crs, df_reporters_new)

saveRDS(df_reporters_crs, file = path_donors_crs_output)
