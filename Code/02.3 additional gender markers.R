
### ---------------
# 0. start loading data an set parameters
###
rm(list = ls())
source("Code/00. boot.R")
gc()
Sys.sleep(10)

print_time_diff <- function(start_time) {
  difftime(Sys.time(),start_time, units = "sec") %>% print
}
source <- "crs"
skip_icov <- T
# job_specific_suffix <- ""
job_specific_suffix <- "_full_"
path_input_crs <- paste0("./Data/intermediate/crs01_1", job_specific_suffix,year(Sys.Date()),  ".feather")
# path_intermediate_crs_after_cleaning <- paste0("./Data/intermediate/crs02_int_clean_titles", job_specific_suffix,year(Sys.Date()),  ".feather")
path_output <- paste0("./Data/intermediate/crs02.3 additional gender markers", job_specific_suffix,year(Sys.Date()), ".feather")

path_input_sdg_markers <- paste0("data/intermediate/02.2b sdg markers UNIQUE goals ", 
                                 year(Sys.Date()), 
                                 ".feather")
start <- Sys.time()


## load the data file
print("Loading document:")
# df_crs_raw <- df_crs
# rm(df_crs)
df_crs_raw <- read_feather(path_input_crs)
print_time_diff(start)



### ---------------
# 1. select the required columns
###

df_crs <- df_crs_raw %>% 
  select(db_ref
         , process_id
         , channelcode
         , agencycode
         , donorcode
         # , finance_t
         # , bi_multi
         # , commitmentdate
  ) 



### ---------------
# 2. prepare the filter data 
###
df_sdg <- read_feather(path_input_sdg_markers)
names(df_sdg)
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
  select(donorcode, agencycode
         # , agencyname = agencynamee
  )  %>% 
  mutate(gen_agency = T)

df_gender_channels <- df_gender_channels %>% 
  filter(gender == 1) %>% 
  select(channelcode= channel.id) %>% 
  mutate(gen_channel = T)

### ---------------
# 3. merge
###

df_crs <- df_crs %>% 
  left_join(df_gender_agencies) %>% 
  left_join(df_gender_channels) %>% 
  left_join(df_sdg)

rm(df_gender_agencies, df_gender_channels, 
   df_sdg)


names(df_crs)

### ---------------
# 4. save
###

df_crs <- df_crs %>% 
  select(#-process_id,
         -channelcode, 
         -agencycode, 
         -donorcode, 
         )


df_crs_gender <- df_crs_gender %>% 
  select(-db_ref)

df_crs_gender <- df_crs_gender %>% 
  mutate(across(gen_agency:gen_sdg, ~ replace_na(.x, F)))

df_crs %>% write_feather(path_output)
