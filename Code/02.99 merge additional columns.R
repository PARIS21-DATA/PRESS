### ---------------
# 0. start loading data an set parameters
###
rm(list = ls())
source("Code/00. boot.R")
gc()
# Sys.sleep(10)

print_time_diff <- function(start_time) {
  difftime(Sys.time(),start_time, units = "sec") %>% print
}
source <- "crs"
skip_icov <- T
# job_specific_suffix <- ""
job_specific_suffix <- "_full_"
path_input_markers <- paste0("./Data/intermediate/crs02.0", job_specific_suffix,year(Sys.Date()), ".feather")

path_input_wrangling <- paste0("./Data/intermediate/crs02.1 data wrangling", job_specific_suffix,year(Sys.Date()), ".feather")

path_input_gender <- paste0("./Data/intermediate/crs02.3 additional gender markers", job_specific_suffix,year(Sys.Date()), ".feather")

path_output <- paste0("./Data/intermediate/crs02 new columns", job_specific_suffix,year(Sys.Date()), ".feather")

# path_finance_type <- "data/auxiliary/finance types 2023.xlsx"
start <- Sys.time()


## load the data file
print("Loading document:")
# df_crs_raw <- df_crs
# rm(df_crs)
df_crs_marker <- read_feather(path_input_markers)
df_crs_wrangling <- read_feather(path_input_wrangling)
df_crs_gender <- read_feather(path_input_gender)
print_time_diff(start)

names(df_crs_gender)


### ---------------
# 1. merge
###
df_crs_marker[,names(df_crs_marker) %in% names(df_crs_wrangling)]
df_crs_wrangling[,names(df_crs_wrangling) %in% names(df_crs_marker)]

df_crs <- df_crs_marker %>% 
  select(-year) %>% 
  inner_join(df_crs_wrangling)  %>% 
  inner_join(df_crs_gender)

### ---------------
# 2. save
###
write_feather(df_crs, path_output)

