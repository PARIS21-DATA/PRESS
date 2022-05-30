### ---------------
# start data cleaning
###


### ----
# step 0: setting parameters
###
rm(list = ls())
print_time_diff <- function(start_time) {
  difftime(Sys.time(),start_time, units = "sec") %>% print
}

job_specific_suffix <- ""
job_specific_suffix <- "_de"
start <- Sys.time()
crs_path <- paste0("./Data/Raw/CRS/crs", job_specific_suffix, ".rds")
crs_path_new <- paste0("./Data/Intermediate/crs", "01_1" ,job_specific_suffix,  ".rds")


### --- 
# step 1: load the rds file 
# the rds file can be very large 
### 
df_crs_raw <- readRDS(crs_path)
print_time_diff(start)
beep()
# Time difference of 57.99225 secs



### --- 
# step 2: create db_refs
# Taking longer time than the previous step 
# this may be skippable at the early stages of the analysis 
start <- Sys.time()
df_crs <- df_crs_raw %>%
  select(
    process_id,
    projecttitle,
    shortdescription,
    longdescription,
    crsid,
    donorcode,
    year ,
    usd_commitment,
    purposecode,
    usd_disbursement,
    recipientcode ,
    usd_received
  )

# problem with db_ref to resolve
df_crs$db_ref <- c(1:nrow(df_crs))

### need to recover this part later 
# df_crs$db_ref <- with(df_crs,
#                       paste(
#                         crsid,
#                         donorcode,
#                         year ,
#                         projecttitle,
#                         shortdescription,
#                         longdescription,
#                         usd_commitment,
#                         purposecode,
#                         usd_disbursement,
#                         recipientcode ,
#                         usd_received, # this is a new addition in 2022
#                         sep="_"
#                       )
# ) %>%
#   as.factor %>%
#   as.numeric
# print_time_diff(start)
# beep()
# 
# start <- Sys.time()
# source("code/01.2 check uniqueness of db_ref.r")
# print_time_diff(start)
# beep(2)
### end of recovery

# df_crs_backup = df_crs

df_crs$db_ref = paste0("df_", df_crs$source, df_crs$db_ref) %>% as.factor

df_crs <- df_crs %>%
  select(db_ref, process_id) %>%
  inner_join(df_crs_raw)
rm(df_crs_raw)
gc()
print_time_diff(start)
# Time difference of 39.46608 secs

saveRDS(df_crs, file  = crs_path_new)
print_time_diff(start)
beep(2)
# Time difference of 141.4641 secs

