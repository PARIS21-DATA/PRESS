### ---------------
# start data cleaning
###


### ----
# step 0: setting parameters
###
rm(list = ls())
gc()
source("code/00. boot.R")
print_time_diff <- function(start_time) {
  difftime(Sys.time(),start_time, units = "sec") %>% print
}

# job_specific_suffix <- ""
job_specific_suffix <- "_full_"
start <- Sys.time()
path_input <- paste0("./Data/Raw/CRS/crs", job_specific_suffix, year(Sys.Date()),".feather")
path_intermediate <- paste0("./Data/Intermediate/crs", "01_1a" ,
                            job_specific_suffix, year(Sys.Date()),
                            ".feather")
path_output <- paste0("./Data/Intermediate/crs", "01_1" ,
                       job_specific_suffix, year(Sys.Date()),
                       ".feather")


### --- 
# step 1: load the rds file 
# the rds file can be very large 
### 
df_crs_raw <- read_feather(path_input)


print_time_diff(start)
beep()
# Time difference of 57.99225 secs
# MAC: 33.78785 secs


### --- 
# step 2: create db_refs
# Taking longer time than the previous step 
# this may be skippable at the early stages of the analysis 
start <- Sys.time()
df_crs <- df_crs_raw %>%
  select(
    process_id,
    projectnumber,
    # projecttitle,
    # shortdescription,
    # longdescription,
    crsid,
    # donorcode,
    year,
    # usd_commitment,
    purposecode,
    usd_disbursement
    ,recipientcode
    # ,usd_received
    # , sdgfocus:pba
    # , grantequiv
    # ,finance_t
    # ,aid_t
    # , bi_multi
    # ,currencycode
  ) 
names(df_crs)
df_crs %>% 
  select(-process_id) %>%
  # select(-text_identifier) %>% 
  # filter(duplicated()) %>% 
  distinct() %>% 
  nrow

df_crs <- df_crs %>% 
  mutate(text_identifier = paste(projectnumber,
                                 crsid, 
                                 year, 
                                 purposecode, 
                                 usd_disbursement, 
                                 recipientcode, 
                                 # as.character(year),
                                 # as.character(purposecode), 
                                 # as.character(usd_disbursement),
                                 # as.character(recipientcode), 
                                 sep = "___"))
# df_crs$projectnumber %>% head
# df_crs_raw$crsid %>% head 
# df_crs_raw %>% filter(is.na(crsid)| crsid =="") %>% nrow
# head(df_crs$text_identifier)

df_crs$text_identifier %>% unique %>% length
df_crs %>% select(text_identifier) %>% distinct %>% nrow

# rm(df_crs_raw)
gc()
print_time_diff(start)

# write_feather(df_crs, path_intermediate)
nchar(df_crs$text_identifier) %>% max


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


ls_hashvalues <- list()
start <- Sys.time()
tmp_n_subsets <- ceiling(nrow(df_crs)/10000) 
for (i in 1:tmp_n_subsets) {
  vec_subset <- df_crs %>% 
    slice(((i-1)*10000+1):(i*10000)) %>% 
    .$text_identifier
  
  ls_hashvalues[[i]] <-vec_subset %>% 
    lapply(function(x) digest(x, algo = "sha1")) %>% unlist
  rm(vec_subset)
  if(i%%20 == 0) print(i)
  print_time_diff(start)
}
rm(tmp_n_subsets)
rm(i)
print_time_diff(start)
beepr::beep()

hash_id <- unlist(ls_hashvalues)
length(hash_id)
length(unique(hash_id))
# hash_longdesc %>% unique %>% length

df_crs$hash_id <- hash_id
df_crs$db_ref <- paste0("df_crs_",df_crs$hash_id) 

rm(hash_id)

df_crs <- df_crs %>% 
  mutate(db_ref = ifelse(duplicated(hash_id), 
                         paste0(db_ref, "_dup_", process_id), 
                         db_ref)) 

df_crs <- df_crs %>% 
  mutate(db_ref = as.factor(db_ref))

# df_crs_raw <- read_feather(path_input)

df_crs <- df_crs %>%
  select(db_ref, process_id, hash_id) %>%
  inner_join(df_crs_raw)
rm(df_crs_raw)
gc()
print_time_diff(start)
# Time difference of 42.49567 secs

write_feather(df_crs, path_output)
print_time_diff(start)
beep(2)
# Time difference of 141.4641 secs

