### ---------------
# start data cleaning 
### 
rm(list = ls())
crs_path <- "./Data/Raw/CRS/crs_sample.rds"
crs_path_new <- paste0("./Data/Intermediate/crs", "01_1" , ".rds")
df_crs_raw <- readRDS(crs_path)


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


df_crs$db_ref <- with(df_crs, 
                      paste(
                        crsid,
                        donorcode, 
                        year ,
                        projecttitle,
                        shortdescription, 
                        longdescription,
                        usd_commitment, 
                        purposecode, 
                        usd_disbursement, 
                        recipientcode , 
                        usd_received, # this is a new addition in 2022
                        sep="_"
                      )
) %>%
  as.factor %>%
  as.numeric

source("code/01.2 check uniqueness of db_ref.r")

df_crs$db_ref = paste0("df_", source, df_crs$db_ref) %>% as.factor

df_crs <- df_crs %>%
  select(db_ref, process_id) %>%
  inner_join(df_crs_raw)
rm(df_crs_raw)
gc()

saveRDS(df_crs, file  = crs_path_new)
