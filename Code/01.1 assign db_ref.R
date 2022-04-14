### ---------------
# start data cleaning 
### 
rm(list = ls())
crs_path <- "./Data/Raw/CRS/crs_sample.rds"
crs_path_new <- paste0("./Data/Intermediate/crs", "01_1" , ".rds")
df_crs_raw <- readRDS(crs_path)

# Subset necessary information
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

# Create unique idetifier db_ref for each project transforming a joined sting of 
# all project information first into factor and then into numeric
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

# Test uniqueness of the project identifier db_ref
source("code/01.2 check uniqueness of db_ref.r")

# Transform identifiers in format "df_source_#####" 
df_crs$db_ref = paste0("df_", df_crs$source, df_crs$db_ref) %>% as.factor

# Add project identifiers to raw data set
df_crs <- df_crs %>%
  select(db_ref, process_id) %>%
  inner_join(df_crs_raw, by = "process_id")
rm(df_crs_raw)
gc()

saveRDS(df_crs, file  = crs_path_new)
