### ---------------
# start data cleaning 
### 
rm(list = ls())
source <- "crs"
crs_path <- "./Data/intermediate/crs01_1.rds"
crs_path_new <- "./Data/intermediate/crs02.rds"
df_crs_raw <- readRDS(crs_path)
rm(crs_path)


# every step, we try to use a subset of the data to make the process quicker
df_crs <- df_crs_raw %>%
  select(
    process_id, 
    projecttitle, 
    shortdescription, 
    longdescription, 
    purposecode
  )


df_crs$description_comb <- with(df_crs, 
                                paste(
                                  projecttitle, 
                                  shortdescription, 
                                  longdescription, 
                                  sep=". ")
                                )
# projectID is used to identify text features, not actually giving project IDs
df_crs$text_id <- as.numeric(as.factor(df_crs$description_comb))


## add SCB identifier

df_crs$scb <- ifelse(df_crs$purposecode==16062,1,0)
df_crs$pop <- ifelse(df_crs$purposecode==13010,1,0)

df_crs <-  df_crs %>%
  mutate(gen_ppcode = ifelse(purposecode %in% c(15170:15180), 1, 0))


df_crs <- inner_join(df_crs, df_crs_raw)

#Output::

saveRDS(df_crs, file=crs_path_new)


