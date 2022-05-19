df_crs_backup <-  readRDS("Data/Raw/CRS/crs_full.rds")

df_crs <- df_crs_backup %>%
  filter(donorname == "Germany")

saveRDS(df_crs, file = "Data/Intermediate/crs_germany_sample.rds")
