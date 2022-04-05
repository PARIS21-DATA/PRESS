df_crs <- df_crs %>%
  select(process_id, setdiff(names(df_crs),names(df_crs_raw)))