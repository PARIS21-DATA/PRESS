## !!! this section below will be moved to a much earlier stage
df_descriptions <- df_crs_01_filtered %>% 
  # filter(!is.na(longdescription), 
  #        longdescription!="") %>% 
  select(db_ref, longdescription) 

print("before hashing values")
print_time_diff(start)

print_time_diff(start)


hash_longdesc <- df_descriptions$longdescription %>% 
  lapply(function(x) digest(x, algo = "sha256"))
beepr::beep()


hash_longdesc <- unlist(hash_longdesc)
length(hash_longdesc)
hash_longdesc %>% unique %>% length

df_descriptions$hash_longdesc <- hash_longdesc

df_descriptions$hash_longdesc %>% unique %>% length

df_descriptions <- df_descriptions %>% 
  mutate(hash_longdesc_num = as.numeric(as.factor(hash_longdesc)))

df_crs_01_filtered <- df_descriptions %>% 
  select(db_ref, hash_longdesc, hash_longdesc_num) %>% 
  inner_join(df_crs_01_filtered)

# df_crs_01_filtered <- df_crs_01_filtered %>% 
#   mutate(year = as.numeric(year)) %>% 
#   filter(!is.na(year))

rm(df_descriptions)
rm(hash_longdesc)