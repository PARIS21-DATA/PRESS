which(!duplicated(select(df_crs, -process_id)) )
df_dup1 <- df_crs[which(duplicated(select(df_crs, -process_id)) ),]
tmp_crs <- df_crs[nrow(df_crs):1,]
head(tmp_crs$process_id)
head(df_crs$process_id)
df_dup2 <- tmp_crs[which(duplicated(select(tmp_crs, -process_id)) ),]
rm(tmp_crs)
df_dup <- rbind(df_dup1, df_dup2) %>% 
  filter(!duplicated(process_id))
rm(df_dup1, df_dup2)

df_dup %>% 
  select(-process_id) %>%
  distinct() %>% 
  nrow

df_dup <- df_crs_raw %>% 
  filter(process_id %in% df_dup$process_id)

df_dup %>% 
  select(-process_id) %>%
  distinct() %>% 
  nrow

df_dup <- df_dup %>% 
  arrange(-process_id) 
df_dup <- df_dup %>% 
  mutate(dup = duplicated(df_dup %>% select(-process_id)))

df_dup %>% write.xlsx("Data/Intermediate/01.2 duplicated crs_2fix_2023.xlsx")
# problem with db_ref to resolve
df_crs$db_ref <- c(1:nrow(df_crs))