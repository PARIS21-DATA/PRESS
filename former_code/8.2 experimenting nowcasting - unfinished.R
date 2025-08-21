df_crs <- df_merged %>% 
  filter(source == "CRS")


df_crs %>% select(ExpectedEndDate) %>% head
df_crs %>% select(ExpectedEndDate) 

substr(df_crs$ExpectedEndDate, 1, 4) %>% table
df_crs %>% .$ExpectedEndDate %>% as.Date('%Y-%m-%d') %>% head(100)



df_crs$endyear <- df_crs %>% .$ExpectedEndDate %>% as.Date('%Y-%m-%d') %>% year 


df_crs %>% arrange(ExpectedEndDate) %>% select(ExpectedEndDate, CommitmentDate, reportedyear) %>% filter(ExpectedEndDate != "") %>% unique
df_crs <- df_crs %>% 
  mutate(endyear = ifelse((!is.na(endyear) )&endyear < 2000, NA, endyear))%>% 
  mutate(endyear = ifelse((!is.na(endyear) )&endyear > 2030, NA, endyear))



