rm(list = ls())
crs_path <- "./Data/intermediate/crs03.rds"
crs_path_new <- "./Data/intermediate/crs03_1.rds"
df_crs <- readRDS(crs_path)
df_crs_original <- df_crs

df_crs <- df_crs_original %>%
  select(text_id, desc_2mine, stats_filter = text_detection_wo_mining_w_scb)


df_crs_descs = df_crs  %>%
  select(-stats_filter) %>%
  filter(!duplicated(text_id)) 

df_crs_descs = df_crs_descs %>%
  mutate(ref = 1:nrow(df_crs_descs))

desc_splited =  df_crs_descs %>%
  .$desc_2mine %>%
  str_split( pattern = "/|[.]", simplify = T) %>%
  as.data.frame() %>%
  mutate(ref = 1:nrow(df_crs_descs))


desc_splited = desc_splited %>%
  gather(key = number, value = text, -ref) %>%
  select(-number) %>%
  mutate(text = trimws(text)) %>%
  filter(!is.na(text))

df_crs_descs <- df_crs_descs %>%
  select(-desc_2mine) %>%
  left_join(desc_splited) %>%
  select(-ref)

df_crs <- df_crs %>%
  select(text_id, stats_filter) %>%
  unique %>%
  left_join(df_crs_descs) 

# a = df_crs %>% select(text_id, stats_filter) %>% unique %>% nrow
# b = df_crs %>% select(text_id) %>% unique %>% nrow
# 
# print(paste0("There are ", a-b, " projects with same names but different purpose code"))

saveRDS(df_crs, file=crs_path_new)

beepr::beep(3)

rm(list = ls())
