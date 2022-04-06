rm(list = ls())
crs_path <- "./Data/intermediate/crs03.rds"
crs_path_new <- "./Data/intermediate/crs03_1.rds"
df_crs <- readRDS(crs_path)


nc <- c("en","fr","es")

df_crs = df_crs %>%
  mutate(language = ifelse(language %in% nc, language, "other"))
# rm(language,nc)
# table(df_crs$language)
# save(df_crs, file="crs_2019_clean2.RData")

################


#Output::
save(df_crs, file="./analysis/df_crs_2021_clean3_notCoverted2ascii.RDs")

# load("./analysis/crs_2021_clean3_notCoverted2ascii.RDs")
df_crs$description_comb = iconv(df_crs$description, 'utf-8', 'ascii', sub=' ')
save(df_crs, file="./analysis/df_crs_2021_clean3_Coverted2ascii.RDs")
df_crs <- cSplit(df_crs, "description_comb", ".", "long")
df_crs <- cSplit(df_crs, "description_comb", " / ", "long")

df_crs$description_comb = tolower(df_crs$description_comb)
head(df_crs$description_comb)
# crs$description_comb = iconv(crs$description, 'utf-8', 'ascii', sub=' ')
# save(crs, file="./analysis/crs_2021_clean3_Coverted2ascii.RDs")
# 
# 
# gc()
# load("./analysis/crs_2021_clean3_Coverted2ascii.RDs")

# load("./analysis/crs_2021_clean3_notCoverted2ascii.RDs")

# crs <- cSplit(crs, "description_comb", ".", "long")
# crs <- cSplit(crs, "description_comb", " / ", "long")



# ?cSplit
save(df_crs, file="./analysis/crs_2021_clean2.9_converted_splitted.RDS")

load("./analysis/crs_2021_clean2.9_converted_splitted.RDS")

df_crs = df_crs %>% select(-stats) %>% join(df_crs_stats)
projectIDs = df_crs_stats %>% filter(stats) %>% .$projectID 


save(df_crs, file="./analysis/crs_2021_clean3_converted_splitted.RDS")
save(projectIDs, file = "analysis/projectIDs_by_title.rds")
# head(crs)

beepr::beep(3)

rm(list = ls())
gc()
