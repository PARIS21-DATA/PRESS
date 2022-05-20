## ----------
## this script should be only run once every update
## ----------

rm(list = ls())
gc()

# Paths to raw data
crs_zip_folder <-  "./Data/Raw/CRS/zip"
crs_txt_folder <-  "./Data/Raw/CRS/txt"

# Extract .zip files that were prviously downloaded into txt folder
crs_zip_files <- paste(crs_zip_folder, list.files(crs_zip_folder), sep = "/")
lapply(crs_zip_files, unzip, overwrite = T, exdir = crs_txt_folder)
rm(crs_zip_folder, crs_zip_files)

# Add directory names to .txt files
crs_txt_files <- paste(crs_txt_folder, list.files(crs_txt_folder), sep = "/")

# Read crs data from .txt files and store each as an entry of list_crs
list_crs <- lapply( crs_txt_files, read.csv , sep = "|", header = T, stringsAsFactors = F, encoding = "utf-8" )
beep()

# df_crs = bind_rows(list_crs) # not successful
# crs_vars = lapply(list_crs, names)

# gc()
# start = Sys.time()
# if(exists("df_crs")) rm(df_crs)
# for (df in list_crs) {
#   if(!exists("df_crs")) df_crs = df else df_crs = rbind(df_crs, df)
# }
# difftime( Sys.time(),start, units = "sec")
# # Time difference of 269.2933 secs
# rm(df_crs, df)
# gc()

# Merge all crs from different years into one data frame
start <- Sys.time()
df_crs <-  rbind(list_crs[[1]], 
               list_crs[[2]], 
               list_crs[[3]], 
               list_crs[[4]], 
               list_crs[[5]], 
               list_crs[[6]], 
               list_crs[[7]], 
               list_crs[[8]], 
               list_crs[[9]], 
               list_crs[[10]], 
               list_crs[[11]], 
               list_crs[[12]], 
               list_crs[[13]], 
               list_crs[[14]] 
)
difftime(Sys.time(),start, units = "sec")
# Time difference of 86.15631 secs
beep(2)


# making basic changes
df_crs <- df_crs %>%
  mutate(source = "crs")
names(df_crs)<- tolower(names(df_crs))
row.names(df_crs) <- c()
df_crs$process_id <- row.names(df_crs)

saveRDS(df_crs, file  = "./Data/Raw/CRS/crs_full.rds")
beep(2)

# If full data available in Data/Raw/, uncomment to load 
df_crs <- readRDS("./Data/Raw/CRS/crs_full.rds")

df_crs_lang <- df_crs %>%
  select(projecttitle, longdescription) %>%
  mutate(projecttitle_lower = tolower(projecttitle), longdescription_lower = tolower(longdescription)) %>%
  mutate(description_comb = paste(projecttitle_lower, longdescription_lower, sep = ".")) %>%
  mutate(title_id = as.numeric(as.factor(description_comb))) %>% 
  filter(!duplicated(title_id)) %>%
  mutate(title_language = cld2::detect_language(projecttitle)) %>%
  mutate(long_language = cld2::detect_language(longdescription))

df_crs_es <- df_crs_lang %>%
  filter(title_language == "es" | long_language == "es")

df_crs_de <- df_crs_lang %>%
  filter(title_language == "de" | long_language == "de")

df_crs_fr <- df_crs_lang %>%
  filter(title_language == "fr" | long_language == "fr")


languages_full_crs <- as.data.frame(table(df_crs_lang$title_language)) %>%
  rename(language = Var1, title_language = Freq) %>% 
  left_join(as.data.frame(table(df_crs_lang$long_language)) %>% 
              rename(language = Var1, long_language = Freq), by = "language")
saveRDS(languages_full_crs, file = "./data/languages_full_crs.rds")

# Germany sample
crs_germany_sample <- readRDS("./Data/Raw/Crs/crs_germany_sample.rds")
crs_germany_sample <- crs_germany_sample %>%
  select(projecttitle, longdescription) %>%
  mutate(projecttitle_lower = tolower(projecttitle), longdescription_lower = tolower(longdescription)) %>%
  mutate(title_id = as.numeric(as.factor(paste(projecttitle_lower, longdescription_lower, sep = ".")))) %>%
  filter(!duplicated(title_id)) %>%
  mutate(title_language = cld2::detect_language(projecttitle)) %>%
  mutate(long_language = cld2::detect_language(longdescription))

languages_germany_crs <- as.data.frame(table(crs_germany_sample$title_language)) %>%
  rename(language = Var1, title_language = Freq) %>% 
  left_join(as.data.frame(table(crs_germany_sample$long_language)) %>% 
              rename(language = Var1, long_language = Freq), by = "language")

library(xlsx)
write.xlsx(languages_full_crs, file = "./Tmp/languages_full_crs.xlsx", row.names = F)
write.xlsx(languages_germany_crs, file = "./Tmp/languages_germany_crs.xlsx", row.names = F)

# Take a sample of the entire data frame for further testing 
df_crs_sample <- df_crs[sample(nrow(df_crs),nrow(df_crs)/10), ]
rm(df_crs)
df_crs <- df_crs_sample
saveRDS(df_crs, file  = "./Data/Raw/CRS/crs_sample.rds") 
beep()
rm(list = ls())
gc()
