rm(list = ls())
source("code/00. boot.R")

## setting up the job type
# job <- "gen"
job <- "stat"
write_rds(job, file ="data/intermediate/crs04_job_utf8_full.rds" )

## setting up and save languages
df_lang <- data.frame(lang = c("en",
                               "fr",
                               "es", "de"), 
                      language = c("english", 
                                   "french", 
                                   "spanish", "german"), 
                      stringsAsFactors = F)
write_rds(df_lang, file = "data/intermediate/crs04_df_lang_utf8_full.rds")

# i = 1
# i <- write_rds(i,"data/Intermediate/crs04_i_utf8_full.rds")

## Tracking total processing time. 
# start <- Sys.time()
# write_rds(start, file = "data/intermediate/crs04_start_time_utf8_full.rds")



for (i in 2:nrow(df_lang)) {
  # save sequence number
  write_rds(i, file = "data/Intermediate/crs04_i_utf8_full.rds")
  # save language 
  df_lang <- read_rds("data/intermediate/crs04_df_lang_utf8_full.rds")
  lang <- df_lang$lang[i]
  language <- df_lang$language[i]
  print(lang)
  save(lang, language, file = "data/intermediate/crs04_lang_utf8_full.rdata")
  
  # running the analysis
  source("code/04.0 separate data frame into two.R")
  print(0)
  source("code/04.1 preprocess and create dict.R")
  print(1)
  source("code/04.2 create dict.R")
  print(2)
  source("code/04.3 dtm crs 0.R")
  print(3)
  source("code/04.4 identify projects.R")
  print(4)
  
  # extracting the sequence number
  i <- read_rds("data/Intermediate/crs04_i_utf8_full.rds")
  print(i)
}
beep(3)


df_lang <- read_rds("data/intermediate/crs04_df_lang_utf8_full.rds")
job <- read_rds("data/Intermediate/crs04_job_utf8_full.rds")
job_specific_suffix <- "_full_"
if(job == "gen") job_specific_suffix <- "_gen_full_"
list_path_ids <- paste0("./Data/intermediate/crs04.4_positive_id_",c(df_lang$lang,"en") , job_specific_suffix, year(Sys.Date()), ".rds") %>% unique

list_id <- c()
for (i in list_path_ids) {
  id_temp <- read_rds(i)
  print(length(id_temp))
  print(i)
  list_id <- c(list_id, id_temp)
}


list_id <- unique(list_id)

crs_path_new <- paste0("./Data/intermediate/crs04_positive_id", job_specific_suffix, year(Sys.Date()), ".rds")

write_rds(list_id, file = crs_path_new)

