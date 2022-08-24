rm(list = ls())

df_lang <- data.frame(lang = c(#"en",
                               "fr","es", "de"), 
                      language = c(#"english", 
                                   "french", "spanish", "german"), 
                      stringsAsFactors = F)
write_rds(df_lang, file = "data/intermediate/crs04_df_lang_utf8_full.rds")
i = 1
i <- write_rds(i,"data/Intermediate/crs04_i_utf8_full.rds")
start <- Sys.time()
write_rds(start, file = "data/intermediate/crs04_start_time_utf8_full.rds")
for (i in 1:nrow(df_lang)) {
  write_rds(i, file = "data/Intermediate/crs04_i_utf8_full.rds")
  df_lang <- read_rds("data/intermediate/crs04_df_lang_utf8_full.rds")
  lang <- df_lang$lang[i]
  language <- df_lang$language[i]
  print(lang)
  save(lang, language, file = "data/intermediate/crs04_lang_utf8_full.rdata")
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
  i <- read_rds("data/Intermediate/crs04_i_utf8_full.rds")
  print(i)
}
beep()
source("code/00. boot.R")
start <- read_rds("data/intermediate/crs04_start_time_utf8_full.rds")
print_time_diff(start)
