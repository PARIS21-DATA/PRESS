rm(list = ls())

df_lang <- data.frame(lang = c("en","fr","es"), 
                      language = c("en", "french", "spanish"), 
                      stringsAsFactors = F)
i = 1
for (i in 1:nrow(df_lang)) {
  write_rds(i, file = "data/Intermediate/crs04_i.rds")
  lang <- df_lang$lang[i]
  language <- df_lang$language[i]
  print(lang)
  save(lang, language, file = "data/intermediate/crs04_lang_utf8_full.rdata")
  source("code/04.0 separate data frame into two.R")
  source("code/04.1 preprocess and create dict.R")
  
  i <- read_rds("data/Intermediate/crs04_i.rds")
}
