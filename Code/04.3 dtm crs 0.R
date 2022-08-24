rm(list = ls())
source("code/00. boot.R")
pkgload:::unload("tidytext")
myDict <- read_rds("data/Intermediate/crs04.2_mydict_utf8_full.rds")

source("code/00.3 functions_yu.R")
source("code/00.2 functions_thilo.R")

# # inspect(corpus_crs_0)
nwords0 <- tidytext::tidy(corpus_crs_0) %>%
  select(text, document = id) %>%
  mutate(total = str_count(string = text, pattern = "\\S+") ) %>%
  select(-text)
write_rds(nwords0, file = "data/Intermediate/crs04.3_nwords0_utf8_full.rds")
print_time_diff(start)
# Time difference of 1049.166 secs


start <- Sys.time()
dtm_crs_0 <- DTM(corpus_crs_0, dict=myDict)
print_time_diff(start)
write_rds(dtm_crs_0, file = "Data/Intermediate/crs04.3_dtm_crs_0_utf8_full.rds")
# Time difference of 446.3028 secs



