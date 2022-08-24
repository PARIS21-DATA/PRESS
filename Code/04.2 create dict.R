rm(list = ls())
source("code/00. boot.R")
source("code/00.2 functions_thilo.R")
source("code/00.3 functions_yu.R")
pkgload:::unload("tidytext") # the stemmer in tidy text might be problematic for our steps here. 
job_specific_suffix <- "_utf8_full"
load("data/intermediate/crs04_lang_utf8_full.rdata")

crs_path <- paste0("./Data/intermediate/crs04.1_freq10_eligibleWords_", lang, job_specific_suffix, ".rdata")
crs_path_new_rdata <- paste0("./Data/intermediate/crs04.2_dicts_", lang ,job_specific_suffix, ".rdata")
crs_path_new_rds <- paste0("./Data/intermediate/crs04.2_mydict_", lang, job_specific_suffix, ".rds")

load(crs_path)

### filter standards
# freq_1 cut off

cutoff_freq1 = 0.0005 # is there a median we can take?
cutoff_odds = 10^(-1.5)

start <- Sys.time()
dict_tf_idf <- freq_all_1_0 %>% 
  filter(term %in% eligible_words_in_doc_1) %>% 
  filter(#freq.y > cutoff_freq1, # the tf-idf keywords will be limited by the frequencies
    odds < cutoff_odds) %>%
  # slice(1:100) %>% 
  .$term
print_time_diff(start)
freq_all_1_0 %>%
  filter(term %in% dict_tf_idf) %>%
  mutate(terml =str_count(string = term, pattern = "\\S+") ) %>% 
  filter(terml==1) %>%
  data.frame() 
print_time_diff(start)

# single_keywords_2rm = freq_all_1_0 %>%
#   filter(term %in% dict_tf_idf) %>%
#   mutate(terml =str_count(string = term, pattern = "\\S+") ) %>% 
#   filter(terml==1) %>%
#   data.frame() %>%
#   .$term
# 
# dict_tf_idf_reduced <- dict_tf_idf[!dict_tf_idf%in% single_keywords_2rm] 
# saveRDS(dict_tf_idf, file = "Data/Intermediate/tf_idf_keywords.rds")
# find a cut-off rate
freq_all_1_0 %>%
  filter(freq.y > cutoff_freq1) %>%
  mutate(log_odds = log10(odds)) %>%
  .$log_odds %>%
  hist()

common_words <- freq_all_1_0 %>% 
  # filter(freq.y > cutoff_freq1) %>% 
  filter(odds > 10, odds < 5) %>%
  .$term
print_time_diff(start)

myDict <- dtm_crs_1$dimnames$Terms 
dict_tf_idf %in% myDict
dict_tf_idf %in% common_words
myDict <- unique(c(myDict, dict_tf_idf))
myDict <- myDict[!(myDict %in% common_words)]
myDict <- unique(dict_tf_idf)

freq_all_1_0 %>% 
  filter(term %in% myDict) %>%
  print

# freq0 = DTM(corpus_crs_0, dict =myDict) %>%
#   tidy %>%
#   group_by(document) %>%
#   summarise(count = sum(count))

save(dict_tf_idf, common_words, myDict, file = crs_path_new_rdata)

write_rds(myDict,file = crs_path_new_rds)
