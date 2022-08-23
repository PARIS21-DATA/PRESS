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

save(dict_tf_idf, common_words, myDict, file = "data/Intermediate/crs04.2_dicts_utf8_full.rdata")
