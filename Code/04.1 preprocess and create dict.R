
detach(tidytext) # the stemmer in tidy text might be problematic for our steps here. 
start <- Sys.time()
Min.1 <- 0.1 ## only consider words that are in more than 10% of statistical projects
Min.0 <- 0.1

corpus_crs_1 <- preprocessingV(df_crs_1$description, language=language)
print_time_diff(start)

dtm_crs_1 <- DTM(corpus_crs_1, Min=Min.1, Max=1)
print_time_diff(start)
corpus_crs_1_simpleDTM <- DTM(corpus_crs_1)
print_time_diff(start)

freq_all1 <- corpus_crs_1_simpleDTM %>% 
  tidytext::tidy() %>%
  group_by(term) %>%
  summarise(cnt = sum(count)) %>%
  ungroup() %>%
  mutate(total = sum(cnt)) %>%
  mutate(freq = cnt / total)
print_time_diff(start)

eligible_words_in_doc_1 <- corpus_crs_1_simpleDTM %>% 
  tidytext::tidy() %>%
  select(document, term) %>%
  # mutate(count = 1) %>% 
  group_by(term) %>% 
  summarise(cnt = n()) %>%
  arrange(desc(cnt)) %>%
  filter(cnt > (length(corpus_crs_1)/100)) %>% # filter by frequency
  .$term
print_time_diff(start)

save(eligible_words_in_doc_1,
       corpus_crs_1,
       dtm_crs_1,
       corpus_crs_1_simpleDTM,
       freq_all1,
     file = "./Data/intermediate/crs04.1_crs1_utf8_full.rdata")

# myDict = unique(c(myDict, dict_lang))

## (3.c. english language only)
# save(dtm_crs_1, file = "dtm_crs_1_en_2021.Rds")

start <- Sys.time()
## create a VCorpus of text
## very time consuming
corpus_crs_0 <- preprocessingV(df_crs_0$description, language=language)
print_time_diff(start)
# Time difference of 1360.409 secs

## Create a DTM of the text corpus
## very time consuming
corpus_crs_0_simpleDTM <- DTM(corpus_crs_0)
print_time_diff(start)

## not time consuming 
freq_all0 <- corpus_crs_0_simpleDTM %>% 
  tidytext::tidy() %>%
  group_by(term) %>%
  summarise(cnt = sum(count)) %>%
  ungroup() %>%
  mutate(total = sum(cnt)) %>%
  mutate(freq = cnt / total)
# beepr::beep(2)


## not time consuming
freq_all_1_0 = right_join(freq_all0, freq_all1, by = "term") %>%
  mutate(odds = freq.x/freq.y) %>%
  arrange(desc(odds), desc(freq.y))

save(# eligible_words_in_doc_0,
     corpus_crs_1,
     # dtm_crs_1,
     corpus_crs_1_simpleDTM,
     freq_all0,
     freq_all_1_0, 
     file = "./Data/intermediate/crs04.1_crs0_utf8_full.rdata")
print_time_diff(start)


beep()

