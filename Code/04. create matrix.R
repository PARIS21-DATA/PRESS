rm(list = ls())
print_time_diff <- function(start_time) {
  difftime(Sys.time(),start_time, units = "sec") %>% print
}

job_specific_suffix <- "_utf8_full"
start <- Sys.time()

crs_path <- paste0("./Data/intermediate/crs03", job_specific_suffix, ".rds")
# crs_path <- paste0("./Data/intermediate/crs03_1", job_specific_suffix, ".rds")
crs_path_new <- paste0("./Data/intermediate/positive_text_id", job_specific_suffix, ".rds")
start <- Sys.time()

lang <-  "en"
language <- "english"
df_crs_original <- read_rds(crs_path)
print_time_diff(start)

df_crs <- df_crs_original


df_crs <- df_crs %>% 
  rename(stats_filter = text_detection_wo_mining_w_scb)

source("code/00.2 functions_thilo.R")
source("code/00.3 functions_yu.R")

# enthreshold = 30
# frthreshold = 5
# esthreshold = 1

## Whitelist is no longer being used
# Create the white list 
# dict_lang <- wlistV(lang) %>% as.character()


df_crs_1 <- df_crs %>%
  filter( stats_filter 
          # , language==lang
          )

df_crs_0 <- df_crs %>%
  filter( !stats_filter 
          # , language==lang
          )
# which(df_crs_0$text_id %in% df_crs_1$text_id)


## 3.b. merge back by projectID to reverse previous split of project by language
df_crs_1 <- df_crs_1 %>% 
  filter(!duplicated(text_id)) %>%
  select(description = desc_2mine, text_id)
  # group_by(text_id) %>% 
  # summarise(description = desc_2mine) %>%
  # data.frame

df_crs_0 <- df_crs_0 %>% 
  filter(!duplicated(text_id))%>%
  select(description = desc_2mine, text_id)
  # group_by(text_id) %>% 
  # summarise(description = paste(text, collapse=". ")) %>%
  # data.frame
# 
# Warning messages:
#   1: In get(object, envir = currentEnv, inherits = TRUE) :
#   restarting interrupted promise evaluation
# 2: In doTryCatch(return(expr), name, parentenv, handler) :
#   restarting interrupted promise evaluation



## 3.c. Pre-process documents
## es=2, fr=5, en=40
# Min.1 <- enthreshold/nrow(df_crs_1) ## only consider words that are in >5 SCB projects
# Min.1 <- Min.2
Min.1 <- 0.1 ## only consider words that are in more than 10% of statistical projects
Min.0 <- 0.1

corpus_crs_1 <- preprocessingV(df_crs_1$description, language=language)
dtm_crs_1 <- DTM(corpus_crs_1, Min=Min.1, Max=1)

corpus_crs_1_simpleDTM <- DTM(corpus_crs_1)

freq_all1 <- corpus_crs_1_simpleDTM %>% 
  tidy() %>%
  group_by(term) %>%
  summarise(cnt = sum(count)) %>%
  ungroup() %>%
  mutate(total = sum(cnt)) %>%
  mutate(freq = cnt / total)

eligible_words_in_doc_1 <- corpus_crs_1_simpleDTM %>% 
  tidy() %>%
  select(document, term) %>%
  # mutate(count = 1) %>% 
  group_by(term) %>% 
  summarise(cnt = n()) %>%
  arrange(desc(cnt)) %>%
  filter(cnt > (length(corpus_crs_1)/100)) %>% # filter by frequency
  .$term

print_time_diff(start)

# myDict = unique(c(myDict, dict_lang))

## (3.c. english language only)
# save(dtm_crs_1, file = "dtm_crs_1_en_2021.Rds")

corpus_crs_0 <- preprocessingV(df_crs_0$description, language=language)


corpus_crs_0_simpleDTM <- DTM(corpus_crs_0)

freq_all0 <- corpus_crs_0_simpleDTM %>% 
  tidy() %>%
  group_by(term) %>%
  summarise(cnt = sum(count)) %>%
  ungroup() %>%
  mutate(total = sum(cnt)) %>%
  mutate(freq = cnt / total)
# beepr::beep(2)

freq_all_1_0 = right_join(freq_all0, freq_all1, by = "term") %>%
  mutate(odds = freq.x/freq.y) %>%
  arrange(desc(odds), desc(freq.y))

print_time_diff(start)

### filter standards
# freq_1 cut off
cutoff_freq1 = 0.0005 # is there a median we can take?
cutoff_odds = 10^(-1.5)
  
dict_tf_idf= freq_all_1_0 %>% 
  filter(term %in% eligible_words_in_doc_1) %>% 
  filter(#freq.y > cutoff_freq1, # the tf-idf keywords will be limited by the frequencies
         odds < cutoff_odds) %>%
  # slice(1:100) %>% 
  .$term
freq_all_1_0 %>%
  filter(term %in% dict_tf_idf) %>%
  mutate(terml =str_count(string = term, pattern = "\\S+") ) %>% 
  filter(terml==1) %>%
  data.frame() 

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

# # inspect(corpus_crs_0)
nwords0 = tidy(corpus_crs_0) %>%
  select(text, document = id) %>%
  mutate(total = str_count(string = text, pattern = "\\S+") ) %>%
  select(-text)

print_time_diff(start)

## ??? This is used before but should I correct after changing 00.4? 
# source("Code/00.4 refining keywords.R")

freq = DTM(corpus_crs_1, dict =myDict) %>%
  tidy %>%
  group_by(document) %>%
  summarise(count = sum(count))

nwords1 = tidy(corpus_crs_1) %>%
  select(text, document = id) %>%
  mutate(total = str_count(string = text, pattern = "\\S+") ) %>%
  select(-text)

nwords1 = nwords1 %>%
  left_join(freq) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  mutate(percentage = count/total) 


threshold = nwords1 %>%
  filter(count > 0) %>%
  .$percentage %>%
  mean

print(paste("Threshold:", threshold))
print("mean:")
mean(nwords1$percentage) %>% print()
print("median:")
median(nwords1$percentage) %>% print()

# 
# x = freq %>% row_sums()
# x1 = freq$i
# x2 = freq$j
# x3 = freq$v
# x4 = freq$nrow
# x5 = freq$ncol
# x6_1 = freq$dimnames$Docs
# x6_2 = freq$dimnames$Terms


dtm_crs_0 <- DTM(corpus_crs_0, dict=myDict)
# dtm_crs_0 <- DTM(corpus_crs_0, dict=keywords)

# a = tidy(dtm_crs_0) %>%
#   group_by(term) %>%
#   summarise(cnt = sum(count))
#   
#   
#   inner_join(nwords0) %>%
#   group_by(document) %>%
#   summarise(count = sum(count)) %>%
#   mutate(percentage  = count/total)

list_identified = tidy(dtm_crs_0) %>%
  group_by(document) %>%
  summarise(count = sum(count)) %>%
  inner_join(nwords0) %>%
  mutate(percentage  = count/total) %>%
  filter(percentage > threshold) %>%
  .$document %>%
  as.numeric

positive_text_id = df_crs_0 %>%
  mutate(document = 1:nrow(df_crs_0)) %>%
  filter(document %in% list_identified) %>%
  .$text_id 
beep()

df_crs_0 %>% filter(text_id %in% positive_text_id) %>% .$description %>% print
# a = df_crs_0 %>% 
#   select(text_id, description) %>% 
#   filter(text_id %in% positive_text_id)
# 
# b = df_crs_0 %>% 
#   select(text_id, description) %>% 
#   filter(text_id %in% positive_text_id1)
# 
# c = full_join(a, b, by = "text_id") %>%
#   filter(is.na(description.x)|is.na(description.y))
# c$description.x
positive_text_id <- positive_text_id[c(2:4, 6)]

# write_rds(positive_text_id, file = crs_path_new)

print_time_diff()
# time spent for 1/40 of projects: 1.416996 mins