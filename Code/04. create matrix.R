rm(list = ls())
crs_path <- "./Data/intermediate/crs03_1.rds"
crs_path_new <- "./Data/intermediate/positive_text_id.rds"
lang <-  "en"
language <- "english"
df_crs <- readRDS(crs_path)
df_crs_original <- df_crs

start_time = Sys.time()
source("code/00.2 functions_thilo.R")
source("code/00.3 functions_yu.R")

# enthreshold = 30
# frthreshold = 5
# esthreshold = 1

dict_lang <- wlistV(lang) %>% as.character()

# Split into to two groups
# 
# df_crs <- df_crs %>%
#   mutate(description = gsub("[.]", "", ))

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
  group_by(text_id) %>% 
  summarise(description = paste(text, collapse=". ")) %>%
  data.frame
df_crs_0 <- df_crs_0 %>% 
  group_by(text_id) %>% 
  summarise(description = paste(text, collapse=". ")) %>%
  data.frame
# 
# Warning messages:
#   1: In get(object, envir = currentEnv, inherits = TRUE) :
#   restarting interrupted promise evaluation
# 2: In doTryCatch(return(expr), name, parentenv, handler) :
#   restarting interrupted promise evaluation

## (3.b. english language only)



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
  filter(cnt > (length(corpus_crs_1)/100)) %>%
  .$term

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

### filter standards
# freq_1 cut off
cutoff_freq1 = 0.0005 # is there a median we can take?
cutoff_odds = 0.1
  
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

single_keywords_2rm = freq_all_1_0 %>%
  filter(term %in% dict_tf_idf) %>%
  mutate(terml =str_count(string = term, pattern = "\\S+") ) %>% 
  filter(terml==1) %>%
  data.frame() %>%
  .$term

dict_tf_idf_reduced <- dict_tf_idf[!dict_tf_idf%in% single_keywords_2rm] 
# saveRDS(dict_tf_idf, file = "Data/Intermediate/tf_idf_keywords.rds")
# find a cut-off rate
# freq_all_1_0 %>% 
#   filter(freq.y > cutoff_freq1) %>%
#   mutate(log_odds = log10(odds)) %>%
#   .$log_odds %>% 
#   hist()

common_words <- freq_all_1_0 %>% 
  # filter(freq.y > cutoff_freq1) %>% 
  filter(odds > 0.25, odds < 5) %>%
  .$term

myDict <- dtm_crs_1$dimnames$Terms 
dict_tf_idf %in% myDict
dict_tf_idf %in% common_words
myDict <- unique(c(myDict, dict_tf_idf))
myDict <- myDict[!(myDict %in% common_words)]


freq_all_1_0 %>% 
  filter(term %in% myDict) %>%
  print

# freq0 = DTM(corpus_crs_0, dict =myDict) %>%
#   tidy %>%
#   group_by(document) %>%
#   summarise(count = sum(count))
# 
# # inspect(corpus_crs_0)
# nwords0 = tidy(corpus_crs_0) %>%
#   select(text, document = id) %>%
#   mutate(total = str_count(string = text, pattern = "\\S+") ) %>%
#   select(-text)


# nwords[which(nwords != nwords0 )] 
# nwords0[which(nwords != nwords0 )]
# dtm_crs_0 <- DTM(corpus_crs_0, dict = myDict)
# list_high_freq_words_0 <- DTM(corpus_crs_0 , dict = dict_lang)
# a <- tidy(list_high_freq_words_0)
# a <- list_high_freq_words_0$dimnames$Docs

list_high_freq_words_0 <- DTM(corpus_crs_0 , Min=Min.0, Max=1)$dimnames$Terms %>% unique

# freq_all_1_0 %>%  
#   filter(term %in% list_high_freq_words_0) 

myDict = myDict[!(myDict %in% list_high_freq_words_0)]
# myDict = unique(c(myDict, dict_lang))

source("Code/00.4 refining keywords.R")

freq = DTM(corpus_crs_1, dict =myDict) %>%
  tidy %>%
  group_by(document) %>%
  summarise(count = sum(count))
# 
# freq = DTM(corpus_crs_1, dict =keywords) %>%
#   tidy %>%
#   group_by(document) %>%
#   summarise(count = sum(count))

nwords1 = tidy(corpus_crs_1) %>%
  select(text, document = id) %>%
  mutate(total = str_count(string = text, pattern = "\\S+") ) %>%
  select(-text)

# gsub("[.]", "", "household budget survey. household budget survey. to assist the design and implementation of the albania household budget survey. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ") %>% trimws
# 
# str_count("household budget survey. household budget survey. to assist the design and implementation of the albania household budget survey. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ", patter = "\\S+")

nwords1 = nwords1 %>%
  left_join(freq) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  mutate(percentage = count/total) 


threshold = nwords1 %>%
  filter(count > 0) %>%
  .$percentage %>%
  mean

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

saveRDS(positive_text_id, file = crs_path_new)

print(Sys.time() - start_time)
# time spent for 1/40 of projects: almost 2 minutes